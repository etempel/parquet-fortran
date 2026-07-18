#!/usr/bin/env python3
"""Traces a raster logo (PNG/JPG/...) into an SVG using vtracer, applies a
polish pass (dimensional gradients, a shadowed/outlined badge), and
rasterizes a PNG back out alongside it. This is the full pipeline behind
doc/media/logo.svg/.png/favicon.png -- rerun it against the original raster
source to regenerate all three from scratch.

Pipeline:
1. vtracer traces the raster input into flat-color paths (a rough, editable
   vector starting point -- not a pixel-perfect reproduction). Spline mode
   (the default) fits smooth curves, which is what gives tile corners their
   soft rounding (traced faithfully from the source raster's own anti-
   aliased corners) -- but at vtracer's own default settings it left two
   kinds of small artifact on edges that should be perfectly straight: a
   wobble along the middle of a side (fixed by raising --length-threshold
   to 16, vs. vtracer's own default 4.0 -- forces more aggressive
   simplification along edges without flattening corner rounding), and a
   small kink right at a tile's ~90-degree tip (fixed by raising
   --corner-threshold to 80, vs. vtracer's own default 60 -- a tip just
   above the default threshold made vtracer spline-fit a smooth curve
   through it instead of treating it as a corner). --max-iterations is
   also raised to 20 (vs. 10) for a better-converged fit. (Switching to
   polygon mode was tried first and rejected: it removes the mid-edge
   wobble but also miters every corner sharp, losing the rounded-corner
   look entirely.)
2. A polish pass then walks the traced paths and, without altering any
   path's shape (`d`/`transform` are untouched), classifies each one by
   its bounding-box area (robust across trace modes -- unlike a `d`
   string-length threshold, which meant something different again once
   the switch to polygon mode made every path's `d` much shorter) and
   upgrades its paint:
   - the largest near-white path (heuristic: all of R/G/B >= 235) is the
     full-canvas background and is left alone; any other sizeable
     near-white path is the "F" letter glyph and gets a subtle offset
     dark-purple shadow duplicate added behind it.
   - the path matching --source-badge-color exactly (vtracer's own traced
     color for the badge, before recoloring -- see --badge-color) gets
     recolored to --badge-color with a diagonal light/base/dark gradient,
     a thick white outline (via paint-order, so only the outward half of
     the stroke shows -- a clean halo, not an inset ring), and a
     drop-shadow filter -- plus its own dark-purple shadow duplicate.
   - any other path within --badge-family-distance of --source-badge-color
     (a badge-edge antialiasing sliver, traced as its own tiny flat-color
     path) is recolored to flat --badge-color too, so it doesn't sit on
     the recolored badge as a visibly mismatched stripe.
   - every other non-background path with a bounding-box area at least
     --tile-min-area (i.e. a real traced tile, not a few-pixel
     antialiasing sliver) gets its own diagonal light/base/dark gradient
     (a subtle dimensional/wood-sheen look). No stroke is added around
     tiles -- one was tried, for crisper seams, but a thin stroke follows
     a path's exact traced outline closely enough to make any remaining
     sub-pixel curve-fitting imperfection suddenly visible as a hard-edged
     step, even where the filled shape alone looked perfectly smooth.
   - anything else (small area, not badge-family) is a trace artifact too
     small to bother enhancing, and is left untouched.
   Pass --no-polish to skip this step and keep vtracer's raw flat-color trace.

Requires the `vtracer` package (not a project dependency -- install with
`pip install vtracer` before running this script).

Usage:
    tools/generate_logo_svg.py logo.png doc/media/logo.svg
    tools/generate_logo_svg.py logo.png doc/media/logo.svg --color-precision 4 --filter-speckle 8
    tools/generate_logo_svg.py logo.png doc/media/logo.svg --png-size 512
    tools/generate_logo_svg.py logo.png doc/media/logo.svg --mode spline
    tools/generate_logo_svg.py logo.png doc/media/logo.svg --no-polish
    tools/generate_logo_svg.py logo.png doc/media/logo.svg --no-png

A PNG is written alongside the SVG (same path, `.png` suffix) by default,
rasterized from the just-generated SVG -- not resized from the original
raster input -- so it reflects whatever the SVG actually contains. A second,
fixed 192x192 PNG (`<stem>-192.png`, e.g. `logo-192.png` -- a common PWA/
Android home-screen icon size) is also written by default; pass
--no-icon192 to skip it, or --icon192-size to change its size. PNG
rasterization currently only works on macOS (uses the built-in `qlmanage`
QuickLook thumbnailer -- no extra package needed); pass `--no-png` and
rasterize manually (e.g. `rsvg-convert`, Inkscape, a browser screenshot) on
other platforms. For the FORD favicon specifically, a further 32x32 PNG is
needed (see CLAUDE.md's "Publishing" notes) -- resize the generated PNG down.
"""
import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

PATH_RE = re.compile(r'<path d="([^"]*)" fill="(#[0-9A-Fa-f]{6})" transform="([^"]*)"\s*/>')
NUMBER_RE = re.compile(r'-?\d+(?:\.\d+)?')


def scale_color(hex_color: str, factor: float) -> str:
    """factor > 0 lightens toward white, factor < 0 darkens toward black."""
    r, g, b = int(hex_color[1:3], 16), int(hex_color[3:5], 16), int(hex_color[5:7], 16)
    if factor >= 0:
        r += (255 - r) * factor
        g += (255 - g) * factor
        b += (255 - b) * factor
    else:
        f = -factor
        r *= (1 - f)
        g *= (1 - f)
        b *= (1 - f)
    return f"#{int(r):02X}{int(g):02X}{int(b):02X}"


def is_near_white(hex_color: str) -> bool:
    r, g, b = int(hex_color[1:3], 16), int(hex_color[3:5], 16), int(hex_color[5:7], 16)
    return r >= 235 and g >= 235 and b >= 235


def color_distance(a: str, b: str) -> float:
    ar, ag, ab = int(a[1:3], 16), int(a[3:5], 16), int(a[5:7], 16)
    br, bg, bb = int(b[1:3], 16), int(b[3:5], 16), int(b[5:7], 16)
    return ((ar - br) ** 2 + (ag - bg) ** 2 + (ab - bb) ** 2) ** 0.5


def path_bbox_area(d: str) -> float:
    """Rough bounding-box area of a path's `d`, from every (x, y) coordinate
    pair appearing in it (commands here always emit coordinates in x, y
    pairs, whether M/L -- one pair -- or C -- three pairs -- so this holds
    regardless of trace mode). Good enough to separate a real traced tile
    from a few-pixel antialiasing sliver; not a substitute for a real SVG
    path parser."""
    nums = [float(n) for n in NUMBER_RE.findall(d)]
    xs, ys = nums[0::2], nums[1::2]
    if not xs or not ys:
        return 0.0
    return (max(xs) - min(xs)) * (max(ys) - min(ys))


def polish_svg(svg_text: str, source_badge_color: str, badge_color: str, tile_min_area: float,
               badge_family_distance: float) -> str:
    matches = list(PATH_RE.finditer(svg_text))

    near_white_matches = [m for m in matches if is_near_white(m.group(2))]
    # The background is the near-white path with by far the largest bounding
    # box (it covers the whole canvas); any other sizeable near-white path is
    # the "F" letter glyph, not background, and gets a shadow duplicate below.
    background_match = max(near_white_matches, key=lambda m: path_bbox_area(m.group(1)), default=None)

    def is_badge_family(color: str) -> bool:
        return color == source_badge_color or color_distance(color, source_badge_color) <= badge_family_distance

    tile_colors_needing_grad = {
        m.group(2) for m in matches
        if not is_near_white(m.group(2))
        and not is_badge_family(m.group(2))
        and path_bbox_area(m.group(1)) >= tile_min_area
    }

    defs = []
    tile_grad_id = {}
    for i, color in enumerate(sorted(tile_colors_needing_grad)):
        gid = f"tileGrad{i}"
        tile_grad_id[color] = gid
        light, dark = scale_color(color, 0.14), scale_color(color, -0.08)
        defs.append(
            f'<linearGradient id="{gid}" x1="0" y1="0" x2="1" y2="1">'
            f'<stop offset="0%" stop-color="{light}"/>'
            f'<stop offset="55%" stop-color="{color}"/>'
            f'<stop offset="100%" stop-color="{dark}"/>'
            f'</linearGradient>'
        )

    badge_light, badge_dark = scale_color(badge_color, 0.30), scale_color(badge_color, -0.22)
    defs.append(
        f'<linearGradient id="badgeGrad" x1="0" y1="0" x2="1" y2="1">'
        f'<stop offset="0%" stop-color="{badge_light}"/>'
        f'<stop offset="55%" stop-color="{badge_color}"/>'
        f'<stop offset="100%" stop-color="{badge_dark}"/>'
        f'</linearGradient>'
    )
    defs.append(
        '<filter id="badgeShadow" x="-40%" y="-40%" width="180%" height="180%">'
        '<feDropShadow dx="7" dy="10" stdDeviation="8" flood-color="#000000" flood-opacity="0.22"/>'
        '</filter>'
    )

    n_tiles_enhanced = 0
    n_artifacts_skipped = 0
    badge_seen = False

    def replace(m: re.Match) -> str:
        nonlocal n_tiles_enhanced, n_artifacts_skipped, badge_seen
        d, fill, transform = m.groups()

        if is_near_white(fill):
            if m is not background_match and path_bbox_area(d) >= tile_min_area:
                # the "F" letter glyph: add a subtle offset dark-purple shadow behind it
                shadow = (
                    f'<path d="{d}" fill="{scale_color(badge_color, -0.55)}" '
                    f'transform="{transform} translate(7,9)" opacity="0.55"/>\n'
                )
                return shadow + m.group(0)
            return m.group(0)

        if fill == source_badge_color:
            badge_seen = True
            shadow = (
                f'<path d="{d}" fill="{scale_color(badge_color, -0.35)}" '
                f'transform="{transform} translate(7,9)" opacity="0.55"/>\n'
            )
            badge = (
                f'<path d="{d}" fill="url(#badgeGrad)" transform="{transform}" '
                f'stroke="#FFFFFF" stroke-width="30" paint-order="stroke fill" '
                f'stroke-linejoin="round" filter="url(#badgeShadow)"/>'
            )
            return shadow + badge

        if is_badge_family(fill):
            # a small badge-edge antialiasing sliver, traced as its own flat-color path --
            # recolor to match the badge gradient's own base tone so it doesn't sit on the
            # recolored badge as a visibly mismatched stripe; no separate outline/shadow of
            # its own (the main badge path underneath already carries those).
            return f'<path d="{d}" fill="{badge_color}" transform="{transform}"/>'

        if path_bbox_area(d) >= tile_min_area:
            n_tiles_enhanced += 1
            gid = tile_grad_id[fill]
            return f'<path d="{d}" fill="url(#{gid})" transform="{transform}"/>'

        n_artifacts_skipped += 1
        return m.group(0)

    new_svg = PATH_RE.sub(replace, svg_text)
    if not badge_seen:
        print(f"warning: no path matched --source-badge-color {source_badge_color}; "
              "badge was not recolored/enhanced -- check the raw trace's actual badge color", file=sys.stderr)

    svg_tag_re = re.compile(r'(<svg[^>]*>)')
    new_svg = svg_tag_re.sub(lambda m: m.group(1) + "\n<defs>\n" + "\n".join(defs) + "\n</defs>", new_svg, count=1)

    print(f"polish: enhanced {n_tiles_enhanced} tile path(s), left {n_artifacts_skipped} tiny artifact path(s) untouched")
    return new_svg


def rasterize_png(svg_path: Path, png_path: Path, size: int) -> bool:
    """Rasterizes svg_path to png_path at up to size x size pixels, via macOS's
    QuickLook thumbnailer (qlmanage). Returns True on success, False if
    unavailable (e.g. not running on macOS) -- prints guidance either way."""
    if shutil.which("qlmanage") is None:
        print("note: PNG rasterization needs macOS's 'qlmanage' (not found on this platform).\n"
              f"Rasterize {svg_path} manually instead, e.g.:\n"
              f"    rsvg-convert -w {size} -h {size} {svg_path} -o {png_path}",
              file=sys.stderr)
        return False

    with tempfile.TemporaryDirectory() as tmpdir:
        result = subprocess.run(
            ["qlmanage", "-t", "-s", str(size), "-o", tmpdir, str(svg_path)],
            capture_output=True, text=True,
        )
        produced = Path(tmpdir) / f"{svg_path.name}.png"
        if result.returncode != 0 or not produced.is_file():
            print(f"warning: qlmanage failed to rasterize {svg_path}:\n{result.stderr}", file=sys.stderr)
            return False
        png_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(produced, png_path)
    return True


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("input", type=Path, help="source raster image (PNG/JPG/...)")
    parser.add_argument("output", type=Path, help="destination .svg path")
    parser.add_argument("--colormode", choices=["color", "binary"], default="color",
                         help="color trace vs. black/white only (default: color)")
    parser.add_argument("--hierarchical", choices=["stacked", "cutout"], default="stacked",
                         help="layer stacking strategy for overlapping shapes (default: stacked)")
    parser.add_argument("--mode", choices=["spline", "polygon", "none"], default="spline",
                         help="curve fitting: smooth splines, straight polygons, or pixel-exact (default: spline)")
    parser.add_argument("--filter-speckle", type=int, default=4,
                         help="discard specks up to this many pixels (default: 4)")
    parser.add_argument("--color-precision", type=int, default=6,
                         help="number of significant bits per color channel when clustering colors (default: 6)")
    parser.add_argument("--layer-difference", type=int, default=16,
                         help="color difference threshold between adjacent layers (default: 16)")
    parser.add_argument("--corner-threshold", type=int, default=80,
                         help="angle (degrees) below which a point is treated as a sharp corner (default: 80, "
                              "higher than vtracer's own default of 60 -- a tile's ~90-degree tip tan being "
                              "just above the default threshold meant vtracer tried to spline-fit a smooth "
                              "curve through it instead of treating it as a corner, producing a visible kink "
                              "right at the tip; raising the threshold restores a clean point there)")
    parser.add_argument("--length-threshold", type=float, default=16.0,
                         help="minimum segment length when simplifying splines (default: 16.0, higher than "
                              "vtracer's own default of 4.0 -- removes edge-wobble/step artifacts on "
                              "otherwise-straight tile sides while still leaving corners softly rounded; "
                              "8.0 was tried first and still left a visible step on one edge)")
    parser.add_argument("--max-iterations", type=int, default=20,
                         help="max spline-fitting iterations (default: 20, higher than vtracer's own default "
                              "of 10 -- a better-converged fit along tile edges)")
    parser.add_argument("--splice-threshold", type=int, default=45,
                         help="angle (degrees) threshold for splicing spline segments (default: 45)")
    parser.add_argument("--path-precision", type=int, default=3,
                         help="decimal digits of precision in the output path coordinates (default: 3)")
    parser.add_argument("--no-polish", action="store_true",
                         help="skip the gradient/shadow/outline polish pass, keeping vtracer's raw flat-color trace")
    parser.add_argument("--source-badge-color", default="#613B93",
                         help="the badge's own fill color as vtracer traces it from the raster source "
                              "(default: #613B93, this repo's original AI-generated logo's purple)")
    parser.add_argument("--badge-color", default="#734F96",
                         help="color to recolor the badge to, with gradient+outline+shadow applied "
                              "(default: #734F96, the Fortran brand purple)")
    parser.add_argument("--tile-min-area", type=float, default=400.0,
                         help="minimum path bounding-box area (in the SVG's own coordinate units) to be "
                              "treated as a real tile rather than a trace artifact sliver (default: 400.0)")
    parser.add_argument("--badge-family-distance", type=float, default=70.0,
                         help="RGB distance within which a color counts as a badge-edge antialiasing sliver "
                              "(traced as its own separate flat-color path) rather than an unrelated tile/"
                              "artifact, and gets recolored to --badge-color too (default: 70.0)")
    parser.add_argument("--no-png", action="store_true",
                         help="skip rasterizing a .png alongside the .svg")
    parser.add_argument("--png-size", type=int, default=1024,
                         help="max width/height in pixels for the rasterized .png (default: 1024)")
    parser.add_argument("--no-icon192", action="store_true",
                         help="skip rasterizing the extra 192x192 <stem>-192.png (e.g. for PWA/Android icons)")
    parser.add_argument("--icon192-size", type=int, default=192,
                         help="width/height in pixels for the extra <stem>-192.png icon (default: 192)")
    return parser.parse_args()


def main():
    args = parse_args()

    if not args.input.is_file():
        print(f"error: input file not found: {args.input}", file=sys.stderr)
        return 1

    try:
        import vtracer
    except ImportError:
        print("error: the 'vtracer' package is required but not installed.\n"
              "Install it with: pip install vtracer", file=sys.stderr)
        return 1

    args.output.parent.mkdir(parents=True, exist_ok=True)

    vtracer.convert_image_to_svg_py(
        str(args.input),
        str(args.output),
        colormode=args.colormode,
        hierarchical=args.hierarchical,
        mode=args.mode,
        filter_speckle=args.filter_speckle,
        color_precision=args.color_precision,
        layer_difference=args.layer_difference,
        corner_threshold=args.corner_threshold,
        length_threshold=args.length_threshold,
        max_iterations=args.max_iterations,
        splice_threshold=args.splice_threshold,
        path_precision=args.path_precision,
    )

    if not args.no_polish:
        svg_text = args.output.read_text()
        svg_text = polish_svg(svg_text, args.source_badge_color, args.badge_color, args.tile_min_area,
                              args.badge_family_distance)
        args.output.write_text(svg_text)

    print(f"wrote {args.output}")

    if not args.no_png:
        png_path = args.output.with_suffix(".png")
        if rasterize_png(args.output, png_path, args.png_size):
            print(f"wrote {png_path}")

        if not args.no_icon192:
            icon192_path = args.output.with_name(args.output.stem + "-192.png")
            if rasterize_png(args.output, icon192_path, args.icon192_size):
                print(f"wrote {icon192_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
