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

Regenerates `doc/media/logo.svg`/`logo.png`/`logo-192.png`/
`favicon.png` from an original raster source: it traces the raster into an editable SVG via
`vtracer`, then a polish pass recolors/gradient-fills the traced badge shape and adds a shadowed
outline to the letter glyph, and finally rasterizes PNGs back out at each needed size. Only needed
if the logo itself changes -- see the script's own header comment for the full pipeline and its
tuning flags.
"""
import argparse
import math
import random
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

PATH_RE = re.compile(r'<path d="([^"]*)" fill="(#[0-9A-Fa-f]{6})" transform="([^"]*)"\s*/>')
NUMBER_RE = re.compile(r'-?\d+(?:\.\d+)?')

all_corners = {
    0: [
        (548.0, 467.0),
        (1115.0, 467.0),
        (1115.0, 1084.0),
        (548.0, 1084.0),
    ],
    1: [
        (422.98, 208.57),
        (623.64, 41.80),
        (764.62, 153.47),
        (564.77, 324.72),
    ],
    2: [
        (98.66, 478.12),
        (403.39, 224.85),
        (545.25, 341.45),
        (241.54, 601.72),
    ],
    3: [
        (584.29, 340.71),
        (784.02, 168.83),
        (921.96, 278.09),
        (723.10, 454.43),
    ],
    4: [
        (743.13, 470.84),
        (941.85, 293.85),
        (1158.53, 465.48),
        (961.34, 649.59),
    ],
    5: [
        (433.00, 470.91),
        (564.78, 357.50),
        (703.67, 471.66),
        (572.41, 588.06),
    ],
    6: [
        (261.22, 618.74),
        (412.83, 488.26),
        (552.32, 605.87),
        (401.23, 739.85),
    ],
    7: [
        (421.44, 757.34),
        (723.72, 488.13),
        (942.06, 667.59),
        (641.79, 947.94),
    ],
    8: [
        (99.48, 757.93),
        (240.29, 636.75),
        (690.08, 1027.20),
        (550.84, 1159.15),
    ],
}

# The "F" letter glyph, as a hand-specified 10-corner orthogonal polygon (a sans-serif F's
# outline: 10 vertices, edges alternating strictly horizontal/vertical) -- measured from
# test_run/Designer_logo.png (cv2.findContours + approxPolyDP on the biggest connected white
# blob inside the badge, i.e. the F itself, excluding the badge's own white outline ring), then
# snapped so every edge is exactly axis-aligned (the raw contour had a few 1-2px diagonal
# wobbles from antialiasing), then mapped from that image's own badge bounding box (purple-fill
# bbox x445-908, y386-891) into this canvas's badge bounding box (all_corners[0]'s corners,
# 548-1115 x, 467-1084 y) via a simple per-axis linear scale -- so it reproduces the designer
# reference's F proportions and position relative to the badge, in this canvas's own
# coordinate space. Used by rebuild_polygons in "hand-drawn" --letter-mode (the default); the
# original vtracer-traced glyph is still reachable via --letter-mode traced as a fallback/backup.
f_letter_corners = [
    (982.74, 563.52),
    (694.95, 563.52),
    (694.95, 993.59),
    (786.80, 993.59),
    (786.80, 827.43),
    (955.80, 827.43),
    (955.80, 746.79),
    (786.80, 746.79),
    (786.80, 650.27),
    (982.74, 650.27),
]


def rounded_polygon_d(points: list, radius: float) -> str:
    """Builds an SVG path `d` string tracing `points` (a closed polygon, in
    order) with each corner rounded off: the two edges meeting at a corner
    are each cut back by `radius` (clamped to half of the shorter adjacent
    edge, so radius can never overshoot past a neighboring corner), and a
    quadratic Bezier curve -- using the original sharp corner itself as the
    control point -- bridges the two cut-back points. Cheaper than fitting a
    true circular arc and visually smooth enough for this logo's tile/badge
    shapes."""
    n = len(points)

    def sub(a, b):
        return (a[0] - b[0], a[1] - b[1])

    def normalize(v):
        length = math.hypot(v[0], v[1])
        return (v[0] / length, v[1] / length) if length else (0.0, 0.0)

    entries, exits = [], []
    for i in range(n):
        prev_pt, curr, next_pt = points[(i - 1) % n], points[i], points[(i + 1) % n]
        d_prev = math.hypot(curr[0] - prev_pt[0], curr[1] - prev_pt[1])
        d_next = math.hypot(next_pt[0] - curr[0], next_pt[1] - curr[1])
        r = min(radius, d_prev / 2, d_next / 2)
        v_prev, v_next = normalize(sub(prev_pt, curr)), normalize(sub(next_pt, curr))
        entries.append((curr[0] + v_prev[0] * r, curr[1] + v_prev[1] * r))
        exits.append((curr[0] + v_next[0] * r, curr[1] + v_next[1] * r))

    parts = [f"M {entries[0][0]:.3f},{entries[0][1]:.3f}"]
    for i in range(n):
        curr = points[i]
        parts.append(f"Q {curr[0]:.3f},{curr[1]:.3f} {exits[i][0]:.3f},{exits[i][1]:.3f}")
        next_entry = entries[(i + 1) % n]
        parts.append(f"L {next_entry[0]:.3f},{next_entry[1]:.3f}")
    parts.append("Z")
    return " ".join(parts)


# Step 4 of feature_logo.md's improvement plan (revised twice -- first cycled 4 distinct wood-
# tone families across the 8 tiles, which didn't match the designer reference's own grouping;
# then a simpler 2-tone light/dark scheme, which was closer but still one shared tone per group).
# This is the final per-tile version: each of the 8 tiles gets its own individually-measured
# tone, keyed by all_corners index (1-8). Measured from test_run/Designer_logo.png: isolate each
# of its 8 wood-tile blobs (connected-component labeling on a brown-hue mask), erode each mask
# inward (~18px) before averaging its color so the erosion excludes the blob's own outer edge --
# that edge reads darker in the reference image from its own bevel/shadow rendering, not the
# board's actual surface tone -- then match each eroded blob to this canvas's all_corners tile
# index by comparing their centroids' relative position within each image's own tile-cluster
# bounding box.
TILE_TONES_BY_INDEX = {
    1: "#B2682D",
    2: "#BD7335",
    3: "#8E4C21",
    4: "#944C17",
    5: "#91522B",
    6: "#844925",
    7: "#7F3F16",
    8: "#A1571F",
}

# Step 5 of feature_logo.md's improvement plan: the badge gradient's own light/dark stops,
# measured directly from test_run/Designer_logo.png's badge (sampled ~50px in from its
# upper-left and lower-right corners, clear of the white outline and the "F" glyph) -- richer
# and more saturated than the previous scale_color(--badge-color, ...)-derived stops.
BADGE_GRAD_LIGHT = "#9C60C0"
BADGE_GRAD_DARK = "#341168"

WOOD_GRAIN_SEED = 123


def generate_wood_grain_paths(corners: list, seed_offset: int, flip_axis: bool = False) -> list:
    """Step 3 of feature_logo.md's improvement plan: a handful of thin, low-opacity, jittered
    polylines following a tile's own long axis, to be clipped to that tile's shape by the
    caller. The long/short axes are derived from the tile's own corners (its longest edge's
    direction is "along the grain"; the shortest edge's length is the tile's width) rather than
    a fixed rotation, so this works for any of the 8 tiles' own actual orientation. The
    line-generation math (jittered sine-wave polyline, dark-fiber/light-fiber opacity ranges) is
    adapted from the reference package's make_wood_tile() grain loop (see feature_logo.md) --
    only the math is reused, not that script's (wrong) tile positions/rotations. A fixed seed
    (WOOD_GRAIN_SEED, offset per tile index) keeps output deterministic across regenerations.
    `flip_axis` swaps which edge pair the grain follows (the shortest edge's direction becomes
    "along the grain" instead of the longest) -- tiles 5 and 6 are close enough to square that
    their longest-edge pick reads as running the "wrong" way; see rebuild_polygons's call site."""
    n = len(corners)
    edges = []
    for k in range(n):
        p0, p1 = corners[k], corners[(k + 1) % n]
        vec = (p1[0] - p0[0], p1[1] - p0[1])
        edges.append((math.hypot(*vec), vec))
    pick = min if flip_axis else max
    other = max if flip_axis else min
    long_len, long_vec = pick(edges, key=lambda e: e[0])
    short_len, _ = other(edges, key=lambda e: e[0])
    long_dir = (long_vec[0] / long_len, long_vec[1] / long_len)
    short_dir = (-long_dir[1], long_dir[0])
    cx = sum(p[0] for p in corners) / n
    cy = sum(p[1] for p in corners) / n

    rng = random.Random(WOOD_GRAIN_SEED + seed_offset)

    def to_abs(s, w):
        return (cx + long_dir[0] * s + short_dir[0] * w, cy + long_dir[1] * s + short_dir[1] * w)

    paths = []
    fibers = (
        (7, "#3A1D0C", (0.12, 0.24)),  # darker fiber lines
        (3, "#FFD08A", (0.08, 0.16)),  # a few lighter fibers keep the wood luminous
    )
    for count, color, (op_lo, op_hi) in fibers:
        for _ in range(count):
            s0 = -long_len / 2 * rng.uniform(0.7, 1.1)
            s1 = long_len / 2 * rng.uniform(0.7, 1.1)
            w0 = rng.uniform(-short_len * 0.4, short_len * 0.4)
            amp = rng.uniform(short_len * 0.01, short_len * 0.05)
            phase = rng.uniform(0, math.tau)
            opacity = rng.uniform(op_lo, op_hi)
            steps = 8
            pts = []
            for k in range(steps + 1):
                t = k / steps
                s = s0 + (s1 - s0) * t
                w = w0 + amp * math.sin(t * math.tau + phase)
                pts.append(to_abs(s, w))
            d = "M " + " L ".join(f"{x:.2f},{y:.2f}" for x, y in pts)
            paths.append(f'<path d="{d}" fill="none" stroke="{color}" stroke-opacity="{opacity:.3f}" '
                        f'stroke-width="1.3" stroke-linecap="round"/>')
    return paths


def build_badge_and_tile_defs(tile_colors: list, badge_color: str) -> str:
    """Builds the shared <defs> block (per-tile diagonal gradients, the badge's
    own gradient, and its drop-shadow filter) used by both the vtracer-trace
    polish pass (polish_svg) and the hand-specified-polygon rebuild
    (rebuild_polygons) -- kept as one function so the two paths can never
    drift apart in how a gradient/filter is defined."""
    defs = []
    for i, color in enumerate(tile_colors):
        light, dark = scale_color(color, 0.14), scale_color(color, -0.08)
        defs.append(
            f'<linearGradient id="tileGrad{i}" x1="0" y1="0" x2="1" y2="1">'
            f'<stop offset="0%" stop-color="{light}"/>'
            f'<stop offset="55%" stop-color="{color}"/>'
            f'<stop offset="100%" stop-color="{dark}"/>'
            f'</linearGradient>'
        )
    # Step 5 of feature_logo.md's improvement plan: a richer, more saturated badge gradient --
    # light/dark stops measured directly from test_run/Designer_logo.png's own badge pixels
    # (sampled ~50px in from its upper-left/lower-right corners, well clear of both the white
    # outline and the "F" glyph) rather than derived from --badge-color via scale_color, which
    # produced a more muted/desaturated spread than the reference's own deep, saturated purple.
    badge_light, badge_dark = BADGE_GRAD_LIGHT, BADGE_GRAD_DARK
    defs.append(
        f'<linearGradient id="badgeGrad" x1="0" y1="0" x2="1" y2="1">'
        f'<stop offset="0%" stop-color="{badge_light}"/>'
        f'<stop offset="55%" stop-color="{badge_color}"/>'
        f'<stop offset="100%" stop-color="{badge_dark}"/>'
        f'</linearGradient>'
    )
    defs.append(
        '<filter id="badgeShadow" x="-40%" y="-40%" width="180%" height="180%">'
        '<feDropShadow dx="11" dy="16" stdDeviation="13" flood-color="#000000" flood-opacity="0.50"/>'
        '</filter>'
    )
    # Step 1 of feature_logo.md's improvement plan: a soft, warm-dark (not pure black),
    # lower-right-offset drop shadow shared by all 8 tiles -- feDropShadow shadows the
    # filtered element itself (source graphic composited over its own blurred/offset alpha),
    # so no separate offset-duplicate shape is needed the way the badge/letter shadows use.
    # Kept deliberately subtler than badgeShadow (lower opacity/blur/offset) so the badge --
    # the topmost element -- still reads as casting the strongest shadow of the composition.
    defs.append(
        '<filter id="tileShadow" x="-60%" y="-60%" width="220%" height="220%">'
        '<feDropShadow dx="6" dy="8" stdDeviation="8" flood-color="#150A04" flood-opacity="0.35"/>'
        '</filter>'
    )
    return "<defs>\n" + "\n".join(defs) + "\n</defs>"


def rebuild_polygons(svg_text: str, all_corners: dict, tile_colors: list,
                     badge_color: str, tile_corner_radius: float, badge_corner_radius: float,
                     letter_dx: float = 0.0, letter_dy: float = 0.0,
                     letter_mode: str = "hand-drawn", f_letter_corners: list = None,
                     letter_corner_radius: float = 0.0) -> str:
    """Replaces an already-generated logo SVG's 9 traced badge/tile polygons
    (however they currently look -- flat or already gradient-polished) with
    fresh ones built from `all_corners`' hand-specified coordinates, rounded
    via rounded_polygon_d, while leaving the background untouched. Used when
    no raw raster source is available to rerun vtracer against -- only the
    already-committed SVG itself is, so this operates on that directly
    instead of on a fresh trace.

    The "F" letter glyph is handled one of two ways, per `letter_mode`:
    - "traced" (the original behavior): the vtracer-traced glyph path (and its
      shadow duplicate) is kept verbatim, only shifted by (letter_dx,
      letter_dy) to reposition it relative to the newly-rebuilt badge.
    - "hand-drawn" (the default): the traced glyph is dropped like the old
      badge/tiles, and a fresh letter is drawn from `f_letter_corners` (a
      hand-measured polygon -- see the module-level f_letter_corners's own
      comment) with the same shadow-duplicate treatment, appended after the
      badge so it renders on top. letter_dx/letter_dy still apply, as a plain
      shift of the polygon's own coordinates.
    Passing --letter-mode traced is the way back to the original glyph if
    the hand-drawn one ever needs reverting.

    A path is kept verbatim if it is near-white (background, and -- in
    "traced" letter mode only -- the letter glyph), or if it is the shadow
    duplicate immediately preceding a kept near-white path (shadow duplicates
    are always emitted as `d`-identical, `transform`-plus-"translate(7,9)",
    opacity="0.55" siblings right before the shape they shadow -- see
    polish_svg's own shadow-adding branches). Everything else (the old badge,
    its shadow, the 8 old tiles, any badge-edge/tile-edge trace artifact
    slivers, and -- in "hand-drawn" letter mode -- the old traced letter and
    its shadow) is dropped, and the new polygons are spliced in at the
    position of the first dropped path -- so the badge (and, in hand-drawn
    mode, the letter) still lands before any surviving near-white path in
    document order."""
    path_re = re.compile(r'<path\s[^>]*?/>')
    matches = list(path_re.finditer(svg_text))

    def attr(tag: str, name: str):
        m = re.search(rf'{name}="([^"]*)"', tag)
        return m.group(1) if m else None

    tags = [m.group(0) for m in matches]
    fills = [attr(t, "fill") for t in tags]
    ds = [attr(t, "d") for t in tags]
    transforms = [attr(t, "transform") for t in tags]
    opacities = [attr(t, "opacity") for t in tags]

    keep = [bool(f and f.startswith("#") and is_near_white(f)) for f in fills]
    for i in range(len(matches) - 1):
        is_shadow_shape = (opacities[i] == "0.55" and ds[i] == ds[i + 1]
                            and transforms[i] == f"{transforms[i + 1]} translate(7,9)")
        if is_shadow_shape and keep[i + 1]:
            keep[i] = True

    # The background is the near-white path with by far the largest bounding box; any other
    # kept near-white path is the "F" letter glyph, which -- along with its own shadow
    # duplicate, if kept alongside it -- either gets nudged by (letter_dx, letter_dy) to
    # reposition it relative to the newly-rebuilt badge (letter_mode "traced"), or is dropped
    # entirely so a hand-drawn replacement can take its place (letter_mode "hand-drawn").
    near_white_areas = [path_bbox_area(ds[i]) if keep[i] and is_near_white(fills[i]) else -1
                        for i in range(len(matches))]
    background_idx = near_white_areas.index(max(near_white_areas)) if matches else -1
    letter_idx = set()
    for i in range(len(matches)):
        if keep[i] and is_near_white(fills[i]) and i != background_idx:
            letter_idx.add(i)
            if i > 0 and opacities[i - 1] == "0.55" and ds[i - 1] == ds[i] and keep[i - 1]:
                letter_idx.add(i - 1)

    if letter_mode == "hand-drawn":
        shift_idx = set()
        for i in letter_idx:
            keep[i] = False
    else:
        shift_idx = letter_idx

    # Tiles are painted first (background layer), the badge last -- on top of any tile it
    # overlaps -- so its own shadow/outline read cleanly instead of being covered by a tile.
    #
    # Step 2 of feature_logo.md's improvement plan (revised approach -- the first attempt, a
    # thin light/dark bevel stroke along each tile's edges, didn't read well and was reverted):
    # each tile gets a solid dark-brown duplicate of its own exact shape, shifted down, painted
    # *before* (behind) the tile itself -- like the board has physical thickness and this is the
    # underside edge peeking out. Uses that tile's own fill color darkened (scale_color(...,
    # -0.5)) so the "thickness" tone stays in that tile's own light/dark family (Step 4).
    new_paths = []
    for i in range(1, 9):
        tile_d = rounded_polygon_d(all_corners[i], tile_corner_radius)
        thickness_color = scale_color(tile_colors[i - 1], -0.5)
        new_paths.append(f'<path d="{tile_d}" fill="{thickness_color}" transform="translate(0,8)"/>')
    for i in range(1, 9):
        tile_d = rounded_polygon_d(all_corners[i], tile_corner_radius)
        new_paths.append(f'<path d="{tile_d}" fill="url(#tileGrad{i - 1})" transform="translate(0,0)" '
                        f'filter="url(#tileShadow)"/>')
        # Step 3 of feature_logo.md's improvement plan: wood-grain fiber lines, clipped to this
        # tile's own shape so they never spill onto neighboring tiles or the white gaps.
        new_paths.append(f'<clipPath id="tileClip{i}"><path d="{tile_d}"/></clipPath>')
        grain_paths = generate_wood_grain_paths(all_corners[i], seed_offset=i, flip_axis=i in (5, 6))
        new_paths.append(f'<g clip-path="url(#tileClip{i})">' + "".join(grain_paths) + "</g>")

    badge_d = rounded_polygon_d(all_corners[0], badge_corner_radius)
    new_paths.append(
        f'<path d="{badge_d}" fill="{scale_color(badge_color, -0.35)}" '
        f'transform="translate(0,0) translate(7,9)" opacity="0.55"/>'
    )
    new_paths.append(
        f'<path d="{badge_d}" fill="url(#badgeGrad)" transform="translate(0,0)" '
        f'stroke="#FFFFFF" stroke-width="30" paint-order="stroke fill" '
        f'stroke-linejoin="round" filter="url(#badgeShadow)"/>'
    )

    if letter_mode == "hand-drawn":
        shifted_corners = [(x + letter_dx, y + letter_dy) for x, y in f_letter_corners]
        letter_d = rounded_polygon_d(shifted_corners, letter_corner_radius)
        new_paths.append(
            f'<path d="{letter_d}" fill="{scale_color(badge_color, -0.55)}" '
            f'transform="translate(0,0) translate(7,9)" opacity="0.55"/>'
        )
        new_paths.append(f'<path d="{letter_d}" fill="#F9F9FA" transform="translate(0,0)"/>')
    new_fragment = "\n".join(new_paths)

    pieces, pos, inserted = [], 0, False
    for i, m in enumerate(matches):
        pieces.append(svg_text[pos:m.start()])
        if keep[i]:
            tag = m.group(0)
            if i in shift_idx:
                tag = tag.replace(f'transform="{transforms[i]}"',
                                  f'transform="{transforms[i]} translate({letter_dx},{letter_dy})"')
            pieces.append(tag)
        elif not inserted:
            pieces.append(new_fragment)
            inserted = True
        pos = m.end()
    pieces.append(svg_text[pos:])
    new_svg = "".join(pieces)

    new_defs = build_badge_and_tile_defs(tile_colors, badge_color)
    new_svg = re.sub(r'<defs>.*?</defs>', new_defs, new_svg, count=1, flags=re.S)
    return new_svg


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

    tile_grad_id = {}
    sorted_tile_colors = sorted(tile_colors_needing_grad)
    for i, color in enumerate(sorted_tile_colors):
        tile_grad_id[color] = f"tileGrad{i}"
    new_defs_block = build_badge_and_tile_defs(sorted_tile_colors, badge_color)

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
    new_svg = svg_tag_re.sub(lambda m: m.group(1) + "\n" + new_defs_block, new_svg, count=1)

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
    parser.add_argument("input", type=Path, nargs="?",
                         help="source raster image (PNG/JPG/...); omit when using --from-svg")
    parser.add_argument("output", type=Path, help="destination .svg path")
    parser.add_argument("--from-svg", type=Path,
                         help="skip vtracer entirely and instead rebuild just the 9 hand-specified badge/tile "
                              "polygons (the all_corners table, indices 0=badge/1-8=tiles) into this existing "
                              "SVG, reusing its background and 'F' letter glyph paths verbatim -- for when no "
                              "raw raster source is available to retrace, only a previously generated SVG "
                              "(e.g. doc/media/logo.svg itself, passed as both --from-svg and output)")
    parser.add_argument("--tile-corner-radius", type=float, default=22.0,
                         help="pixel radius for rounding the 8 new tile polygons' corners (all_corners indices "
                              "1-8), used only with --from-svg (default: 22.0)")
    parser.add_argument("--badge-corner-radius", type=float, default=120.0,
                         help="pixel radius for rounding the new badge polygon's corners (all_corners index 0) "
                              "-- deliberately different from --tile-corner-radius, used only with --from-svg "
                              "(default: 120.0, ~21%% of the badge's side length)")
    parser.add_argument("--letter-mode", choices=["hand-drawn", "traced"], default="hand-drawn",
                         help="how to draw the 'F' letter glyph, used only with --from-svg: 'hand-drawn' (default) "
                              "draws it from the f_letter_corners polygon (measured off the designer reference "
                              "logo); 'traced' falls back to keeping the original vtracer-traced glyph path "
                              "verbatim (only shifted by --letter-dx/--letter-dy) -- a way back to the original "
                              "letter if the hand-drawn one ever needs reverting")
    parser.add_argument("--letter-corner-radius", type=float, default=0.0,
                         help="pixel radius for rounding the hand-drawn letter polygon's corners, used only with "
                              "--from-svg and --letter-mode hand-drawn (default: 0.0, sharp corners, matching the "
                              "designer reference logo's own blocky glyph)")
    parser.add_argument("--letter-dx", type=float, default=0.0,
                         help="horizontal shift applied to the 'F' letter glyph (and its shadow duplicate) to "
                              "reposition it relative to the rebuilt badge, used only with --from-svg -- applies "
                              "to either --letter-mode (default: 0.0)")
    parser.add_argument("--letter-dy", type=float, default=0.0,
                         help="vertical shift applied to the 'F' letter glyph (and its shadow duplicate) to "
                              "reposition it relative to the rebuilt badge, used only with --from-svg -- applies "
                              "to either --letter-mode (default: 0.0; the hand-drawn polygon is already mapped "
                              "from the designer reference logo's own badge-relative position, so it needs no "
                              "default nudge the way the traced glyph did)")
    default_tile_colors = ",".join(TILE_TONES_BY_INDEX[i] for i in range(1, 9))
    parser.add_argument("--tile-colors",
                         default=default_tile_colors,
                         help="comma-separated list of 8 hex colors assigned to the new tile polygons "
                              "(all_corners indices 1-8, in order), used only with --from-svg (default: Step 4 "
                              "of feature_logo.md's improvement plan -- TILE_TONES_BY_INDEX's own individually-"
                              "measured tone per tile, from test_run/Designer_logo.png)")
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
    parser.add_argument("--badge-color", default="#63358F",
                         help="the badge gradient's 55%% stop, and the base tone its shadow/letter-shadow "
                              "colors are darkened from (default: #63358F, Step 5 of feature_logo.md's "
                              "improvement plan -- linearly interpolated 55%% of the way between "
                              "BADGE_GRAD_LIGHT and BADGE_GRAD_DARK, both measured from "
                              "test_run/Designer_logo.png, so all three gradient stops are consistent with "
                              "a single measured gradient rather than a separately-chosen brand color)")
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

    if not args.input and not args.from_svg:
        print("error: provide either a raster 'input' to trace, or --from-svg to rebuild polygons "
              "into an existing SVG", file=sys.stderr)
        return 1
    if args.input and args.from_svg:
        print("error: pass only one of a raster 'input' or --from-svg, not both", file=sys.stderr)
        return 1

    args.output.parent.mkdir(parents=True, exist_ok=True)

    if args.from_svg:
        if not args.from_svg.is_file():
            print(f"error: --from-svg file not found: {args.from_svg}", file=sys.stderr)
            return 1
        tile_colors = args.tile_colors.split(",")
        if len(tile_colors) != 8:
            print(f"error: --tile-colors needs exactly 8 colors, got {len(tile_colors)}", file=sys.stderr)
            return 1
        svg_text = args.from_svg.read_text()
        svg_text = rebuild_polygons(svg_text, all_corners, tile_colors, args.badge_color,
                                    args.tile_corner_radius, args.badge_corner_radius,
                                    args.letter_dx, args.letter_dy, args.letter_mode,
                                    f_letter_corners, args.letter_corner_radius)
        args.output.write_text(svg_text)
    else:
        if not args.input.is_file():
            print(f"error: input file not found: {args.input}", file=sys.stderr)
            return 1

        try:
            import vtracer
        except ImportError:
            print("error: the 'vtracer' package is required but not installed.\n"
                  "Install it with: pip install vtracer", file=sys.stderr)
            return 1

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
