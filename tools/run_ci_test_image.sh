#!/usr/bin/env bash
# Runs a command inside the cached CI-environment image that
# tools/build_ci_test_image.sh builds (parquet-fortran-ci-test:latest, override
# with IMAGE_NAME) -- the fast half of the pair: building the image installs a
# whole toolchain and takes minutes, running against it starts in seconds.
#
# On its own it prints the image's toolchain versions (fpm, Arrow/Parquet,
# gfortran, flang, ifx) and does nothing else. That version banner IS the point
# for the common case -- confirming what a given image actually contains, which
# is otherwise guesswork once an image has been sitting around for a while --
# and the marked placeholder below it is where a one-off command goes when you
# want to run something in that environment by hand.
#
# Two things it deliberately does NOT do:
#
#   * It does not mount this repository into the container. A bind mount would
#     let `fpm test` run against the working tree, but the container is amd64
#     (see build_ci_test_image.sh's header for why that is required) while the
#     host may not be, so both would then be writing objects of different
#     architectures into the same build/ tree -- exactly the stale-cache trap
#     CLAUDE.md warns about, and one that presents as a baffling link error
#     rather than as a mount problem. Add `-v` here only together with a
#     separate FPM_BUILD_DIR for the container, and read that note first.
#   * It does not build the image. If it is missing this fails with a pointer
#     to the script that builds it, rather than silently pulling or rebuilding
#     something that takes minutes.
#
# oneAPI's setvars.sh is sourced with `set +u` around it because it reads
# optional variables that may be unset, which `set -u` treats as fatal -- the
# same accommodation build_ci_test_image.sh makes for the same call.
#
# Usage: tools/run_ci_test_image.sh
#   IMAGE_NAME=other-image:tag tools/run_ci_test_image.sh
#
# Run from anywhere -- it cds to the repo root itself.

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

IMAGE_NAME="${IMAGE_NAME:-parquet-fortran-ci-test:latest}"

if ! docker image inspect "${IMAGE_NAME}" >/dev/null 2>&1; then
	echo "run_ci_test_image.sh: image '${IMAGE_NAME}' not found." >&2
	echo "Build it first with: tools/build_ci_test_image.sh" >&2
	exit 1
fi

docker run --rm --platform linux/amd64 "${IMAGE_NAME}" bash -lc '
set -uo pipefail

if [ -f /opt/intel/oneapi/setvars.sh ]; then
	set +u
	source /opt/intel/oneapi/setvars.sh >/dev/null 2>&1 || true
	set -u
fi

# Print versions of the key tools in this image
echo -e "\nfpm:"
fpm --version || echo "fpm: not found"
echo -e "\nArrow:"
dpkg-query -W -f='"'"'${Version}'"'"' libarrow-dev || echo "arrow: not installed"
echo -e "\nParquet:"
dpkg-query -W -f='"'"'${Version}'"'"' libparquet-dev || echo "parquet: not installed"
echo -e "\n\nGFortran:"
gfortran --version || echo "gfortran: not found"
echo -e "\nFlang:"
flang --version || echo "flang: not found"
echo -e "\nIfx:"
ifx --version || echo "ifx: not found"

# Place your actual script below this line
echo "Nothing to do yet..."

'
