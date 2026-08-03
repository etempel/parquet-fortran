#!/usr/bin/env bash
# Builds the cached "test" job environment image -- everything .gitlab-ci.yml's
# before_script installs (gfortran/gcc/g++, fpm via the official bootstrap,
# Arrow/Parquet C++ dev packages), baked into one image tagged
# parquet-fortran-ci-test:latest.
#
# Uses `docker run --platform linux/amd64` + `docker commit` rather than
# `docker build --platform ...`: this machine's docker CLI only has the legacy
# builder (no buildx plugin), which silently IGNORES --platform and builds for
# the host's native arch instead -- confirmed directly (a build tagged
# --platform linux/amd64 still reported `uname -m` == aarch64 on this Mac). That
# defeats the whole point (see the pipx/fpm note below), so this script drives a
# real amd64 container step by step and commits the result instead of going
# through `docker build` at all. If buildx is ever installed
# (MacPorts: `docker-buildx-plugin`), this could be simplified back into a
# Dockerfile -- not necessary today.
#
# --platform linux/amd64 itself is required, not cosmetic: the "fpm" PyPI package
# has no prebuilt wheel for linux/arm64, so on an arm64 Docker host (e.g. Colima on
# Apple Silicon) `pipx install fpm` falls back to building fpm from source via
# CMake -- which fails with a toml-f/jonquil CMake "add_library ... already
# exists" error, unrelated to this project. amd64 (matching the real GitLab
# runner's architecture) gets the prebuilt wheel instead.
#
# Run this once, and again whenever this script's install steps or .gitlab-ci.yml's
# "test" job before_script changes -- NOT before every test run. Once built, use
# tools/run_ci_test_image.sh to run something against it, which skips this whole
# install step and starts in seconds.
#
# Usage: tools/build_ci_test_image.sh
# Run from the parquet-fortran repo root (or anywhere -- it cds to the repo root itself).

set -euo pipefail

IMAGE_NAME="${IMAGE_NAME:-parquet-fortran-ci-test:latest}"
CONTAINER_NAME="parquet-fortran-ci-test-build-$$"

cleanup() {
  docker rm -f "${CONTAINER_NAME}" >/dev/null 2>&1 || true
}
trap cleanup EXIT

docker run \
  --platform linux/amd64 \
  --name "${CONTAINER_NAME}" \
  -e DEBIAN_FRONTEND=noninteractive \
  ubuntu:24.04 \
  bash -euxc '
    # ---- before_script (verbatim from .gitlab-ci.yml, minus git/git-lfs setup
    # that only matters once a repo is mounted at run time) ----
    apt-get update -y
    apt-get upgrade -y
    apt-get install -y --no-install-recommends ca-certificates lsb-release wget gnupg \
      gfortran gcc g++ make git git-lfs

    # Prefer a newer Flang on Ubuntu 24.04 amd64 when available.
    if apt-get install -y --no-install-recommends flang-20; then
      echo "Installed flang-20"
    elif apt-get install -y --no-install-recommends flang-19; then
      echo "Installed flang-19"
    else
      apt-get install -y --no-install-recommends flang
      echo "Installed distro default flang"
    fi

    # Normalize command name so callers can always run `flang`.
    if command -v flang-20 >/dev/null 2>&1; then
      ln -sf "$(command -v flang-20)" /usr/local/bin/flang
    elif command -v flang-19 >/dev/null 2>&1; then
      ln -sf "$(command -v flang-19)" /usr/local/bin/flang
    elif command -v flang-new >/dev/null 2>&1; then
      ln -sf "$(command -v flang-new)" /usr/local/bin/flang
    fi

    apt-get install -y python3-venv python3-pip
    python3 -m pip install --no-cache-dir --break-system-packages pipx
    export PATH=/root/.local/bin:$PATH
    python3 -m pipx ensurepath

    echo "Installing fpm via the official source bootstrap installer from the fpm docs."
    rm -rf /tmp/fpm-src
    git clone --depth 1 https://github.com/fortran-lang/fpm /tmp/fpm-src
    cd /tmp/fpm-src
    ./install.sh --prefix=/usr/local
    # Installing fpm via pipx is not recommended because the PyPI package is not
    # maintained by the fpm team and is often out of date.
    #pipx install fpm

    pipx install "gcovr>=7.1,<8.4"

    echo "Installing Intel oneAPI compiler-fortran package from Intel apt repository so ifx is available in the image too."
    # Add Intel oneAPI apt repository so ifx is available in the image too.
    # Intel historical key URLs returned 404 in this environment; use a
    # trusted repo entry so apt can still consume the oneAPI feed.
    echo "deb [trusted=yes] https://apt.repos.intel.com/oneapi all main" \
        >/etc/apt/sources.list.d/intel-oneapi.list
    apt-get update -y
    ACCEPT_EULA=accept apt-get install -y intel-oneapi-compiler-fortran

    # Load full oneAPI environment in this build shell.
    # oneAPI setvars.sh reads optional vars that may be unset, which is
    # incompatible with nounset (-u). Relax -u only for this call.
    set +u
    source /opt/intel/oneapi/setvars.sh
    set -u

    # Auto-load full oneAPI environment in future interactive bash sessions.
    printf "%s\n" "source /opt/intel/oneapi/setvars.sh" >/etc/profile.d/oneapi.sh
    chmod 0644 /etc/profile.d/oneapi.sh

    command -v ifx
    ifx --version | head -n 1

    echo "Installing Arrow/Parquet C++ dev packages from Arrow apt repository so fpm can build against them."
    # Install the Arrow/Parquet C++ library from Arrow apt repository.
    distro_id="$(lsb_release --id --short | tr 'A-Z' 'a-z')"
    distro_codename="$(lsb_release --codename --short)"
    wget "https://packages.apache.org/artifactory/arrow/${distro_id}/apache-arrow-apt-source-latest-${distro_codename}.deb"
    apt-get install -y -V "./apache-arrow-apt-source-latest-${distro_codename}.deb"
    apt-get update -y
    apt-get install -y -V libarrow-dev libarrow-compute-dev libparquet-dev
    rm -f ./apache-arrow-apt-source-latest-*.deb
  '

# --change applies Dockerfile-style instructions to the committed image without
# a rebuild -- this is what makes `fpm` resolvable by bare name in a later
# `docker run` against the committed image (the export above only lasted for
# that one RUN's shell.
docker commit \
  --change 'ENV PATH=/root/.local/bin:/opt/intel/oneapi/compiler/latest/bin:${PATH}' \
  --change 'WORKDIR /builds/parquet-fortran' \
  "${CONTAINER_NAME}" "${IMAGE_NAME}"

echo "Built ${IMAGE_NAME}"
