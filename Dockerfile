# syntax=docker/dockerfile:1

FROM haskell:9.12.2@sha256:0dabe39ff7116832afd8219ce4ef9e7b35f5ba7bb779c83da83fa0195ee9c73f AS build

# hmatrix needs BLAS/LAPACK headers to build.
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libblas-dev=3.11.0-2 \
  liblapack-dev=3.11.0-2 \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /src

# The executable depends on the quarc package's own in-repo library, so `--only-dependencies`
# can't build without the sources present. Copy everything, then build in one
# shot. Release images are built on tags (rarely), so a full recompile is fine;
# the expensive base-image and apt layers above still cache across builds.
COPY . .

RUN cabal update \
  && cabal build --project-file=cabal.project.release exe:quarc \
  && cp "$(cabal list-bin --project-file=cabal.project.release exe:quarc)" /usr/local/bin/quarc

# Benchmark image (`docker build --target bench -t quarc-bench .`).
# Mount bench/results to keep the output.
FROM build AS bench

RUN apt-get update \
  && apt-get install -y --no-install-recommends python3 \
  && rm -rf /var/lib/apt/lists/*

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

RUN cabal build --project-file=cabal.project.release --builddir=dist-release \
  --enable-benchmarks bench:pipeline-bench

ENV HOME=/root
RUN git config --system --add safe.directory "*" \
  && chmod 755 /root \
  && chmod -R a+rwX /root/.cache /root/.local/state /src/dist-release /src/bench

ARG GIT_COMMIT
ENV QUARC_GIT_COMMIT=${GIT_COMMIT}

ENTRYPOINT ["python3", "bench/run_benchmarks.py"]
CMD []

FROM debian:trixie-slim@sha256:a29215f6a35e51e22adffa17f89e9d2ef06214e64a2bad10d765c46aea49f11f

# QUARC prints Unicode (diagnostic pretty-printer). Without a UTF-8 locale, GHC's
# stdout handle defaults to the C encoding and throws on non-ASCII output.
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

# Runtime shared libs for hmatrix + Graphviz `dot` for PNG/SVG rendering.
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libblas3=3.12.1-6 \
  liblapack3=3.12.1-6 \
  graphviz=2.42.4-3 \
  && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/bin/quarc /usr/local/bin/quarc

WORKDIR /work
ENTRYPOINT ["quarc"]
CMD ["--help"]
