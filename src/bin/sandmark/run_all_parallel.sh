#!/bin/bash

# If using RUN_BENCH_TARGET=run_orunchrt the parallel benchmarks
# use `chrt -r 1`. You may need to setup permissions to allow the
# user to execute `chrt`. For example, this could be done with:
#   sudo setcap cap_sys_nice=ep /usr/bin/chrt
#

TAG='"macro_bench"' make run_config.json

USE_SYS_DUNE_HACK=1 \
                 RUN_BENCH_TARGET=run_orunchrt \
                 BUILD_BENCH_TARGET=bytebench \
                 RUN_CONFIG_JSON=run_config.json \
                 make ocaml-versions/5.0.0+stable.bench