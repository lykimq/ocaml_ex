(*
Using 'q' to quit the perf environment.

1. Set up perf_event: /proc/sys/kernel/perf_event_paranoid 
-1: allow use of (almost) all events by all users.

To allow non-root users to use perf on Linux, you may need to execute:

`echo 0 | sudo tee /proc/sys/kernel/perf_event_paranoid`

-------
usage: perf COMMAND

Profiling: perf record

- record: run a command and record its profile into perf.data
  + -F, --freq=: Profile at this frequency. Use max to use the currently maximum 
  allowed frequency, ie. the value in the kernel.perf_event_max_sample_rate sysctl.
  Will throttle down to the currently maximum allowed frequency.
  + -p:, --pid: record events on existing process ID (comma separte list)
  + -e, --e: select the PMU event (period, freq, time, call-graph, ...)
  + -g: enables call-graph (stack chain/backtrace) recording for both kernel space
    and user space

- report: Read perf.data (created by perf record) and display the profile
  $ sudo perf report 
  $ sudo perf report -n: show perf.data with a column for sample count
  $ sudo perf report --stdio: show perf.data as a text report, with data coalesced and percentages.
  $ sudo perf report --stdio -n -g folded: report, with stacks in folded format: one line
      per stack

- script: list all events from perf.data
  $ sudo perf script --header: list all perf.data events, with data header
  $ sudo perf script -D: Dump raw contents from perf.data as hex (for debugging)

  - annotate: disassemble and annotate instructions with percentages (needs some debuginfo)
  $ sudo annotate --stdio

Run examples:
Have to run at root: 

# Sample on-CPU functions for the specified command, at 99 Hertz:

output: 
$ sudo perf record -F 99 ~/ocaml_ex/src/_build/default/bin/profiling_perf/tests.exe
f_int 777 = 777
f_int32 777 = 777
f_int64 777 = 777

[ perf record: Woken up 1 times to write data ]
[ perf record: Captured and wrote 0,031 MB perf.data (5 samples) ]

# call-graph dwarf/lbr (last branch record)
sudo perf record -F 99 -a --call-graph dwarf  ~/ocaml_ex/src/_build/default/bin/profiling_perf/tests.exe
f_int 777 = 777
f_int32 777 = 777
f_int64 777 = 777

[ perf record: Woken up 1 times to write data ]
[ perf record: Captured and wrote 10,611 MB perf.data (40 samples) ]


--- using flamegraphs for export results from perf

# git clone https://github.com/brendangregg/FlameGraph 
# cd FlameGraph
# perf record --callgraph dwarf -- program-to-run program-arguments
  $ sudo perf  record --call-graph dwarf -- ~/ocaml_ex/src/_build/default/bin/profiling_perf/tests.exe
# perf script | ./stackcollapse-perf.pl | ./flamegraph.pl > perf-flamegraph.svg

Summary using perf and display flamegraph:
(https://www.brendangregg.com/perf.html#FlameGraphs)

Note: the rate for perf is 99 Hertz, the generated flame graph from a 1000 Hertz profile.
Choosing 99 Hertz instead of 100 Hertz, is to avoid accidentally sampling in 
lockstep with some periodic activity, which would produce skewed results.
You can also increase to higher rates (eg, up to 997 Hertz) for finer resolution.
Bear in mind that higher frequencies means higher overhead.

Go inside FlameGraph folder:
Sampling CPU stacks at 99 Hertz, for the entire system (-a, for all CPUs), with
stack traces (-g, for all graphs), for 60 seconds

$ sudo perf record -F 99 -ag -- sleep 60
$ sudo perf script | ./stackcollapse-perf.pl | ./flamegraph.pl > perf-flamegraph.svg
(or this can split into two and re-run ./flamgraph.pl multiple times)
- $ sudo perf  script | ./stackcollapse-perf.pl > out.perf-folded
- $ cat out.perf-folded | ./flamegraph.pl > perf-kernel.svg

Another example to use flame graph, before that already run perf record with graph (-g) to get the perf.data:
# perf script | ./stackcollapse-perf.pl > out.perf-folded
# grep -v cpu_idle out.perf-folded | ./flamegraph.pl > nonidle.svg
# grep ext4 out.perf-folded | ./flamegraph.pl > ext4internals.svg
# egrep 'system_call.*sys_(read|write)' out.perf-folded | ./flamegraph.pl > rw.svg

use perf report:
# perf report --stdio --no-children -n -g folded,0,caller,count -s comm | \
    awk '/^ / { comm = $3 } /^[0-9]/ { print comm ";" $2, $1 }'


---
Understand Flame Graphs 
It shows:
- x-axis: the sample population
- y-axis: stack depth

Each function (stack frame) is drawn as a rectangle, with the width relative to the
number of samples.

More advance on using flame graph:
https://www.brendangregg.com/FlameGraphs/offcpuflamegraphs.html#BlockIO


--- using eBPF profile

--- use speedscope.app
https://www.speedscope.app/
https://github.com/jlfwong/speedscope#usage

# https://github.com/jlfwong/speedscope/wiki/Importing-from-perf-(linux)
Use perf as a tool for profiling and read on speedscope:

$ sudo perf record -a -F 99 -g  ~/ocaml_ex/src/_build/default/bin/profiling_perf/tests.exe > perf.data
$ sudo perf script -i perf.data > profile.linux-perf.txt -f 

Then use : https://www.speedscope.app/ load the profile.linux-perf.txt file

--- Git of flamegraph 
https://github.com/brendangregg/FlameGraph
---
$ sudo perf report --stdio
$ sudo perf report -n

# using trace
sudo perf  record -R -- ~/ocaml_ex/src/_build/default/bin/profiling_perf/tests.exe
sudo perf sched latency: summarize scheduler latencies by task, including average and maximum delay





*)
let f_int n =
  let rec loop i sum =
    if i < n 
    then  loop (i + 1) (sum + 1)
    else sum 
  in 
  loop 0 0

let f_int32 n =
  let rec loop i sum =
    if i < n 
    then loop (i + 1) (Int32.add sum Int32.one)
    else sum
  in 
  Int32.to_int (loop 0 Int32.zero)

let f_int64 n =
  let rec loop i sum =
    if i < n 
    then loop (i + 1) (Int64.add sum Int64.one)
    else sum  
  in 
  Int64.to_int (loop 0 Int64.zero)   


(* Main function for benchmarks *)
let main =
  Printf.printf "f_int 777 = %d\n" (f_int 777);
  Printf.printf "f_int32 777 = %d\n" (f_int32 777);
  Printf.printf "f_int64 777 = %d\n" (f_int64 777);
  print_newline ()
