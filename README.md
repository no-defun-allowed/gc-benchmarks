# A smattering of garbage collector benchmarks

To be seen in *Parallel garbage collection for SBCL*. You will need two copies of
[SWCL](https://github.com/no-defun-allowed/swcl) to run this benchmark suite; the
harness client uses meters only accessible in SWCL (for measuring real and run time
of garbage collection, which differ in parallel settings).

## Running

Load `harness-server.lisp` and evaluate `(run-tests)` to run the tests. To avoid
having to wake up your computer monitor so often, providing a Discord webhook URL
in a file named `webhook-url` will send messages indicating the progress of running
the benchmark suite.

The Kandria harness must be run manually, because we haven't gotten [captures](http://shirakumo.github.io/trial/capture.html)
in Trial to work, and you'll need to play them manually. It writes out the files
`/tmp/kandria-frame.csv` and `/tmp/kandria-pause.csv` for frame and pause time
histograms.

## Credits

- `boehm-gc.lisp` is derived from the [cl-bench benchmark of the same name](https://gitlab.common-lisp.net/ansi-test/cl-bench/-/blob/master/files/boehm-gc.lisp) in the public domain.
- `gc-latency.lisp` is derived from the [gc-latency-experiment benchmarks](https://github.com/WillSewell/gc-latency-experiment/blob/master/common-lisp/main.lisp) [provided](https://github.com/WillSewell/gc-latency-experiment/issues/28) under the MIT license.
- `regrind.lisp` and `regrind-compiling.lisp` are derived from the [one-more-re-nightmare test suite](https://github.com/telekons/one-more-re-nightmare/blob/master/Tests/regrind.lisp) by the author, provided under the same license as one-more-re-nightmare.
- `kandria.lisp` is derived from [Trial](https://github.com/Shirakumo/trial) and [Kandria](https://github.com/Shirakumo/kandria), both under the Zlib license.

The benchmark harness (`harness-server.lisp` and `harness-client.lisp`) are provided
under the BSD 2-clause license, provided in the file `LICENSE`.
