Each of the sub-directories are described here.

* cbench/
=============================================

This directory contains the full benchmark suite presented in
the paper

  Sinn, M., Zuleger, F., Veith, H.: Difference constraints:
  An adequate abstraction for complexity analysis of imperative programs.
  In FMCAD'15

It also contains a script that can conveniently used to run
our tool on all the functions of the benchmark.  The results
of running this script on my development machine with a 3-second
timeout are in the file

    cbench/results3s.txt


~ Querying the results
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One convenient way to query the results is to use grep:

  + Find the number of functions bounded:
        grep OK cbench/results3s.txt | wc -l

  + Find the number of constant bounds:
        grep 'Degree: 0' cbench/results3s.txt | wc -l


~ Running the experiments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
You can re-run the experiments in the VM with the commands

    cd cbench/                    # Must be in cbench/
    sh runall.sh > resultsVM.txt

You will likely have less results than what there is in
cbench/results3s.txt since the virtual machine runs more
slowly than the computer I used to get these results.

/!\ NOTE:  It is normal to see plenty of warnings when
running the experiments.  This is because the version of
LLVM in this virtual machine differs from the one used to
create the benchmark.


* generated_coq/
=============================================

Each successful run of our analysis on the cbench benchmark
generates one .v Coq file in generated_coq/.

I already run Coq on the .v files generated (it takes a
long time in the virtual machine).  To see how many bounds
were successfully checked you can use

    ls generated_coq/*.vo | wc -l  # I get 421

Instead of checking for yourself by removing all .vo files
and re-compiling everything, you can check only 10 bounds
at random

    cd generated_coq/
    make random

This should not take more than a few minutes, and should
work with the 2G of RAM of this VM.

/!\ NOTE:  If you attempt to recompile all .vo files, some
errors will be displayed.  This is to be expected: not all
bounds can be checked because of the inexactness of
floating-point numbers the LP solver provides.


* playground/
=============================================

Some smaller examples are present in this directory.  They
use a Python-like syntax I used to develop our tool.  In
all the examples, the variable "z" is bounded at the end of
the "start" function (or the only function available).

The workflow for one example is the following:

    cd playground/
    cat hello.imp
    analyze hello.imp  # Should print a bound and
                       # generate generated_coq.v

    emacs generated_coq.v

At the very end of the generated_coq.v file is the theorem
expressing the bound.  In this case:

    Theorem bound_valid:
      forall s1 s2, steps P_f (proc_start P_f) s1 (proc_end P_f) s2 ->
        (s2 V_f_z <= (13 # 4))%Q.

It can be read as such: If the procedure "f" starts in
initial state s1 and finishes in final state s2, then:

    z (in s2) <= (13/4)

That is, 3.25 is bound on the final value of the variable "z".

To check that this theorem is true in emacs, use CTRL-c CTRL-b,
this asks Coq to check the whole file.


Examples available (more details in the files):

  * hello.imp      the simple example of the paper
  * amort.imp      an example of amortized complexity
  * frame.imp      a simple recursive that requires framing
  * qsort.imp      the quicksort example of the paper
                   (pass "-degree 2" to the analyze program)
  * mutur.imp      two small mutually-recursive functions
