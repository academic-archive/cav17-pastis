The cBench Benchmark is taken from ctuning.org (cBench V1.1) it can be downloaded at

http://sourceforge.net/projects/cbenchmark/files/cBench/V1.1/

we removed code duplicates and applied the following command line to get unique line numbers for all loop headers:

 clang -E foo.c | tr "\n" " " | indent -kr -l 9999 2>/dev/null >foo.preproc.c


We generated the inputs for the diffrent tools as follows:

	1. we compiled the ".c" files in "cBench" with clang 3.5.1 using the options "-c -emit-llvm" (loopus needs additionally "-g" for the line numbers)
	2. we extracted the 1659 functions using "llvm-extract"
	3. we ran "llvm2kittel" with the options "-only-loop-conditions -multi-pred-control" for generating the input for KoAT (we also tried without "-only-loop-conditions -multi-pred-control" but the results were worse)
	4. we used the transformation described in http://abu.se.informatik.tu-darmstadt.de/aeflores/experimental_results.pdf for generating the CoFloCo files from the KoAT files
 
