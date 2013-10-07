This program compares the performance of Higher-order functions and First-order functions in Fortran. It also has OpenMP directives. To run it in Linux type on the command line:
 - ulimit -s unlimited
 - export OMP_NUM_THREADS=4
 - f95 -xopenmp=parallel -o OMPR ModuleopenMPRefactor.f95 MainOpenMPRefactor.f95 
 - ./HOF
 - gprof HOF gmon.out>output.txt
 - cat output.txt 

---------

The code is under the GPL license so feel free to use it!

