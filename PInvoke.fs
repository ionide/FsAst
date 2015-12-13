// https://msdn.microsoft.com/library/hh304361(v=vs.100).aspx
module PInvoke

open System.Runtime.InteropServices

[<DllImport("blas.dll",EntryPoint="dgemm_")>]
extern void dgemm_
  ( char *transa, char *transb, 
    int *m, int *n, int *k, 
    double *alpha, double *a, int *lda, 
    double *b, int *ldb, double *beta, 
    double *c, int *ldc );