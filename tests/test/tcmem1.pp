{ %target=linux,freebsd,darwin }
{ cmem after cthreads: the RTL frees no block of the FPC heap through cmem at exit, issue #32198 }
program tcmem1;

uses
  cthreads, cmem;

begin
end.
