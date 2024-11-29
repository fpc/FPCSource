{ %fail }
program ie2017110102;

type
  tt = procedure; internproc;

var
  ff: tt;

begin
  ff; // project1.lpr(10,5) Error: Internal error 2017110102
end.
