{ %maxversion=1.0.99 }
{ %FAIL }
{ Old file: tbf0230.pp }
{ several strange happen on the ln function: ln(0): no FPE and writeln can't write non numeric values Gives out an exception on compiling because of zero div OK 0.99.11 (PM) }

{
  This test is only for 1.0.x.
  1.1+ supports Nan and Inf
}

var
   e : extended;

begin
 e:=-1.0;
 writeln(ln(0));
 writeln(power(0,1.0));
 writeln(ln(e));
end .
