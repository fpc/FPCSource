{ Old file: tbs0305.pp }
{ Finally is not handled correctly after inputting 0 }

{$mode objfpc}
uses
(* sysutils does not work correctly with DPMIEXCP unit
  anyway, its not needed anymore
  since the exception handler is now in system unit
{$ifdef go32v2}
dpmiexcp,
{$endif} *)
sysutils;
var i,j,k:real;
const except_called : boolean = false;
begin
  i:=100;
  j:=0;
  try
    k:=i/j;
    writeln(k:5:3);
  except
    k:=0;
    writeln('Illegal Input');
    except_called:=true;
  end;
  if not except_called then
    begin
      Writeln('Error in except handling');
      Halt(1);
    end;
end.
