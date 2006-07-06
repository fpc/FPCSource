{ %SKIPEMU=qemu-arm }
{ %SKIPTARGET=wince }

{ Source provided for Free Pascal Bug Report 1152 }
{ Submitted by "Dirk Verwiebe" on  2000-09-30 }
{ e-mail: dirk@verwiebe.de }

{$mode objfpc}

program exception;
uses sysutils,crt;
var
  saveexit : pointer;
  finally_called : boolean;

procedure my_exit;
  begin
    exitproc:=saveexit;
    if not finally_called then
      begin
        Writeln('Problem with exception handling if crt unit is used');
        RunError(1);
      end
    else
      begin
        Writeln('Exception handling works');
        exitcode:=0;
      end;
  end;


var
  p : pointer;
BEGIN
  saveexit:=exitproc;
  exitproc:=@my_exit;
  finally_called:=false;
try
  p:=pointer(-1);
  longint(p^):=0;
finally
  finally_called:=true;
  writeln('Error !!!');
end;
END.
