{ %opt=-gh }

program resultmemleak;

{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
//compile with -gh
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;
type
 integerarty = array of integer;
 
function testproc: integerarty;
begin
 setlength(result,100);
 raise exception.create('');
end;

var
 ar1: integerarty;
begin
 HaltOnNotReleased := true;
 try
  ar1:= testproc;
 except
 end;
end.
