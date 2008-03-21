program stringconcat;
//compile with -gh

{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;

var
 wstr1: widestring;
begin
 winwidestringalloc:= false;
 //crash exist with winwidestringalloc also but with bigger application only
 wstr1:= '123';
 wstr1:= 'ABC'+wstr1+'abc';
 writeln(wstr1);
 flush(output);
end.
