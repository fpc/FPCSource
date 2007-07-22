program openarrayhigh;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;

procedure testproc(const par1: array of string);
var
 str1: ansistring;
begin
 writeln(high(par1));
 flush(output);
 str1:= par1[high(par1)];
 writeln(str1);
 flush(output);
end;

var
 str1: ansistring;
begin
 str1:= 'abcdefg';
 testproc(str1);
end.
