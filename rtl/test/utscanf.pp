unit utscanf;

{$mode objfpc}
{$h+}
interface

uses
  sysutils;
  
implementation

uses utrtl, punit;

Function utsscanf : string;

var
  e : extended;
  s : string;
  l : longint;
begin
  Result:='';
  sscanf('asdf 1'+DecimalSeparator+'2345 1234','%s %f %d',[@s,@e,@l]);
  if AssertEquals('Detected float',1.2345,e) then
    If AssertEquals('Detected integer',1234,l) then
      AssertEquals('Detected string','asdf',s) 
end;

begin
  SysutilsTest('utsscanf',@utsscanf);
end.

