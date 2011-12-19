#!/usr/bin/env instantfpc
{$mode objfpc}{$H+}
uses
  SysUtils;
var
  i: Integer;
begin
  for i:=0 to Paramcount do
    writeln('Param ',i,' ',ParamStr(i));
  for i:=0 to GetEnvironmentVariableCount-1 do
    writeln('Env ',GetEnvironmentString(i));
end.

