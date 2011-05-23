#!/usr/bin/env instantfpc

{$mode objfpc}{$H+}

uses SysUtils;

var i: integer;
begin
  i:=StrToInt(ParamStr(1));
  writeln('exit code: ',i);
  Halt(i);
end.

