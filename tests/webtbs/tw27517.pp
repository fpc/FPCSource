program Project1;

{$mode objfpc}{$H+}

var
  pTyped: PInteger;
  p: Pointer;
begin
  p := nil;
  pTyped := @(PByte(p)+1)^; //project1.lpr(21,23) Fatal: Syntax error, ")" expected but "+" found
end.
