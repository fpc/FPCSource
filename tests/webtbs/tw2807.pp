{$mode delphi}

uses
  SysUtils;

var
  a:array of integer;
begin
  a := nil;
  SetLength(a,3);
  a[0] := 0;
  a[1] := 1;
  a[2] := 2;
  WriteLn(IntToStr(a[0]));
  a := nil;
end.
