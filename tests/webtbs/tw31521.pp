program tw31521;

{$mode delphi}

uses SysUtils;

var
  X: Int64;
begin
  if X.MaxValue <> High(X) then
    Halt(1);
  {Writeln(X.MaxValue.ToString);
  Writeln(IntToStr(High(X)));
  Readln;}
end.
