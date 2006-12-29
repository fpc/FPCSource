{ %fail }

uses
  uenum2a;

var
  e: tenum;
begin
  e := tone;
  inc(e);
  writeln(byte(e));
end.
