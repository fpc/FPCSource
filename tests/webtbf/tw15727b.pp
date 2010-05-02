{ %fail }

{$mode objfpc}
{$r+}

uses
  SysUtils;

procedure test(a: array of const);
begin
  if (a[0].vtype<>vtinteger) or
     (a[0].vinteger<>longint($f0f0f0f0)) then
    halt(1);
end;

var
  z: cardinal;
begin
  // next line produces compilation error "Error: range check error while evaluating constants"

  // accepted now in Delphi mode, not in FPC mode because this value is
  // implicitly converted to a longint, and $f0f0f0f0 is an invalid longint
  // value (use longint($f0f0f0f0) instead)
  test([$F0F0F0F0]);
end.
