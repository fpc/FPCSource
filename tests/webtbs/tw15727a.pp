{$mode delphi}
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
  Z:=$F0F0F0F0;
  // next line works OK
  writeln('Z=',Z);

  // next line produces compilation error "Error: range check error while evaluating constants"
  test([$F0F0F0F0]);

  // next line gives run-time error: "ERangeError : Range check error"
  test([Z]);
end.
