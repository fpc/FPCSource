program tconstref4;

{$mode objfpc}{$h+}

procedure TestConstRefSafecallAlias(AParam: PInteger); safecall; [external name '_TESTCONSTREFSAFECALL'];
procedure TestConstRefSafecall(constref AParam: integer); safecall; [public, alias: '_TESTCONSTREFSAFECALL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefCdeclAlias(AParam: PInteger); cdecl; [external name '_TESTCONSTREFCDECL'];
procedure TestConstRefCdecl(constref AParam: integer); cdecl; [public, alias: '_TESTCONSTREFCDECL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefStdcallAlias(AParam: PInteger); cdecl; [external name '_TESTCONSTREFSTDCALL'];
procedure TestConstRefStdcall(constref AParam: integer); cdecl; [public, alias: '_TESTCONSTREFSTDCALL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefRegisterAlias(AParam: PInteger); cdecl; [external name '_TESTCONSTREFREGISTER'];
procedure TestConstRefRegister(constref AParam: integer); cdecl; [public, alias: '_TESTCONSTREFREGISTER'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

var a : integer;
begin
  a := $1234567;
  TestConstRefSafecallAlias(@a);
  TestConstRefStdcallAlias(@a);
  TestConstRefRegisterAlias(@a);
  TestConstRefCdeclAlias(@a);
end.

