program tconstref4;


{ This include file defines FPC_HAS_UNDERSCORE_PREFIX
  for targets for which Cprefix='_' }

{$include lcpref.inc}

{$mode objfpc}{$h+}

procedure TestConstRefSafecallAlias(AParam: PInteger); safecall; [external name '_TESTCONSTREFSAFECALL'];
procedure TestConstRefSafecall(constref AParam: integer); safecall; [public, alias: '_TESTCONSTREFSAFECALL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;


{ For cdecl type function Cprefix added for
  external name but not for alias ... }
{$ifdef FPC_HAS_UNDERSCORE_PREFIX}
procedure TestConstRefCdeclAlias(AParam: PInteger); cdecl; [external name 'TESTCONSTREFCDECL'];
{$else not FPC_HAS_UNDERSCORE_PREFIX}
procedure TestConstRefCdeclAlias(AParam: PInteger); cdecl; [external name '_TESTCONSTREFCDECL'];
{$endif not FPC_HAS_UNDERSCORE_PREFIX}
procedure TestConstRefCdecl(constref AParam: integer); cdecl; [public, alias: '_TESTCONSTREFCDECL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

{ For cppdecl type function Cprefix added for
  external name but not for alias ... }
{$ifdef FPC_HAS_UNDERSCORE_PREFIX}
procedure TestConstRefCPPdeclAlias(AParam: PInteger); cppdecl; [external name 'TESTCONSTREFCPPDECL'];
{$else not FPC_HAS_UNDERSCORE_PREFIX}
procedure TestConstRefCPPdeclAlias(AParam: PInteger); cppdecl; [external name '_TESTCONSTREFCPPDECL'];
{$endif not FPC_HAS_UNDERSCORE_PREFIX}
procedure TestConstRefCPPdecl(constref AParam: integer); cppdecl; [public, alias: '_TESTCONSTREFCPPDECL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefStdcallAlias(AParam: PInteger); stdcall; [external name '_TESTCONSTREFSTDCALL'];
procedure TestConstRefStdcall(constref AParam: integer); stdcall; [public, alias: '_TESTCONSTREFSTDCALL'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefRegisterAlias(AParam: PInteger); register; [external name '_TESTCONSTREFREGISTER'];
procedure TestConstRefRegister(constref AParam: integer); register; [public, alias: '_TESTCONSTREFREGISTER'];
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

