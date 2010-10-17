program tconstref3;

{$mode objfpc}{$h+}

uses
  SysUtils;

const
  CGuid: TGuid = '{67BD8D43-8960-491C-AA3A-50EC74A02F36}';

type
  PSmallRecord = ^TSmallRecord;
  TSmallRecord = record
                   p: PtrInt;
                 end;

  PAclass = ^TAclass;
  TAclass = class
  public
    p: PtrInt;
  end;

procedure TestConstRefIntegerAlias(AParam: PInteger); [external name '_TESTCONSTREFINTEGER'];
procedure TestConstRefInteger(constref AParam: integer); [public, alias: '_TESTCONSTREFINTEGER'];
begin
  if AParam<>$1234567 then
    halt(1);
end;

procedure TestConstRefStringAlias(AParam: PString); [external name '_TESTCONSTREFSTRING'];
procedure TestConstRefString(constref AParam: String); [public, alias: '_TESTCONSTREFSTRING'];
begin
  if AParam<>'1234567' then
    halt(1);
end;

procedure TestConstRefGUIDAlias(AParam: PGuid); [external name '_TESTCONSTREFGUID'];
procedure TestConstRefGUID(constref AParam: TGuid); [public, alias: '_TESTCONSTREFGUID'];
begin
  if GUIDToString(AParam)<>'{67BD8D43-8960-491C-AA3A-50EC74A02F36}' then
    halt(1);
end;

procedure TestConstRefRecordAlias(AParam: PSmallRecord); [external name '_TESTCONSTREFRECORD'];
procedure TestConstRefRecord(constref AParam: TSmallRecord); [public, alias: '_TESTCONSTREFRECORD'];
begin
  if AParam.p<>$7654321 then
    halt(1);
end;

procedure TestConstRefClassAlias(AParam: PAClass); [external name '_TESTCONSTREFCLASS'];
procedure TestConstRefClass(constref AParam: TAClass); [public, alias: '_TESTCONSTREFCLASS'];
begin
  if AParam.p<>$3456789 then
    halt(1);
end;

var a: integer;
    s: string;
    p: tguid;
    sr: TSmallRecord;
    ac: TAclass;

begin
  a := $1234567;
  TestConstRefIntegerAlias(@a);

  s := '1234567';
  TestConstRefStringAlias(@s);

  p := CGuid;
  TestConstRefGUIDAlias(@p);

  sr.p:=$7654321;
  TestConstRefRecordAlias(@sr);

  ac := TAclass.Create;
  ac.p := $3456789;
  TestConstRefClassAlias(@ac);
  ac.Free;
end.

