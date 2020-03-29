program tarrconstr7;

{$mode objfpc}

type
  TTest = class(TInterfacedObject, IInterface)
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

var
  gRefCount: LongInt = 0;

function TTest._AddRef: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := inherited _AddRef;
  gRefCount := Result;
end;

function TTest._Release: LongInt; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := inherited _Release;
  gRefCount := Result;
end;

procedure TestArrConstr(const aIntf: IInterface);
var
  a: array of IInterface;
begin
  a := [aIntf];
  a := Nil;
end;

procedure Test;
var
  t: IInterface;
  c: LongInt;
begin
  t := TTest.Create;

  c := gRefCount;
  TestArrConstr(t);
  if gRefCount <> c then
    Halt(1);
end;

begin
  Test;
  if gRefCount <> 0 then
    Halt(2);
end.
