program tarray12;

{$mode objfpc}

uses
  SysUtils;

procedure PrintArray(a: array of LongInt);
var
  i: LongInt;
begin
  Writeln('Length: ', Length(a));
  Write('Data: ');
  for i := Low(a) to High(a) do begin
    if i > Low(a) then
      Write(', ');
    Write(a[i]);
  end;
  Writeln;
end;

procedure CheckArray(aExpected, aGot: array of LongInt);
var
  i: LongInt;
begin
  if Length(aExpected) <> Length(aGot) then
    Halt(1);
  for i := Low(aExpected) to High(aExpected) do begin
    if aExpected[i] <> aGot[i] then
      Halt(2);
  end;
end;

function InitArray(aCount: LongInt): specialize TArray<LongInt>;
var
  i: LongInt;
begin
  SetLength(Result, aCount);
  for i := 0 to aCount - 1 do
    Result[i] := i;
end;

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

function GetIntf: IInterface;
begin
  Result := TTest.Create;
end;

procedure TestIntf;

  procedure DoInsert(const aArg1: specialize TArray<IInterface>; var aArg2: specialize TArray<IInterface>);
  begin
    Insert(aArg1, aArg2, 0);
  end;

var
  ai1, ai2: specialize TArray<IInterface>;
  intf: IInterface;
  c: LongInt;
begin
  intf := GetIntf;
  SetLength(ai1, 1);
  c := gRefCount;
  ai1[0] := intf;
  if c >= gRefCount then
    Halt(3);
  intf := Nil;
  if c <> gRefCount then
    Halt(4);
  DoInsert(ai1, ai2);
  if c >= gRefCount then
    Halt(5);
  ai1 := Nil;
  if gRefCount = 0 then
    Halt(6);
  ai2 := Nil;
  if gRefCount <> 0 then
    Halt(7);
end;

var
  t, t2: specialize TArray<LongInt>;
  t3: array[0..2] of LongInt;
begin
  t := Nil;
  Insert([1, 3, 5], t, 0);
  PrintArray(t);
  CheckArray(t, [1, 3, 5]);

  t := Nil;
  Insert([], t, 0);
  PrintArray(t);
  CheckArray(t, []);

  t := InitArray(5);
  Insert([], t, 0);
  PrintArray(t);
  CheckArray(t, [0, 1, 2, 3, 4]);

  t := InitArray(5);
  Insert([1, 3, 5], t, 2);
  PrintArray(t);
  CheckArray(t, [0, 1, 1, 3, 5, 2, 3, 4]);

  t := InitArray(5);
  Insert(5, t, 2);
  PrintArray(t);
  CheckArray(t, [0, 1, 5, 2, 3, 4]);

{  t := InitArray(5);
  Insert([1, 3, 5] + [4, 6], t, 2);
  PrintArray(t);
  CheckArray(t, [0, 1, 1, 3, 5, 4, 6, 2, 3, 4]);}

  t := InitArray(5);
  Insert([1, 3, 5], t, -1);
  PrintArray(t);
  CheckArray(t, [1, 3, 5, 0, 1, 2, 3, 4]);

  t := InitArray(5);
  Insert([1, 3, 5], t, 5);
  PrintArray(t);
  CheckArray(t, [0, 1, 2, 3, 4, 1, 3, 5]);

  t := InitArray(5);
  Insert([1, 3, 5], t, 6);
  PrintArray(t);
  CheckArray(t, [0, 1, 2, 3, 4, 1, 3, 5]);

  t2 := specialize TArray<LongInt>.Create(1, 3, 5);

  t := InitArray(5);
  Insert(t2, t, 1);
  PrintArray(t);
  CheckArray(t, [0, 1, 3, 5, 1, 2, 3, 4]);

  { support for static arrays is not Delphi compatible, but whatever :) }
  t := InitArray(5);
  t3[0] := 2;
  t3[1] := 4;
  t3[2] := 6;
  Insert(t3, t, 2);
  PrintArray(t);
  CheckArray(t, [0, 1, 2, 4, 6, 2, 3, 4]);

  TestIntf;

  Writeln('Ok');
end.
