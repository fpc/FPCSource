{ DynArrayRefCount, issue #41453 }
program tdynarrrefcount;

{$mode objfpc}{$h+}

type
  TIntArray = array of Integer;
  TStrArray = array of string;

procedure CheckInParam(const aArr: TIntArray; aExpected: SizeInt; aCode: Integer);

begin
  if DynArrayRefCount(Pointer(aArr))<>aExpected then
    halt(aCode);
end;


var
  lA, lB: TIntArray;
  lS: TStrArray;

begin
  lA:=nil;
  if DynArrayRefCount(Pointer(lA))<>0 then
    halt(1);
  SetLength(lA,3);
  if DynArrayRefCount(Pointer(lA))<>1 then
    halt(2);
  lB:=lA;
  if DynArrayRefCount(Pointer(lA))<>2 then
    halt(3);
  if DynArrayRefCount(Pointer(lB))<>2 then
    halt(4);
  CheckInParam(lA,2,5);
  lB:=nil;
  if DynArrayRefCount(Pointer(lA))<>1 then
    halt(6);
  lB:=lA;
  SetLength(lB,5);
  if DynArrayRefCount(Pointer(lA))<>1 then
    halt(7);
  if DynArrayRefCount(Pointer(lB))<>1 then
    halt(8);
  lB:=Copy(lA);
  if DynArrayRefCount(Pointer(lB))<>1 then
    halt(9);

  SetLength(lS,2);
  if DynArrayRefCount(Pointer(lS))<>1 then
    halt(11);
  lS:=nil;
  if DynArrayRefCount(Pointer(lS))<>0 then
    halt(12);
  writeln('ok');
end.
