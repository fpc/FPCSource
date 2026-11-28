{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ VariantChangeTypeEx copies its source into a work variant of its own and
  converts from that.  The copy clears its destination first, so a work
  variant left uninitialised was cleared as whatever the stack held: the
  conversion could fail, or free memory it did not own. }
program tvarchangetmp;

{$mode objfpc}

uses
  varutils;

var
  gSink: LongInt;

{ fills stack storage below the caller, which the next call's frame can
  reuse, with a type code VariantClear rejects }
procedure DirtyStack;
var
  lBytes: array[0..4095] of Byte;
  lIdx: LongInt;
begin
  FillChar(lBytes, SizeOf(lBytes), $0F);
  for lIdx := 0 to High(lBytes) do
    Inc(gSink, lBytes[lIdx]);
end;

procedure Check(aCase: LongInt; aOk: Boolean);
begin
  if not aOk then
    begin
      WriteLn('failed: case ', aCase);
      Halt(aCase);
    end;
end;

var
  lSource, lDest: TVarData;
  lResult: HRESULT;
begin
  VariantInit(lSource);
  lSource.VType := varInteger;
  lSource.VInteger := 7;
  VariantInit(lDest);
  DirtyStack;
  lResult := VariantChangeTypeEx(lDest, lSource, 0, 0, varInteger);
  Check(1, lResult = VAR_OK);
  Check(2, (lDest.VType = varInteger) and (lDest.VInteger = 7));
  { one variant as source and destination, as the Variants unit's cast
    fallback passes it }
  DirtyStack;
  lResult := VariantChangeTypeEx(lDest, lDest, 0, 0, varInt64);
  Check(3, lResult = VAR_OK);
  Check(4, (lDest.VType = varInt64) and (lDest.VInt64 = 7));
  VariantClear(lDest);
  WriteLn('ok');
end.
