{ %TARGET=win32,win64 }
program tw38821;

{$mode objfpc}

uses
  JwaWinType;

var
  lULongLong: ULONGLONG;
  lULong64: ULONG64;
  lDWord64: DWORD64;
  lUInt64: UINT64;

begin
  if Low(ULONGLONG)<>0 then
    halt(1);
  if High(ULONGLONG)<>High(QWord) then
    halt(2);
  if High(ULONG64)<>High(QWord) then
    halt(3);
  if High(DWORD64)<>High(QWord) then
    halt(4);
  if TypeInfo(UINT64)<>TypeInfo(System.UInt64) then
    halt(5);
  lULongLong:=High(QWord);
  if lULongLong<=High(Int64) then
    halt(6);
  lULong64:=QWord($8000000000000000);
  if lULong64<=0 then
    halt(7);
  lDWord64:=QWord($FFFFFFFF00000000);
  if (lDWord64 shr 32)<>$FFFFFFFF then
    halt(8);
  lUInt64:=lULongLong;
  if lUInt64<>High(QWord) then
    halt(9);
  writeln('ok');
end.
