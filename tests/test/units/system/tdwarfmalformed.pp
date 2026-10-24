{ %target=linux }
{ %opt=-gwl }
{ Line info lookups end without hang or exception on malformed .debug_* sections, issue #41394 }
program tdwarfmalformed;

{$mode objfpc}{$h+}

uses
  cthreads, BaseUnix, SysUtils, Classes;

var
  gSeed: LongWord;


{ Returns the next pseudo random number }
function NextRandom: LongWord;

begin
  gSeed:=gSeed*1664525+1013904223;
  Result:=gSeed shr 8;
end;


{ Returns the backtrace string for an address inside this routine }
function OwnLineInfo: string;

begin
  Result:=BackTraceStrFunc(get_pc_addr);
end;


{ Raises an exception }
procedure Fail;

begin
  raise Exception.Create('test');
end;


{ Stops the process when the lookups have not finished after 20 seconds }
function Watchdog(aParam: Pointer): PtrInt;

begin
  Result:=0;
  Sleep(20000);
  FpExit(3);
end;


{ Overwrites all .debug_* sections: aFlip false fills them with random bytes, true changes 2% of their bytes }
procedure CorruptDebugSections(aData: PByte; aSize: SizeInt; aFlip: Boolean);

var
  lShOff, lNameOfs, lStrOfs, lOfs, lLen, j: QWord;
  lShEntSize, lShNum, lShStrNdx: Word;
  lHdr: PByte;
  i: Integer;

begin
  lShOff:=PQWord(aData+$28)^;
  lShEntSize:=PWord(aData+$3A)^;
  lShNum:=PWord(aData+$3C)^;
  lShStrNdx:=PWord(aData+$3E)^;
  lStrOfs:=PQWord(aData+lShOff+QWord(lShStrNdx)*lShEntSize+$18)^;
  for i:=0 to lShNum-1 do
    begin
    lHdr:=aData+lShOff+QWord(i)*lShEntSize;
    lNameOfs:=lStrOfs+PLongWord(lHdr)^;
    if (lNameOfs>=QWord(aSize)) or (StrLComp(PAnsiChar(aData+lNameOfs),'.debug_',7)<>0) then
      continue;
    lOfs:=PQWord(lHdr+$18)^;
    lLen:=PQWord(lHdr+$20)^;
    if (lLen=0) or (lOfs+lLen>QWord(aSize)) then
      continue;
    if aFlip then
      begin
      for j:=0 to lLen div 50 do
        aData[lOfs+(NextRandom mod lLen)]:=NextRandom and $FF;
      end
    else
      for j:=0 to lLen-1 do
        aData[lOfs+j]:=NextRandom and $FF;
    end;
end;


{ Runs a copy of this program with corrupted debug sections, returns its exit code }
function RunCorrupted(aSeed: LongWord; aFlip: Boolean): Integer;

var
  lStream: TMemoryStream;
  lCopy: string;

begin
  gSeed:=aSeed;
  lCopy:=ParamStr(0)+'-malformed';
  lStream:=TMemoryStream.Create;
  try
    lStream.LoadFromFile(ParamStr(0));
    CorruptDebugSections(lStream.Memory,lStream.Size,aFlip);
    lStream.SaveToFile(lCopy);
  finally
    lStream.Free;
  end;
  FpChmod(lCopy,&755);
  Result:=ExecuteProcess(lCopy,['child']);
  DeleteFile(lCopy);
end;


var
  lSeed: LongWord;
  lId: TThreadID;
  lRes, i: Integer;

begin
  if ParamStr(1)='child' then
    begin
    BeginThread(@Watchdog,nil,lId);
    try
      OwnLineInfo;
      try
        Fail;
      except
        BackTraceStrFunc(ExceptAddr);
        for i:=0 to ExceptFrameCount-1 do
          BackTraceStrFunc(ExceptFrames[i]);
      end;
    except
      halt(2);
    end;
    halt(0);
    end;

  if Pos(' line ',OwnLineInfo)=0 then
    halt(1);
  for lSeed:=1 to 8 do
    begin
    lRes:=RunCorrupted(lSeed,False);
    if lRes<>0 then
      halt(10+lRes);
    lRes:=RunCorrupted(lSeed,True);
    if lRes<>0 then
      halt(20+lRes);
    end;
  writeln('ok');
end.
