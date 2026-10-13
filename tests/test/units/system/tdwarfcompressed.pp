{ %target=linux }
{ %opt=-gwl }
{ Line info ignores .debug_* sections flagged SHF_COMPRESSED, issue #41394 }
program tdwarfcompressed;

{$mode objfpc}{$h+}

uses
  BaseUnix, SysUtils, Classes;

const
  SHF_COMPRESSED = $800;


{ Returns the backtrace string for an address inside this routine }
function OwnLineInfo: string;

begin
  Result:=BackTraceStrFunc(get_pc_addr);
end;


{ Returns True when the backtrace string has a source line }
function HasLineInfo: Boolean;

begin
  Result:=Pos(' line ',OwnLineInfo)>0;
end;


{ Sets SHF_COMPRESSED on all .debug_* section headers, returns the number of sections changed }
function FlagDebugSections(aData: PByte; aSize: SizeInt): Integer;

var
  lIs64: Boolean;
  lShOff, lNameOfs, lStrOfs: QWord;
  lShEntSize, lShNum, lShStrNdx: Word;
  lHdr: PByte;
  lName: PAnsiChar;
  i: Integer;

begin
  Result:=0;
  lIs64:=aData[4]=2;
  if lIs64 then
    begin
    lShOff:=PQWord(aData+$28)^;
    lShEntSize:=PWord(aData+$3A)^;
    lShNum:=PWord(aData+$3C)^;
    lShStrNdx:=PWord(aData+$3E)^;
    lStrOfs:=PQWord(aData+lShOff+QWord(lShStrNdx)*lShEntSize+$18)^;
    end
  else
    begin
    lShOff:=PLongWord(aData+$20)^;
    lShEntSize:=PWord(aData+$2E)^;
    lShNum:=PWord(aData+$30)^;
    lShStrNdx:=PWord(aData+$32)^;
    lStrOfs:=PLongWord(aData+lShOff+QWord(lShStrNdx)*lShEntSize+$10)^;
    end;
  if lShOff+QWord(lShNum)*lShEntSize>QWord(aSize) then
    exit;
  for i:=0 to lShNum-1 do
    begin
    lHdr:=aData+lShOff+QWord(i)*lShEntSize;
    lNameOfs:=lStrOfs+PLongWord(lHdr)^;
    if lNameOfs>=QWord(aSize) then
      continue;
    lName:=PAnsiChar(aData+lNameOfs);
    if StrLComp(lName,'.debug_',7)<>0 then
      continue;
    if lIs64 then
      PQWord(lHdr+8)^:=PQWord(lHdr+8)^ or SHF_COMPRESSED
    else
      PLongWord(lHdr+8)^:=PLongWord(lHdr+8)^ or SHF_COMPRESSED;
    Inc(Result);
    end;
end;


var
  lStream: TMemoryStream;
  lCopy: string;
  lRes: Integer;

begin
  if ParamStr(1)='child' then
    begin
    try
      if HasLineInfo then
        halt(1);
    except
      halt(2);
    end;
    halt(0);
    end;

  if not HasLineInfo then
    halt(10);
  lCopy:=ParamStr(0)+'-compressed';
  lStream:=TMemoryStream.Create;
  try
    lStream.LoadFromFile(ParamStr(0));
    if FlagDebugSections(lStream.Memory,lStream.Size)<4 then
      halt(11);
    lStream.SaveToFile(lCopy);
  finally
    lStream.Free;
  end;
  FpChmod(lCopy,&755);
  lRes:=ExecuteProcess(lCopy,['child']);
  DeleteFile(lCopy);
  if lRes<>0 then
    halt(20+lRes);
  writeln('ok');
end.
