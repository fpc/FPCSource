{ %target=linux }
{ %opt=-gwl }
{ Line info skips DWARF units with an unsupported version, issue #41395 }
program tdwarfversion;

{$mode objfpc}{$h+}

uses
  BaseUnix, SysUtils, Classes;


{ Returns the backtrace string for an address inside this routine }
function OwnLineInfo: string;

begin
  Result:=BackTraceStrFunc(get_pc_addr);
end;


{ Returns True when the backtrace string has a source line }
function HasLine(const aInfo: string): Boolean;

begin
  Result:=Pos(' line ',aInfo)>0;
end;


{ Returns True when the backtrace string has the name of OwnLineInfo }
function HasFunc(const aInfo: string): Boolean;

begin
  Result:=Pos('OWNLINEINFO',UpperCase(aInfo))>0;
end;


{ Sets the version of all units in section aSection, returns the number of units changed }
function SetUnitVersions(aData: PByte; aSize: SizeInt; const aSection: string; aVersion: Word): Integer;

var
  lShOff, lNameOfs, lStrOfs, lOfs, lEnd, lLen: QWord;
  lShEntSize, lShNum, lShStrNdx: Word;
  lHdr: PByte;
  i: Integer;

begin
  Result:=0;
  lShOff:=PQWord(aData+$28)^;
  lShEntSize:=PWord(aData+$3A)^;
  lShNum:=PWord(aData+$3C)^;
  lShStrNdx:=PWord(aData+$3E)^;
  lStrOfs:=PQWord(aData+lShOff+QWord(lShStrNdx)*lShEntSize+$18)^;
  for i:=0 to lShNum-1 do
    begin
    lHdr:=aData+lShOff+QWord(i)*lShEntSize;
    lNameOfs:=lStrOfs+PLongWord(lHdr)^;
    if (lNameOfs>=QWord(aSize)) or (StrComp(PAnsiChar(aData+lNameOfs),PAnsiChar(aSection))<>0) then
      continue;
    lOfs:=PQWord(lHdr+$18)^;
    lEnd:=lOfs+PQWord(lHdr+$20)^;
    while lOfs+6<=lEnd do
      begin
      lLen:=PLongWord(aData+lOfs)^;
      if lLen=$FFFFFFFF then
        begin
        lLen:=PQWord(aData+lOfs+4)^;
        PWord(aData+lOfs+12)^:=aVersion;
        Inc(lOfs,12+lLen);
        end
      else
        begin
        PWord(aData+lOfs+4)^:=aVersion;
        Inc(lOfs,4+lLen);
        end;
      Inc(Result);
      end;
    end;
end;


{ Runs a copy of this program with changed unit versions in aSection, returns its exit code }
function RunPatched(const aSection: string; aVersion: Word): Integer;

var
  lStream: TMemoryStream;
  lCopy: string;

begin
  lCopy:=ParamStr(0)+'-version';
  lStream:=TMemoryStream.Create;
  try
    lStream.LoadFromFile(ParamStr(0));
    if SetUnitVersions(lStream.Memory,lStream.Size,aSection,aVersion)=0 then
      exit(9);
    lStream.SaveToFile(lCopy);
  finally
    lStream.Free;
  end;
  FpChmod(lCopy,&755);
  Result:=ExecuteProcess(lCopy,[aSection]);
  DeleteFile(lCopy);
end;


const
  Versions: array[0..2] of Word = (0, 1, 6);

var
  lInfo: string;
  i: Integer;

begin
  lInfo:=OwnLineInfo;
  if ParamStr(1)='.debug_line' then
    begin
    if HasLine(lInfo) or not HasFunc(lInfo) then
      halt(1);
    halt(0);
    end;
  if ParamStr(1)='.debug_info' then
    begin
    if not HasLine(lInfo) or HasFunc(lInfo) then
      halt(1);
    halt(0);
    end;

  if not (HasLine(lInfo) and HasFunc(lInfo)) then
    halt(1);
  for i:=Low(Versions) to High(Versions) do
    begin
    if RunPatched('.debug_line',Versions[i])<>0 then
      halt(10+i);
    if RunPatched('.debug_info',Versions[i])<>0 then
      halt(20+i);
    end;
  writeln('ok');
end.
