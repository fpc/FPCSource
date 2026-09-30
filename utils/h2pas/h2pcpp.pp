{
  h2pas: runs the C preprocessor on the input file, and keeps the text of the selected files.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit h2pCpp;

{$mode objfpc}{$H+}

interface

// Runs the C preprocessor on aInput with its defines kept, and returns the name of a file with the text
// of the kept files and their line markers. Halts when the preprocessor cannot be run or fails.
function PreprocessInput(const aInput : AnsiString) : AnsiString;
// Removes the files written by PreprocessInput.
procedure RemovePreprocessedFiles;

implementation

uses
  SysUtils, Classes, h2poptions;

var
  RawFileName, KeptFileName : AnsiString;


// Returns the full path of the program aProgram, searched in the PATH when it has no directory.
function FindProgram(const aProgram : AnsiString) : AnsiString;

begin
  if ExtractFilePath(aProgram)<>'' then
    Result:=aProgram
  else
    Result:=ExeSearch(aProgram,GetEnvironmentVariable('PATH'));
end;


// Returns true when the file name aFile of a line marker is one of the files in aKeep.
function IsKeptFile(const aFile : AnsiString; aKeep : TStrings) : boolean;

var
  i : integer;
  lName : AnsiString;

begin
  Result:=false;
  for i:=0 to aKeep.Count-1 do
    begin
    lName:=aKeep[i];
    if (aFile=lName) or (ExpandFileName(aFile)=ExpandFileName(lName)) then
      exit(true);
    if (length(aFile)>length(lName)) and (copy(aFile,length(aFile)-length(lName)+1,length(lName))=lName)
       and (aFile[length(aFile)-length(lName)] in AllowDirectorySeparators) then
      exit(true);
    end;
end;


// Returns the file name of the line marker aLine (# number "file" flags), or '' when aLine is no line marker.
function MarkerFileName(const aLine : AnsiString) : AnsiString;

var
  i, lEnd : integer;

begin
  Result:='';
  if (length(aLine)<4) or (aLine[1]<>'#') or (aLine[2]<>' ') or not (aLine[3] in ['0'..'9']) then
    exit;
  i:=3;
  while (i<=length(aLine)) and (aLine[i] in ['0'..'9']) do
    inc(i);
  if (i+1>length(aLine)) or (aLine[i]<>' ') or (aLine[i+1]<>'"') then
    exit;
  lEnd:=i+2;
  while (lEnd<=length(aLine)) and (aLine[lEnd]<>'"') do
    inc(lEnd);
  Result:=copy(aLine,i+2,lEnd-i-2);
end;


// Copies the lines of the kept files in aRaw to aKept, with the line markers of the kept files.
procedure FilterPreprocessed(const aRaw, aKept : AnsiString; aKeep : TStrings);

var
  lIn, lOut : TStringList;
  lFile : AnsiString;
  i : integer;
  lOn : boolean;

begin
  lIn:=TStringList.Create;
  lOut:=TStringList.Create;
  try
    lIn.LoadFromFile(aRaw);
    lOn:=false;
    for i:=0 to lIn.Count-1 do
      begin
      lFile:=MarkerFileName(lIn[i]);
      if lFile<>'' then
        begin
        lOn:=IsKeptFile(lFile,aKeep);
        if lOn then
          lOut.Add(lIn[i]);
        end
      else if lOn then
        lOut.Add(lIn[i]);
      end;
    lOut.SaveToFile(aKept);
  finally
    lOut.Free;
    lIn.Free;
  end;
end;


// Adds the words of the option string aOptions to aArgs; words between double quotes may contain spaces.
procedure AddOptions(const aOptions : AnsiString; aArgs : TStrings);

var
  i : integer;
  lWord : AnsiString;
  lQuoted : boolean;

begin
  lWord:='';
  lQuoted:=false;
  for i:=1 to length(aOptions) do
    if aOptions[i]='"' then
      lQuoted:=not lQuoted
    else if (aOptions[i] in [' ',#9]) and not lQuoted then
      begin
      if lWord<>'' then
        aArgs.Add(lWord);
      lWord:='';
      end
    else
      lWord:=lWord+aOptions[i];
  if lWord<>'' then
    aArgs.Add(lWord);
end;


// Adds the files listed in the file aListFile, one per line, to aKeep.
procedure ReadKeepList(const aListFile : AnsiString; aKeep : TStrings);

var
  lList : TStringList;
  i : integer;

begin
  if not FileExists(aListFile) then
    begin
    writeln('Error : the list of kept files ',aListFile,' does not exist');
    RemovePreprocessedFiles;
    halt(1);
    end;
  lList:=TStringList.Create;
  try
    lList.LoadFromFile(aListFile);
    for i:=0 to lList.Count-1 do
      if Trim(lList[i])<>'' then
        aKeep.Add(Trim(lList[i]));
  finally
    lList.Free;
  end;
end;


function PreprocessInput(const aInput : AnsiString) : AnsiString;

var
  lProgram : AnsiString;
  lArgList, lKeep : TStringList;
  lArgs : array of RawByteString;
  i, lExit : integer;

begin
  lProgram:=FindProgram(PreprocessorProgram);
  if lProgram='' then
    begin
    writeln('Error : C preprocessor ',PreprocessorProgram,' not found');
    halt(1);
    end;
  RawFileName:=ChangeFileExt(outputfilename,'.i');
  KeptFileName:=ChangeFileExt(outputfilename,'.pre.h');
  lArgList:=TStringList.Create;
  lKeep:=TStringList.Create;
  try
    lArgList.Add('-E');
    lArgList.Add('-dD');
    lArgList.Add('-C');
    lArgList.Add('-x');
    lArgList.Add('c');
    AddOptions(PreprocessorOptions,lArgList);
    lArgList.Add(aInput);
    lArgList.Add('-o');
    lArgList.Add(RawFileName);
    SetLength(lArgs,lArgList.Count);
    for i:=0 to lArgList.Count-1 do
      lArgs[i]:=lArgList[i];
    try
      lExit:=ExecuteProcess(lProgram,lArgs);
    except
      on E : Exception do
        begin
        writeln('Error : cannot run the C preprocessor ',lProgram,': ',E.Message);
        halt(1);
        end;
    end;
    if lExit<>0 then
      begin
      writeln('Error : the C preprocessor ',lProgram,' failed with exit code ',lExit);
      RemovePreprocessedFiles;
      halt(1);
      end;
    if Copy(PreprocessorKeep,1,1)='@' then
      ReadKeepList(Copy(PreprocessorKeep,2,Length(PreprocessorKeep)-1),lKeep)
    else
      begin
      lKeep.StrictDelimiter:=true;
      lKeep.Delimiter:=';';
      lKeep.DelimitedText:=PreprocessorKeep;
      end;
    lKeep.Add(aInput);
    FilterPreprocessed(RawFileName,KeptFileName,lKeep);
    DeleteFile(RawFileName);
  finally
    lKeep.Free;
    lArgList.Free;
  end;
  Result:=KeptFileName;
end;


procedure RemovePreprocessedFiles;

begin
  if (RawFileName<>'') and FileExists(RawFileName) then
    DeleteFile(RawFileName);
  if (KeptFileName<>'') and FileExists(KeptFileName) then
    DeleteFile(KeptFileName);
end;

end.
