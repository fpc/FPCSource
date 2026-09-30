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


// Returns the name of the macro that the define line aLine (#define name...) defines, or ''.
function DefineName(const aLine : AnsiString) : AnsiString;

var
  i : integer;

begin
  Result:='';
  if copy(aLine,1,8)<>'#define ' then
    exit;
  i:=9;
  while (i<=length(aLine)) and (aLine[i] in ['A'..'Z','a'..'z','0'..'9','_']) do
    inc(i);
  Result:=copy(aLine,9,i-9);
end;


// Adds the identifiers of aLine that start with an underscore to aNames.
procedure AddUnderscoreNames(const aLine : AnsiString; aNames : TStrings);

var
  i, lStart : integer;

begin
  i:=1;
  while i<=length(aLine) do
    if aLine[i] in ['A'..'Z','a'..'z','_'] then
      begin
      lStart:=i;
      while (i<=length(aLine)) and (aLine[i] in ['A'..'Z','a'..'z','0'..'9','_']) do
        inc(i);
      if aLine[lStart]='_' then
        aNames.Add(copy(aLine,lStart,i-lStart));
      end
    else
      inc(i);
end;


// Inserts at the start of aOut the defines of the preprocessor itself in aBuiltins, as name=line, that the lines
// of aOut, or of the inserted defines, use.
procedure InsertUsedBuiltins(aOut, aBuiltins : TStringList);

var
  lUsed, lAdded : TStringList;
  i, lIndex, lCount : integer;

begin
  lUsed:=TStringList.Create;
  lAdded:=TStringList.Create;
  try
    lUsed.Sorted:=true;
    lUsed.Duplicates:=dupIgnore;
    lUsed.CaseSensitive:=true;
    for i:=0 to aOut.Count-1 do
      AddUnderscoreNames(aOut[i],lUsed);
    repeat
      lCount:=lAdded.Count;
      for i:=0 to aBuiltins.Count-1 do
        if (lUsed.IndexOf(aBuiltins.Names[i])>=0) and (lAdded.IndexOf(aBuiltins[i])<0) then
          begin
          lAdded.Add(aBuiltins[i]);
          AddUnderscoreNames(aBuiltins.ValueFromIndex[i],lUsed);
          end;
    until lAdded.Count=lCount;
    (* in the order of the preprocessor *)
    lIndex:=0;
    for i:=0 to aBuiltins.Count-1 do
      if lAdded.IndexOf(aBuiltins[i])>=0 then
        begin
        aOut.Insert(lIndex,aBuiltins.ValueFromIndex[i]);
        inc(lIndex);
        end;
  finally
    lAdded.Free;
    lUsed.Free;
  end;
end;


// Copies the lines of the kept files in aRaw to aKept, with the line markers of the kept files, after the
// defines of the preprocessor itself that they use.
procedure FilterPreprocessed(const aRaw, aKept : AnsiString; aKeep : TStrings);

var
  lIn, lOut, lBuiltins : TStringList;
  lFile, lName : AnsiString;
  i : integer;
  lOn, lBuiltin : boolean;

begin
  lIn:=TStringList.Create;
  lOut:=TStringList.Create;
  lBuiltins:=TStringList.Create;
  try
    lIn.LoadFromFile(aRaw);
    lOn:=false;
    lBuiltin:=false;
    for i:=0 to lIn.Count-1 do
      begin
      lFile:=MarkerFileName(lIn[i]);
      if lFile<>'' then
        begin
        lBuiltin:=lFile='<built-in>';
        lOn:=IsKeptFile(lFile,aKeep);
        if lOn then
          lOut.Add(lIn[i]);
        end
      else if lOn then
        lOut.Add(lIn[i])
      else if lBuiltin then
        begin
        lName:=DefineName(lIn[i]);
        if lName<>'' then
          lBuiltins.Add(lName+'='+lIn[i]);
        end;
      end;
    InsertUsedBuiltins(lOut,lBuiltins);
    lOut.SaveToFile(aKept);
  finally
    lBuiltins.Free;
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
