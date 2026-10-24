{ %target=linux,freebsd,netbsd,openbsd,dragonfly,solaris,haiku,darwin }
{ FindFirst/FindNext wildcard matching honours FileNameCaseSensitive }
program tfindcase;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  cEte = #$C3#$A9't'#$C3#$A9'.TXT';

var
  gErrors: Integer = 0;
  gDir: string;


procedure CheckEqual(const aName, aValue, aExpected: string);

begin
  if aValue=aExpected then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR: got "',aValue,'", expected "',aExpected,'"');
    Inc(gErrors);
    end;
end;


procedure CreateEmptyFile(const aName: string);

var
  lHandle: THandle;

begin
  lHandle:=FileCreate(gDir+aName);
  if lHandle=THandle(-1) then
    begin
    Writeln('Cannot create ',gDir+aName);
    Halt(1);
    end;
  FileClose(lHandle);
end;


// The sorted, comma separated names of the files in gDir matching aPattern.
function FindNames(const aPattern: string): string;

var
  lList: TStringList;
  lInfo: TSearchRec;

begin
  lList:=TStringList.Create;
  try
    lList.Sorted:=True;
    if FindFirst(gDir+aPattern,faAnyFile and not faDirectory,lInfo)=0 then
      try
        repeat
          lList.Add(lInfo.Name);
        until FindNext(lInfo)<>0;
      finally
        FindClose(lInfo);
      end;
    lList.Delimiter:=',';
    lList.StrictDelimiter:=True;
    Result:=lList.DelimitedText;
  finally
    lList.Free;
  end;
end;


procedure CheckInsensitive;

begin
  CheckEqual('Insensitive *.txt',FindNames('*.txt'),'Alpha.TXT,beta.txt,'+cEte);
  CheckEqual('Insensitive a*',FindNames('a*'),'Alpha.TXT');
  CheckEqual('Insensitive ?LPHA.*',FindNames('?LPHA.*'),'Alpha.TXT');
  CheckEqual('Insensitive G*a.DAT',FindNames('G*a.DAT'),'GAMMA.dat');
  CheckEqual('Insensitive *X*',FindNames('*X*'),'Alpha.TXT,beta.txt,'+cEte);
  CheckEqual('Insensitive non-ASCII name',FindNames(#$C3#$A9't'#$C3#$A9'.t?t'),cEte);
  CheckEqual('Insensitive no match',FindNames('*.doc'),'');
end;


var
  lSensitiveVolume: Boolean;

begin
  gDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tfindcase'+IntToStr(GetProcessID);
  if not ForceDirectories(gDir) then
    begin
    Writeln('Cannot create ',gDir);
    Halt(1);
    end;
  gDir:=IncludeTrailingPathDelimiter(gDir);
  CreateEmptyFile('Alpha.TXT');
  CreateEmptyFile('beta.txt');
  CreateEmptyFile('GAMMA.dat');
  CreateEmptyFile(cEte);
  try
    lSensitiveVolume:=IsFileNameCaseSensitive(gDir);
    if lSensitiveVolume then
      begin
      CheckEqual('Sensitive *.txt',FindNames('*.txt'),'beta.txt');
      CheckEqual('Sensitive a*',FindNames('a*'),'');
      CheckEqual('Sensitive G*a.DAT',FindNames('G*a.DAT'),'');
      CheckEqual('Sensitive *A*',FindNames('*A*'),'Alpha.TXT,GAMMA.dat');
      end
    else
      CheckInsensitive;
    FileNameCaseSensitive:=False;
    try
      CheckInsensitive;
    finally
      FileNameCaseSensitive:=True;
    end;
    if lSensitiveVolume then
      CheckEqual('Sensitive again *.txt',FindNames('*.txt'),'beta.txt');
  finally
    DeleteFile(gDir+'Alpha.TXT');
    DeleteFile(gDir+'beta.txt');
    DeleteFile(gDir+'GAMMA.dat');
    DeleteFile(gDir+cEte);
    RemoveDir(gDir);
  end;
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
