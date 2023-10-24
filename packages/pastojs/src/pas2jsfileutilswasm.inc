{%MainUnit pas2jsfileutils.pas}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Unix backend of pas2jsfileutils

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

function FilenameIsAbsolute(const aFilename: string): boolean;
begin
  Result:=ExpandFileName(aFilename)=aFileName;
end;

function ExpandFileNamePJ(const FileName: string; BaseDir: string): string;
var
  IsAbs: Boolean;
  HomeDir, Fn: String;
begin
  Fn := FileName;
  ForcePathDelims(Fn);
  IsAbs := FileNameIsUnixAbsolute(Fn);
  if (not IsAbs) then
  begin
    if ((Length(Fn) > 1) and (Fn[1] = '~') and (Fn[2] = '/')) or (Fn = '~') then
    begin
      HomeDir := GetEnvironmentVariablePJ('HOME');
      if not FileNameIsUnixAbsolute(HomeDir) then
        HomeDir := ExpandFileNamePJ(HomeDir,'');
      Fn := HomeDir + Copy(Fn,2,length(Fn));
      IsAbs := True;
    end;
  end;
  if IsAbs then
  begin
    Result := ResolveDots(Fn);
  end
  else
  begin
    if (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(GetCurrentDirPJ) + Fn
    else
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    Fn := ResolveDots(Fn);
    //if BaseDir is not absolute then this needs to be expanded as well
    if not FileNameIsUnixAbsolute(Fn) then
      Fn := ExpandFileNamePJ(Fn, '');
    Result := Fn;
  end;
end;

function GetCurrentDirPJ: String;
begin
  Result:=GetCurrentDir;
end;

function GetPhysicalFilename(const Filename: string; ExceptionOnError: boolean
  ): string;
var
  OldPath: String;
  NewPath: String;
  p: PAnsiChar;
begin
  Result:=Filename;
  p:=PAnsiChar(Result);
  repeat
    while p^='/' do
      inc(p);
    if p^=#0 then exit;
    if p^<>'/' then
    begin
      repeat
        inc(p);
      until p^ in [#0,'/'];
      OldPath:=LeftStr(Result,p-PAnsiChar(Result));
      NewPath:=ResolveSymLinks(OldPath,ExceptionOnError);
      if NewPath='' then exit('');
      if OldPath<>NewPath then
      begin
        Result:=NewPath+copy(Result,length(OldPath)+1,length(Result));
        p:=PAnsiChar(Result)+length(NewPath);
      end;
    end;
  until false;
end;

function ResolveSymLinks(const Filename: string; ExceptionOnError: boolean
  ): string;
var
  LinkFilename: rawbytestring;
  AText: string;
begin
  Result:=Filename;
  if not FileGetSymLinkTarget(FileName,LinkFileName) then
    raise EFOpenError.Create(AText);
  if not FilenameIsAbsolute(LinkFilename) then
    Result:=ExtractFilePath(Result)+LinkFilename
  else
    Result:=LinkFilename;
end;

function IsUNCPath(const Path: String): Boolean;
begin
  Result := false;
  if Path='' then ;
end;

function ExtractUNCVolume(const Path: String): String;
begin
  Result := '';
  if Path='' then ;
end;

function FileIsWritable(const AFilename: string): boolean;

var
  FD : THandle;

begin
  FD := FileOpen(aFileName,fmOpenWrite);
  Result:=FD>0;
  if Result then
    FileClose(FD);
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=False;
end;

function GetEnvironmentVariableCountPJ: Integer;
begin
  Result:=GetEnvironmentVariableCount;
end;

function GetEnvironmentStringPJ(Index: Integer): string;
begin
  Result:=ConsoleToUTF8(GetEnvironmentString(Index));
end;

function GetEnvironmentVariablePJ(const EnvVar: string): String;
begin
  Result:=ConsoleToUTF8(GetEnvironmentVariable(EnvVar));
end;

