{%MainUnit pas2jsfileutils.pas}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    NodeJS backend of pas2jsfileutils

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

function FilenameIsAbsolute(const aFilename: string): boolean;
begin
  writeln('FilenameIsAbsolute ToDo ',aFilename);
  Result:=FilenameIsUnixAbsolute(aFilename);
  raise Exception.Create('FilenameIsAbsolute ToDo');
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
  writeln('GetCurrentDirPJ ToDo');
  Result:='';
  raise Exception.Create('GetCurrentDirPJ ToDo');
end;

function GetPhysicalFilename(const Filename: string; ExceptionOnError: boolean
  ): string;
var
  OldPath, NewPath: String;
  p, l: integer;
begin
  Result:=Filename;
  p:=1;
  l:=length(Result);
  while p<=l do
  begin
    while (p<=l) and (Result[p]='/') do
      inc(p);
    if p>l then exit;
    if Result[p]<>'/' then
    begin
      repeat
        inc(p);
      until (p>l) or (Result[p]='/');
      OldPath:=LeftStr(Result,p-1);
      NewPath:=ResolveSymLinks(OldPath,ExceptionOnError);
      if NewPath='' then exit('');
      if OldPath<>NewPath then
      begin
        Result:=NewPath+copy(Result,length(OldPath)+1,length(Result));
        p:=length(NewPath)+1;
      end;
    end;
  end;
end;

function ResolveSymLinks(const Filename: string; ExceptionOnError: boolean
  ): string;
begin
  writeln('ResolveSymLinks ToDo ',Filename,' ',ExceptionOnError);
  Result:=Filename;
  raise Exception.Create('ResolveSymLinks ToDo');
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  writeln('FileIsWritable ToDo ',AFilename);
  Result := false;
  raise Exception.Create('FileIsWritable ToDo');
end;

function GetEnvironmentVariableCountPJ: Integer;
begin
  Result:=GetEnvironmentVariableCount;
end;

function GetEnvironmentStringPJ(Index: Integer): string;
begin
  Result:=GetEnvironmentString(Index);
end;

function GetEnvironmentVariablePJ(const EnvVar: string): String;
begin
  Result:=GetEnvironmentVariable(EnvVar);
end;

function GetConsoleTextEncoding: string;
begin
  Result:=GetDefaultTextEncoding;
end;

procedure InitPlatform;
begin

end;

