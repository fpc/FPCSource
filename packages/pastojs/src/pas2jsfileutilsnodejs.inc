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
  Result:=NJS_Path.isAbsolute(aFilename);
end;

function ExpandFileNamePJ(const FileName: string; BaseDir: string): string;
var
  IsAbs: Boolean;
  HomeDir, Fn: String;
begin
  Fn := GetForcedPathDelims(Filename);
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
var
  LinkFilename: string;
  AText: string;
  Depth: Integer;
begin
  Result:=Filename;
  Depth:=0;
  while Depth<12 do
  begin
    inc(Depth);
    try
      LinkFilename:=NJS_FS.readlinkSync(Result);
    except
      if not ExceptionOnError then
        exit;
      if isString(JSExceptValue) then
        AText:=String(JSExceptValue)
      else if isObject(JSExceptValue) and isString(TJSObject(JSExceptValue)['message']) then
      begin
        if TJSObject(JSExceptValue)['code']='EINVAL' then
        begin
          // not a symbolic link
          exit;
        end;
        AText:=String(TJSObject(JSExceptValue)['message']);
      end else
        AText:='uknown error ('+jsTypeOf(JSExceptValue)+')';
      if Pos(Filename,AText)<1 then
        AText+=' "'+Filename+'"';
      raise EFOpenError.Create(AText);
    end;
    if LinkFilename='' then
    begin
      // not a symbolic link, just a regular file
      exit;
    end;
    if not FilenameIsAbsolute(LinkFilename) then
      Result:=ExtractFilePath(Result)+LinkFilename
    else
      Result:=LinkFilename;
  end;
  // probably an endless loop
  if ExceptionOnError then
    raise EFOpenError.Create('too many links, maybe an endless loop.')
  else
    Result:='';
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
begin
  try
    NJS_FS.accessSync(AFilename,W_OK);
  except
    exit(false);
  end;
  Result:=true;
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  try
    NJS_FS.accessSync(AFilename,X_OK);
  except
    exit(false);
  end;
  Result:=true;
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

{$IFDEF FPC_HAS_CPSTRING}
function GetConsoleTextEncoding: string;
begin
  Result:=GetDefaultTextEncoding;
end;
{$ENDIF}

end.

