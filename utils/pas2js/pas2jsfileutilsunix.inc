{%MainUnit pas2jsfileutils.pas}

function FilenameIsAbsolute(const aFilename: string): boolean;
begin
  Result:=FilenameIsUnixAbsolute(aFilename);
end;

function ExpandFileNameUTF8(const FileName: string; BaseDir: string): string;
var
  IsAbs: Boolean;
  CurDir, HomeDir, Fn: String;
begin
  Fn := FileName;
  ForcePathDelims(Fn);
  IsAbs := FileNameIsUnixAbsolute(Fn);
  if (not IsAbs) then
  begin
    CurDir := GetCurrentDirUtf8;
    if ((Length(Fn) > 1) and (Fn[1] = '~') and (Fn[2] = '/')) or (Fn = '~') then
    begin
      HomeDir := GetEnvironmentVariableUTF8('HOME');
      if not FileNameIsUnixAbsolute(HomeDir) then
        HomeDir := ExpandFileNameUtf8(HomeDir,'');
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
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    Fn := ResolveDots(Fn);
    //if BaseDir is not absolute then this needs to be expanded as well
    if not FileNameIsUnixAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;

function GetCurrentDirUTF8: String;
begin
  Result:=GetCurrentDir;
end;

function GetPhysicalFilename(const Filename: string; ExceptionOnError: boolean
  ): string;
var
  OldPath: String;
  NewPath: String;
  p: PChar;
begin
  Result:=Filename;
  p:=PChar(Result);
  repeat
    while p^='/' do
      inc(p);
    if p^=#0 then exit;
    if p^<>'/' then
    begin
      repeat
        inc(p);
      until p^ in [#0,'/'];
      OldPath:=LeftStr(Result,p-PChar(Result));
      NewPath:=ResolveSymLinks(OldPath,ExceptionOnError);
      if NewPath='' then exit('');
      if OldPath<>NewPath then
      begin
        Result:=NewPath+copy(Result,length(OldPath)+1,length(Result));
        p:=PChar(Result)+length(NewPath);
      end;
    end;
  until false;
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
  while Depth<12 do begin
    inc(Depth);
    LinkFilename:=fpReadLink(Result);
    if LinkFilename='' then begin
      AText:='"'+Filename+'"';
      case fpGetErrno() of
      ESysEAcces:
        AText:='read access denied for '+AText;
      ESysENoEnt:
        AText:='a directory component in '+AText
                            +' does not exist or is a dangling symlink';
      ESysENotDir:
        AText:='a directory component in '+AText+' is not a directory';
      ESysENoMem:
        AText:='insufficient memory';
      ESysELoop:
        AText:=AText+' has a circular symbolic link';
      else
        // not a symbolic link, just a regular file
        exit;
      end;
      if (not ExceptionOnError) then begin
        Result:='';
        exit;
      end;
      raise EFOpenError.Create(AText);
    end else begin
      if not FilenameIsAbsolute(LinkFilename) then
        Result:=ExtractFilePath(Result)+LinkFilename
      else
        Result:=LinkFilename;
    end;
  end;
  // probably an endless loop
  if ExceptionOnError then
    raise EFOpenError.Create('too many links, maybe an endless loop.')
  else
    Result:='';
end;

function GetEnvironmentVariableCountUTF8: Integer;
begin
  Result:=GetEnvironmentVariableCount;
end;

function GetEnvironmentStringUTF8(Index: Integer): string;
begin
  Result:=ConsoleToUTF8(GetEnvironmentString(Index));
end;

function GetEnvironmentVariableUTF8(const EnvVar: string): String;
begin
  Result:=ConsoleToUTF8(GetEnvironmentVariable(EnvVar));
end;

{$IFNDEF Darwin}
function GetUnixEncoding: string;
var
  i: integer;
begin
  Result:=EncodingSystem;
  i:=pos('.',Lang);
  if (i>0) and (i<=length(Lang)) then
    Result:=copy(Lang,i+1,length(Lang)-i);
end;
{$ENDIF}

function GetConsoleTextEncoding: string;
begin
  Result:=GetDefaultTextEncoding;
end;

function UTF8ToSystemCP(const s: string): string;
begin
  if NonUTF8System and not IsASCII(s) then
  begin
    Result:=UTF8ToAnsi(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
  end
  else
    Result:=s;
end;

function SystemCPToUTF8(const s: string): string;
begin
  if NonUTF8System and not IsASCII(s) then
  begin
    Result:=AnsiToUTF8(s);
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
  end
  else
    Result:=s;
end;

function ConsoleToUTF8(const s: string): string;
begin
  Result:=SystemCPToUTF8(s);
end;

function UTF8ToConsole(const s: string): string;
begin
  Result:=UTF8ToSystemCP(s);
end;

procedure InitPlatform;
begin

end;

procedure FinalizePlatform;
begin

end;
