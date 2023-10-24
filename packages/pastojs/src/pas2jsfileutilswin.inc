{%MainUnit pas2jsfileutils.pas}
{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
{$IF DEFINED(UTF8_RTL) AND NOT DEFINED(WINCE)}
  {$DEFINE ArgsWAsUTF8}
{$ENDIF}

{$IFDEF OldStuff}
//Function prototypes
var _ParamStrUtf8: Function(Param: Integer): string;
{$ENDIF}

var
  ArgsW: Array of WideString;
  ArgsWCount: Integer; // length(ArgsW)+1
  {$IFDEF ArgsWAsUTF8}
  ArgsUTF8: Array of String; // the ArgsW array as UTF8
  OldArgV: PPAnsiChar = nil;
  {$IFEND}

{$ifndef wince}
{$IFDEF OldStuff}
function ParamStrUtf8Ansi(Param: Integer): String;
begin
  Result:=ObjPas.ParamStr(Param);
end;
{$ENDIF}
{$endif wince}

{$IFDEF OldStuff}
function ParamStrUtf8Wide(Param: Integer): String;
begin
  if ArgsWCount <> ParamCount then
  begin
    //DebugLn('Error: ParamCount <> ArgsWCount!');
    Result := ObjPas.ParamStr(Param);
  end
  else
  begin
    if (Param <= ArgsWCount) then
      {$IFDEF ACP_RTL}
      Result := String(UnicodeString(ArgsW[Param]))
      {$ELSE}
      Result := UTF16ToUTF8(ArgsW[Param])
      {$ENDIF ACP_RTL}
    else
      Result := '';
  end;
end;
{$ENDIF oldstuff}

{$IFDEF ArgsWAsUTF8}
procedure SetupArgvAsUtf8;
var
  i: Integer;
begin
  SetLength(ArgsUTF8,length(ArgsW));
  OldArgV:=argv;
  GetMem(argv,SizeOf(Pointer)*length(ArgsW));
  for i:=0 to length(ArgsW)-1 do
  begin
    ArgsUTF8[i]:=ArgsW{%H-}[i];
    argv[i]:=PAnsiChar(ArgsUTF8[i]);
  end;
end;
{$endif}

procedure SetupCommandlineParametersWide;
var
  ArgLen, Start, CmdLen, i, j: SizeInt;
  Quote   : Boolean;
  Buf: array[0..259] of WChar;  // need MAX_PATH bytes, not 256!
  PCmdLineW: PWideChar;
  CmdLineW: WideString;

  procedure AllocArg(Idx, Len:longint);
  begin
    if (Idx >= ArgsWCount) then
      SetLength(ArgsW, Idx + 1);
    SetLength(ArgsW[Idx], Len);
  end;

begin
  { create commandline, it starts with the executed filename which is argv[0] }
  { Win32 passes the command NOT via the args, but via getmodulefilename}
  ArgsWCount := 0;
  ArgLen := GetModuleFileNameW(0, @buf[0], sizeof(buf));

  //writeln('ArgLen = ',Arglen);

  buf[ArgLen] := #0; // be safe, no terminating 0 on XP
  allocarg(0,arglen);
  move(buf[0],ArgsW[0][1],arglen * SizeOf(WChar));

  //writeln('ArgsW[0] = ',ArgsW[0]);

  PCmdLineW := nil;
  { Setup cmdline variable }
  PCmdLineW := GetCommandLineW;
  CmdLen := StrLen(PCmdLineW);

  //writeln('StrLen(PCmdLineW) = ',CmdLen);

  SetLength(CmdLineW{%H-}, CmdLen);
  Move(PCmdLineW^, CmdLineW[1], CmdLen * SizeOf(WChar));


  //debugln(CmdLineW);
  //for i := 1 to CmdLen do DbgOut(DbgS(i mod 10)); debugln;

  i := 1;
  while (i <= CmdLen) do
  begin
    //debugln('Next');
    //DbgOut('i=',DbgS(i),' CmdLineW[',DbgS(i),']=');if i<=CmdLen then debugln(CmdLineW[i]) else debugln('#0');
    //skip leading spaces
    while (i <= CmdLen) and (CmdLineW[i] <= #32) do Inc(i);
    //DbgOut('After skipping spaces: i=',DbgS(i),' CmdLineW[',DbgS(i),']=');if i<=CmdLen then debugln(CmdLineW[i]) else debugln('#0');
    if (i > CmdLen) then Break;
    Quote := False;
    Start := i;
    ArgLen := 0;
    while (i <= CmdLen) do
    begin //find next commandline parameter
      case CmdLineW[i] of
        #1..#32:
        begin
          if Quote then
          begin
            //debugln('i=',DbgS(i),': Space in Quote');
            Inc(ArgLen)
          end
          else
          begin
            //debugln('i=',DbgS(i),': Space in NOT Quote');
            Break;
          end;
        end;
        '"':
        begin
          if (i < CmdLen) and (CmdLineW[i+1] <> '"') then
          begin
            //debugln('i=',DbgS(i),': Quote := not Quote');
            Quote := not Quote
          end
          else
          begin
            //debugln('i=',DbgS(i),': Skip Quote');
            Inc(i);
          end;
        end;
        else Inc(ArgLen);
      end;//case
      Inc(i);
    end; //find next commandline parameter

    //debugln('ArgWCount=',DbgS(ArgsWCount),' Start=',DbgS(start),' ArgLen=',DbgS(arglen),' i=',DbgS(i));

    //we already have (a better) ArgW[0]
    if (ArgsWCount > 0) then
    begin //Process commandline parameter
      AllocArg(ArgsWCount, ArgLen);
      Quote := False;
      i := Start;
      j := 1;
      while (i <= CmdLen) do
      begin
        case CmdLineW[i] of
          #1..#32:
          begin
            if Quote then
            begin
              //if j > ArgLen then debugln('Error whitespace: j > ArgLen: j=',DbgS(j),' ArgLen=',DbgS(arglen));
              ArgsW[ArgsWCount][j] := CmdLineW[i];
              Inc(j);
            end
            else
              Break;
          end;
          '"':
          begin
            if (i < CmdLen) and (CmdLineW[i+1] <> '"') then
              Quote := not Quote
            else
              Inc(i);
          end;
          else
          begin
            //if j > ArgLen then debugln('Error Quote: j > ArgLen: j=',DbgS(j),' ArgLen=',DbgS(arglen));
            ArgsW[ArgsWCount][j] := CmdLineW[i];
            Inc(j);
          end;
        end;
        Inc(i);
      end;

      //debugln('ArgsW[',DbgS(ArgsWCount),'] = ',ArgsW[ArgsWCount]);
    end; // Process commandline parameter
    Inc(ArgsWCount);

  end;
  Dec(ArgsWCount);
  //Note:
  //On WinCe Argsv is a static function, so we cannot change it.
  //This might change in the future if Argsv on WinCE will be declared as a function variable
  {$IFDEF ArgsWAsUTF8}
  if DefaultSystemCodePage=CP_UTF8 then
    SetupArgvAsUtf8;
  {$IFEND}
end;

function FilenameIsAbsolute(const aFilename: string): boolean;
begin
  Result:=FilenameIsWinAbsolute(aFilename);
end;

procedure GetDirUtf8(DriveNr: Byte; var Dir: String);
{This procedure may not be threadsafe, because SetCurrentDirectory isn't}
{$ifndef WinCE}
var
  w, D: WideString;
  SavedDir: WideString;
  res : Integer;
{$endif}
begin
  {$ifdef WinCE}
  Dir := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  //writeln('GetDirWide START');
  if not (DriveNr = 0) then
  begin
    res := GetCurrentDirectoryW(0, nil);
    SetLength(SavedDir{%H-}, res);
    res:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.GetCurrentDirectoryW(res, @SavedDir[1]);
    SetLength(SavedDir,res);

    D := WideChar(64 + DriveNr) + ':';
    if not SetCurrentDirectoryW(@D[1]) then
    begin
      Dir := AnsiChar(64 + DriveNr) + ':\';
      SetCurrentDirectoryW(@SavedDir[1]);
      Exit;
    end;
  end;
  res := GetCurrentDirectoryW(0, nil);
  SetLength(w{%H-}, res);
  res := GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Dir:=UTF16ToUTF8(w);
  if not (DriveNr = 0) then SetCurrentDirectoryW(@SavedDir[1]);
  //writeln('GetDirWide END');
  {$endif}
end;

function ExpandFileNamePJ(const FileName: string; {const} BaseDir: String = ''): String;
var
  IsAbs, StartsWithRoot, CanUseBaseDir : Boolean;
  {$ifndef WinCE}
  HasDrive: Boolean;
  FnDrive, CurDrive, BaseDirDrive: AnsiChar;
  {$endif}
  CurDir, Fn: String;
begin
  //writeln('LazFileUtils.ExpandFileNamePJ');
  //writeln('FileName = "',FileName,'"');
  //writeln('BaseDir  = "',BaseDir,'"');

  Fn := FileName;
  //if Filename uses ExtendedLengthPath scheme then it cannot be expanded
  //AND it should not be altered by ForcePathDelims or ResolveDots
  //See: http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
  if (Length(Fn) > 3) and (Fn[1] = PathDelim) and (Fn[2] = PathDelim) and
     (Fn[3] = '?') and (Fn[4] = PathDelim) //Do NOT use AllowDirectorySeparators here!
     then Exit(FN);
  ForcePathDelims(Fn);
  IsAbs := FileNameIsWinAbsolute(Fn);
  if not IsAbs then
  begin
    StartsWithRoot := (Fn = '\') or
                      ((Length(Fn) > 1) and
                      (Fn[1] = DirectorySeparator) and
                      (Fn[2] <> DirectorySeparator));
    {$ifndef WinCE}
    HasDrive := (Length(Fn) > 1) and
                (Fn[2] = ':') and
                (UpCase(Fn[1]) in ['A'..'Z']);

    if HasDrive then
    begin
      FnDrive := UpCase(Fn[1]);
      GetDirUtf8(Byte(FnDrive)-64, CurDir{%H-});
      CurDrive := UpCase(GetCurrentDirPJ[1]);
    end
    else
    begin
      CurDir := GetCurrentDirPJ;
      FnDrive := UpCase(CurDir[1]);
      CurDrive := FnDrive;
    end;

    //writeln('HasDrive = ',HasDrive,' Fn = ',Fn);
    //writeln('CurDir = ',CurDir);
    //writeln('CurDrive = ',CurDrive);
    //writeln('FnDrive  = ',FnDrive);

    if (Length(BaseDir) > 1) and (UpCase(BaseDir[1]) in ['A'..'Z']) and (BaseDir[2] = ':') then
    begin
      BaseDirDrive := BaseDir[1]
    end
    else
    begin
      if HasDrive then
        BaseDirDrive := CurDrive
      else
        BaseDirDrive := #0;
    end;

    //You cannot use BaseDir if both FileName and BaseDir includes a drive and they are not the same
    CanUseBaseDir := ((BaseDirDrive = #0) or
                     (not HasDrive) or
                     (HasDrive and (FnDrive = BaseDirDrive)))
                     and (BaseDir <> '');

    //writeln('CanUseBaseDir = ',CanUseBaseDir);

    if not HasDrive and StartsWithRoot and not CanUseBaseDir then
    begin
      //writeln('HasDrive and StartsWithRoot');
      Fn := Copy(CurDir,1,2) + Fn;
      HasDrive := True;
      IsAbs := True;
    end;
    //FileNames like C:foo, strip Driveletter + colon
    if HasDrive and not IsAbs then Delete(Fn,1,2);

    //writeln('HasDrive = ',Hasdrive,' Fn = ',Fn);
    {$else}
    CanUseBaseDir := True;
    {$endif WinCE}
  end;
  if IsAbs then
  begin
    //writeln('IsAbs = True -> Exit');
    Result := ResolveDots(Fn);
  end
  else
  begin
    if not CanUseBaseDir or (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
    begin
      if (Length(Fn) > 0) and (Fn[1] = DirectorySeparator) then Delete(Fn,1,1);
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    end;

    Fn := ResolveDots(Fn);
    //if BaseDir is something like 'z:foo\' or '\' then this needs to be expanded as well
    if not FileNameIsAbsolute(Fn) then
      Fn := ExpandFileNamePJ(Fn, '');
    Result := Fn;
  end;
end;

function GetCurrentDirPJ: String;
{$ifndef WinCE}
var
  w   : UnicodeString;
  res : Integer;
  {$endif}
begin
  {$ifdef WinCE}
  Result := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  res:=GetCurrentDirectoryW(0, nil);
  SetLength(w{%H-}, res);
  res:={$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Result:=UTF16ToUTF8(w);
  {$endif}
end;

function GetPhysicalFilename(const Filename: string; ExceptionOnError: boolean
  ): string;
begin
  Result:=Filename;
  if ExceptionOnError then ;
end;

function ResolveSymLinks(const Filename: string; ExceptionOnError: boolean
  ): string;
begin
  Result:=Filename;
end;

function IsUNCPath(const Path: String): Boolean;
begin
  Result := (Length(Path) > 2)
    and (Path[1] in AllowDirectorySeparators)
    and (Path[2] in AllowDirectorySeparators);
end;

function ExtractUNCVolume(const Path: String): String;
var
  I, Len: Integer;

  // the next function reuses Len variable
  function NextPathDelim(const Start: Integer): Integer;// inline;
  begin
    Result := Start;
    while (Result <= Len) and not (Path[Result] in AllowDirectorySeparators) do
      inc(Result);
  end;

begin
  if not IsUNCPath(Path) then
    Exit('');
  I := 3;
  Len := Length(Path);
  if Path[I] = '?' then
  begin
    // Long UNC path form like:
    // \\?\UNC\ComputerName\SharedFolder\Resource or
    // \\?\C:\Directory
    inc(I);
    if not (Path[I] in AllowDirectorySeparators) then
      Exit('');
    if UpperCase(Copy(Path, I + 1, 3)) = 'UNC' then
    begin
      inc(I, 4);
      if I < Len then
        I := NextPathDelim(I + 1);
      if I < Len then
        I := NextPathDelim(I + 1);
    end;
  end
  else
  begin
    I := NextPathDelim(I);
    if I < Len then
      I := NextPathDelim(I + 1);
  end;
  Result := Copy(Path, 1, I);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=Integer({$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.GetFileAttributesW(PWideChar(UTF8Decode(FileName))));
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result:=((FileGetAttrUTF8(AFilename) and faReadOnly) = 0);
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=FileExists(AFilename);
end;

function GetEnvironmentVariableCountPJ: Integer;
var
  hp,p : PWideChar;
begin
  Result:=0;
  p:=GetEnvironmentStringsW;
  if p=nil then exit;
  hp:=p;
  while hp^<>#0 do
  begin
    Inc(Result);
    hp:=hp+strlen(hp)+1;
  end;
  FreeEnvironmentStringsW(p);
end;

function GetEnvironmentStringPJ(Index: Integer): string;
var
  hp,p : PWideChar;
begin
  Result:='';
  p:=GetEnvironmentStringsW;
  if p=nil then exit;
  hp:=p;
  while (hp^<>#0) and (Index>1) do
  begin
    Dec(Index);
    hp:=hp+strlen(hp)+1;
  end;
  if (hp^<>#0) then
    Result:=UTF16ToUTF8(hp);
  FreeEnvironmentStringsW(p);
end;

function GetEnvironmentVariablePJ(const EnvVar: string): String;
begin
  Result:=UTF16ToUTF8({$IFDEF FPC_DOTTEDUNITS}System.{$ENDIF}SysUtils.GetEnvironmentVariable(UTF8ToUTF16(EnvVar)));
end;

// AConsole - If false, it is the general system encoding,
//            if true, it is the console encoding
function GetWindowsEncoding(AConsole: Boolean = False): string;
var
  cp : UINT;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
begin
  if AConsole then cp := GetOEMCP
  else cp := GetACP;

  case cp of
    CP_UTF8: Result := EncodingUTF8;
  else
    Result:='cp'+IntToStr(cp);
  end;
end;

{$DEFINE HAVE_INITPLATFORM}
procedure InitPlatform;
begin
  {$ifndef WinCE}
  if Win32MajorVersion <= 4 then
  begin
    {$IFDEF OldStuff}
    _ParamStrUtf8 := @ParamStrUtf8Ansi;
    {$ENDIF}
  end
  else
  {$endif}
  begin
    ArgsWCount := -1;
    {$IFDEF OldStuff}
    _ParamStrUtf8 := @ParamStrUtf8Wide;
    {$ENDIF}
    SetupCommandlineParametersWide;
  end;
end;

{$DEFINE HAVE_FINALIZEPLATFORM}
procedure FinalizePlatform;
{$IFDEF ArgsWAsUTF8}
var
  p: PPAnsiChar;
{$ENDIF}
begin
  {$IFDEF ArgsWAsUTF8}
  // restore argv and free memory
  if OldArgV<>nil then
  begin
    p:=argv;
    argv:=OldArgV;
    Freemem(p);
  end;
  {$ENDIF}
end;
