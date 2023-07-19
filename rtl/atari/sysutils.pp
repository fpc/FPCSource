{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    Sysutils unit for Atari

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit sysutils;

interface

{$MODE objfpc}
{$MODESWITCH OUT}
{ force ansistrings }
{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

{$DEFINE OS_FILESETDATEBYNAME}
{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}

{OS has only 1 byte version for ExecuteProcess}
{$define executeprocuni}

{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }


implementation

uses
{  dos,} sysconst;

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{ Include platform independent implementation part }
{$i sysutils.inc}


{$i gemdos.inc}

var
  basepage: PPD; external name '__base';



{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}{ Required for correct usage of these routines }


(****** non portable routines ******)

function FileOpen(const FileName: rawbytestring; Mode: Integer): THandle;
begin
  { Mode has some Share modes. Maybe something for MiNT? }
  { Lower three bits of Mode are actually TOS compatible }
  FileOpen:=gemdos_fopen(PAnsiChar(FileName), Mode and 3);
  if FileOpen < -1 then
    FileOpen:=-1;
end;


function FileGetDate(Handle: THandle) : Int64;
var
  td: TDOSTIME;
begin
  { Fdatime doesn't report errors... }
  gemdos_fdatime(@td,handle,0);
  result:=(td.date shl 16) or td.time;
end;


function FileSetDate(Handle: THandle; Age: Int64) : LongInt;
var
  td: TDOSTIME;
begin
  td.date:=(Age shr 16) and $ffff;
  td.time:=Age and $ffff;
  gemdos_fdatime(@td,handle,1);
  { Fdatime doesn't report errors... }
  result:=0;
end;


function FileSetDate(const FileName: RawByteString; Age: Int64) : LongInt;
var
  f: THandle;
begin
  FileSetDate:=-1;
  f:=FileOpen(FileName,fmOpenReadWrite);
  if f < 0 then
    exit;
  FileSetDate(f,Age);
  FileClose(f);
end;


function FileCreate(const FileName: RawByteString) : THandle;
begin
  FileCreate:=gemdos_fcreate(PAnsiChar(FileName),0);
  if FileCreate < -1 then
    FileCreate:=-1;
end;

function FileCreate(const FileName: RawByteString; Rights: integer): THandle;
begin
  { Rights are Un*x extension. Maybe something for MiNT? }
  FileCreate:=FileCreate(FileName);
end;

function FileCreate(const FileName: RawByteString; ShareMode: integer; Rights : integer): THandle;
begin
  { Rights and ShareMode are Un*x extension. Maybe something for MiNT? }
  FileCreate:=FileCreate(FileName);
end;


function FileRead(Handle: THandle; out Buffer; Count: LongInt): LongInt;
begin
  FileRead:=-1;
  if (Count<=0) then
    exit;

  FileRead:=gemdos_fread(handle, count, @buffer);
  if FileRead < -1 then
    FileRead:=-1;
end;


function FileWrite(Handle: THandle; const Buffer; Count: LongInt): LongInt;
begin
  FileWrite:=-1;
  if (Count<=0) then 
    exit;

  FileWrite:=gemdos_fwrite(handle, count, @buffer);
  if FileWrite < -1 then
    FileWrite:=-1;
end;


function FileSeek(Handle: THandle; FOffset, Origin: LongInt) : LongInt;
var
  dosResult: longint;
begin
  FileSeek:=-1;

  { TOS seek mode flags are actually compatible to DOS/TP }
  dosResult:=gemdos_fseek(FOffset, Handle, Origin);
  if dosResult < 0 then
    exit;

  FileSeek:=dosResult;
end;

function FileSeek(Handle: THandle; FOffset: Int64; Origin: Longint): Int64;
begin
  FileSeek:=FileSeek(Handle,LongInt(FOffset),Origin);
end;


procedure FileClose(Handle: THandle);
begin
  gemdos_fclose(handle);
end;


function FileTruncate(Handle: THandle; Size: Int64): Boolean;
begin
  FileTruncate:=False;
end;


function DeleteFile(const FileName: RawByteString) : Boolean;
begin
  DeleteFile:=gemdos_fdelete(PAnsiChar(FileName)) >= 0;
end;


function RenameFile(const OldName, NewName: RawByteString): Boolean;
begin
  RenameFile:=gemdos_frename(0,PAnsiChar(oldname),PAnsiChar(newname)) >= 0;
end;


(****** end of non portable routines ******)


function FileAge (const FileName : RawByteString): Int64;
var
  f: THandle;
begin
  FileAge:=-1;
  f:=FileOpen(FileName,fmOpenRead);
  if f < 0 then
    exit;
  FileAge:=FileGetDate(f);
  FileClose(f);
end;


function FileGetSymLinkTarget(const FileName: RawByteString; out SymLinkRec: TRawbyteSymLinkRec): Boolean;
begin
  Result := False;
end;


function FileExists (const FileName : RawByteString; FollowLink : Boolean) : Boolean;
var
  Attr: longint;
begin
  FileExists:=false;
  Attr:=FileGetAttr(FileName);
  if Attr < 0 then
    exit;

  result:=(Attr and (faVolumeID or faDirectory)) = 0;
end;


type
  PInternalFindData = ^TInternalFindData;
  TInternalFindData = record
    dta_original: pointer;
    dta_search: TDTA;
  end;


Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint;
var
  dosResult: longint;
  IFD: PInternalFindData;
begin
  result:=-1; { We emulate Linux/Unix behaviour, and return -1 on errors. }

  new(IFD);
  IFD^.dta_original:=gemdos_getdta;
  gemdos_setdta(@IFD^.dta_search);

  Rslt.FindHandle:=nil;
  dosResult:=gemdos_fsfirst(PAnsiChar(path), Attr and faAnyFile);
  if dosResult < 0 then
    begin
      InternalFindClose(IFD);
      exit;
    end;

  Rslt.FindHandle:=IFD;
  with IFD^.dta_search do
    begin
      Name:=d_fname;
      SetCodePage(Name,DefaultFileSystemCodePage,false);

      Rslt.Time:=(d_date shl 16) or d_time;
      Rslt.Size:=d_length;

      { "128" is Windows "NORMALFILE" attribute. Some buggy code depend on this... :( (KB) }
      Rslt.Attr := 128 or d_attrib;
    end;

  result:=0;
end;


Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint;
var
  dosResult: longint;
  IFD: PInternalFindData;
begin
  result:=-1;
  IFD:=PInternalFindData(Rslt.FindHandle);
  if not assigned(IFD) then
    exit;

  dosResult:=gemdos_fsnext;
  if dosResult < 0 then
    exit;

  with IFD^.dta_search do
    begin
      Name:=d_fname;
      SetCodePage(Name,DefaultFileSystemCodePage,false);

      Rslt.Time:=(d_date shl 16) or d_time;
      Rslt.Size:=d_length;

      { "128" is Windows "NORMALFILE" attribute. Some buggy code depend on this... :( (KB) }
      Rslt.Attr := 128 or d_attrib;
    end;

  result:=0;
end;


Procedure InternalFindClose(var Handle: Pointer);
var
  IFD: PInternalFindData;
begin
  IFD:=PInternalFindData(Handle);
  if not assigned(IFD) then
    exit;

  gemdos_setdta(IFD^.dta_original);

  dispose(IFD);
  IFD:=nil;
end;


(****** end of non portable routines ******)

Function FileGetAttr (Const FileName : RawByteString) : Longint;
begin
  FileGetAttr:=gemdos_fattrib(PAnsiChar(FileName),0,0);
end;


Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
begin
  FileSetAttr:=gemdos_fattrib(PAnsiChar(FileName),1,Attr and faAnyFile);

  if FileSetAttr < -1 then
    FileSetAttr:=-1
  else
    FileSetAttr:=0;
end;



{****************************************************************************
                              Disk Functions
****************************************************************************}

function DiskSize(Drive: Byte): Int64;
var
  dosResult: longint;
  di: TDISKINFO;
begin
  DiskSize := -1;

  dosResult:=gemdos_dfree(@di,drive);
  if dosResult < 0 then
    exit;

  DiskSize:=di.b_total * di.b_secsiz * di.b_clsiz;
end;

function DiskFree(Drive: Byte): Int64;
var
  dosResult: longint;
  di: TDISKINFO;
begin
  DiskFree := -1;

  dosResult:=gemdos_dfree(@di,drive);
  if dosResult < 0 then
    exit;

  DiskFree:=di.b_free * di.b_secsiz * di.b_clsiz;
end;

function DirectoryExists(const Directory: RawByteString; FollowLink : Boolean): Boolean;
var
  Attr: longint;
begin
  DirectoryExists:=false;
  Attr:=FileGetAttr(Directory);
  if Attr < 0 then
    exit;

  result:=(Attr and faDirectory) <> 0;
end;



{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
var
  TOSTime: Longint;
begin
   LongRec(TOSTime).hi:=gemdos_tgetdate;
   LongRec(TOSTime).lo:=gemdos_tgettime;
   DateTimeToSystemTime(FileDateToDateTime(TOSTime),SystemTime);
end;


Procedure InitAnsi;
Var
  i : longint;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 191 do
    UpperCaseTable[i] := chr(i);
  Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 191 do
    LowerCaseTable[i] := chr(i);
  Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;


Procedure InitInternational;
begin
  InitInternationalGeneric;
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;
begin
  Result:=Format(SUnknownErrorCode,[ErrorCode]);
end;

function GetLastOSError: Integer;
begin
  result:=-1;
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

function fpGetEnv(const envvar : ShortString): RawByteString; external name '_fpc_atari_getenv';

function GetPathString: String;
begin
  {writeln('Unimplemented GetPathString');}
  result := '';
end;

Function GetEnvironmentVariable(Const EnvVar : String) : String;
begin
   GetEnvironmentVariable := fpgetenv(envvar);
end;

Function GetEnvironmentVariableCount : Integer;
var
  hp : PAnsiChar;
begin
  result:=0;
  hp:=basepage^.p_env;
  If (Hp<>Nil) then
    while hp^<>#0 do
      begin
      Inc(Result);
      hp:=hp+strlen(hp)+1;
      end;
end;

Function GetEnvironmentString(Index : Integer) : {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
var
  hp : PAnsiChar;
begin
  result:='';
  hp:=basepage^.p_env;
  If (Hp<>Nil) then
    begin
      while (hp^<>#0) and (Index>1) do
        begin
          Dec(Index);
          hp:=hp+strlen(hp)+1;
        end;
    If (hp^<>#0) then
      begin
        Result:=hp;
      end;
    end;
end;

function ExecuteProcess (const Path: RawByteString; const ComLine: RawByteString;Flags:TExecuteFlags=[]):
                                                                       integer;
var
  tmpPath: RawByteString;
  pcmdline: ShortString;
  CommandLine: RawByteString;
  E: EOSError;
  env, s: PAnsiChar;
  buf, start: PAnsiChar;
  enlen, len: SizeInt;
  hp : PAnsiChar;

begin
  tmpPath:=ToSingleByteFileSystemEncodedFileName(Path);
  pcmdline:=ToSingleByteFileSystemEncodedFileName(ComLine);

  { count up space needed for environment }
  enlen := 0;
  hp:=basepage^.p_env;
  If (Hp<>Nil) then
    while hp^<>#0 do
      begin
      len := strlen(hp) + 1;
      inc(enlen, len);
      inc(hp, len);
      end;

  { count up space needed for arguments }
  len := strlen(PAnsiChar(tmpPath)) + 1;
  inc(enlen, len);
  buf := PAnsiChar(ComLine);
  while (buf^<>#0) do                   // count nr of args
   begin
     while (buf^ in [' ',#9,#10]) do    // Kill separators.
      inc(buf);
     if buf^=#0 Then
       break;
     if buf^='"' Then                   // quotes argument?
       begin
         inc(buf);
         start := buf;
         while not (buf^ in [#0,'"']) do // then end of argument is end of string or next quote
           inc(buf);
         len := buf - start;
         if len=0 then len := 1; (* TODO: needs to set NULL environment variable *)
         inc(len);
         inc(enlen, len);
         if buf^='"' then                // skip closing quote.
           inc(buf);
       end
     else
       begin                            // else std
         start := buf;
         while not (buf^ in [' ',#0,#9,#10]) do
           inc(buf);
         len := buf - start + 1;
         inc(enlen, len);
       end;
   end;

  inc(enlen, 64); { filler for stuff like ARGV= and zeros }

  env := gemdos_malloc(enlen);
  if env = nil then
    result := ENSMEM
  else
    begin
      s := env;
      { copy the environment }
      hp:=basepage^.p_env;
      If (Hp<>Nil) then
        while hp^<>#0 do
          begin
          len := strlen(hp) + 1;
          strcopy(s, hp);
          inc(hp, len);
          inc(s, len);
          end;

      { start of arguments }
      strcopy(s, 'ARGV=');
      inc(s, 6); { s+=sizeof("ARGV=") }

      { copy argv[0] }
      buf := PAnsiChar(tmpPath);
      len := strlen(buf) + 1;
      strcopy(s, buf);
          inc(s, len);

      { copy the parameters }
          buf:=PAnsiChar(ComLine);
          while (buf^<>#0) do
           begin
             while (buf^ in [' ',#9,#10]) do    // Kill separators.
               inc(buf);
             if buf^=#0 Then
               break;
             if buf^='"' Then                   // quotes argument?
               begin
                 inc(buf);
                 start := buf;
                 while not (buf^ in [#0,'"']) do // then end of argument is end of string or next quote
                   begin
                     s^ := buf^;
                     inc(s);
                     inc(buf);
                   end;
                 if buf = start then
                   begin
                     s^ := ' ';
                     inc(s);
                   end;
                 if buf^='"' then                // skip closing quote.
                   inc(buf);
                 s^ := #0;
                 inc(s);
               end
             else
               begin
                 start := buf;
                 while not (buf^ in [' ',#0,#9,#10]) do
                   begin
                     s^ := buf^;
                     inc(s);
                     inc(buf);
                   end;
                 s^ := #0;
                 inc(s);
               end;
           end;

      { tie off environment }
      s^ := #0;
      inc(s);
      s^ := #0;

      { signal Extended Argument Passing }
      pcmdline[0] := #127;
      { the zero offset for cmdline is actually correct here. pexec() expects
        pascal formatted string for cmdline, so length in first byte }
      result:=gemdos_pexec(0,PAnsiChar(tmpPath),@pcmdline[0],env);
      gemdos_mfree(env);
    end;

  if result < 0 then begin
    if ComLine = '' then
      CommandLine := Path
    else
      CommandLine := Path + ' ' + ComLine;

    E := EOSError.CreateFmt (SExecuteProcessFailed, [CommandLine, result]);
    E.ErrorCode := result;
    raise E;
  end;
end;

function ExecuteProcess (const Path: RawByteString;
                                  const ComLine: array of RawByteString;Flags:TExecuteFlags=[]): integer;
var
  CommandLine: RawByteString;
  I: integer;

begin
  Commandline := '';
  for I := 0 to High (ComLine) do
   if Pos (' ', ComLine [I]) <> 0 then
    CommandLine := CommandLine + ' ' + '"' + ToSingleByteFileSystemEncodedFileName(ComLine [I]) + '"'
   else
    CommandLine := CommandLine + ' ' + ToSingleByteFileSystemEncodedFileName(Comline [I]);
  ExecuteProcess := ExecuteProcess (Path, CommandLine);
end;

procedure Sleep(Milliseconds: cardinal);
begin
  {writeln('Unimplemented Sleep');}
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;
  InitInternational;    { Initialize internationalization settings }
  OnBeep:=Nil;          { No SysBeep() on Atari for now. }

Finalization
  FreeTerminateProcs;
  DoneExceptions;
end.
