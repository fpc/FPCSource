unit cutils;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, WinSock2,
{$ELSE}
  BaseUnix,
{$ENDIF}
  ctypes;

const
  LIB_NAME = {$IFDEF MSWINDOWS}'msvcrt'{$ELSE}'c'{$ENDIF};
{$IFDEF UNIX}
  UINT16_MAX = 65535;
{$ENDIF}
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
{$IFDEF MSWINDOWS}
  DELTA_EPOCH_IN_MICROSECS: culonglong = 11644473600000000;

  SIGINT = 2;
  SIGILL = 4;
  SIGFPE = 8;
  SIGSEGV = 11;
  SIGTERM = 15;
  SIGBREAK = 21;
  SIGABRT = 22;
{$ENDIF}

type
{$IFDEF UNIX}
  __off_t = longint;
{$ENDIF}
  Pcchar = PAnsiChar;
  Ppcchar = ^Pcchar;
  FILEptr = ^File;
  seek_mode = longint;
  open_mode = (fopenread, fopenwrite, fappendwrite);
  signal_func = procedure(sig: cint); cdecl;

{$IFDEF MSWINDOWS}
function fpgettimeofday(tv: PTimeVal; tz: PTimeZone): cint;
procedure _tzset; cdecl; external LIB_NAME name '_tzset';
function _timezone: cint; cdecl; external LIB_NAME name '_timezone';
function _daylight: clong; cdecl; external LIB_NAME name '__daylight';
{$ENDIF}

{$IFDEF UNIX}
function sscanf(s: Pcchar; format: Pcchar): cint; cdecl; varargs; external LIB_NAME name 'sscanf';
function lseek(fd: cint; offset: __off_t; whence: cint): __off_t; cdecl; external LIB_NAME name 'lseek';
function isprint(p: Char): cint; cdecl; external LIB_NAME name 'isprint';
function strdup(para1: Pcchar): Pcchar; cdecl; external LIB_NAME name 'strdup';
function strchr(para1: Pcchar; para2: cint): Pcchar; cdecl; external LIB_NAME name 'strchr';
function strstr(haystack: Pcchar; needle: Pcchar): Pcchar; cdecl; external LIB_NAME name 'strstr';
function sprintf(s: Pcchar; format: Pcchar): cint; cdecl; varargs; external LIB_NAME name 'sprintf';
function asprintf(resultp: Ppcchar; format: Pcchar): cint; cdecl; varargs; external LIB_NAME name 'asprintf';
function errno: PInteger; cdecl; external LIB_NAME name '__errno_location';
{$ENDIF}
function memset(s: pointer; c: longint; n: size_t): pointer; cdecl; external LIB_NAME name 'memset';
function snprintf(str: Pcchar; size: size_t; format: Pcchar): cint; cdecl; varargs; external LIB_NAME Name {$IFDEF MSWINDOWS}'_snprintf'{$ELSE}'snprintf'{$ENDIF};
function rand: cint; cdecl; external LIB_NAME name 'rand';
function strerror(errnum: cint): Pchar; cdecl; external LIB_NAME name 'strerror';
function strncat(a, b: Pcchar; sz: size_t): Pchar; cdecl; external LIB_NAME name 'strncat';
function strcpy(a, b: Pcchar): Pchar; cdecl; external LIB_NAME name 'strcpy';
function strncmp(a, b: Pcchar; sz: size_t): cint; cdecl;  external LIB_NAME name 'strncmp';
function signal(sig: cint; func: signal_func): signal_func; cdecl; external LIB_NAME Name 'signal';

function fopen(filename: PAnsiChar; mode: open_mode): FILEptr;
procedure fclose(fp: FILEptr);
function fseek(fp: FILEptr; recPos: longint; mode: seek_mode): longint;
function fread(buf: pointer; recSize: longint; recCount: longint; fp: FILEptr): longint;
function fwrite(buf: pointer; recSize: longint; recCount: longint; fp: FILEptr): longint;
function ftell(fp: FILEptr): LongInt;
function feof(fp: FILEptr): LongInt;

implementation

{$IFDEF MSWINDOWS}
function fpgettimeofday(tv: PTimeVal; tz: PTimeZone): cint;
const
  tzflag: cint = 0;
var
  ft: FILETIME;
  tmpres: QWord = 0;
begin
  if nil <> tv then
  begin
    GetSystemTimeAsFileTime(@ft);
    tmpres := tmpres or ft.dwHighDateTime;
    tmpres := tmpres shl 32;
    tmpres := tmpres or ft.dwLowDateTime;
    tmpres := tmpres div 10;
    tmpres -= DELTA_EPOCH_IN_MICROSECS;
    tv^.tv_sec := clong(tmpres div culong(1000000));
    tv^.tv_usec := clong(tmpres mod culong(1000000));
  end;
  if nil <> tz then
  begin
    if tzflag <> 1 then
    begin
      _tzset;
      Inc(tzflag);
    end;
    tz^.tz_minuteswest := _timezone div 60;
    tz^.tz_dsttime := _daylight;
  end;
  Result := 0;
end;
{$ENDIF}

function fopen(filename: PAnsiChar; mode: open_mode): FILEptr;
var
  fp: FILEptr;
  OldFileMode: Byte;
begin
  fp := nil;
  OldFileMode := FileMode;
  GetMem(fp, SizeOf(File));
  Assign(fp^, StrPas(filename));
{$PUSH}{$I-}
  case mode of
    fopenread:
    begin
      FileMode := 0;
      Reset(fp^, 1);
    end;
    fopenwrite:
    begin
      FileMode := 1;
      ReWrite(fp^, 1);
    end;
    fappendwrite:
    begin
      FileMode := 2;
      Reset(fp^, 1);
      if IOResult = 2 then
        ReWrite(fp^, 1);
      Seek(fp^, FileSize(fp^));
    end;
  end;
  FileMode := OldFileMode;
{$POP}
  if IOResult <> 0 then
  begin
    FreeMem(fp, SizeOf(File));
    fp := nil;
  end;
  fopen := fp;
end;

procedure fclose(fp : FILEptr);
begin
  if Assigned(fp) then
  begin
{$PUSH}{$I-}
    Close(fp^);
{$POP}
    if IOresult = 0 then
      FreeMem(fp, SizeOf(File));
  end;
end;

function fread(buf: Pointer; recSize: LongInt; recCount: LongInt;
  fp : FILEptr): LongInt;
var
  totalSize, readcount : LongInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * LongInt(recSize);
{$PUSH}{$I-}{$HINTS OFF}
    BlockRead(fp^, buf^, totalSize, readcount);
    if readcount <> totalSize then
      fread := readcount div recSize
    else
      fread := recCount;
{$POP}
  end
  else
    fread := 0;
end;

function fwrite(buf: Pointer; recSize: LongInt; recCount: LongInt;
  fp: FILEptr) : LongInt;
var
  totalSize, written: LongInt;
begin
  if Assigned(buf) then
  begin
    totalSize := recCount * LongInt(recSize);
{$PUSH}{$I-}{$HINTS OFF}
    BlockWrite(fp^, buf^, totalSize, written);
    if written <> totalSize then
      fwrite := written div recSize
    else
      fwrite := recCount;
{$POP}
  end
  else
    fwrite := 0;
end;

function fseek(fp: FILEptr; recPos: LongInt; mode: seek_mode): LongInt;
begin
{$PUSH}{$I-}
  case mode of
    SEEK_SET: Seek(fp^, recPos);
    SEEK_CUR: Seek(fp^, FilePos(fp^) + recPos);
    SEEK_END: Seek(fp^, FileSize(fp^) - 1 - recPos);
  end;
{$POP}
  fseek := IOResult;
end;

function ftell(fp: FILEptr): LongInt;
begin
  ftell := FilePos(fp^);
end;

function feof(fp: FILEptr): LongInt;
begin
  feof := 0;
  if Assigned(fp) then
    if eof(fp^) then
      feof := 1
    else
      feof := 0;
end;

end.
