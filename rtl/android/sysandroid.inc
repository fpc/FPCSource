{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Yury Sidorov,
    member of the Free Pascal development team.

    Android-specific part of the System unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

procedure atexit(p: pointer); cdecl; external;

var
  _SaveStdOut: THandle;
  _SaveStdErr: THandle;

procedure SysAndroidLibExit; cdecl;
var
  ioclosed: boolean;
begin
  // Check if stdio is closed now
  ioclosed:=do_syscall(syscall_nr_fcntl, TSysParam(1), 1 {F_GETFD}) = -1;
  // If stdio is closed, restore stdout and stderr
  if ioclosed then
    begin
      FpDup2(_SaveStdOut, 1);
      FpDup2(_SaveStdErr, 2);
    end;
  // Close saved handles
  FpClose(_SaveStdOut);
  FpClose(_SaveStdErr);
  // Finalize the library
  lib_exit;
  // Close stdout and stderr if stdio has been closed
  if ioclosed then
    begin
      FpClose(1);
      FpClose(2);
    end;
end;

procedure SysInitAndroidLib; [public, alias:'FPC_LIB_INIT_ANDROID'];
begin
  { Starting from Android 4.4 stdio handles are closed by libc prior to calling
    finalization routines of shared libraries. This causes a error while trying to
    writeln during library finalization and finally a crash because the error can
    not be printed too.
    It is needed to save stdout and stderr handles by duplicating them and restore
    them before library finalization.
  }
  _SaveStdOut:=FpDup(1);
  _SaveStdErr:=FpDup(2);
  // Register the finalization routine
  atexit(@SysAndroidLibExit);
end;

function __system_property_get(name:Pchar; value:Pchar):longint;cdecl;external 'c' name '__system_property_get';

function GetSystemProperty(Name: PAnsiChar): shortstring;
begin
  SetLength(Result, __system_property_get(Name, @Result[1]));
end;

var
  _ApiLevel: shortint = -1;

function SystemApiLevel: shortint;
var
  s: string;
  c: integer;
begin
  if _ApiLevel < 0 then
    begin
      s:=GetSystemProperty('ro.build.version.sdk');
      Val(s, _ApiLevel, c);
      if c <> 0 then
        _ApiLevel:=0;
    end;
  Result:=_ApiLevel;
end;

// ************* Android log

var
  DefaultLogTag: string[20];

function __android_log_write(prio: longint; tag, text: pchar): longint; cdecl; external 'log' name '__android_log_write';

procedure SysLogWrite(Priority: longint; Tag, Msg: PAnsiChar);
begin
  __android_log_write(Priority, Tag, Msg);
end;

procedure SysLogWrite(Priority: longint; Msg: PAnsiChar);
begin
  SysLogWrite(Priority, @DefaultLogTag[1], Msg);
end;

procedure SysLogWrite(Msg: PAnsiChar);
begin
  SysLogWrite(DefaultSysLogPriority, @DefaultLogTag[1], Msg);
end;

// ************* STDIO redirection to Android log

const
  IOBufferLength = 512;
var
  IOBuf : array[0..IOBufferLength] of char;
  IOLen : SizeInt;
  IORedirected: boolean;

procedure OutputIOBuffer(Var F: TextRec);
var
  p: longint;
begin
  if (@F = @ErrOutput) or (@F = @StdErr) then
    p:=ANDROID_LOG_ERROR
  else
    p:=DefaultSysLogPriority;
  SysLogWrite(p, IOBuf);
  IOLen:=0;
end;

procedure IOWrite(Var F: TextRec);
var
  i, len : SizeInt;
Begin
  while F.BufPos>0 do
    begin
      begin
        if F.BufPos + IOLen > IOBufferLength then
          len:=IOBufferLength - IOLen
        else
          len:=F.BufPos;
        i:=0;
        while i < len do
          begin
            if F.bufptr^[i] in [#10, #13] then
              begin
                IOBuf[IOLen]:=#0;
                OutputIOBuffer(F);
                Inc(i);
                if (i < len) and (F.bufptr^[i - 1] = #13) and (F.bufptr^[i] = #10) then
                  Inc(i);
              end
            else
              begin
                IOBuf[IOLen]:=F.bufptr^[i];
                Inc(IOLen);
                Inc(i);
              end;
          end;
        IOBuf[IOLen]:=#0;
      end;
      if IOLen = IOBufferLength then
        OutputIOBuffer(F);
      Dec(F.BufPos, len);
    end;
End;

procedure IOClose(Var F: TextRec);
begin
  if IOLen > 0 then
    OutputIOBuffer(F);
end;

procedure IOOpen(Var F: TextRec);
Begin
  TextRec(F).InOutFunc:=@IOWrite;
  TextRec(F).FlushFunc:=@IOWrite;
  TextRec(F).CloseFunc:=@IOClose;
  IOLen:=0;
End;

procedure RedirectFile(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@IOOpen;
  Rewrite(T);
end;

procedure RedirectOutputToSysLog;
begin
  if IORedirected then exit;
  IORedirected:=True;
  RedirectFile(Output);
  RedirectFile(StdOut);
  RedirectFile(ErrOutput);
  RedirectFile(StdErr);
end;

procedure SetDefaultSysLogTag(const Tag: string);
var
  len: longint;
begin
  DefaultLogTag:=Tag;
  len:=Length(DefaultLogTag);
  if len = High(DefaultLogTag) then
    Dec(len);
  DefaultLogTag[len + 1]:=#0;
end;

// ************* JNI init

function JNI_OnLoad_Real(vm: pointer; reserved: pointer): longint;{$ifdef windows} stdcall {$else} cdecl {$endif}; external name 'FPC_JNI_ON_LOAD';
procedure PascalMain; external name 'PASCALMAIN';

// This proxy function is called when JVM calls the JNI_OnLoad() exported function
function JNI_OnLoad_Proxy(vm: pointer; reserved: pointer): longint;{$ifdef windows} stdcall {$else} cdecl {$endif}; [public, alias:'FPC_JNI_ON_LOAD_PROXY'];
begin
  IsJniLibrary:=True;
  // Call library initialization
  PascalMain;
  // Call user's JNI_OnLoad().
  Result:=JNI_OnLoad_Real(vm, reserved);
end;

// This procedure is called instead of library initialization when JNI_OnLoad is exported
procedure JniLibMain; [public, alias:'FPC_JNI_LIB_MAIN_ANDROID'];
begin
  // Must be empty.
end;

// ************* System init

procedure InitAndroid;
var
  i: integer;
  s: string;
begin
  if IsJniLibrary then
    begin
      // The library is loaded by a Java app. The proper tag will be set by SysUtils.
      SetDefaultSysLogTag('FPC');
      RedirectOutputToSysLog;
    end
  else
    begin
      s:=ParamStr(0);
      i:=Length(s);
      while (i > 0) and (s[i] <> '/') do
        Dec(i);
      SetDefaultSysLogTag(Copy(s, i + 1, MaxInt));
    end;
end;
