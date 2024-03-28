{
    This file is part of the Free Pascal Test Suite
    Copyright (c) 1999-2000 by Pierre Muller

    Unit to redirect output and error to files

    Adapted from code donated to public domain by Schwartz Gabriel 20/03/1993

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit Redir;
Interface

{$mode objfpc}
{$H+}
{$R-}
{$ifndef Linux}
{$ifndef Unix}
  {$S-}
{$endif}
{$endif}

{$ifdef Go32v2}
{$define implemented}
{$endif}
{$ifdef OS2}
{$define implemented}
{$endif}

{$ifdef windows}
{$define implemented}
{$if (FPC_FULLVERSION > 30300)}
{$define EXECUTEREDIR_USES_PROCESS}
{$ENDIF}
{$define USES_UNIT_PROCESS}
{$endif}

{$IFDEF UNIX}
{$define implemented}
{$ifndef MACOS}
{$if (FPC_FULLVERSION > 30300)}
{$define EXECUTEREDIR_USES_PROCESS}
{$ENDIF}
{$define USES_UNIT_PROCESS}
{$endif}
{$ENDIF}

Var
  IOStatus                   : Integer;
  RedirErrorOut,RedirErrorIn,
  RedirErrorError            : Integer;
  ExecuteResult              : Longint;

{------------------------------------------------------------------------------}
procedure InitRedir;
function ExecuteRedir (Const ProgName, ComLine : String; RedirStdIn, RedirStdOut, RedirStdErr: String): boolean;
procedure DosExecute(ProgName, ComLine : String);

function  ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedirOut;
procedure DisableRedirOut;
procedure EnableRedirOut;
function  ChangeRedirIn(Const Redir : String) : Boolean;
procedure RestoreRedirIn;
procedure DisableRedirIn;
procedure EnableRedirIn;
function  ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedirError;
procedure DisableRedirError;
procedure EnableRedirError;
procedure RedirDisableAll;
procedure RedirEnableAll;

{ unused in UNIX }
const
  UseComSpec : boolean = true;

Implementation

//or defined(windows)
{$if defined(macos) or defined(shell_implemented) or defined(go32v2)}
{$define usedos}
{$endif}

{$if defined(windows) and not defined(usedos)}
  {$ifdef ver2_4}
    {$define redirexecuteprocess}
  {$endif}
{$endif}

Uses
{$ifdef go32v2}
  go32,
{$endif go32v2}
{$ifdef windows}
  windows,
{$endif windows}
{$IFDEF OS2}
 {$IFNDEF EMX}
  DosCalls,
 {$ENDIF EMX}
{$ENDIF OS2}
{$ifdef unix}
    baseunix,
    unix,
{$endif unix}
{$ifdef redirexecuteprocess}
    sysconst,

{$endif}

{$ifdef USES_UNIT_PROCESS}
  process,
{$endif USES_UNIT_PROCESS}

{$ifdef usedos}
  dos;
{$else}
  sysutils;
{$endif}

Const
{$ifdef UNIX}
  DirSep='/';
  listsep = [';',':'];
  exeext = '';
{$else UNIX}
{$ifdef MACOS}
  DirSep=':';
  listsep = [','];
  exeext = '';
{$else MACOS}
  DirSep='\';
  listsep = [';'];
  exeext = '.exe';
{$endif MACOS}
{$endif UNIX}

{$ifndef usedos}
{ code from:                                                 }
{ Lithuanian Text Tool version 0.9.0  (2001-04-19)           }
{ Copyright (c) 1999-2001 Marius Gedminas <mgedmin@delfi.lt> }
{ (GPLv2 or later)                                           }

function FExpand(const S: string): string;
begin
  FExpand := ExpandFileName(S);
end;

type
  PathStr = string;
  DirStr = string;
  NameStr = string;
  ExtStr = string;

procedure FSplit(Path: PathStr; var Dir: DirStr; var Name: NameStr; var Ext: ExtStr);
begin
  Dir := ExtractFilePath(Path);
  Name := ChangeFileExt(ExtractFileName(Path), '');
  Ext := ExtractFileExt(Path);
end;

{$endif}

var
  FIN,FOUT,FERR     : ^File;
  RedirStdErrToStdOut,
  RedirChangedOut,
  RedirChangedIn    : Boolean;
  RedirChangedError : Boolean;
  InRedirDisabled,OutRedirDisabled,ErrorRedirDisabled : Boolean;


{*****************************************************************************
                                     Helpers
*****************************************************************************}

function FixPath(const s:string):string;
var
  i : longint;
begin
  { Fix separator }
  setlength(fixpath,length(s));
  for i:=1 to length(s) do
   if s[i] in ['/','\'] then
    fixpath[i]:=DirSep
   else
    fixpath[i]:=s[i];
end;


{*****************************************************************************
                                     Dos
*****************************************************************************}

{$ifdef implemented}


{$ifndef usedos}
{$if defined(ver2_4_0) or defined(ver2_4_1)}

Type
  TExecuteFlags= set of (ExecInheritsHandles);
{$ifdef redirexecuteprocess}

function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString;Flags:TExecuteFlags=[]):integer;
// win specific  function
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWord;
  CommandLine : ansistring;
  e : EOSError;
  ExecInherits : longbool;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path)=0 then
    CommandLine:='"'+path+'"'
  else
    CommandLine:=path;
  if ComLine <> '' then
    CommandLine:=Commandline+' '+ComLine+#0
  else
    CommandLine := CommandLine + #0;

  ExecInherits:=ExecInheritsHandles in Flags;

  if not CreateProcess(nil, pchar(CommandLine),
    Nil, Nil, ExecInherits,$20, Nil, Nil, SI, PI) then
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
      e.ErrorCode:=GetLastError;
      raise e;
    end;
  Proc:=PI.hProcess;
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    begin
      GetExitCodeProcess(Proc,l);
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      result:=l;
    end
  else
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
      e.ErrorCode:=GetLastError;
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      raise e;
    end;
end;
{$else}
{$ifndef USES_UNIT_PROCESS}
function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString;Flags:TExecuteFlags=[]):integer;
begin
    result:=ExecuteProcess(path,comline);
end;
{$endif ndef USES_UNIT_PROCESS}
{$endif}
{$ifend}
{$endif}


{$ifndef windows}
var
  TempHOut, TempHIn,TempHError : longint;
{$endif ndef windows}

{
For Unix the following functions exist
Function  fpdup(oldfile:longint;var newfile:longint):Boolean;
Function  fpdup2(oldfile,newfile:longint):Boolean;
Function  fpClose(fd:longint):boolean;
}
{$ifdef go32v2}

function fpdup(fh : longint) : longint;
var
  Regs : Registers;
begin
    Regs.ah:=$45;
    Regs.bx:=fh;
    MsDos (Regs);
    If (Regs.Flags and fCarry)=0 then
      fpdup:=Regs.Ax
    else
      fpdup:=-1;
end;

function fpdup2(fh,nh : longint) : longint;
var
  Regs : Registers;
begin
    fpdup2:=0;
    If fh=nh then
      exit;
    Regs.ah:=$46;
    Regs.bx:=fh;
    Regs.cx:=nh;
    MsDos (Regs);
    If (Regs.Flags and fCarry)<>0 then
      fpdup2:=-1;
end;

Function fpclose (Handle : Longint) : boolean;
var Regs: registers;
begin
  Regs.Eax := $3e00;
  Regs.Ebx := Handle;
  MsDos(Regs);
  fpclose:=(Regs.Flags and fCarry)=0;
end;

{$endif def go32v2}

{$ifdef windows}
Function fpclose (Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  fpclose:=true;
end;
{$endif}

{$IFDEF OS2}
 {$IFDEF EMX}
{$ASMMODE INTEL}
function fpDup (FH: longint): longint; assembler;
asm
  mov ebx, eax
  mov ah, 45h
  call syscall
  jnc @fpdup_end
  mov eax, -1
@fpdup_end:
end;

function fpDup2 (FH, NH: longint): longint; assembler;
asm
  cmp eax, edx
  jnz @fpdup2_go
  mov eax, 0
  jmp @fpdup2_end
@fpdup2_go:
  push ebx
  mov ebx, eax
  mov ecx, edx
  mov ah, 46h
  call syscall
  pop ebx
  jnc @fpdup2_end
  mov eax, -1
@fpdup2_end:
end;

function fpClose (Handle: longint): boolean; assembler;
asm
  push ebx
  mov ebx, eax
  mov ah, 3Eh
  call syscall
  pop ebx
  mov eax, 1
  jnc @fpclose_end
  dec eax
end;

{$ASMMODE DEFAULT}
 {$ELSE EMX}

function fpDup (FH: longint): longint;
var
  NH: THandle;
begin
  NH := THandle (-1);
  if DosDupHandle (THandle (FH), NH) = 0 then
   fpDup := longint (NH)
  else
   fpDup := -1;
end;

function fpDup2 (FH, NH: longint): longint;
begin
  if FH = NH then
   fpDup2 := 0
  else
   if DosDupHandle (THandle (FH), THandle (NH)) <> 0 then
    fpDup2 := -1;
end;

function fpClose (Handle: longint): boolean;
begin
  fpClose := DosClose (THandle (Handle)) = 0;
end;
 {$ENDIF EMX}
{$ENDIF OS2}


{$I-}
function FileExist(const FileName : PathStr) : Boolean;
{$ifdef usedos}
var
  f : file;
  Attr : word;
{$endif}
begin
{$ifdef usedos}
  Assign(f, FileName);
  GetFAttr(f, Attr);
  FileExist := DosError = 0;
{$else}
  FileExist := Sysutils.FileExists(filename);
{$endif}
end;

function CompleteDir(const Path: string): string;
begin
  { keep c: untouched PM }
  if (Path<>'') and (Path[Length(Path)]<>DirSep) and
     (Path[Length(Path)]<>':') then
   CompleteDir:=Path+DirSep
  else
   CompleteDir:=Path;
end;


function LocateExeFile(var FileName:string): boolean;
var
  dir,s: string;
  d: dirstr;
  n: namestr;
  e: extstr;
  i : longint;
begin
  LocateExeFile:=False;
  if FileExist(FileName) then
    begin
      LocateExeFile:=true;
      Exit;
    end;

  Fsplit(Filename,d,n,e);

  if (e='') and FileExist(FileName+exeext) then
    begin
      FileName:=FileName+exeext;
      LocateExeFile:=true;
      Exit;
    end;

{$ifdef usedos}
  S:=GetEnv('PATH');
{$else}
  S:=GetEnvironmentVariable('PATH');
{$endif}
  While Length(S)>0 do
    begin
      i:=1;
      While (i<=Length(S)) and not (S[i] in ListSep) do
        Inc(i);
      Dir:=CompleteDir(Copy(S,1,i-1));
      if i<Length(S) then
        Delete(S,1,i)
      else
        S:='';
      if FileExist(Dir+FileName) then
        Begin
           FileName:=Dir+FileName;
           LocateExeFile:=true;
           Exit;
        End;
   end;
end;


{............................................................................}

function ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
  begin
    ChangeRedirOut:=False;
    If Redir = '' then Exit;
    Assign (FOUT^, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(FOUT^,1);
      Seek(FOUT^,FileSize(FOUT^));
      End else Rewrite (FOUT^);

    RedirErrorOut:=IOResult;
    IOStatus:=RedirErrorOut;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleOut:=Handles^[StdOutputHandle];
    Handles^[StdOutputHandle]:=Handles^[FileRec (FOUT^).Handle];
    ChangeRedirOut:=True;
    OutRedirDisabled:=False;
{$else}
{$ifdef windows}
    if SetStdHandle(Std_Output_Handle,FileRec(FOUT^).Handle) then
{$else not windows}
    TempHOut:=fpdup(StdOutputHandle);
    fpdup2(FileRec(FOUT^).Handle,StdOutputHandle);
    if (TempHOut<>UnusedHandle) and
       (StdOutputHandle<>UnusedHandle) then
{$endif not windows}
      begin
         ChangeRedirOut:=True;
         OutRedirDisabled:=False;
      end;
{$endif def FPC}
     RedirChangedOut:=True;
  end;

function ChangeRedirIn(Const Redir : String) : Boolean;
  begin
    ChangeRedirIn:=False;
    If Redir = '' then Exit;
    Assign (FIN^, Redir);
    Reset(FIN^,1);

    RedirErrorIn:=IOResult;
    IOStatus:=RedirErrorIn;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleIn:=Handles^[StdInputHandle];
    Handles^[StdInputHandle]:=Handles^[FileRec (FIN^).Handle];
    ChangeRedirIn:=True;
    InRedirDisabled:=False;
{$else}
{$ifdef windows}
    if SetStdHandle(Std_Input_Handle,FileRec(FIN^).Handle) then
{$else not windows}
    TempHIn:=fpdup(StdInputHandle);
    fpdup2(FileRec(FIn^).Handle,StdInputHandle);
    if (TempHIn<>UnusedHandle) and
       (StdInputHandle<>UnusedHandle) then
{$endif not windows}
      begin
         ChangeRedirIn:=True;
         InRedirDisabled:=False;
      end;
{$endif def FPC}
     RedirChangedIn:=True;
  end;


function ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
  var
    PF : ^File;
  begin
    ChangeRedirError:=False;
    If Redir = '' then
      Exit;
    RedirStdErrToStdOut:=(Redir='stdout');
    if RedirStdErrToStdOut then
      begin
        PF:=FOut;
      end
    else
      begin
        Assign (FERR^, Redir);
        If AppendToFile and FileExist(Redir) then
          Begin
            Reset(FERR^,1);
            Seek(FERR^,FileSize(FERR^));
          End
        else
          Rewrite (FERR^);

        RedirErrorError:=IOResult;
        IOStatus:=RedirErrorError;
        If IOStatus <> 0 then Exit;
        PF:=FErr;
      end;

{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleError:=Handles^[StdErrorHandle];
    Handles^[StdErrorHandle]:=Handles^[FileRec (PF^).Handle];
    ChangeRedirError:=True;
    ErrorRedirDisabled:=False;
{$else}
{$ifdef windows}
    if SetStdHandle(Std_Error_Handle,FileRec(PF^).Handle) then
{$else not windows}
    TempHError:=fpdup(StdErrorHandle);
    fpdup2(FileRec(PF^).Handle,StdErrorHandle);
    if (TempHError<>UnusedHandle) and
       (StdErrorHandle<>UnusedHandle) then
{$endif not windows}
      begin
         ChangeRedirError:=True;
         ErrorRedirDisabled:=False;
      end;
{$endif}
    RedirChangedError:=True;
  end;


  procedure RestoreRedirOut;

  begin
    If not RedirChangedOut then Exit;
{$ifdef windows}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not windows}
    fpdup2(TempHOut,StdOutputHandle);
{$endif not windows}
    Close (FOUT^);
{$ifndef windows}
    fpclose(TempHOut);
{$endif ndef windows}
    RedirChangedOut:=false;
  end;

  {............................................................................}

  procedure RestoreRedirIn;

  begin
    If not RedirChangedIn then Exit;
{$ifndef FPC}
    Handles^[StdInputHandle]:=OldHandleIn;
    OldHandleIn:=StdInputHandle;
{$else}
{$ifdef windows}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not windows}
    fpdup2(TempHIn,StdInputHandle);
{$endif not windows}
{$endif}
    Close (FIn^);
{$ifndef windows}
    fpclose(TempHIn);
{$endif ndef windows}
    RedirChangedIn:=false;
  end;

  {............................................................................}

  procedure DisableRedirIn;

  begin
    If not RedirChangedIn then Exit;
    If InRedirDisabled then Exit;
{$ifndef FPC}
    Handles^[StdInputHandle]:=OldHandleIn;
{$else}
{$ifdef windows}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not windows}
    fpdup2(TempHIn,StdInputHandle);
{$endif not windows}
{$endif}
    InRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirIn;

  begin
    If not RedirChangedIn then Exit;
    If not InRedirDisabled then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    Handles^[StdInputHandle]:=Handles^[FileRec (FIn^).Handle];
{$else}
{$ifdef windows}
    SetStdHandle(Std_Input_Handle,FileRec(FIn^).Handle);
{$else not windows}
    fpdup2(FileRec(FIn^).Handle,StdInputHandle);
{$endif not windows}
{$endif}
    InRedirDisabled:=False;
  end;

  {............................................................................}

  procedure DisableRedirOut;

  begin
    If not RedirChangedOut then Exit;
    If OutRedirDisabled then Exit;
{$ifndef FPC}
    Handles^[StdOutputHandle]:=OldHandleOut;
{$else}
{$ifdef windows}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not windows}
    fpdup2(TempHOut,StdOutputHandle);
{$endif not windows}
{$endif}
    OutRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirOut;

  begin
    If not RedirChangedOut then Exit;
    If not OutRedirDisabled then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    Handles^[StdOutputHandle]:=Handles^[FileRec (FOut^).Handle];
{$else}
{$ifdef windows}
    SetStdHandle(Std_Output_Handle,FileRec(FOut^).Handle);
{$else not windows}
    fpdup2(FileRec(FOut^).Handle,StdOutputHandle);
{$endif not windows}
{$endif}
    OutRedirDisabled:=False;
  end;

  {............................................................................}

  procedure RestoreRedirError;

  begin
    If not RedirChangedError then Exit;
{$ifndef FPC}
    Handles^[StdErrorHandle]:=OldHandleError;
    OldHandleError:=StdErrorHandle;
{$else}
{$ifdef windows}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not windows}
    fpdup2(TempHError,StdErrorHandle);
{$endif not windows}
{$endif}
    { don't close when redirected to STDOUT }
    if not RedirStdErrToStdOut then
      Close (FERR^);
{$ifndef windows}
    fpclose(TempHError);
{$endif ndef windows}
    RedirChangedError:=false;
  end;

  {............................................................................}

  procedure DisableRedirError;

  begin
    If not RedirChangedError then Exit;
    If ErrorRedirDisabled then Exit;
{$ifndef FPC}
    Handles^[StdErrorHandle]:=OldHandleError;
{$else}
{$ifdef windows}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not windows}
    fpdup2(TempHError,StdErrorHandle);
{$endif not windows}
{$endif}
    ErrorRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirError;

  begin
    If not RedirChangedError then Exit;
    If not ErrorRedirDisabled then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    Handles^[StdErrorHandle]:=Handles^[FileRec (FErr^).Handle];
{$else}
{$ifdef windows}
    SetStdHandle(Std_Error_Handle,FileRec(FErr^).Handle);
{$else not windows}
    fpdup2(FileRec(FERR^).Handle,StdErrorHandle);
{$endif not windows}
{$endif}
    ErrorRedirDisabled:=False;
  end;

{............................................................................}

{$ifdef EXECUTEREDIR_USES_PROCESS}
function ExecuteRedir (Const ProgName, ComLine : String; RedirStdIn, RedirStdOut, RedirStdErr: String): boolean;

const
  max_count = 60000;

var
  P : TProcess;

begin
  P := TProcess.Create(nil);
  try
    P.CommandLine:=Progname + ' ' + ComLine;
    P.InputDescriptor.FileName:=RedirStdIn;
    P.OutputDescriptor.FileName:=RedirStdOut;
    if RedirStdErr='stdout' then
      P.Options:=P.options+[poStdErrToOutput]
    else  
      P.ErrorDescriptor.FileName:=RedirStdErr;
    P.Execute;
    Result:=P.WaitOnExit(max_count);
    if Result then  
      ExecuteResult:=P.ExitCode
    else
      begin
      Writeln(stderr,'Terminate requested for ',Progname,' ',ComLine);
      { Issue it also to output, so it gets added to log file
                  if ExecuteRedir is in use }
      Writeln('Terminate requested for ',Progname,' ',ComLine);      
      Repeat 
        P.Terminate(255);
        Sleep(10);
      Until not P.Running;  
      ExecuteResult:=1000+P.ExitCode;
      end;  
    Result:=ExecuteResult=0;
  finally
    P.Free;
  end;
end;
{$ELSE}

function ExecuteRedir (Const ProgName, ComLine : String; RedirStdIn, RedirStdOut, RedirStdErr: String): boolean;
Begin
  RedirErrorOut:=0; RedirErrorIn:=0; RedirErrorError:=0;
  ExecuteResult:=0;
  IOStatus:=0;
  if RedirStdIn<>'' then
    ChangeRedirIn(RedirStdIn);
  if RedirStdOut<>'' then
    ChangeRedirOut(RedirStdOut,false);
  if RedirStdErr<>'stderr' then
    ChangeRedirError(RedirStdErr,false);
  DosExecute(ProgName,ComLine);
  RestoreRedirOut;
  RestoreRedirIn;
  RestoreRedirError;
  ExecuteRedir:=(IOStatus=0) and (RedirErrorOut=0) and
                (RedirErrorIn=0) and (RedirErrorError=0) and
                (ExecuteResult=0);
End;
{$ENDIF}

{............................................................................}

procedure RedirDisableAll;
  begin
    If RedirChangedIn and not InRedirDisabled then
      DisableRedirIn;
    If RedirChangedOut and not OutRedirDisabled then
      DisableRedirOut;
    If RedirChangedError and not ErrorRedirDisabled then
      DisableRedirError;
  end;

{............................................................................}

procedure RedirEnableAll;
  begin
    If RedirChangedIn and InRedirDisabled then
      EnableRedirIn;
    If RedirChangedOut and OutRedirDisabled then
      EnableRedirOut;
    If RedirChangedError and ErrorRedirDisabled then
      EnableRedirError;
  end;


procedure InitRedir;
begin
end;

{$else not  implemented}


{*****************************************************************************
                                 Fake
*****************************************************************************}

{$IFDEF SHELL_IMPLEMENTED}
{$I-}
function FileExist(const FileName : PathStr) : Boolean;
var
  f : file;
  Attr : word;
begin
  Assign(f, FileName);
  GetFAttr(f, Attr);
  FileExist := DosError = 0;
end;

function CompleteDir(const Path: string): string;
begin
  { keep c: untouched PM }
  if (Path<>'') and (Path[Length(Path)]<>DirSep) and
     (Path[Length(Path)]<>':') then
   CompleteDir:=Path+DirSep
  else
   CompleteDir:=Path;
end;

function LocateExeFile(var FileName:string): boolean;
var
{$IFDEF USEDOS}
  dir,s,d,n,e : shortstring;
{$ELSE USEDOS}
  dir,s,d,n,e : string;
{$ENDIF USEDOS}
  i : longint;
begin
  LocateExeFile:=False;
  if FileExist(FileName) then
    begin
      LocateExeFile:=true;
      Exit;
    end;

  Fsplit(Filename,d,n,e);

  if (e='') and FileExist(FileName+exeext) then
     begin
      FileName:=FileName+exeext;
      LocateExeFile:=true;
      Exit;
     end;
{$ifdef macos}
  S:=GetEnv('Commands');
{$else}
  S:=GetEnv('PATH');
{$endif}
  While Length(S)>0 do
    begin
      i:=1;
      While (i<=Length(S)) and not (S[i] in ListSep) do
        Inc(i);
      Dir:=CompleteDir(Copy(S,1,i-1));
      if i<Length(S) then
        Delete(S,1,i)
      else
        S:='';
  
       if FileExist(Dir+FileName) then
        Begin
           FileName:=Dir+FileName;
           LocateExeFile:=true;
           Exit;
        End;
   end;
end;

function ExecuteRedir (Const ProgName, ComLine : String; RedirStdIn, RedirStdOut, RedirStdErr: String): boolean;
var
 CmdLine2: string;

begin
 {$ifdef macos}
 if Lowercase(RedirStdIn) = 'stdin' then RedirStdIn := 'Dev:StdIn';
 if Lowercase(RedirStdOut) = 'stdout' then RedirStdOut := 'Dev:Output';
 if Lowercase(RedirStdOut) = 'stderr' then RedirStdOut := 'Dev:Error';
 if Lowercase(RedirStdErr) = 'stdout' then RedirStdErr := 'Dev:Output';
 if Lowercase(RedirStdErr) = 'stderr' then RedirStdErr := 'Dev:Error';
 {$endif macos}

 CmdLine2 := ComLine;
 if RedirStdIn <> '' then CmdLine2 := CmdLine2 + ' < ' + RedirStdIn;

 {$ifndef macos}
 if RedirStdOut <> '' then CmdLine2 := CmdLine2 + ' > ' + RedirStdOut;
 if RedirStdErr <> '' then
 begin
  if RedirStdErr = RedirStdOut then
    CmdLine2 := CmdLine2 + ' 2>&1'
  else
    CmdLine2 := CmdLine2 + ' 2> ' + RedirStdErr;
 end;
 {$else macos}
 if RedirStdErr <> RedirStdOut then
   if RedirStdOut <> '' then CmdLine2 := CmdLine2 + ' > ' + RedirStdOut;
 if RedirStdErr <> '' then
 begin
  if RedirStdErr = RedirStdOut then
    CmdLine2 := CmdLine2 + ' ' + #183 + ' ' + RedirStdErr  {#183 is "capital sigma" char in MacRoman}
  else
    CmdLine2 := CmdLine2 + ' ' + #179 + ' ' + RedirStdErr; {#179 is "greater or equal" char in MacRoman}
 end;
 {$endif macos}

 DosExecute (ProgName, CmdLine2);
 ExecuteRedir:=(IOStatus=0) and (ExecuteResult=0);
end;

{$ELSE SHELL_IMPLEMENTED}
function ExecuteRedir (Const ProgName, ComLine : String; RedirStdIn, RedirStdOut, RedirStdErr: String): boolean;
begin
  ExecuteRedir:=false;
end;
{$ENDIF SHELL_IMPLEMENTED}

function  ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
begin
  ChangeRedirOut:=false;
end;


procedure RestoreRedirOut;
begin
end;


procedure DisableRedirOut;
begin
end;


procedure EnableRedirOut;
begin
end;


function  ChangeRedirIn(Const Redir : String) : Boolean;
begin
  ChangeRedirIn:=false;
end;


procedure RestoreRedirIn;
begin
end;


procedure DisableRedirIn;
begin
end;


procedure EnableRedirIn;
begin
end;


function  ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
begin
  ChangeRedirError:=false;
end;


procedure RestoreRedirError;
begin
end;


procedure DisableRedirError;
begin
end;


procedure EnableRedirError;
begin
end;


procedure RedirDisableAll;
begin
end;


procedure RedirEnableAll;
begin
end;


procedure InitRedir;
begin
end;
{$endif not implemented}

{............................................................................}
{$ifdef UNIX}
function TransformfpSystemToShell(s:cint):cint;
// transforms standarized (fp)System(3) result to the conventions of the old Unix.shell function.
begin
 if s=-1 then exit(-1);
 if wifexited(s) then
   TransformfpSystemToShell:=wexitstatus(s)
 else if (s>0) then
   TransformfpSystemToShell:=-s
 else
   TransformfpSystemToShell:=s;
end;
{$endif def UNIX}



{****************************************************************************
                                Helpers
****************************************************************************}

{$ifdef USES_UNIT_PROCESS}
const
  max_count = 60000; { should be 60 seconds }

function ExecuteProcess(const Path: string; const ComLine: string; Flags:TExecuteFlags=[]): integer;
var
  P: TProcess;
  counter : longint;
  TerminateSentCount : longint;

begin
  result := -1;
  TerminateSentCount:=0;

  P := TProcess.Create(nil);
  try
    P.CommandLine := Path + ' ' + ComLine;

    P.InheritHandles:=(execinheritshandles in flags);

    P.Execute;
{$if FPC_FULLVERSION < 30100}
{$ifdef Windows}
    WaitForSingleObject(P.ProcessHandle,max_count);
    counter:=max_count;
{$else not Windows}
    counter:=0;
{$endif not Windows}
{$else}
    P.WaitOnExit(max_count);
    counter:=max_count;
{$endif}

    while P.Running do
      begin
        if counter>max_count then
          begin
            P.Terminate(255);
            if TerminateSentCount=0 then
              { also write ComLine in order to know which test is not ended in time }
              begin
                Writeln(stderr,'Terminate requested for ',Path,' ',ComLine);
                { Issue it also to output, so it gets added to log file
                  if ExecuteRedir is in use }
                Writeln('Terminate requested for ',Path,' ',ComLine);
              end;
            Inc(TerminateSentCount);
          end;

        Sleep(1);
        inc(counter);
      end;

    { Be sure to return a non-zero value if Terminate was requested }
    if (TerminateSentCount>0) and (P.ExitStatus>=0) then
      result := 1000 + P.ExitStatus
    else
      result := P.ExitStatus;
  finally
    P.Free;
  end;
end;
{$endif HAS_UNIT_PROCESS}


  procedure DosExecute(ProgName, ComLine : String);


  Begin
{$IfDef MsDos}
  SmallHeap;
{$EndIf MsDos}
{$ifdef usedos}
    SwapVectors;
{$endif usedos}
    { Must use shell/fpsystem() for *nix for the wildcard expansion (PFV) }
{$ifdef UNIX}
    IOStatus:=0;
    ExecuteResult:=Transformfpsystemtoshell(fpsystem((FixPath(Progname)+' '+Comline)));
    if ExecuteResult<0 then
      begin
        IOStatus:=(-ExecuteResult) and $7f;
        ExecuteResult:=((-ExecuteResult) and $ff00) shr 8;
      end;
{$else}
  {$ifdef windows}

    { Avoid dialog boxes if dll loading fails }
    SetErrorMode(SEM_FAILCRITICALERRORS);
  {$endif windows}
    If UseComSpec then
      begin
      {$ifndef usedos}
        try
          ExecuteResult:=ExecuteProcess (Getenvironmentvariable('COMSPEC'),'/C '+FixPath(progname)+' '+Comline,[ExecInheritsHandles])
        except
          on e : exception do
            IOStatus:=2;
          end;
      {$else}
        DosError:=0;
        Exec (Getenv('COMSPEC'),'/C '+FixPath(progname)+' '+Comline);
        IOStatus:=DosError;
        ExecuteResult:=DosExitCode;
      {$endif}
      end
    else
      begin
        if LocateExeFile(progname) then
          begin
           {$ifndef usedos}
            try
              ExecuteResult:=ExecuteProcess(ProgName,Comline,[execinheritshandles])
            except
              on e : exception do
              IOStatus:=2;
              end;
            {$else}
              doserror:=0;
              {$ifdef macos}
                Dos.Exec(''''+ProgName+'''',Comline); {Quotes needed !}
              {$else}
                Dos.Exec(ProgName,Comline);
             {$endif}
             IOStatus:=DosError;
             ExecuteResult:=DosExitCode;
           {$endif}
          end
        else
          IOStatus:=2
          ;
      end;
  {$ifdef windows}
    SetErrorMode(0);
  {$endif windows}

{$endif}
{$ifdef usedos}
    SwapVectors;
{$endif}
{$ifdef CPU86}
    { reset the FPU }
    {$asmmode att}
    asm
      fninit
    end;
{$endif CPU86}
{$IfDef MsDos}
  Fullheap;
{$EndIf MsDos}
End;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
  New(FIn); New(FOut); New(FErr);

finalization
  Dispose(FIn); Dispose(FOut); Dispose(FErr);
End.
