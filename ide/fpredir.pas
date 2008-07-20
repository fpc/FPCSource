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
Unit FpRedir;
Interface

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
{$define shell_implemented}
{$endif}
{$ifdef Windows}
{$define implemented}
{$endif}
{$ifdef linux}
{$define implemented}
{$endif}
{$ifdef BSD}
{$define implemented}
{$endif}
{$ifdef netwlibc}
{$define implemented}
{$endif}
{$ifdef netware_clib}
{$define implemented}
{$endif}

Var
  IOStatus                   : Integer;
  RedirErrorOut,RedirErrorIn,
  RedirErrorError            : Integer;
  ExecuteResult              : Word;

{------------------------------------------------------------------------------}
procedure InitRedir;
function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
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

Uses
{$ifdef go32v2}
  go32,
{$endif go32v2}
{$ifdef netwlibc}
  Libc,
{$endif netwlibc}
{$ifdef netware_clib}
  nwserv,
{$endif netware_clib}
{$ifdef Windows}
  windows,
{$endif Windows}
{$ifdef unix}
  baseunix,
  unix,
{$endif unix}
  dos;

Const
{$ifdef UNIX}
  DirSep='/';
  listsep = [';',':'];
  exeext = '';
{$else UNIX}
  DirSep='\';
  listsep = [';'];
  exeext = '.exe';
{$endif UNIX}


var
  FIN,FOUT,FERR     : ^File;
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
  for i:=1 to length(s) do
   if s[i] in ['/','\'] then
    fixpath[i]:=DirSep
   else
    fixpath[i]:=s[i];
  fixpath[0]:=s[0];
end;


    function maybequoted(const s:string):string;
      var
        s1 : string;
        i  : integer;
        quoted : boolean;
      begin
        quoted:=false;
        s1:='"';
        for i:=1 to length(s) do
         begin
           case s[i] of
             '"' :
               begin
                 quoted:=true;
                 s1:=s1+'\"';
               end;
             ' ',
             #128..#255 :
               begin
                 quoted:=true;
                 s1:=s1+s[i];
               end;
             else
               s1:=s1+s[i];
           end;
         end;
        if quoted then
          maybequoted:=s1+'"'
        else
          maybequoted:=s;
      end;


{*****************************************************************************
                                     Dos
*****************************************************************************}

{$ifdef implemented}

var
  TempHOut, TempHIn,TempHError : longint;

{
For linux the following functions exist
Function  fpdup(oldfile:longint;var newfile:longint):Boolean;
Function  fpdup2(oldfile,newfile:longint):Boolean;
Function  fpClose(fd:longint):boolean;
}
{$ifdef go32v2}

function dup(fh : longint;var nh : longint) : boolean;
var
  Regs : Registers;
begin
    Regs.ah:=$45;
    Regs.bx:=fh;
    MsDos (Regs);
    dup:=true;
    If (Regs.Flags and fCarry)=0 then
      nh:=Regs.Ax
    else
      dup:=false;
end;

function dup2(fh,nh : longint) : boolean;
var
  Regs : Registers;
begin
    dup2:=true;
    If fh=nh then
      exit;
    Regs.ah:=$46;
    Regs.bx:=fh;
    Regs.cx:=nh;
    MsDos (Regs);
    If (Regs.Flags and fCarry)<>0 then
      dup2:=false;
end;

function fpdup(fh:longint):longint;
begin
  if not dup(fh,fpdup) then
   fpdup:=-1;
end;

function fpdup2(fh,nh:longint):longint;
begin
  if dup2(fh,nh) then
   fpdup2:=0
  else
   fpdup2:=-1;
end;


function fpclose(Handle : Longint) : boolean;
var Regs: registers;
begin
  Regs.Eax := $3e00;
  Regs.Ebx := Handle;
  MsDos(Regs);
  fpclose:=(Regs.Flags and fCarry)=0;
end;

{$endif def go32v2}

{$ifdef Windows}
Function fpclose(Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  fpclose:=true;
end;
{$endif}

{$ifdef os2}
Function fpclose (Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  fpclose:=true;
end;
{$endif}

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
  dir,s,d,n,e : string;
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

  S:=GetEnv('PATH');
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
{$ifdef Windows}
    if SetStdHandle(Std_Output_Handle,FileRec(FOUT^).Handle) then
{$else not Windows}
    TempHOut:=fpdup(StdOutputHandle);
    fpdup2(FileRec(FOUT^).Handle,StdOutputHandle);
    if (TempHOut<>UnusedHandle) and
       (StdOutputHandle<>UnusedHandle) then
{$endif not Windows}
      begin
         ChangeRedirOut:=True;
         OutRedirDisabled:=False;
      end;
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
{$ifdef Windows}
    if SetStdHandle(Std_Input_Handle,FileRec(FIN^).Handle) then
{$else not Windows}
    TempHIn:=fpdup(StdInputHandle);
    fpdup2(FileRec(FIn^).Handle,StdInputHandle);
    if (TempHIn<>UnusedHandle) and
       (StdInputHandle<>UnusedHandle) then
{$endif not Windows}
      begin
         ChangeRedirIn:=True;
         InRedirDisabled:=False;
      end;
     RedirChangedIn:=True;
  end;

function ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
  begin
    ChangeRedirError:=False;
    If Redir = '' then Exit;
    Assign (FERR^, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(FERR^,1);
      Seek(FERR^,FileSize(FERR^));
      End else Rewrite (FERR^);

    RedirErrorError:=IOResult;
    IOStatus:=RedirErrorError;
    If IOStatus <> 0 then Exit;
{$ifdef Windows}
    if SetStdHandle(Std_Error_Handle,FileRec(FERR^).Handle) then
{$else not Windows}
    TempHError:=fpdup(StdErrorHandle);
    fpdup2(FileRec(FERR^).Handle,StdErrorHandle);
    if (TempHError<>UnusedHandle) and
       (StdErrorHandle<>UnusedHandle) then
{$endif not Windows}
      begin
         ChangeRedirError:=True;
         ErrorRedirDisabled:=False;
      end;
     RedirChangedError:=True;
  end;

  procedure RestoreRedirOut;

  begin
    If not RedirChangedOut then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not Windows}
    fpdup2(TempHOut,StdOutputHandle);
{$endif not Windows}
    Close (FOUT^);
    fpclose(TempHOut);
    RedirChangedOut:=false;
  end;

  {............................................................................}


  procedure RestoreRedirIn;
  begin
    If not RedirChangedIn then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not Windows}
    fpdup2(TempHIn,StdInputHandle);
{$endif not Windows}
    Close (FIn^);
    fpclose(TempHIn);
    RedirChangedIn:=false;
  end;

  {............................................................................}

  procedure DisableRedirIn;

  begin
    If not RedirChangedIn then Exit;
    If InRedirDisabled then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not Windows}
    fpdup2(TempHIn,StdInputHandle);
{$endif not Windows}
    InRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirIn;

  begin
    If not RedirChangedIn then Exit;
    If not InRedirDisabled then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Input_Handle,FileRec(FIn^).Handle);
{$else not Windows}
    fpdup2(FileRec(FIn^).Handle,StdInputHandle);
{$endif not Windows}
    InRedirDisabled:=False;
  end;

  {............................................................................}

  procedure DisableRedirOut;

  begin
    If not RedirChangedOut then Exit;
    If OutRedirDisabled then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not Windows}
    fpdup2(TempHOut,StdOutputHandle);
{$endif not Windows}
    OutRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirOut;

  begin
    If not RedirChangedOut then Exit;
    If not OutRedirDisabled then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Output_Handle,FileRec(FOut^).Handle);
{$else not Windows}
    fpdup2(FileRec(FOut^).Handle,StdOutputHandle);
{$endif not Windows}
    OutRedirDisabled:=False;
  end;

  {............................................................................}

  procedure RestoreRedirError;

  begin
    If not RedirChangedError then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not Windows}
    fpdup2(TempHError,StdErrorHandle);
{$endif not Windows}
    Close (FERR^);
    fpclose(TempHError);
    RedirChangedError:=false;
  end;

  {............................................................................}

  procedure DisableRedirError;

  begin
    If not RedirChangedError then Exit;
    If ErrorRedirDisabled then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not Windows}
    fpdup2(TempHError,StdErrorHandle);
{$endif not Windows}
    ErrorRedirDisabled:=True;
  end;

  {............................................................................}

  procedure EnableRedirError;

  begin
    If not RedirChangedError then Exit;
    If not ErrorRedirDisabled then Exit;
{$ifdef Windows}
    SetStdHandle(Std_Error_Handle,FileRec(FErr^).Handle);
{$else not Windows}
    fpdup2(FileRec(FERR^).Handle,StdErrorHandle);
{$endif not Windows}
    ErrorRedirDisabled:=False;
  end;

{............................................................................}

function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
{$ifdef Windows}
var
  mode,modebefore : word;
{$endif Windows}
Begin
{$ifdef Windows}
  GetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), @modebefore);
{$endif Windows}
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
{$ifdef Windows}
  // restore previous mode
  GetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), @mode);
  //mode:=mode or ENABLE_MOUSE_INPUT;
  SetConsoleMode(GetStdHandle(cardinal(Std_Input_Handle)), modebefore);
{$endif Windows}
End;

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
  dir,s,d,n,e : string;
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

  S:=GetEnv('PATH');
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

function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr: String): boolean;
var
 CmdLine2: string;
begin
 CmdLine2 := ComLine;
 if RedirStdIn <> '' then CmdLine2 := CmdLine2 + ' < ' + RedirStdIn;
 if RedirStdOut <> '' then CmdLine2 := CmdLine2 + ' > ' + RedirStdOut;
 if RedirStdErr <> '' then
 begin
  if RedirStdErr = RedirStdOut
          then CmdLine2 := CmdLine2 + ' 2>&1'
                              else CmdLine2 := CmdLine2 + ' 2> ' + RedirStdErr;
 end;
 DosExecute (ProgName, CmdLine2);
 ExecuteRedir := true;
end;
{$ELSE SHELL_IMPLEMENTED}
function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
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

  procedure DosExecute(ProgName, ComLine : String);
{$ifdef Windows}
    var
      StoreInherit : BOOL;
{$endif Windows}

  Begin
    SwapVectors;
{$ifdef UNIX}
    IOStatus:=0;
    {We need to use fpsystem to get wildcard expansion and avoid being
     interrupted by ctrl+c (SIGINT).};
    ExecuteResult:=fpsystem(MaybeQuoted(FixPath(Progname))+' '+Comline);
    if ExecuteResult<0 then
      begin
        IOStatus:=(-ExecuteResult) and $7f;
        ExecuteResult:=((-ExecuteResult) and $ff00) shr 8;
      end;
{$else}
  {$ifdef Windows}
    StoreInherit:=ExecInheritsHandles;
    ExecInheritsHandles:=true;
  {$endif Windows}
    DosError:=0;
    If UseComSpec then
      Dos.Exec (Getenv('COMSPEC'),'/C '+MaybeQuoted(FixPath(progname))+' '+Comline)
    else
      begin
        if LocateExeFile(progname) then
          Dos.Exec(ProgName,Comline)
        else
          DosError:=2;
      end;
  {$ifdef Windows}
    ExecInheritsHandles:=StoreInherit;
  {$endif Windows}
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
{$endif}
    SwapVectors;
{$ifdef CPU86}
    { reset the FPU }
    {$asmmode att}
    asm
      fninit
    end;
{$endif CPU86}
End;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

initialization
  New(FIn); New(FOut); New(FErr);

finalization
  Dispose(FIn); Dispose(FOut); Dispose(FErr);
End.
