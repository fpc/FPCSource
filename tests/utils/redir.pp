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

{$H+}
{$R-}
{$ifndef Linux}
{$ifndef Unix}
  {$S-}
{$endif}
{$endif}

{$ifdef TP}
{$define implemented}
{$endif TP}
{$ifdef Go32v2}
{$define implemented}
{$endif}
{$ifdef OS2}
{$define shell_implemented}
{$endif}
{$ifdef windows}
{$define implemented}
{$endif}
{$ifdef linux}
{$define implemented}
{$endif}
{$ifdef BSD}
{$define implemented}
{$endif}
{$ifdef BEOS}
{$define implemented}
{$endif}
{$ifdef macos}
{$define shell_implemented}
{$endif}
{$ifdef sunos}
{$define implemented}
{$endif}

{ be sure msdos is not set for FPC compiler }
{$ifdef FPC}
{$UnDef MsDos}
{$endif FPC}

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

{$if defined(macos) or defined(windows) or defined(shell_implemented)}
{$define usedos}
{$endif}

Uses
{$ifdef go32v2}
  go32,
{$endif go32v2}
{$ifdef windows}
  windows,
{$endif windows}
{$ifdef unix}
  {$ifdef ver1_0}
    linux,
  {$else}
    baseunix,
    unix,
  {$endif}
{$endif unix}
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

{$ifdef TP}

{$ifndef windows}
const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;
{$endif windows}

Type
  PtrRec = packed record
             Ofs, Seg : Word;
           end;

  PHandles = ^THandles;
  THandles = Array [Byte] of Byte;

  PWord = ^Word;

Var
  MinBlockSize : Word;
  MyBlockSize  : Word;
  Handles      : PHandles;
  PrefSeg      : Word;
  OldHandleOut,OldHandleIn,OldHandleError    : Byte;
{$endif TP}

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

{$ifndef ver1_0}
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
{$endif ver1_0}


Function {$ifdef ver1_0}fdclose{$else}fpclose{$endif} (Handle : Longint) : boolean;
var Regs: registers;
begin
  Regs.Eax := $3e00;
  Regs.Ebx := Handle;
  MsDos(Regs);
  {$ifdef ver1_0}fdclose{$else}fpclose{$endif}:=(Regs.Flags and fCarry)=0;
end;

{$endif def go32v2}

{$ifdef windows}
Function {$ifdef ver1_0}fdclose{$else}fpclose{$endif} (Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  {$ifdef ver1_0}fdclose{$else}fpclose{$endif}:=true;
end;
{$endif}

{$ifdef os2}
Function {$ifdef ver1_0}fdclose{$else}fpclose{$endif} (Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  {$ifdef ver1_0}fdclose{$else}fpclose{$endif}:=true;
end;
{$endif}

{$ifdef TP}
Function {$ifdef ver1_0}fdclose{$else}fpclose{$endif} (Handle : Longint) : boolean;
begin
  { if executed as under GO32 this hangs the DOS-prompt }
  {$ifdef ver1_0}fdclose{$else}fpclose{$endif}:=true;
end;

{$endif}

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
    {$ifdef ver1_0}
    dup(StdOutputHandle,TempHOut);
    dup2(FileRec(FOUT^).Handle,StdOutputHandle);
    {$else}
    TempHOut:=fpdup(StdOutputHandle);
    fpdup2(FileRec(FOUT^).Handle,StdOutputHandle);
    {$endif}
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
    {$ifdef ver1_0}
    dup(StdInputHandle,TempHIn);
    dup2(FileRec(FIn^).Handle,StdInputHandle);
    {$else}
    TempHIn:=fpdup(StdInputHandle);
    fpdup2(FileRec(FIn^).Handle,StdInputHandle);
    {$endif}
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
    {$ifdef ver1_0}
    dup(StdErrorHandle,TempHError);
    dup2(FileRec(PF^).Handle,StdErrorHandle);
    {$else}
    TempHError:=fpdup(StdErrorHandle);
    fpdup2(FileRec(PF^).Handle,StdErrorHandle);
    {$endif}
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


{$IfDef MsDos}
{Set HeapEnd Pointer to Current Used Heapsize}
Procedure SmallHeap;assembler;
asm
                mov     bx,word ptr HeapPtr
                shr     bx,4
                inc     bx
                add     bx,word ptr HeapPtr+2
                mov     ax,PrefixSeg
                sub     bx,ax
                mov     es,ax
                mov     ah,4ah
                int     21h
end;



{Set HeapEnd Pointer to Full Heapsize}
Procedure FullHeap;assembler;
asm
                mov     bx,word ptr HeapEnd
                shr     bx,4
                inc     bx
                add     bx,word ptr HeapEnd+2
                mov     ax,PrefixSeg
                sub     bx,ax
                mov     es,ax
                mov     ah,4ah
                int     21h
end;

{$EndIf MsDos}


  procedure RestoreRedirOut;

  begin
    If not RedirChangedOut then Exit;
{$ifndef FPC}
    Handles^[StdOutputHandle]:=OldHandleOut;
    OldHandleOut:=StdOutputHandle;
{$else}
{$ifdef windows}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not windows}
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(TempHOut,StdOutputHandle);
{$endif not windows}
{$endif FPC}
    Close (FOUT^);
    {$ifdef ver1_0}fdclose{$else}fpclose{$endif}(TempHOut);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(TempHIn,StdInputHandle);
{$endif not windows}
{$endif}
    Close (FIn^);
    {$ifdef ver1_0}fdclose{$else}fpclose{$endif}(TempHIn);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(TempHIn,StdInputHandle);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(FileRec(FIn^).Handle,StdInputHandle);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(TempHOut,StdOutputHandle);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(FileRec(FOut^).Handle,StdOutputHandle);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(TempHError,StdErrorHandle);
{$endif not windows}
{$endif}
    { don't close when redirected to STDOUT }
    if not RedirStdErrToStdOut then
      Close (FERR^);
    {$ifdef ver1_0}fdclose{$else}fpclose{$endif}(TempHError);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(TempHError,StdErrorHandle);
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
    {$ifdef ver1_0}dup2{$else}fpdup2{$endif}(FileRec(FERR^).Handle,StdErrorHandle);
{$endif not windows}
{$endif}
    ErrorRedirDisabled:=False;
  end;

{............................................................................}

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
{$ifndef FPC}
  PrefSeg:=PrefixSeg;
{$endif FPC}
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

  procedure DosExecute(ProgName, ComLine : String);
{$ifdef windows}
    var
      StoreInherit : BOOL;
{$endif windows}

  Begin
{$IfDef MsDos}
  SmallHeap;
{$EndIf MsDos}
{$ifdef usedos}
    SwapVectors;
{$endif usedos}
    { Must use shell() for linux for the wildcard expansion (PFV) }
{$ifdef UNIX}
    IOStatus:=0;
    ExecuteResult:=Shell(FixPath(Progname)+' '+Comline);
  {$ifdef ver1_0}
    { Signal that causes the stop of the shell }
    IOStatus:=ExecuteResult and $7F;
    { Exit Code seems to be in the second byte,
      is this also true for BSD ??
      $80 bit is a CoreFlag apparently }
    ExecuteResult:=(ExecuteResult and $ff00) shr 8;
  {$else}
    if ExecuteResult<0 then
      begin
        IOStatus:=(-ExecuteResult) and $7f;
        ExecuteResult:=((-ExecuteResult) and $ff00) shr 8;
      end;
  {$endif}
{$else}
  {$ifdef windows}
    StoreInherit:=ExecInheritsHandles;
    ExecInheritsHandles:=true;
    { Avoid dialog boxes if dll loading fails }
    SetErrorMode(SEM_FAILCRITICALERRORS);
  {$endif windows}
    DosError:=0;
    If UseComSpec then
      {$ifndef usedos}
      Sysutils.ExecuteProcess (Getenv('COMSPEC'),'/C '+FixPath(progname)+' '+Comline)
      {$else}
      Exec (Getenv('COMSPEC'),'/C '+FixPath(progname)+' '+Comline)
      {$endif}
    else
      begin
        if LocateExeFile(progname) then
          {$ifndef usedos}
          Sysutils.ExecuteProcess(ProgName,Comline)
          {$else}
          {$ifdef macos}
          Dos.Exec(''''+ProgName+'''',Comline) {Quotes needed !}
          {$else}
          Dos.Exec(ProgName,Comline)
          {$endif}
          {$endif}
        else
          DosError:=2;
      end;
  {$ifdef windows}
    ExecInheritsHandles:=StoreInherit;
    SetErrorMode(0);
  {$endif windows}
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
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
