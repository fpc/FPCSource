{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Unit to redirect output and error to files

    Adapted from code donated to public domain by Schwartz Gabriel 20/03/1993

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit FPRedir;
Interface

{$R-}
{$ifndef linux}
  {$S-}
{$endif}

{$ifdef TP}
{$define implemented}
{$endif TP}
{$ifdef Go32v2}
{$define implemented}
{$endif}
{$ifdef Win32}
{$define implemented}
{$endif}
{$ifdef linux}
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

Implementation

Uses
{$ifdef go32v2}
  go32,
{$endif go32v2}
{$ifdef win32}
  windows,
{$endif win32}
{$ifdef linux}
  linux,
{$endif linux}
  dos;

var
  FIN,FOUT,FERR     : ^File;
  RedirChangedOut,
  RedirChangedIn    : Boolean;
  RedirChangedError : Boolean;
  InRedirDisabled,OutRedirDisabled,ErrorRedirDisabled : Boolean;

{*****************************************************************************
                                     Dos
*****************************************************************************}

{$ifdef implemented}

{$ifdef TP}

{$ifndef win32}
const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;
{$endif win32}

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

{ For linux the following functions exist
Function  Dup(oldfile:longint;var newfile:longint):Boolean;
Function  Dup2(oldfile,newfile:longint):Boolean;
Function  fdClose(fd:longint):boolean;
}
{$ifdef go32v2}

function dup(fh : longint;var nh : longint) : boolean;
  var
    Regs : Registers;

begin
    Regs.ah:=$45;
    Regs.bx:=fh;
    MsDos (Regs);
    Dup:=true;
    If (Regs.Flags and fCarry)=0 then
      nh:=Regs.Ax
    else
      Dup:=false;
end;

function dup2(fh,nh : longint) : boolean;
  var
    Regs : Registers;

begin
    Dup2:=true;
    If fh=nh then
      exit;
    Regs.ah:=$46;
    Regs.bx:=fh;
    Regs.cx:=nh;
    MsDos (Regs);
    If (Regs.Flags and fCarry)<>0 then
      Dup2:=false;
end;

Function FdClose (Handle : Longint) : boolean;
var Regs: registers;
begin
  Regs.Eax := $3e00;
  Regs.Ebx := Handle;
  MsDos(Regs);
  FdClose:=(Regs.Flags and fCarry)=0;
end;

{$endif def go32v2}

{$ifdef win32}
Function FdClose (Handle : Longint) : boolean;
begin
  { Do we need this ?? }
  FdClose:=true;
end;
{$endif}

{$ifdef TP}
Function FdClose (Handle : Longint) : boolean;
begin
  { if executed as under GO32 this hangs the DOS-prompt }
  FdClose:=true;
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
{$ifdef win32}
    if SetStdHandle(Std_Output_Handle,FileRec(FOUT^).Handle) then
{$else not win32}
    if dup(StdOutputHandle,TempHOut) and
       dup2(FileRec(FOUT^).Handle,StdOutputHandle) then
{$endif not win32}
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
{$ifdef win32}
    if SetStdHandle(Std_Input_Handle,FileRec(FIN^).Handle) then
{$else not win32}
    if dup(StdInputHandle,TempHIn) and
       dup2(FileRec(FIN^).Handle,StdInputHandle) then
{$endif not win32}
      begin
         ChangeRedirIn:=True;
         InRedirDisabled:=False;
      end;
{$endif def FPC}
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
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandleError:=Handles^[StdErrorHandle];
    Handles^[StdErrorHandle]:=Handles^[FileRec (FERR^).Handle];
    ChangeRedirError:=True;
    ErrorRedirDisabled:=False;
{$else}
{$ifdef win32}
    if SetStdHandle(Std_Error_Handle,FileRec(FERR^).Handle) then
{$else not win32}
    if dup(StdErrorHandle,TempHError) and
       dup2(FileRec(FERR^).Handle,StdErrorHandle) then
{$endif not win32}
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
{$ifdef win32}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not win32}
    dup2(TempHOut,StdOutputHandle);
{$endif not win32}
{$endif FPC}
    Close (FOUT^);
    fdClose(TempHOut);
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
{$ifdef win32}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not win32}
    dup2(TempHIn,StdInputHandle);
{$endif not win32}
{$endif}
    Close (FIn^);
    fdClose(TempHIn);
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
{$ifdef win32}
    SetStdHandle(Std_Input_Handle,StdInputHandle);
{$else not win32}
    dup2(TempHIn,StdInputHandle);
{$endif not win32}
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
{$ifdef win32}
    SetStdHandle(Std_Input_Handle,FileRec(FIn^).Handle);
{$else not win32}
    dup2(FileRec(FIn^).Handle,StdInputHandle);
{$endif not win32}
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
{$ifdef win32}
    SetStdHandle(Std_Output_Handle,StdOutputHandle);
{$else not win32}
    dup2(TempHOut,StdOutputHandle);
{$endif not win32}
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
{$ifdef win32}
    SetStdHandle(Std_Output_Handle,FileRec(FOut^).Handle);
{$else not win32}
    dup2(FileRec(FOut^).Handle,StdOutputHandle);
{$endif not win32}
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
{$ifdef win32}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not win32}
    dup2(TempHError,StdErrorHandle);
{$endif not win32}
{$endif}
    Close (FERR^);
    fdClose(TempHError);
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
{$ifdef win32}
    SetStdHandle(Std_Error_Handle,StdErrorHandle);
{$else not win32}
    dup2(TempHError,StdErrorHandle);
{$endif not win32}
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
{$ifdef win32}
    SetStdHandle(Std_Error_Handle,FileRec(FErr^).Handle);
{$else not win32}
    dup2(FileRec(FERR^).Handle,StdErrorHandle);
{$endif not win32}
{$endif}
    ErrorRedirDisabled:=False;
  end;

{............................................................................}

function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
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

function ExecuteRedir (Const ProgName, ComLine, RedirStdIn, RedirStdOut, RedirStdErr : String) : boolean;
begin
  ExecuteRedir:=false;
end;

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
{$ifdef win32}
    var
      StoreInherit : BOOL;
{$endif win32}

  Begin
{$IfDef MsDos}
  SmallHeap;
{$EndIf MsDos}
    SwapVectors;
    { Must use shell() for linux for the wildcard expansion (PFV) }
{$ifdef linux}
    Shell(Progname+' '+Comline);
{$else}
{$ifdef win32}
    StoreInherit:=ExecInheritsHandles;
    ExecInheritsHandles:=true;
{$endif win32}
    DosError:=0;
    Dos.Exec (ProgName, ComLine);
{$ifdef win32}
    ExecInheritsHandles:=StoreInherit;
{$endif win32}
{$endif}
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
    SwapVectors;
{$IfDef MsDos}
  Fullheap;
{$EndIf MsDos}
  End;

{*****************************************************************************
                                  Initialize
*****************************************************************************}

var oldexit : pointer;

procedure RedirExit; {$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
  Dispose(FIn); Dispose(FOut); Dispose(FErr);
end;

Begin
  oldexit:=exitproc;
  exitproc:=@RedirExit;
  New(FIn); New(FOut); New(FErr);
End.
{
  $Log$
  Revision 1.1  2000-07-13 09:48:35  michael
  + Initial import

  Revision 1.25  2000/05/17 10:19:53  pierre
   * Reset DosError to zero !

  Revision 1.24  1999/11/10 17:10:59  pierre
   + DosExecute to interface

  Revision 1.23  1999/09/22 13:03:27  pierre
   * Win32 ExecInheritsHandles typo problem

  Revision 1.22  1999/09/22 09:03:58  peter
    * typo fixed for win32

  Revision 1.21  1999/09/21 11:28:22  pierre
   + Redir for win32

  Revision 1.20  1999/08/05 16:54:36  peter
    * win32 fixes

  Revision 1.19  1999/08/03 20:22:36  peter
    + TTab acts now on Ctrl+Tab and Ctrl+Shift+Tab...
    + Desktop saving should work now
       - History saved
       - Clipboard content saved
       - Desktop saved
       - Symbol info saved
    * syntax-highlight bug fixed, which compared special keywords case sensitive
      (for ex. 'asm' caused asm-highlighting, while 'ASM' didn't)
    * with 'whole words only' set, the editor didn't found occourences of the
      searched text, if the text appeared previously in the same line, but didn't
      satisfied the 'whole-word' condition
    * ^QB jumped to (SelStart.X,SelEnd.X) instead of (SelStart.X,SelStart.Y)
      (ie. the beginning of the selection)
    * when started typing in a new line, but not at the start (X=0) of it,
      the editor inserted the text one character more to left as it should...
    * TCodeEditor.HideSelection (Ctrl-K+H) didn't update the screen
    * Shift shouldn't cause so much trouble in TCodeEditor now...
    * Syntax highlight had problems recognizing a special symbol if it was
      prefixed by another symbol character in the source text
    * Auto-save also occours at Dos shell, Tool execution, etc. now...

  Revision 1.17  1999/05/01 23:45:07  pierre
   * FIn FOut FErr dispsoed at exit

  Revision 1.16  1999/04/29 22:57:09  pierre
   + added code for temporary disabling of redirection

  Revision 1.15  1999/04/07 21:55:52  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.14  1999/03/20 00:04:49  pierre
   * handle loss fixed

  Revision 1.13  1999/03/09 01:34:35  peter
    * linux unit

  Revision 1.12  1999/03/08 14:58:10  peter
    + prompt with dialogs for tools

  Revision 1.11  1999/03/01 15:42:01  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.10  1999/02/22 12:46:58  peter
    * small fixes for linux and grep

  Revision 1.9  1999/02/22 11:12:33  pierre
    * dup and dup2 work for go32v2
    + also should work for linux (after linux.pp patch)

  Revision 1.8  1999/02/22 02:15:18  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.7  1999/02/20 15:18:32  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.6  1999/02/05 13:51:43  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

}