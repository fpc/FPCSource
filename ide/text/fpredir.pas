{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Unit to redirect output and error to files

    Adapted from code donated to public domain by Schwartz Gabriel.   20/03/1993.

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

function  ChangeRedirOut(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedirOut;
function  ChangeRedirIn(Const Redir : String) : Boolean;
procedure RestoreRedirIn;
function  ChangeRedirError(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedirError;


Implementation

Uses
{$ifdef go32v2}
  go32,
{$endif go32v2}
{$ifdef linux}
  linux,
{$endif linux}
  dos;


{*****************************************************************************
                                     Dos
*****************************************************************************}

{$ifdef implemented}

{$ifdef TP}

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

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
  FIN,FOUT,FERR     : ^File;
  RedirChangedOut,
  RedirChangedIn    : Boolean;
  RedirChangedError : Boolean;
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
{$else}
    if dup(StdOutputHandle,TempHOut) and
       dup2(FileRec(FOUT^).Handle,StdOutputHandle) then
      ChangeRedirOut:=True;
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
{$else}
    if dup(StdInputHandle,TempHIn) and
       dup2(FileRec(FIN^).Handle,StdInputHandle) then
      ChangeRedirIn:=True;
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
{$else}
    if dup(StdErrorHandle,TempHError) and
       dup2(FileRec(FERR^).Handle,StdErrorHandle) then
      ChangeRedirError:=True;
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
    dup2(TempHOut,StdOutputHandle);
{$endif}
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
    dup2(TempHIn,StdInputHandle);
{$endif}
    Close (FIn^);
    fdClose(TempHIn);
    RedirChangedIn:=false;
  end;

  {............................................................................}

  procedure RestoreRedirError;

  begin
    If not RedirChangedError then Exit;
{$ifndef FPC}
    Handles^[StdErrorHandle]:=OldHandleError;
    OldHandleError:=StdErrorHandle;
{$else}
    dup2(TempHError,StdErrorHandle);
{$endif}
    Close (FERR^);
    fdClose(TempHError);
    RedirChangedError:=false;
  end;

{............................................................................}

  procedure DosExecute(ProgName, ComLine : String);

  Begin
{$IfDef MsDos}
  SmallHeap;
{$EndIf MsDos}
    SwapVectors;
    { Must use shell() for linux for the wildcard expansion (PFV) }
{$ifdef linux}
    Shell(Progname+' '+Comline);
{$else}
    Dos.Exec (ProgName, ComLine);
{$endif}
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
    SwapVectors;
{$IfDef MsDos}
  Fullheap;
{$EndIf MsDos}
  End;

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


procedure InitRedir;
begin
{$ifndef FPC}
  PrefSeg:=PrefixSeg;
{$endif FPC}
end;

{$else not  implemented}

{*****************************************************************************
                                 Linux
*****************************************************************************}

function ExecuteRedir (Const ProgName, ComLine, RedirStdOut, RedirStdErr : String) : boolean;
begin
  ExecuteRedir:=false;
end;

function ChangeRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
begin
  ChangeRedir:=false;
end;

procedure RestoreRedir;
begin
end;

function ChangeErrorRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
begin
  ChangeErrorRedir:=false;
end;

procedure RestoreErrorRedir;
begin
end;

procedure InitRedir;
begin
end;

{$endif not implemented}


{*****************************************************************************
                                  Initialize
*****************************************************************************}

Begin
  New(FIn); New(FOut); New(FErr);
End.
{
  $Log$
  Revision 1.15  1999-04-07 21:55:52  peter
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
