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
  IOStatus      : Integer;
  RedirError    : Integer;
  ExecuteResult : Word;

{------------------------------------------------------------------------------}
function ExecuteRedir (Const ProgName, ComLine, RedirStdOut, RedirStdErr : String) : boolean;

function ChangeRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreRedir;

function ChangeErrorRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
procedure RestoreErrorRedir;


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
  PrefSeg      : Word;
  MinBlockSize : Word;
  MyBlockSize  : Word;
  Handles      : PHandles;
  OldHandle,OldErrorHandle    : Byte;
{$endif TP}

Var
  F,FE	            : File;
  RedirChanged      : Boolean;
  RedirErrorChanged : Boolean;
  TempH, TempErrorH : longint;

{ For linux the following functions exist
Function  Dup(oldfile:longint;var newfile:longint):Boolean;
Function  Dup2(oldfile,newfile:longint):Boolean; }
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

{$endif def go32v2}

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

function ChangeRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
  begin
    ChangeRedir:=False;
    If Redir = '' then Exit;
    Assign (F, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(F,1);
      Seek(F,FileSize(F));
      End else Rewrite (F);

    RedirError:=IOResult;
    IOStatus:=RedirError;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldHandle:=Handles^[1];
    Handles^[1]:=Handles^[FileRec (F).Handle];
    ChangeRedir:=True;
{$else}
    if dup(StdOutputHandle,TempH) and
       dup2(FileRec(F).Handle,StdOutputHandle) then
      ChangeRedir:=True;
{$endif def FPC}
     RedirChanged:=True;
  end;

function ChangeErrorRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
  begin
    ChangeErrorRedir:=False;
    If Redir = '' then Exit;
    Assign (FE, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(FE,1);
      Seek(FE,FileSize(FE));
      End
    else
      Rewrite (FE);

    RedirError:=IOResult;
    IOStatus:=RedirError;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldErrorHandle:=Handles^[2];
    Handles^[2]:=Handles^[FileRec (FE).Handle];
    ChangeErrorRedir:=True;
{$else}
    if dup(StdErrorHandle,TempErrorH) and
       dup2(FileRec(FE).Handle,StdErrorHandle) then
      ChangeErrorRedir:=True;
{$endif}
    RedirErrorChanged:=True;
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


  procedure RestoreRedir;

  begin
    If not RedirChanged then Exit;
{$ifndef FPC}
    Handles^[1]:=OldHandle;
    OldHandle:=StdOutputHandle;
{$else}
    dup2(TempH,StdOutputHandle);
{$endif}
    Close (F);
    RedirChanged:=false;
  end;

  {............................................................................}

  procedure RestoreErrorRedir;

  begin
    If not RedirErrorChanged then Exit;
{$ifndef FPC}
    Handles^[2]:=OldErrorHandle;
    OldErrorHandle:=StdErrorHandle;
{$else}
    dup2(TempErrorH,StdErrorHandle);
{$endif}
    Close (FE);
    RedirErrorChanged:=false;
  end;

{............................................................................}

  procedure DosExecute(ProgName, ComLine : String);

  Begin
{$IfDef MsDos}
    SmallHeap;
{$EndIf MsDos}
    SwapVectors;
    Dos.Exec (ProgName, ComLine);
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
    SwapVectors;
{$IfDef MsDos}
    Fullheap;
{$EndIf MsDos}
  End;

{............................................................................}

function ExecuteRedir (Const ProgName, ComLine, RedirStdOut, RedirStdErr : String) : boolean;
Begin
  RedirError:=0;
  ExecuteResult:=0;
  IOStatus:=0;
  if RedirStdOut<>'' then
    ChangeRedir(RedirStdOut,false);
  if RedirStdErr<>'stderr' then
    RedirErrorChanged:=ChangeErrorRedir(RedirStdErr,false);
  DosExecute(ProgName,ComLine);
  RestoreRedir;
  RestoreErrorRedir;
  ExecuteRedir:=(IOStatus=0) and (RedirError=0) and (ExecuteResult=0);
End;


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

End.
{
  $Log$
  Revision 1.9  1999-02-22 11:12:33  pierre
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
