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
  {$define in_dos}
{$endif TP}
{$ifdef Go32v2}
  {$define in_dos}
{$endif}

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
  dos;


{*****************************************************************************
                                     Dos
*****************************************************************************}

{$ifdef in_dos}

Type
  PtrRec = packed record
             Ofs, Seg : Word;
           end;

  PHandles = ^THandles;
  THandles = Array [Byte] of Byte;

  PWord = ^Word;

Var
  PrefSeg      : Word;
{$IfDef MsDos}
  MinBlockSize : Word;
  MyBlockSize  : Word;
{$endif}
  F,FE         : File;
  RedirChanged : Boolean;
  RedirErrorChanged : Boolean;
  OldHandle,OldErrorHandle    : Byte;
{$ifdef UseDUP}
  TempH, TempErrorH : longint;
{$endif}
{$ifdef FPC}
  HandlesOffset : word;
{$else}
  Handles      : PHandles;
{$endif FPC}

function dup(fh : longint) : longint;
  var
    Regs : Registers;

begin
    Regs.ax:=$45;
    Regs.bx:=fh;
    MsDos (Regs);
    If (Regs.Flags and fCarry)=0 then
      Dup:=Regs.Ax
    else
      Dup:=-1;
end;

function dup2(fh,nh : longint) : longint;
  var
    Regs : Registers;

begin
    If fh=nh then
      begin
        dup2:=nh;
        exit;
      end;
    Regs.ax:=$46;
    Regs.bx:=fh;
{$ifndef TP}
    Regs.cs:=nh;
{$endif}
    MsDos (Regs);
    If (Regs.Flags and fCarry)=0 then
      Dup2:=nh
    else
      Dup2:=-1;
end;

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
  var temp : byte;
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
{$ifdef UseDUP}
    TempH:=dup(1);
    if dup2(1,FileRec(F).Handle)=FileRec(F).Handle then
{$else UseDUP}
    DosMemGet(prefseg,HandlesOffset+1,OldHandle,1);
    DosMemGet(prefseg,HandlesOffset+FileRec(F).handle,temp,1);
    dosmemput(prefseg,HandlesOffset+1,temp,1);
    { NO MEM use as %fs is distroyed somewhere !!
    OldHandle:=Mem[prefseg:HandlesOffset+1];
    Mem[prefseg:HandlesOffset+1]:=Mem[prefseg:HandlesOffset+FileRec(F).handle];}
{$endif UseDUP}
      ChangeRedir:=True;
{$endif}
     RedirChanged:=True;
  end;

function ChangeErrorRedir(Const Redir : String; AppendToFile : Boolean) : Boolean;
  var temp : byte;
  begin
    ChangeErrorRedir:=False;
    If Redir = '' then Exit;
    Assign (FE, Redir);
    If AppendToFile and FileExist(Redir) then
      Begin
      Reset(FE,1);
      Seek(FE,FileSize(FE));
      End else Rewrite (FE);

    RedirError:=IOResult;
    IOStatus:=RedirError;
    If IOStatus <> 0 then Exit;
{$ifndef FPC}
    Handles:=Ptr (prefseg, PWord (Ptr (prefseg, $34))^);
    OldErrorHandle:=Handles^[2];
    Handles^[2]:=Handles^[FileRec (FE).Handle];
    ChangeErrorRedir:=True;
{$else}
{$ifdef UseDUP}
    TempErrorH:=dup(2);
    if dup2(2,FileRec(F).Handle)=FileRec(F).Handle then
{$else UseDUP}
    DosMemGet(prefseg,HandlesOffset+2,OldErrorHandle,1);
    DosMemGet(prefseg,HandlesOffset+FileRec(F).handle,temp,1);
    dosmemput(prefseg,HandlesOffset+1,temp,1);
    {OldErrorHandle:=Mem[prefseg:HandlesOffset+2];
    Mem[prefseg:HandlesOffset+2]:=Mem[prefseg:HandlesOffset+FileRec(FE).handle];}
{$endif UseDUP}
      ChangeErrorRedir:=True;
{$endif}
     RedirErrorChanged:=True;
  end;

{............................................................................}

{$IfDef MsDos}
  procedure CompactHeap;

  var
    Regs : Registers;

  begin
    Regs.AH:=$4A;
    Regs.ES:=PrefSeg;
    Regs.BX:=MinBlockSize + (PtrRec (HeapPtr).Seg - PtrRec (HeapOrg).Seg);
    MsDos (Regs);
  end;

{............................................................................}

  procedure ExpandHeap;

  var
    Regs : Registers;

  begin
    Regs.AH:=$4A;
    Regs.ES:=PrefSeg;
    Regs.BX:=MyBlockSize;
    MsDos (Regs);
  end;

{$EndIf MsDos}
{............................................................................}

  procedure RestoreRedir;

  begin
    If not RedirChanged then Exit;
{$ifndef FPC}
    Handles^[1]:=OldHandle;
{$else}
{$ifdef UseDUP}
    dup2(1,TempH);
{$else UseDUP}
    dosmemput(prefseg,HandlesOffset+1,OldHandle,1);
    {Mem[prefseg:HandlesOffset+1]:=OldHandle;}
{$endif UseDUP}
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
{$else}
{$ifdef UseDUP}
    dup2(1,TempErrorH);
{$else UseDUP}
    dosmemput(prefseg,HandlesOffset+2,OldErrorHandle,1);
    {Mem[prefseg:HandlesOffset+2]:=OldErrorHandle;}
{$endif UseDUP}
{$endif}
    Close (FE);
    RedirErrorChanged:=false;
  end;

{............................................................................}

  procedure DosExecute(ProgName, ComLine : String);

  Begin
{$IfDef MsDos}
  CompactHeap;
{$EndIf MsDos}
    SwapVectors;
    Dos.Exec (ProgName, ComLine);
    IOStatus:=DosError;
    ExecuteResult:=DosExitCode;
    SwapVectors;
{$IfDef MsDos}
  Expandheap;
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


procedure InitRedir;
begin
{$ifndef FPC}
  PrefSeg:=PrefixSeg;
{$else FPC}
 {$ifdef go32v2}
  PrefSeg:=go32_info_block.linear_address_of_original_psp div 16;
  HandlesOffset:=Memw[prefseg:$34];
 {$else }
  PrefSeg:=0;
 {$endif }
{$endif FPC}
end;

{$endif indos}


{*****************************************************************************
                                 Linux
*****************************************************************************}

{$ifdef linux}

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

{$endif linux}


{*****************************************************************************
                                  Initialize
*****************************************************************************}

Begin
  InitRedir;
End.
{
  $Log$
  Revision 1.7  1999-02-20 15:18:32  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.6  1999/02/05 13:51:43  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

}
