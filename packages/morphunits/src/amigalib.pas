{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005-2015 Karoly Balogh

    abox.lib implementation for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INLINE ON}
unit amigalib
  deprecated 'Unit will be removed. Functions are moved to intuition, utility unit.';

interface

function DoMethod(obj : longword; const msg : array of LongWord): longword; inline;
function DoMethod(obj : pointer; const msg : array of LongWord): longword; inline;
function DoMethodA(obj : longword; msg1 : Pointer): longword; inline;

function DoSuperMethod(class_: longword; obj : longword; const msg : array of LongWord): longword; inline;
function DoSuperMethodA(class_: longword; obj : longword; msg1 : Pointer): longword; inline;
function DoSuperMethodA(class_: pointer; obj : pointer; msg1 : Pointer): longword; inline;

function DoSuperNew(class_: pointer; obj: pointer; const tags: array of LongWord): longword;

{ This procedure is used to pop Dispatcher arguments from the EmulHandle }
procedure DISPATCHERARG(var cl; var obj; var msg);

function HookEntry: PtrUInt;

implementation

uses
  exec, intuition, utility;

function DoMethod(obj : longword; const msg : array of LongWord): longword; inline;
begin
  DoMethod := Intuition.DoMethod(PObject_(Obj), Msg);
end;

function DoMethod(obj : pointer; const msg : array of LongWord): longword; inline;
begin
  DoMethod := Intuition.DoMethod(PObject_(Obj), Msg);
end;

function DoMethodA(obj : longword; msg1 : Pointer): longword; inline;
begin
  DoMethodA := Intuition.DoMethodA(PObject_(Obj), msg1);
end;

function DoSuperMethod(class_: longword; obj : longword; const msg : array of LongWord): longword; inline;
begin
  DoSuperMethod := Intuition.DoSuperMethod(PIClass(Class_), PObject_(Obj), Msg);
end;

function DoSuperMethodA(class_: longword; obj : longword; msg1 : Pointer): longword; inline;
begin
  DoSuperMethodA := Intuition.DoSuperMethodA(PIClass(class_), PObject_(obj), msg1);
end;

function DoSuperMethodA(class_: pointer; obj : pointer; msg1 : Pointer): longword; inline;
begin
  DoSuperMethodA := Intuition.DoSuperMethodA(PIClass(class_), PObject_(Obj), Msg1);
end;

function DoSuperNew(class_: pointer; obj: pointer; const tags: array of LongWord): longword;
begin
  DoSuperNew := Intuition.DoSuperNew(PIClass(class_), PObject_(Obj), Tags);
end;

{ This procedure is used to pop Dispatcher arguments from the EmulHandle }
procedure DISPATCHERARG(var cl; var obj; var msg);
begin
  with GetEmulHandle^ do
  begin
    PtrUInt(cl) := reg[regA0];
    PtrUInt(obj) := reg[regA2];
    PtrUInt(msg) := reg[regA1];
  end;
end;
{
// assembler implementation, kept for reference
asm
  lwz r6,32(r2) // REG_a0
  stw r6,(r3)   // cl
  lwz r6,40(r2) // REG_a2
  stw r6,(r4)   // obj
  lwz r6,36(r2) // REG_a1
  stw r6,(r5)   // msg
end;}

type
  THookSubEntryFunc = function(a, b, c: Pointer): PtrUInt;

function HookEntry: PtrUInt;
var
  hook: PHook;
begin
  hook := REG_A0;
  HookEntry := THookSubEntryFunc(hook^.h_SubEntry)(hook, REG_A2, REG_A1);
end;

end.
