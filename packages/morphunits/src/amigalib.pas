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
unit amigalib;

interface

function DoMethod(obj : longword; const msg : array of LongWord): longword;
function DoMethod(obj : pointer; const msg : array of LongWord): longword;
function DoMethodA(obj : longword; msg1 : Pointer): longword; inline;

function DoSuperMethod(class_: longword; obj : longword; const msg : array of LongWord): longword;
function DoSuperMethodA(class_: longword; obj : longword; msg1 : Pointer): longword; inline;
function DoSuperMethodA(class_: pointer; obj : pointer; msg1 : Pointer): longword; inline;

function DoSuperNew(class_: pointer; obj: pointer; const tags: array of LongWord): longword;

{ This procedure is used to pop Dispatcher arguments from the EmulHandle }
procedure DISPATCHERARG(var cl; var obj; var msg);

function HookEntry: longword;

implementation

uses exec, intuition, utility;


function DoMethodA(obj : longword; msg1 : Pointer): longword;
var
  hook: PHook;
begin
  hook:=@THook(OCLASS(p_Object(obj))^.cl_Dispatcher);
  with GetEmulHandle^ do
    begin
      reg[regA0]:=longword(hook);
      reg[regA1]:=longword(msg1);
      reg[regA2]:=obj;

      { This is magic, but it essentially calls the class Dispatcher Hook entry point }
      DoMethodA:=EmulCallDirect68k(hook^.h_Entry);
    end;
end;
{
// the old assembler implementation which trashes r31, kept for reference
asm
  mflr r31

  lwz r9,-4(r3)
  stw r9,32(r2)
  stw r4,36(r2)
  stw r3,40(r2)

  lwz r11,104(r2)
  lwz r3,8(r9)
  mtlr r11
  blrl

  mtlr r31
end ['R31'];
}
function DoMethod(obj : longword; const msg : array of LongWord): longword; inline;
begin
  DoMethod:=DoMethodA(obj, @msg);
end;

function DoMethod(obj : pointer; const msg : array of LongWord): longword; inline;
begin
  DoMethod:=DoMethodA(DWord(obj), @msg);
end;

function DoSuperMethodA(class_: longword; obj : longword; msg1 : Pointer): longword; inline;
var
  hook: PHook;
begin
  hook:=@PIClass(class_)^.cl_Super^.cl_Dispatcher;
  with GetEmulHandle^ do
    begin
      reg[regA0]:=longword(hook);
      reg[regA1]:=longword(msg1);
      reg[regA2]:=obj;

      { This is magic, but it calls the superclass Dispatcher hook entry point }
      DoSuperMethodA:=EmulCallDirect68k(hook^.h_Entry);
    end;
end;
{
// the old assembler implementation which trashes r31, kept for reference
asm
  mflr r31

  lwz r9,24(r3)
  stw r9,32(r2)
  stw r5,36(r2)
  stw r4,40(r2)

  lwz r11,104(r2)
  lwz r3,8(r9)
  mtlr r11
  blrl

  mtlr r31
end ['R31'];
}

function DoSuperMethodA(class_: pointer; obj : pointer; msg1 : Pointer): longword; inline;
begin
  DoSuperMethodA:=DoSuperMethodA(DWord(class_),DWord(obj),msg1);
end;

function DoSuperMethod(class_: longword; obj : longword; const msg : array of LongWord): longword;
begin
  DoSuperMethod:=DoSuperMethodA(class_, obj, @msg);
end;

function DoSuperNew(class_: pointer; obj: pointer; const tags: array of LongWord): longword;
var opSet: topSet;
begin
  opSet.MethodID := OM_NEW;
  opSet.ops_AttrList := @tags;
  opSet.ops_GInfo := nil;
  DoSuperNew:=DoSuperMethodA(class_,obj,@opset);
end;

{ This procedure is used to pop Dispatcher arguments from the EmulHandle }
procedure DISPATCHERARG(var cl; var obj; var msg);
begin
  with GetEmulHandle^ do
    begin
      DWord(cl):=reg[regA0];
      DWord(obj):=reg[regA2];
      DWord(msg):=reg[regA1];
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
  THookSubEntryFunc = function(a, b, c: Pointer): longword;

function HookEntry: longword;
var
  hook: PHook;
begin
  hook:=REG_A0;
  HookEntry:=THookSubEntryFunc(hook^.h_SubEntry)(hook, REG_A2, REG_A1);
end;

end.
