{
   $Id$

   This unit provides compiler-independent mechanisms to call special
   functions, i.e. local functions/procedures, constructors, methods,
   destructors, etc. As there are no procedural variables for these
   special functions, there is no Pascal way to call them directly.

   Copyright (c) 1997 Matthias K"oppe <mkoeppe@csmd.cs.uni-magdeburg.de>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
unit CallSpec;

{
  As of this version, the following compilers are supported. Please
  port CallSpec to other compilers (including earlier versions) and
  send your code to the above address.

  Compiler                    Comments
  --------------------------- -------------------------------------
  Turbo Pascal 6.0
  Borland/Turbo Pascal 7.0
  FPC Pascal 0.99.8
}

interface

{$i platform.inc}

{
  The frame pointer points to the local variables of a procedure.
  Use CurrentFramePointer to address the locals of the current procedure;
  use PreviousFramePointer to addess the locals of the calling procedure.
}
type
{$ifdef BIT_16}
   FramePointer = Word;
{$endif}
{$ifdef BIT_32}
   FramePointer = pointer;
{$endif}

function CurrentFramePointer: FramePointer;
function PreviousFramePointer: FramePointer;

{ This version of CallSpec supports four classes of special functions.
  (Please write if you need other classes.)
  For each, two types of argument lists are allowed:

  `Void' indicates special functions with no explicit arguments.
    Sample: constructor T.Init;
  `Pointer' indicates special functions with one explicit pointer argument.
    Sample: constructor T.Load(var S: TStream);
}

{ Constructor calls.

  Ctor     Pointer to the constructor.
  Obj      Pointer to the instance. NIL if new instance to be allocated.
  VMT      Pointer to the VMT (obtained by TypeOf()).
  returns  Pointer to the instance.
}
function CallVoidConstructor(Ctor: pointer; Obj: pointer; VMT: pointer): pointer;
function CallPointerConstructor(Ctor: pointer; Obj: pointer; VMT: pointer; Param1: pointer): pointer;

{ Method calls.

  Method   Pointer to the method.
  Obj      Pointer to the instance. NIL if new instance to be allocated.
  returns  Pointer to the instance.
}
function CallVoidMethod(Method: pointer; Obj: pointer): pointer;
function CallPointerMethod(Method: pointer; Obj: pointer; Param1: pointer): pointer;

{ Local-function/procedure calls.

  Func     Pointer to the local function (which must be far-coded).
  Frame    Frame pointer of the wrapping function.
}

function CallVoidLocal(Func: pointer; Frame: FramePointer): pointer;
function CallPointerLocal(Func: pointer; Frame: FramePointer; Param1: pointer): pointer;

{ Calls of functions/procedures local to methods.

  Func     Pointer to the local function (which must be far-coded).
  Frame    Frame pointer of the wrapping method.
  Obj      Pointer to the object that the method belongs to.
}
function CallVoidMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer): pointer;
function CallPointerMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer; Param1: pointer): pointer;


implementation

{$ifdef PPC_FPC}

{$ifdef CPUI386}
{$ASMMODE ATT}
{$endif CPUI386}

{ This indicates an FPC version which uses the same call scheme for
  method-local and procedure-local procedures, but which expects the
  ESI register be loaded with the Self pointer in method-local procs. }

type
  VoidLocal = function(_EBP: FramePointer): pointer;
  PointerLocal = function(_EBP: FramePointer; Param1: pointer): pointer;
  VoidMethodLocal = function(_EBP: FRAMEPOINTER): pointer;
  PointerMethodLocal = function(_EBP: FRAMEPOINTER; Param1: pointer): pointer;
  VoidConstructor = function(VMT: pointer; Obj: pointer): pointer;
  PointerConstructor = function(VMT: pointer; Obj: pointer; Param1: pointer): pointer;
  VoidMethod = function(Obj: pointer): pointer;
  PointerMethod = function(Obj: pointer; Param1: pointer): pointer;


function CallVoidConstructor(Ctor: pointer; Obj: pointer; VMT: pointer): pointer;
begin
{$ifdef VER1_0}
  { load the object pointer }
{$ifdef CPUI386}
  asm
        movl Obj, %esi
  end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
        move.l Obj, a5
  end;
{$endif CPU68K}
{$endif VER1_0}

  CallVoidConstructor := VoidConstructor(Ctor)(VMT, Obj)
end;


function CallPointerConstructor(Ctor: pointer; Obj: pointer; VMT: pointer; Param1: pointer): pointer;
begin
{$ifdef VER1_0}
{ load the object pointer }
{$ifdef CPUI386}
  asm
        movl Obj, %esi
  end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
        move.l Obj, a5
  end;
{$endif CPU68K}
{$endif VER1_0}
  CallPointerConstructor := PointerConstructor(Ctor)(VMT, Obj, Param1)
end;


function CallVoidMethod(Method: pointer; Obj: pointer): pointer;
begin
{$ifdef VER1_0}
  { load the object pointer }
{$ifdef CPUI386}
  asm
        movl Obj, %esi
  end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
        move.l Obj, a5
  end;
{$endif CPU68K}
{$endif VER1_0}
  CallVoidMethod := VoidMethod(Method)(Obj)
end;


function CallPointerMethod(Method: pointer; Obj: pointer; Param1: pointer): pointer;
begin
{$ifdef VER1_0}
  { load the object pointer }
{$ifdef CPUI386}
  asm
        movl Obj, %esi
  end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
        move.l Obj, a5
  end;
{$endif CPU68K}
{$endif VER1_0}
  CallPointerMethod := PointerMethod(Method)(Obj, Param1)
end;


function CallVoidLocal(Func: pointer; Frame: FramePointer): pointer;
begin
  CallVoidLocal := VoidLocal(Func)(Frame)
end;


function CallPointerLocal(Func: pointer; Frame: FramePointer; Param1: pointer): pointer;
begin
  CallPointerLocal := PointerLocal(Func)(Frame, Param1)
end;


function CallVoidMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer): pointer;
begin
{$ifdef VER1_0}
  { load the object pointer }
{$ifdef CPUI386}
  asm
        movl Obj, %esi
  end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
        move.l Obj, a5
  end;
{$endif CPU68K}
{$endif VER1_0}
  CallVoidMethodLocal := VoidMethodLocal(Func)(Frame)
end;


function CallPointerMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer; Param1: pointer): pointer;
begin
{$ifdef VER1_0}
  { load the object pointer }
{$ifdef CPUI386}
  asm
        movl Obj, %esi
  end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
        move.l Obj, a5
  end;
{$endif CPU68K}
{$endif VER1_0}
  CallPointerMethodLocal := PointerMethodLocal(Func)(Frame, Param1)
end;


function CurrentFramePointer: FramePointer;assembler;
{$ifdef CPUI386}
asm
    movl %ebp,%eax
end ['EAX'];
{$endif CPUI386}
{$ifdef CPU68K}
asm
    move.l a6, d0
end['D0'];
{$endif CPU68K}


function PreviousFramePointer: FramePointer;assembler;
{$ifdef CPUI386}
asm
    movl (%ebp),%eax
end ['EAX'];
{$endif CPUI386}
{$ifdef CPU68K}
asm
    move.l (a6), d0
end['D0'];
{$endif CPU68K}

{$endif PPC_FPC}


{$ifdef PPC_BP}
type
  VoidConstructor = function(VmtOfs: Word; Obj: pointer): pointer;
  PointerConstructor = function(Param1: pointer; VmtOfs: Word; Obj: pointer): pointer;
  VoidMethod = function(Obj: pointer): pointer;
  PointerMethod = function(Param1: pointer; Obj: pointer): pointer;

function CallVoidConstructor(Ctor: pointer; Obj: pointer; VMT: pointer): pointer;
begin
  CallVoidConstructor := VoidConstructor(Ctor)(Ofs(VMT^), Obj)
end;


function CallPointerConstructor(Ctor: pointer; Obj: pointer; VMT: pointer; Param1: pointer): pointer;
begin
  CallPointerConstructor := PointerConstructor(Ctor)(Param1, Ofs(VMT^), Obj)
end;


function CallVoidMethod(Method: pointer; Obj: pointer): pointer;
begin
  CallVoidMethod := VoidMethod(Method)(Obj)
end;


function CallPointerMethod(Method: pointer; Obj: pointer; Param1: pointer): pointer;
begin
  CallPointerMethod := PointerMethod(Method)(Param1, Obj)
end;


function CallVoidLocal(Func: pointer; Frame: FramePointer): pointer; assembler;
asm
{$IFDEF Windows}
        MOV     AX,[Frame]
        AND     AL,0FEH
        PUSH    AX
{$ELSE}
        push    [Frame]
{$ENDIF}
        call    dword ptr Func
end;


function CallPointerLocal(Func: pointer; Frame: FramePointer; Param1: pointer): pointer; assembler;
asm
        mov     ax, word ptr Param1
        mov     dx, word ptr Param1+2
        push    dx
        push    ax
{$IFDEF Windows}
        MOV     AX,[Frame]
        AND     AL,0FEH
        PUSH    AX
{$ELSE}
        push    [Frame]
{$ENDIF}
        call    dword ptr Func
end;


function CallVoidMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer): pointer; assembler;
asm
{$IFDEF Windows}
        MOV     AX,[Frame]
        AND     AL,0FEH
        PUSH    AX
{$ELSE}
        push    [Frame]
{$ENDIF}
        call    dword ptr Func
end;


function CallPointerMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer; Param1: pointer): pointer; assembler;
asm
        mov     ax, word ptr Param1
        mov     dx, word ptr Param1+2
        push    dx
        push    ax
{$IFDEF Windows}
        MOV     AX,[Frame]
        AND     AL,0FEH
        PUSH    AX
{$ELSE}
        push    [Frame]
{$ENDIF}
        call    dword ptr Func
end;


function CurrentFramePointer: FramePointer; assembler;
asm
        mov     ax, bp
end;


function PreviousFramePointer: FramePointer; assembler;
asm
        mov     ax, ss:[bp]
end;

{$endif PPC_BP}


end.
{
  $Log$
  Revision 1.3  2004-02-06 20:08:58  jonas
    * version from FV

  Revision 1.4  2003/11/12 15:49:59  peter
    * fix crash with 1.9

  Revision 1.3  2001/07/30 08:27:58  pierre
   * fix I386 compilation problem

  Revision 1.2  2001/07/29 20:23:18  pierre
   * support for m68k cpu

  Revision 1.1  2001/01/29 21:56:04  peter
    * updates for new fpcmake

  Revision 1.1  2001/01/29 11:31:26  marco
   * added from API. callspec renamed to .pp

  Revision 1.1  2000/07/13 06:29:38  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:30  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.2  1998/12/16 21:57:16  peter
    * fixed currentframe,previousframe
    + testcall to test the callspec unit

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.5  1998/12/04 09:53:44  peter
    * removed objtemp global var

  Revision 1.4  1998/11/24 17:14:24  peter
    * fixed esi loading


  Date       Version  Who     Comments
  ---------- -------- ------- -------------------------------------
  19-Sep-97  0.1      mkoeppe Initial version.
  22-Sep-97  0.11     fk      0.9.3 support added, self isn't expected
                              on the stack in local procedures of methods
  23-Sep-97  0.12     mkoeppe Cleaned up 0.9.3 conditionals.
  03-Oct-97  0.13     mkoeppe Fixed esi load in FPC 0.9
  22-Oct-98  0.14     pfv     0.99.8 support for FPC
}
