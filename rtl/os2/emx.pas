{****************************************************************************

EMX - Interface unit for the EMX dynamic runtime library.

Part of Free Pascal runtime library for OS/2


History:
    2 June 1997 : Creation.

This unit is Copyright (c) 1999-2000 by Daniel Mantione.
Free Pascal is Copyright (c) -1999-2000 by Florian Klaempfl.
EMX.DLL is Copyright (c) -1999-2000 by Eberhard Mattes.

Modifying this unit is allowed, under the following conditions:

- You will not make anyone beleive that you or someone else wrote this.
- Unless you are developing on the official version of FPC, you will make a
  note in this file that it is not the original one.

****************************************************************************}

unit emx;

type    PFar=record
            Segment,Offset:word;
        end;

{! Don't call this one. It is used by the startup code.}
procedure __emxinit;
{! Calling this is not recommended. Use ___syscall instead.}
procedure __emx_syscall;
{This one converts 16:16 far pointers to 32 bit flat ones.}
procedure __emx_16to32(APtr:PFar):pointer;
{This one converts 32 bit flat pointers to 16:16 far ones.}
procedure __emx_32to16(APtr:pointer):PFar;
{This one should be called to call 16-bit procedures and functions.}
procedure __emx_thunk1(APtr:pointer)

