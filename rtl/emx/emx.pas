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

Unit emx;

Interface

{$Mode ObjFpc}

{16:16 far pointer}
type
  Far16Ptr=record
    Segment, Offset: Word;
  end;

{! Don't call this one. It is used by the startup code.}
//procedure emxinit; cdecl;
//  external 'emx' index 1;

{! Calling this is not recommended. Use ___syscall instead.}
//procedure emx_syscall; cdecl;
//  external 'emx' index 2;

{This one converts 16:16 far pointers to 32 bit flat ones.}
function emx_16to32(APtr: Far16Ptr): pointer; cdecl;
  external 'emx' index 3;

{This one converts 32 bit flat pointers to 16:16 far ones.}
function emx_32to16(APtr: pointer): Far16Ptr; cdecl;
  external 'emx' index 4;

{This one should be called to call 16-bit procedures and functions.}
function emx_thunk1(Args: Pointer; Fun: Pointer): cardinal; cdecl;
  external 'emx' index 5;

procedure emx_exception; cdecl;
  external 'emx' index 6;

// REXX function
//ULONG emx_revision (PCSZ name, LONG argc, const RXSTRING *argv,
//                    PCSZ queuename, PRXSTRING retstr)
procedure emx_revision; cdecl;
  external 'emx' index 128;

Implementation

End.
