{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
{$ifdef fpc_abi_call0}
   jmp_buf = record
     a0,a1,a8,a12,a13,a14,a15 : DWord;
   end;
{$endif fpc_abi_call0}
{$ifdef fpc_abi_windowed}
   jmp_buf = record
     data : array[0..16] of DWord;
   end;
{$endif fpc_abi_windowed}
   pjmp_buf = ^jmp_buf;

{$if defined(freertos) and defined(fpc_abi_windowed)}
function setjmp(var S : jmp_buf) : longint;external;
procedure longjmp(var S : jmp_buf;value : longint);external;
{$else defined(freertos) and defined(fpc_abi_windowed)}
function setjmp(var S : jmp_buf) : longint;[external name 'FPC_SETJMP'];
procedure longjmp(var S : jmp_buf;value : longint);[external name 'FPC_LONGJMP'];
{$endif defined(freertos) and defined(fpc_abi_windowed)}
