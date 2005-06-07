{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ some declarations for Netware API calls }
{ I nwlibc.inc}
{$I errno.inc}
{$define INCLUDED_FROM_SYSTEM}
{$I libc.pp}

var
  {$ifdef autoHeapRelease}
  HeapListAllocResourceTag,
  {$endif}
  HeapAllocResourceTag : rtag_t;
  NLMHandle : pointer;
  ReleaseThreadVars : TSysReleaseThreadVars = nil;
  AllocateThreadVars: TSysReleaseThreadVars = nil;
  SetThreadDataAreaPtr : TSysSetThreadDataAreaPtr = nil;
  TerminatingThreadID : dword = 0;

procedure NWSysSetThreadFunctions (atv:TSysReleaseThreadVars;
                                   rtv:TSysReleaseThreadVars;
                                   stdata:TSysSetThreadDataAreaPtr);
begin
  AllocateThreadVars := atv;
  ReleaseThreadVars := rtv;
  SetThreadDataAreaPtr := stdata;
end;


