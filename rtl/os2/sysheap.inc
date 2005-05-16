{
    $Id: sysheap.inc,v 1.1 2005/02/06 16:57:18 peter Exp $
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


{****************************************************************************

                    Heap management releated routines.

****************************************************************************}

{Get some memory.
 P          = Pointer to memory will be returned here.
 Size       = Number of bytes to get. The size is rounded up to a multiple
              of 4096. This is probably not the case on non-intel 386
              versions of OS/2.
 Flags      = One or more of the mfXXXX constants.}

function DosAllocMem(var P:pointer;Size,Flag:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 299;

function DosSetMem(P:pointer;Size,Flag:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 305;

function DosFreeMem (P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 304;

{$IFDEF DUMPGROW}
 {$DEFINE EXTDUMPGROW}
{$ENDIF DUMPGROW}

{$IFDEF EXTDUMPGROW}
var
  Int_HeapSize: cardinal;
{$ENDIF EXTDUMPGROW}

{function GetHeapSize: longint; assembler;
asm
  movl Int_HeapSize, %eax
end ['EAX'];
}


function SysOSAlloc (Size: PtrInt): pointer;
var
  P: pointer;
  RC: cardinal;
begin
{$IFDEF EXTDUMPGROW}
  if Int_HeapSize <> high (cardinal) then
{
  if Int_HeapSize = high (cardinal) then
   WriteLn ('Trying to allocate first heap of size ', Size)
  else
}
   WriteLn ('Trying to grow heap by ', Size, ' to ', Int_HeapSize);
{$ENDIF}

  if UseHighMem then
   RC := DosAllocMem (P, Size, $403)
  else
   RC := DosAllocMem (P, Size, 3);
  if RC = 0 then
   begin
{$IFDEF EXTDUMPGROW}
  if Int_HeapSize <> high (cardinal) then
    WriteLn ('DosAllocMem returned memory at ', cardinal (P));
{$ENDIF}
    RC := DosSetMem (P, Size, $410);
    if RC = 0 then
     begin
{$IFDEF EXTDUMPGROW}
      if Int_HeapSize <> high (cardinal) then
       WriteLn ('New heap at ', cardinal (P));
{$ENDIF EXTDUMPGROW}
      SysOSAlloc := P;
{$IFDEF EXTDUMPGROW}
      if Int_HeapSize = high (cardinal) then
       Int_HeapSize := Size
      else
       Inc (Int_HeapSize, Size);
{$ENDIF EXTDUMPGROW}
     end
    else
     begin
{$IFDEF EXTDUMPGROW}
      if Int_HeapSize <> high (cardinal) then
       begin
        WriteLn ('Error ', RC, ' in DosSetMem while trying to commit memory!');
{      if Int_HeapSize = high (cardinal) then
        WriteLn ('No allocated memory comitted yet!')
       else
}
        WriteLn ('Total allocated memory is ', Int_HeapSize);
       end;
{$ENDIF EXTDUMPGROW}
      RC := DosFreeMem (P);
      SysOSAlloc := nil;
     end;
   end
  else
   begin
    SysOSAlloc := nil;
{$IFDEF EXTDUMPGROW}
    if Int_HeapSize <> high (cardinal) then
     begin
      WriteLn ('Error ', RC, ' during additional memory allocation (DosAllocMem)!');
{     if Int_HeapSize = high (cardinal) then
       WriteLn ('No memory allocated yet!')
      else
}
      WriteLn ('Total allocated memory is ', Int_HeapSize);
     end;
{$ENDIF EXTDUMPGROW}
   end;
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree (P: pointer; Size: PtrInt);
var
  RC: cardinal;
begin
{$IFDEF EXTDUMPGROW}
  WriteLn ('Trying to free memory!');
  WriteLn ('Total allocated memory is ', Int_HeapSize);
  Dec (Int_HeapSize, Size);
{$ENDIF EXTDUMPGROW}
  RC := DosSetMem (P, Size, $20);
  if RC = 0 then
   begin
    RC := DosFreeMem (P);
{$IFDEF EXTDUMPGROW}
    if RC <> 0 then
     begin
      WriteLn ('Error ', RC, ' during memory deallocation (DosFreeMem)!');
      WriteLn ('Total allocated memory is ', Int_HeapSize);
     end;
{$ENDIF EXTDUMPGROW}
   end
{$IFDEF EXTDUMPGROW}
  else
   begin
    WriteLn ('Error ', RC, ' in DosSetMem while trying to decommit memory!');
    WriteLn ('Total allocated memory is ', Int_HeapSize);
   end;
{$ENDIF EXTDUMPGROW}
end;


{
   $Log: sysheap.inc,v $
   Revision 1.1  2005/02/06 16:57:18  peter
     * threads for go32v2,os,emx,netware

   Revision 1.1  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

}

