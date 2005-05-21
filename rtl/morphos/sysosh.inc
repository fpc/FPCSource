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

{Platform specific information}
type
{$ifdef CPU64}
  THandle = Int64;
{$else CPU64}
  THandle = Longint;
{$endif CPU64}
  TThreadID = THandle;
  
  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = record
   Locked: boolean
  end;


{
   $Log: sysosh.inc,v $
   Revision 1.2  2005/04/13 20:10:50  florian
     + TThreadID

   Revision 1.1  2005/02/07 21:30:12  peter
     * system unit updated

   Revision 1.1  2005/02/06 16:57:18  peter
     * threads for go32v2,os,emx,netware

   Revision 1.1  2005/02/06 13:06:20  peter
     * moved file and dir functions to sysfile/sysdir
     * win32 thread in systemunit

}

