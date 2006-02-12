Unit FreeBSD;
{
   This file is part of the Free Pascal run time library.
   (c) 2005 by Marco van de Voort
   member of the Free Pascal development team.
   based on the sendfile conversion of Ales Katona 30.01.2006

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Unit for FreeBSD specific calls. Calls may move to "BSD" unit in time,
   if turns out that more BSDs include them. 
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$inline on}
  {$Macro On}
  {$ifdef FPC_USE_LIBC}
     {$define directives:=cdecl; external 'c';}
  {$else}
     {$define directives:=inline;}
  {$endif}
{$ENDIF}
              
interface

uses
  BaseUnix,Unix;

const
  SF_NODISKIO = $00000001;  // don't wait for disk IO, similar to non-blocking socket setting

Type  
  SF_HDTR = record
    headers: PIOVec;   //* pointer to an array of header struct iovec's */
    hdr_cnt: cint;         //* number of header iovec's */
    trailers: PIOVec;     //* pointer to an array of trailer struct iovec's */
    trl_cnt: cint;           //* number of trailer iovec's */
  end;
  TSF_HDTR = SF_HDTR; 
  PSF_HDTR = ^TSF_HDTR;

function sendfile(fd: cint; s: cint; Offset: TOff; nBytes: TSize;
                    HDTR: PSF_HDTR; sBytes: POff; Flags: cint): cint; directives

implementation

Uses
{$ifndef FPC_USE_LIBC}  SysCall; {$else} InitC; {$endif}

{$IFNDEF FPC_USE_LIBC}  
function SendFile(fd: cint; s: cint; Offset: TOff; nBytes: TSize;
                  HDTR: PSF_HDTR; sBytes: POff; Flags: cint): cint;
begin

  SendFile:=Do_Syscall(syscall_nr_sendfile, fd, s, 
 {$IFNDEF CPU64} 
   {$IFDEF LITTLE_ENDIAN} // little endian is lo - hi
      Lo(Offset), Hi(Offset), 
   {$ELSE}  	          // big endian is hi - lo
      Hi(Offset), Lo(Offset), 
   {$ENDIF}
 {$ELSE}  // 64-bit doesn't care. 
    TSysParam(Offset),
 {$ENDIF}
    nBytes, TSysParam(HDTR), TSysParam(sBytes), Flags);
end;
{$ENDIF}

end.
