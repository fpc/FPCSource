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
     {$define extdecl:=cdecl; external 'c'}
  {$else}
     {$define extdecl:=inline}
  {$endif}
{$ENDIF}
              
interface

uses
  BaseUnix,Unix;

const
  SF_NODISKIO = $00000001;  // don't wait for disk IO, similar to non-blocking socket setting

Type  
  SF_HDTR = record
    headers: PIOVec;        {* pointer to an array of header struct iovec's *}
    hdr_cnt: cint;          {* number of header iovec's *}
    trailers: PIOVec;       {* pointer to an array of trailer struct iovec's *}
    trl_cnt: cint;          {* number of trailer iovec's *}
  end;
  TSF_HDTR = SF_HDTR;
  PSF_HDTR = ^TSF_HDTR;
  
  kld_file_stat = record
    Version: cInt;            {* set to sizeof(linker_file_stat) *}
    Name: array[0..MAXPATHLEN-1] of Char;
    Refs: cInt;
    ID: cInt;
    Address: pChar;           {* load address *}
    Size: size_t;             {* size in bytes *}
  end;
  tkld_file_stat = kld_file_stat;
  pkld_file_stat = ^kld_file_stat;
  TKldFileStat = kld_file_stat;
  PKldFileStat = ^kld_file_stat;
  
  kld_sym_lookup = record
    Version: cInt;            {* sizeof(struct kld_sym_lookup) *}
    SymName: pChar;           {* Symbol name we are looking up *}
    SymValue: culong;
    SymSize: size_t;
  end;
  tkld_sym_lookup = kld_sym_lookup;
  pkld_sym_lookup = ^kld_sym_lookup;
  TKldSymLookup = kld_sym_lookup;
  PKldSymLookup = ^kld_sym_lookup;

  function sendfile(fd: cint; s: cint; Offset: TOff; nBytes: TSize;
                      HDTR: PSF_HDTR; sBytes: POff; Flags: cint): cint; extdecl;
                    
  function kldload(FileName: pChar): cInt; extdecl;

  function kldunload(fileid: cInt): cInt; extdecl;

  function kldfind(FileName: pChar): cInt; extdecl;

  function kldnext(fileid: cInt): cInt; extdecl;

  function kldstat(fileid: cInt; kld_file_stat: pKldFileStat): cInt; extdecl;

  function kldfirstmod(fileid: cInt): cInt; extdecl;

  function kldsym(fileid: cInt; command: cInt; data: PKldSymLookup): cInt; extdecl;

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

function kldload(FileName: pChar): cInt;
begin
  kldload:=do_sysCall(syscall_nr_kldload, TSysParam(FileName));
end;

function kldunload(fileid: cInt): cInt;
begin
  kldunload:=do_sysCall(syscall_nr_kldunload, TSysParam(fileid));
end;

function kldfind(FileName: pChar): cInt;
begin
  kldfind:=do_sysCall(syscall_nr_kldfind, TSysParam(FileName));
end;

function kldnext(fileid: cInt): cInt;
begin
  kldnext:=do_sysCall(syscall_nr_kldnext, TSysParam(fileid));
end;

function kldstat(fileid: cInt; kld_file_stat: pKldFileStat): cInt;
begin
  kldstat:=do_sysCall(syscall_nr_kldstat, TSysParam(fileid),
                                          TSysParam(kld_file_stat));
end;

function kldfirstmod(fileid: cInt): cInt;
begin
  kldfirstmod:=do_sysCall(syscall_nr_kldfirstmod, TSysParam(fileid));
end;

function kldsym(fileid: cInt; command: cInt; data: PKldSymLookup): cInt;
begin
  kldsym:=do_sysCall(syscall_nr_kldsym, TSysParam(fileid), TSysParam(command),
                     TSysParam(data));
end;

{$ENDIF}

end.
