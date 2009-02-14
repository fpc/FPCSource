{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2008 by the Free Pascal development team

    This file implements dyn. lib calls calls for Unix

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dl;

interface

const
{$ifdef BSD}   // dlopen is in libc on FreeBSD.
  LibDL = 'c';
{$else}
  {$ifdef HAIKU}
    LibDL = 'root';
  {$else}
    LibDL = 'dl';
  {$endif}
{$endif}

{$if defined(linux) and defined(cpuarm)}
{ arm-linux seems to require this }
{$linklib c}
{$endif}

  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;
  RTLD_GLOBAL       = $100;
  RTLD_NEXT         = pointer(-1);
{$ifdef LINUX}
  RTLD_DEFAULT      = nil;
{$endif}
{$ifdef BSD}
  RTLD_DEFAULT      = pointer(-2);
  RTLD_MODEMASK     = RTLD_BINDING_MASK;
{$endif}

type
  Pdl_info = ^dl_info;
  dl_info = record
    dli_fname      : Pchar;
    dli_fbase      : pointer;
    dli_sname      : Pchar;
    dli_saddr      : pointer;
  end;

function dlopen(Name : PChar; Flags : longint) : Pointer; cdecl; external libdl;
function dlsym(Lib : Pointer; Name : Pchar) : Pointer; cdecl; external Libdl;
function dlclose(Lib : Pointer) : Longint; cdecl; external libdl;
function dlerror() : Pchar; cdecl; external libdl;
{ overloaded for compatibility with hmodule }
function dlsym(Lib : PtrInt; Name : Pchar) : Pointer; cdecl; external Libdl;
function dlclose(Lib : PtrInt) : Longint; cdecl; external libdl;
function dladdr(Lib: pointer; info: Pdl_info): Longint; cdecl; external;

implementation

  function PosLastSlash(const s : string) : longint;
    var
      i : longint;
    begin 
      PosLastSlash:=0;
      for i:=1 to length(s) do
        if s[i]='/' then
          PosLastSlash:=i;
    end;
    
    
  function SimpleExtractFilename(const s : string) : string;
    begin
      SimpleExtractFilename:=Copy(s,PosLastSlash(s)+1,Length(s)-PosLastSlash(s));
    end;
      

  procedure UnixGetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: openstring);
    var
      dlinfo: dl_info;
    begin
      baseaddr:=nil;
      FillChar(dlinfo, sizeof(dlinfo), 0);
      dladdr(addr, @dlinfo);
      baseaddr:=dlinfo.dli_fbase;
      filename:=String(dlinfo.dli_fname);
      if SimpleExtractFilename(filename)=SimpleExtractFilename(ParamStr(0)) then
        baseaddr:=nil;
    end;

begin
  UnixGetModuleByAddrHook:=@UnixGetModuleByAddr;
end.
