{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS-independent loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFDEF FPC}
{$MODE OBJFPC}
{$ENDIF}

unit dynlibs;

interface

{$i rtldefs.inc}

{ ---------------------------------------------------------------------
  Read OS-dependent interface declarations.
  ---------------------------------------------------------------------}

{ Note: should define the TOrdinalEntry type and define
        DYNLIBS_SUPPORTS_ORDINAL if the operating system supports loading
        functions by a ordinal like e.g. Windows or OS/2 do }

{$define readinterface}
{$i dynlibs.inc}
{$undef  readinterface}

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}
{$IFNDEF DYNLIBS_SUPPORTS_ORDINAL}
type
 TOrdinalEntry = SizeUInt;
{$ENDIF DYNLIBS_SUPPORTS_ORDINAL}

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
Function LoadLibrary(const Name : RawByteString) : TLibHandle;
Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
Function LoadLibrary(const Name : UnicodeString) : TLibHandle;

Function GetProcedureAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;
Function GetProcedureAddress(Lib : TLibHandle; Ordinal: TOrdinalEntry) : Pointer;
Function UnloadLibrary(Lib : TLibHandle) : Boolean;
Function GetLoadErrorStr: string;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean;
Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;

Type
  HModule = TLibHandle; 

Implementation

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}

{ Note: should define the TOrdinalEntry overload if the operating system
        supports loading functions by a ordinal like e.g. Windows or OS/2 do }
{$i dynlibs.inc}

{$ifndef FPCRTL_FILESYSTEM_TWO_BYTE_API}
Function DoSafeLoadLibrary(const Name : RawByteString) : TLibHandle;
{$else not FPCRTL_FILESYSTEM_TWO_BYTE_API}
Function DoSafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
{$endif not FPCRTL_FILESYSTEM_TWO_BYTE_API}
{$if defined(cpui386) or defined(cpux86_64)}
  var
    fpucw : Word;
    ssecw : DWord;
{$endif}
  begin
    try
{$if defined(cpui386) or defined(cpux86_64)}
      fpucw:=Get8087CW;
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        ssecw:=GetMXCSR;
{$endif}
      Result:=doloadlibrary(Name);
      finally
{$if defined(cpui386) or defined(cpux86_64)}
      Set8087CW(fpucw);
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        SetMXCSR(ssecw);
{$endif}
    end;
  end;

{$ifndef FPCRTL_FILESYSTEM_SINGLE_BYTE_API}
Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:=DoSafeLoadLibrary(UnicodeString(Name));
end;

Function LoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:=DoLoadLibrary(UnicodeString(Name));
end;

{$else not FPCRTL_FILESYSTEM_SINGLE_BYTE_API}

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:=DoSafeLoadLibrary(ToSingleByteFileSystemEncodedFileName(Name));
end;

Function LoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:=DoLoadLibrary(ToSingleByteFileSystemEncodedFileName(Name));
end;
{$endif not FPCRTL_FILESYSTEM_SINGLE_BYTE_API}


{$ifndef FPCRTL_FILESYSTEM_TWO_BYTE_API}
Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:=DoSafeLoadLibrary(ToSingleByteFileSystemEncodedFileName(Name));
end;

Function LoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:=DoLoadLibrary(ToSingleByteFileSystemEncodedFileName(Name));
end;

{$else not FPCRTL_FILESYSTEM_TWO_BYTE_API}

Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:=DoSafeLoadLibrary(Name);
end;

Function LoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:=DoLoadLibrary(Name);
end;
{$endif not FPCRTL_FILESYSTEM_TWO_BYTE_API}

{$ifndef DYNLIBS_SUPPORTS_ORDINAL}
{ OS does not support loading by ordinal (or it's not implemented yet), so by
  default we simply return Nil }
Function GetProcedureAddress(Lib : TLibHandle; Ordinal : TOrdinalEntry) : Pointer;
begin
  Result := Nil;
end;
{$endif not DYNLIBS_SUPPORTS_ORDINAL}

Function FreeLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=UnloadLibrary(lib);
end;

Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=GetProcedureAddress(Lib,Procname);
end;


end.
