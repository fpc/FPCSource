{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2015 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_SYSTEM_HAS_SYSDLH}
{$i sysdlh.inc}
{$else FPC_SYSTEM_HAS_SYSDLH}
{ These are fallback declarations }
type
  TLibHandle = THandle;
  HINST = TLibHandle;

const
  NilHandle = TLibHandle(0);
{$endif FPC_SYSTEM_HAS_SYSDLH}

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}
type
{$if not declared(TOrdinalEntry)}
  TOrdinalEntry = SizeUInt;
{$endif not declared(TOrdinalEntry)}

  TLoadLibraryUHandler = function(const Name: UnicodeString): TLibHandle;
  TLoadLibraryAHandler = function(const Name: RawByteString): TLibHandle;
  TGetProcAddressHandler = function(Lib: TLibHandle; const ProcName: AnsiString): {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
  TGetProcAddressOrdinalHandler = function(Lib: TLibHandle; Ordinal: TOrdinalEntry): {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
  TUnloadLibraryHandler = function(Lib: TLibHandle): Boolean;
  TGetLoadErrorStrHandler = function: AnsiString;

  TSymbolAddressInfo = record
    // Name of the symbol at or before the address, empty when unknown
    SymbolName: ShortString;
    // Start address of that symbol, nil when unknown
    SymbolAddr: CodePointer;
    // Last 255 characters of the path of the module containing the address
    ModuleName: ShortString;
    // Address in the module file as used by addr2line: the RVA on Windows, the address minus the load bias
    // on ELF and Mach-O; valid when ModuleName is not empty
    ModuleOffset: CodePtrUInt;
  end;
  TGetSymbolByAddrHandler = function(Addr: CodePointer; out Info: TSymbolAddressInfo): Boolean;

  TDynLibsManager = record
    LoadLibraryU: TLoadLibraryUHandler;
    LoadLibraryA: TLoadLibraryAHandler;
    GetProcAddress: TGetProcAddressHandler;
    GetProcAddressOrdinal: TGetProcAddressOrdinalHandler;
    UnloadLibrary: TUnloadLibraryHandler;
    GetLoadErrorStr: TGetLoadErrorStrHandler;
    GetSymbolByAddr: TGetSymbolByAddrHandler;
  end;

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
Function LoadLibrary(const Name : RawByteString) : TLibHandle;
Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
Function LoadLibrary(const Name : UnicodeString) : TLibHandle;

Function GetProcedureAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
Function GetProcedureAddress(Lib : TLibHandle; Ordinal: TOrdinalEntry) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif};
Function UnloadLibrary(Lib : TLibHandle) : Boolean;
Function GetLoadErrorStr: AnsiString;
// Finds the symbol at or before Addr and the module containing it; False when unknown or unsupported
Function GetSymbolByAddress(Addr: CodePointer; out Info: TSymbolAddressInfo): Boolean;

// Kylix/Delphi compatibility

Function FreeLibrary(Lib : TLibHandle) : Boolean; inline;
Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : {$ifdef cpui8086}FarPointer{$else}Pointer{$endif}; inline;

Procedure GetDynLibsManager (Out Manager : TDynLibsManager);
Procedure SetDynLibsManager (Const New : TDynLibsManager);
Procedure SetDynLibsManager (Const New : TDynLibsManager; out Old: TDynLibsManager);

// Delphi compatibility, needs to be expanded when packages arrive.
Type
  TEnumModuleFuncLW = function (HInstance: THandle; Data: Pointer): Boolean;

procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer);



