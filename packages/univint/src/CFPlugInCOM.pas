{	CFPlugInCOM.h
	Copyright (c) 1999-2005, Apple, Inc. All rights reserved.
}
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit CFPlugInCOM;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,CFPlugIn,CFUUID;
{$ALIGN POWER}


{ ================= IUnknown definition (C struct) ================= }

{ All interface structs must have an IUnknownStruct at the beginning. }
{ The _reserved field is part of the Microsoft COM binary standard on Macintosh. }
{ You can declare new C struct interfaces by defining a new struct that includes "IUNKNOWN_C_GUTS;" before the first field of the struct. }

type
	HRESULT = SInt32;
	ULONG = UInt32;
	LPVOID = UnivPtr;
	REFIID = CFUUIDBytes;

{ Macros for more detailed HRESULT analysis }

function SUCCEEDED( Status: HRESULT ): Boolean; inline;
function FAILED( Status: HRESULT ): Boolean; inline;
function IS_ERROR( Status: HRESULT ): Boolean; inline;
function HRESULT_CODE( hr: HRESULT ): HRESULT; inline;
function HRESULT_FACILITY( hr: HRESULT ): HRESULT; inline;
function HRESULT_SEVERITY( hr: HRESULT ): HRESULT; inline;

const
	SEVERITY_SUCCESS = 0;
const
	SEVERITY_ERROR = 1;

{ Creating an HRESULT from its component pieces }
function MAKE_HRESULT( sev, fac, code: ULONG ): HRESULT; inline;

{ Pre-defined success HRESULTS }
const
	S_OK = 0;
const
	S_FALSE = 1;

{ Common error HRESULTS }
const
	E_UNEXPECTED = $8000FFFF;
	E_NOTIMPL = $80000001;
	E_OUTOFMEMORY = $80000002;
	E_INVALIDARG = $80000003;
	E_NOINTERFACE = $80000004;
	E_POINTER = $80000005;
	E_HANDLE = $80000006;
	E_ABORT = $80000007;
	E_FAIL = $80000008;
	E_ACCESSDENIED = $80000009;

{ This macro should be used when defining all interface functions (as it is for the IUnknown functions below). }
{#define STDMETHODCALLTYPE}

{ The __RPC_FAR macro is for COM source compatibility only. This macro is used a lot in COM interface definitions.  If your CFPlugIn interfaces need to be COM interfaces as well, you can use this macro to get better source compatibility.  It is not used in the IUnknown definition below, because when doing COM, you will be using the Microsoft supplied IUnknown interface anyway. }
{#define __RPC_FAR}

{ The IUnknown interface }
function IUnknownUUID: CFUUIDRef; inline;

type
	IUnknownVTbl = record
		_reserved: UnivPtr;
		QueryInterface: function( thisPointer: UnivPtr; iid: REFIID; var ppv: LPVOID ): HRESULT;
		AddRef: function( thisPointer: UnivPtr ): ULONG;
		Release: function( thisPointer: UnivPtr ): ULONG;
	end;

{ End of extern "C" stuff }


{ C++ specific stuff }

implementation


{$R-}

function SUCCEEDED( Status: HRESULT ): Boolean; inline;
begin
	SUCCEEDED := Status >= 0;
end;

function FAILED( Status: HRESULT ): Boolean; inline;
begin
	FAILED := Status < 0;
end;

function IS_ERROR( Status: HRESULT ): Boolean; inline;
begin
	IS_ERROR := Status shr 31 = SEVERITY_ERROR;
end;

function HRESULT_CODE( hr: HRESULT ): HRESULT; inline;
begin
	HRESULT_CODE := hr and $FFFF;
end;

function HRESULT_FACILITY( hr: HRESULT ): HRESULT; inline;
begin
	HRESULT_FACILITY := (hr shr 16) and $1FFF;
end;

function HRESULT_SEVERITY( hr: HRESULT ): HRESULT; inline;
begin
	HRESULT_SEVERITY := (hr shr 31) and $01;
end;

function MAKE_HRESULT( sev, fac, code: ULONG ): HRESULT; inline;
begin
	MAKE_HRESULT := HRESULT((sev shl 31) or (fac shl 16) or code);
end;

function IUnknownUUID: CFUUIDRef; inline;
begin
	IUnknownUUID:= CFUUIDGetConstantUUIDWithBytes( nil, $00, $00, $00, $00, $00, $00, $00, $00, ByteParameter($C0), $00, $00, $00, $00, $00, $00, $46 )
end;


end.
