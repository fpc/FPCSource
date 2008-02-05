{	CFByteOrder.h
	Copyright (c) 1995-2005, Apple, Inc. All rights reserved.
}
{   Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
{   Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, April 2006, February 2008 }
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

unit CFByteOrders;
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
uses MacTypes,CFBase;
{$ALIGN POWER}


type
	CFByteOrder = SInt32;
const
	CFByteOrderUnknown = 0;
	CFByteOrderLittleEndian = 1;
	CFByteOrderBigEndian = 2;

function CFByteOrderGetCurrent: CFByteOrder; inline;
function CFSwapInt16( arg: UInt16 ): UInt16; inline;
function CFSwapInt32( arg: UInt32 ): UInt32; inline;
function CFSwapInt64( arg: UInt64 ): UInt64; inline;

function CFSwapInt16BigToHost( arg: UInt16 ): UInt16; inline;
function CFSwapInt32BigToHost( arg: UInt32 ): UInt32; inline;
function CFSwapInt64BigToHost( arg: UInt64 ): UInt64; inline;
function CFSwapInt16HostToBig( arg: UInt16 ): UInt16; inline;
function CFSwapInt32HostToBig( arg: UInt32 ): UInt32; inline;
function CFSwapInt64HostToBig( arg: UInt64 ): UInt64; inline;

{$ifc TARGET_RT_BIG_ENDIAN}


{$elsec}



{$endc}

function CFSwapInt16LittleToHost( arg: UInt16 ): UInt16; inline;
function CFSwapInt32LittleToHost( arg: UInt32 ): UInt32; inline;
function CFSwapInt64LittleToHost( arg: UInt64 ): UInt64; inline;
function CFSwapInt16HostToLittle( arg: UInt16 ): UInt16; inline;
function CFSwapInt32HostToLittle( arg: UInt32 ): UInt32; inline;
function CFSwapInt64HostToLittle( arg: UInt64 ): UInt64; inline;

{$ifc TARGET_RT_LITTLE_ENDIAN}


{$elsec}



{$endc}

type
	CFSwappedFloat32 = record
		v: UInt32;
	end;
type
	CFSwappedFloat64 = record
		v: UInt64;
	end;

function CFConvertFloat32HostToSwapped( arg: Float32 ): CFSwappedFloat32; inline;
function CFConvertFloat32SwappedToHost( arg: CFSwappedFloat32 ): Float32; inline;
function CFConvertFloat64HostToSwapped( arg: Float64 ): CFSwappedFloat64; inline;
function CFConvertFloat64SwappedToHost( arg: CFSwappedFloat64 ): Float64; inline;
function CFConvertFloatHostToSwapped( arg: Float32 ): CFSwappedFloat32; inline;
function CFConvertFloatSwappedToHost( arg: CFSwappedFloat32 ): Float32; inline;
function CFConvertDoubleHostToSwapped( arg: Float64 ): CFSwappedFloat64; inline;
function CFConvertDoubleSwappedToHost( arg: CFSwappedFloat64 ): Float64; inline;

{$ifc TARGET_RT_LITTLE_ENDIAN}



{$elsec}


{$endc}

implementation

{$R-}

function CFByteOrderGetCurrent: CFByteOrder; inline;
	var
		x: UInt32 = (CFByteOrderBigEndian shl 24) or CFByteOrderLittleEndian;
begin
	CFByteOrderGetCurrent := CFByteOrder(UInt8Ptr(@x)^);
end;

function CFSwapInt16( arg: UInt16 ): UInt16; inline;
begin
	CFSwapInt16 := (( arg shl 8) and $0FF00) or (( arg shr 8) and $00FF);
end;

function CFSwapInt32( arg: UInt32 ): UInt32; inline;
begin
    CFSwapInt32 := ((arg and $FF) shl 24) or ((arg and $0FF00) shl 8) or ((arg shr 8) and $0FF00) or ((arg shr 24) and $FF);
end;

function CFSwapInt64( arg: UInt64 ): UInt64; inline;
begin
	CFSwapInt64 := (UInt64(CFSwapInt32( arg and $FFFFFFFF )) shl 32) or CFSwapInt32( (arg shr 32) and $FFFFFFFF );
end;

{$ifc TARGET_RT_BIG_ENDIAN}
function CFSwapInt16BigToHost( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16BigToHost := arg;
end;

function CFSwapInt32BigToHost( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32BigToHost := arg;
end;

function CFSwapInt64BigToHost( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64BigToHost := arg;
end;

function CFSwapInt16HostToBig( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16HostToBig := arg;
end;

function CFSwapInt32HostToBig( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32HostToBig := arg;
end;

function CFSwapInt64HostToBig( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64HostToBig := arg;
end;

function CFSwapInt16LittleToHost( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16LittleToHost := CFSwapInt16(arg);
end;

function CFSwapInt32LittleToHost( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32LittleToHost := CFSwapInt32(arg);
end;

function CFSwapInt64LittleToHost( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64LittleToHost := CFSwapInt64(arg);
end;

function CFSwapInt16HostToLittle( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16HostToLittle := CFSwapInt16(arg);
end;

function CFSwapInt32HostToLittle( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32HostToLittle := CFSwapInt32(arg);
end;

function CFSwapInt64HostToLittle( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64HostToLittle := CFSwapInt64(arg);
end;

function CFConvertFloat32HostToSwapped( arg: Float32 ): CFSwappedFloat32; inline;
begin
  CFConvertFloat32HostToSwapped := CFSwappedFloat32(arg);
end;

function CFConvertFloat32SwappedToHost( arg: CFSwappedFloat32 ): Float32; inline;
begin
  CFConvertFloat32SwappedToHost := Float32(arg);
end;

function CFConvertFloat64HostToSwapped( arg: Float64 ): CFSwappedFloat64; inline;
begin
  CFConvertFloat64HostToSwapped := CFSwappedFloat64(arg);
end;

function CFConvertFloat64SwappedToHost( arg: CFSwappedFloat64 ): Float64; inline;
begin
  CFConvertFloat64SwappedToHost := Float64(arg);
end;

function CFConvertFloatHostToSwapped( arg: Float32 ): CFSwappedFloat32; inline;
begin
  CFConvertFloatHostToSwapped := CFSwappedFloat32(arg);
end;

function CFConvertFloatSwappedToHost( arg: CFSwappedFloat32 ): Float32; inline;
begin
  CFConvertFloatSwappedToHost := Float32(arg);
end;

function CFConvertDoubleHostToSwapped( arg: Float64): CFSwappedFloat64; inline;
begin
  CFConvertDoubleHostToSwapped := CFSwappedFloat64(arg);
end;

function CFConvertDoubleSwappedToHost( arg: CFSwappedFloat64 ): Float64; inline;
begin
  CFConvertDoubleSwappedToHost := Float64(arg);
end;

{$elsec}

function CFSwapInt16LittleToHost( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16LittleToHost := arg;
end;

function CFSwapInt32LittleToHost( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32LittleToHost := arg;
end;

function CFSwapInt64LittleToHost( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64LittleToHost := arg;
end;

function CFSwapInt16HostToLittle( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16HostToLittle := arg;
end;

function CFSwapInt32HostToLittle( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32HostToLittle := arg;
end;

function CFSwapInt64HostToLittle( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64HostToLittle := arg;
end;

function CFSwapInt16BigToHost( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16BigToHost := CFSwapInt16(arg);
end;

function CFSwapInt32BigToHost( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32BigToHost := CFSwapInt32(arg);
end;

function CFSwapInt64BigToHost( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64BigToHost := CFSwapInt64(arg);
end;

function CFSwapInt16HostToBig( arg: UInt16 ): UInt16; inline;
begin
  CFSwapInt16HostToBig := CFSwapInt16(arg);
end;

function CFSwapInt32HostToBig( arg: UInt32 ): UInt32; inline;
begin
  CFSwapInt32HostToBig := CFSwapInt32(arg);
end;

function CFSwapInt64HostToBig( arg: UInt64 ): UInt64; inline;
begin
  CFSwapInt64HostToBig := CFSwapInt64(arg);
end;

function CFConvertFloat32HostToSwapped( arg: Float32 ): CFSwappedFloat32; inline;
begin
  CFConvertFloat32HostToSwapped.v := CFSwapInt32(CFSwappedFloat32(arg).v);
end;

function CFConvertFloat32SwappedToHost( arg: CFSwappedFloat32 ): Float32; inline;
begin
  CFConvertFloat32SwappedToHost := Float32(CFSwappedFloat32(CFSwapInt32(arg.v)));
end;

function CFConvertFloat64HostToSwapped( arg: Float64 ): CFSwappedFloat64; inline;
begin
  CFConvertFloat64HostToSwapped.v := CFSwapInt64(CFSwappedFloat64(arg).v);
end;

function CFConvertFloat64SwappedToHost( arg: CFSwappedFloat64 ): Float64; inline;
begin
  CFConvertFloat64SwappedToHost := Float64(CFSwappedFloat64(CFSwapInt64(arg.v)));
end;

function CFConvertFloatHostToSwapped( arg: Float32 ): CFSwappedFloat32; inline;
begin
  CFConvertFloatHostToSwapped.v := CFSwapInt32(CFSwappedFloat32(arg).v);
end;

function CFConvertFloatSwappedToHost( arg: CFSwappedFloat32 ): Float32; inline;
begin
  CFConvertFloatSwappedToHost := Float32(CFSwappedFloat32(CFSwapInt32(arg.v)));
end;

function CFConvertDoubleHostToSwapped( arg: Float64 ): CFSwappedFloat64; inline;
begin
  CFConvertDoubleHostToSwapped.v := CFSwapInt64(CFSwappedFloat64(arg).v);
end;

function CFConvertDoubleSwappedToHost( arg: CFSwappedFloat64 ): Float64; inline;
begin
  CFConvertDoubleSwappedToHost := Float64(CFSwappedFloat64(CFSwapInt64(arg.v)));
end;
{$endc}


end.
