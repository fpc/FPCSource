{ CoreGraphics - CGFunction.h
 * Copyright (c) 1999-2002 Apple Computer, Inc. (unpublished)
 * All rights reserved.
 }
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CGFunction;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

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
uses MacTypes,CGBase,CFBase;
{$ALIGN POWER}


{! @header CGFunction
 *   A general floating-point function evaluator, using a callback mapping
 *   an arbitrary number of float inputs to an arbitrary number of float
 *   outputs.
 }

type
	CGFunctionRef = ^SInt32; { an opaque 32-bit type }


{! @typedef CGFunctionEvaluateCallback
 *   This callback evaluates a function, using <tt>in</tt> as inputs, and
 *   places the result in <tt>out</tt>.
 *
 * @param info
 *   The info parameter passed to CGFunctionCreate.
 *
 * @param inData
 *   An array of <tt>domainDimension</tt> floats.
 *
 * @param outData
 *   An array of <tt>rangeDimension</tt> floats.
 }

type
	CGFunctionEvaluateCallback = procedure( info: UnivPtr; inp: {const} Float32Ptr; out: Float32Ptr );

{! @typedef CGFunctionReleaseInfoCallback
 *   This callback releases the info parameter passed to the CGFunction
 *   creation functions when the function is deallocated.
 *
 * @param info
 *   The info parameter passed to CGFunctionCreate.
 }

type
	CGFunctionReleaseInfoCallback = procedure( info: UnivPtr );

{! @typedef CGFunctionCallbacks
 *   Structure containing the callbacks of a CGFunction.
 *
 * @field version
 *   The version number of the structure passed to the CGFunction creation
 *   functions. This structure is version 0.
 *
 * @field evaluate
 *   The callback used to evaluate the function.
 *
 * @field releaseInfo
 *   If non-NULL, the callback used to release the info parameter passed to
 *   the CGFunction creation functions when the function is deallocated.
 }

type
	CGFunctionCallbacks = record
		version: UInt32;
		evaluate: CGFunctionEvaluateCallback;
		releaseInfo: CGFunctionReleaseInfoCallback;
	end;


{! @function CGFunctionGetTypeID
 *   Return the CFTypeID for CGFunctionRefs.
 }

function CGFunctionGetTypeID: CFTypeID; external name '_CGFunctionGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGFunctionCreate
 *   Create a function.
 *
 * @param info
 *   The parameter passed to the callback functions.
 *
 * @param domainDimension
 *   The number of inputs to the function.
 *
 * @param domain
 *   An array of <tt>2*domainDimension</tt> floats used to specify the
 *   valid intervals of input values.  For each <tt>k</tt> from <tt>0</tt>
 *   to <tt>domainDimension - 1</tt>, <tt>domain[2*k]</tt> must be less
 *   than or equal to <tt>domain[2*k+1]</tt>, and the <tt>k</tt>'th input
 *   value <tt>in[k]</tt> will be clipped to lie in the interval
 *   <tt>domain[2*k] <= in[k] <= domain[2*k+1]</tt>.  If this parameter is
 *   NULL, then the input values are not clipped.  However, it's strongly
 *   recommended that this parameter be specified; each domain interval
 *   should specify reasonable values for the minimum and maximum in each
 *   dimension.
 *
 * @param rangeDimension
 *   The number of outputs from the function.
 *
 * @param range
 *   An array of <tt>2*rangeDimension</tt> floats used to specify the valid
 *   intervals of output values.  For each <tt>k</tt> from <tt>0</tt> to
 *   <tt>rangeDimension - 1</tt>, <tt>range[2*k]</tt> must be less than or
 *   equal to <tt>range[2*k+1]</tt>, and the <tt>k</tt>'th output value
 *   <tt>out[k]</tt> will be clipped to lie in the interval <tt>range[2*k]
 *   <= out[k] <= range[2*k+1]</tt>.  If this parameter is NULL, then the
 *   output values are not clipped.  However, it's strongly recommended
 *   that this parameter be specified; each range interval should specify
 *   reasonable values for the minimum and maximum in each dimension.
 *
 * @param callbacks
 *   A pointer to a CGFunctionCallbacks structure.  The function uses these
 *   callbacks to evaluate values.  The contents of the callbacks structure
 *   is copied, so, for example, a pointer to a structure on the stack can
 *   be passed in.  }

function CGFunctionCreate( info: UnivPtr; domainDimension: size_t; domain: {const} Float32Ptr; rangeDimension: size_t; range: {const} Float32Ptr; const (*var*) callbacks: CGFunctionCallbacks ): CGFunctionRef; external name '_CGFunctionCreate'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGFunctionRetain
 *
 * Equivalent to <tt>CFRetain(function)</tt>.
 }

function CGFunctionRetain( func: CGFunctionRef ): CGFunctionRef; external name '_CGFunctionRetain'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{! @function CGFunctionRelease
 *
 * Equivalent to <tt>CFRelease(function)</tt>.
 }

procedure CGFunctionRelease( func: CGFunctionRef ); external name '_CGFunctionRelease'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


end.
