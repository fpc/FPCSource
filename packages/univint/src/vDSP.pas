{
     File:       vecLib/vDSP.h
 
     Contains:   AltiVec DSP Interfaces
 
     Version:    vecLib-268.0
 
     Copyright:  © 2000-2009 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit vDSP;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{  For documentation on vDSP, see _vDSP Library_ at
    http://developer.apple.com/documentation/Performance/index-date.html or
    search for "vDSP Library" at http://developer.apple.com.
}

{$ALIGN POWER}


{	These symbols describe the vecLib version associated with this header.

	vDSP_Version0 is a major version number.
	vDSP_Version1 is a minor version number.
}
const
	vDSP_Version0 = 268;
const
	vDSP_Version1 = 0;


type
	vDSP_Length = UNSIGNEDLONG;
{    vDSP_Length is for numbers of elements in arrays and indices of
    elements in arrays.

    (It is also used for the base-two logarithm of numbers of elements,
    although a much smaller type is suitable for that.)
    }
type
	vDSP_Stride = SIGNEDLONG;
{    vDSP_Stride is for differences of indices of elements (which of
    course includes strides).
    }

type
	DSPComplexPtr = ^DSPComplex;
	DSPComplex = record
		real: Float32;
		imag: Float32;
	end;
type
	DSPSplitComplexPtr = ^DSPSplitComplex;
	DSPSplitComplex = record
		realp: Float32Ptr;
		imagp: Float32Ptr;
	end;
type
	DSPDoubleComplexPtr = ^DSPDoubleComplex;
	DSPDoubleComplex = record
		real: Float64;
		imag: Float64;
	end;
type
	DSPDoubleSplitComplexPtr = ^DSPDoubleSplitComplex;
	DSPDoubleSplitComplex = record
		realp: Float64Ptr;
		imagp: Float64Ptr;
	end;
type
	FFTSetup = ^OpaqueFFTSetup; { an opaque type }
	OpaqueFFTSetup = record end;
	FFTSetupD = ^OpaqueFFTSetupD; { an opaque type }
	OpaqueFFTSetupD = record end;

type
	FFTDirection = SInt32;
	FFTRadix = SInt32;
const
	kFFTDirection_Forward = 1;
	kFFTDirection_Inverse = -1;

const
	kFFTRadix2 = 0;
	kFFTRadix3 = 1;
	kFFTRadix5 = 2;

const
	vDSP_HALF_WINDOW = 1;
	vDSP_HANN_DENORM = 0;
	vDSP_HANN_NORM = 2;


{  create_fftsetup and create_ffsetupD allocate memory and prepare constants
    used by single- and double-precision FFT routines, respectively.

    destroy_fftsetup and destroy_fftsetupD free the memory.
}
{
 *  vDSP_create_fftsetup()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
function vDSP_create_fftsetup( __vDSP_log2n: vDSP_Length; __vDSP_radix: FFTRadix ): FFTSetup; external name '_vDSP_create_fftsetup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_destroy_fftsetup()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_destroy_fftsetup( __vDSP_setup: FFTSetup ); external name '_vDSP_destroy_fftsetup';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_create_fftsetupD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function vDSP_create_fftsetupD( __vDSP_log2n: vDSP_Length; __vDSP_radix: FFTRadix ): FFTSetupD; external name '_vDSP_create_fftsetupD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_destroy_fftsetupD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_destroy_fftsetupD( __vDSP_setup: FFTSetupD ); external name '_vDSP_destroy_fftsetupD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  ctoz and ctozD convert a complex array to a complex-split array.
    ztoc and ztocD convert a complex-split array to a complex array.
}
{
 *  vDSP_ctoz()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_ctoz( {const} __vDSP_C: {variable-size-array} DSPComplexPtr; __vDSP_strideC: vDSP_Stride; __vDSP_Z: DSPSplitComplexPtr; __vDSP_strideZ: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_ctoz';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_ztoc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_ztoc( const __vDSP_Z: DSPSplitComplexPtr; __vDSP_strideZ: vDSP_Stride; __vDSP_C: {variable-size-array} DSPComplexPtr; __vDSP_strideC: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_ztoc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_ctozD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_ctozD( {const} __vDSP_C: {variable-size-array} DSPDoubleComplexPtr; __vDSP_strideC: vDSP_Stride; __vDSP_Z: DSPDoubleSplitComplexPtr; __vDSP_strideZ: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_ctozD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_ztocD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_ztocD( const __vDSP_Z: DSPDoubleSplitComplexPtr; __vDSP_strideZ: vDSP_Stride; __vDSP_C: {variable-size-array} DSPDoubleComplexPtr; __vDSP_strideC: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_ztocD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  In-place complex Discrete Fourier Transform routines.

    fft_zip Single-precision without temporary memory.
    fft_zipt    Single-precision with temporary memory.
    fft_zipD    Double-precision without temporary memory.
    fft_ziptD   Double-precision with temporary memory.
}
{
 *  vDSP_fft_zip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zip( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zip';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zipt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zipt( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zipt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zipD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zipD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zipD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft_ziptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_ziptD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_ziptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Out-of-place complex Discrete Fourier Transform routines.

    fft_zop Single-precision without temporary memory.
    fft_zopt    Single-precision with temporary memory.
    fft_zopD    Double-precision without temporary memory.
    fft_zoptD   Double-precision with temporary memory.
}
{
 *  vDSP_fft_zop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zop';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zopt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zopt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zopt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zopD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zopD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zopD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft_zoptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zoptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zoptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  In-place real-to-complex Discrete Fourier Transform routines.

    fft_zrip    Single-precision without temporary memory.
    fft_zript   Single-precision with temporary memory.
    fft_zripD   Double-precision without temporary memory.
    fft_zriptD  Double-precision with temporary memory.
}
{
 *  vDSP_fft_zrip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zrip( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zrip';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zript()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zript( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zript';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zripD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zripD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft_zripD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft_zriptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zriptD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_stride: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft_zriptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Out-of-place real-to-complex Discrete Fourier Transform routines.

    fft_zrop    Single-precision without temporary memory.
    fft_zropt   Single-precision with temporary memory.
    fft_zropD   Double-precision without temporary memory.
    fft_zroptD  Double-precision with temporary memory.
}
{
 *  vDSP_fft_zrop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zrop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zrop';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zropt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft_zropt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft_zropt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft_zropD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zropD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft_zropD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft_zroptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft_zroptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft_zroptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  In-place two-dimensional complex Discrete Fourier Transform routines.

    fft2d_zip   Single-precision without temporary memory.
    fft2d_zipt  Single-precision with temporary memory.
    fft2d_zipD  Double-precision without temporary memory.
    fft2d_ziptD Double-precision with temporary memory.
}
{
 *  vDSP_fft2d_zip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zip( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft2d_zip';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zipt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zipt( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft2d_zipt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zipD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zipD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft2d_zipD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft2d_ziptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_ziptD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft2d_ziptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Out-of-place two-dimensional complex Discrete Fourier Transform routines.

    fft2d_zop   Single-precision without temporary memory.
    fft2d_zopt  Single-precision with temporary memory.
    fft2d_zopD  Double-precision without temporary memory.
    fft2d_zoptD Double-precision with temporary memory.
}
{
 *  vDSP_fft2d_zop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStrideInRow: vDSP_Stride; __vDSP_signalStrideInCol: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResultInRow: vDSP_Stride; __vDSP_strideResultInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zop';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zopt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zopt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStrideInRow: vDSP_Stride; __vDSP_signalStrideInCol: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResultInRow: vDSP_Stride; __vDSP_strideResultInCol: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zopt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zopD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zopD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStrideInRow: vDSP_Stride; __vDSP_signalStrideInCol: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResultInRow: vDSP_Stride; __vDSP_strideResultInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zopD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft2d_zoptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zoptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStrideInRow: vDSP_Stride; __vDSP_signalStrideInCol: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResultInRow: vDSP_Stride; __vDSP_strideResultInCol: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zoptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  In-place two-dimensional real-to-complex Discrete Fourier Transform
    routines.

    fft2d_zrip      Single-precision without temporary memory.
    fft2d_zript     Single-precision with temporary memory.
    fft2d_zripD     Double-precision without temporary memory.
    fft2d_zriptD    Double-precision with temporary memory.
}
{
 *  vDSP_fft2d_zrip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zrip( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft2d_zrip';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zript()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zript( __vDSP_setup: FFTSetup; __vDSP_ioData: DSPSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_direction: FFTDirection ); external name '_vDSP_fft2d_zript';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zripD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zripD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zripD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft2d_zriptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zriptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_strideInRow: vDSP_Stride; __vDSP_strideInCol: vDSP_Stride; __vDSP_bufferTemp: DSPDoubleSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zriptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Out-of-place two-dimensional real-to-complex Discrete Fourier Transform
    routines.

    fft2d_zrop      Single-precision without temporary memory.
    fft2d_zropt     Single-precision with temporary memory.
    fft2d_zropD     Double-precision without temporary memory.
    fft2d_zroptD    Double-precision with temporary memory.
}
{
 *  vDSP_fft2d_zrop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zrop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStrideInRow: vDSP_Stride; __vDSP_signalStrideInCol: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResultInRow: vDSP_Stride; __vDSP_strideResultInCol: vDSP_Stride; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zrop';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zropt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_fft2d_zropt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStrideInRow: vDSP_Stride; __vDSP_signalStrideInCol: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResultInRow: vDSP_Stride; __vDSP_strideResultInCol: vDSP_Stride; __vDSP_bufferTemp: DSPSplitComplexPtr; __vDSP_log2nInCol: vDSP_Length; __vDSP_log2nInRow: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zropt';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  vDSP_fft2d_zropD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zropD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_Kr: vDSP_Stride; __vDSP_Kc: vDSP_Stride; __vDSP_ioData2: DSPDoubleSplitComplexPtr; __vDSP_Ir: vDSP_Stride; __vDSP_Ic: vDSP_Stride; __vDSP_log2nc: vDSP_Length; __vDSP_log2nr: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zropD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft2d_zroptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft2d_zroptD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_Kr: vDSP_Stride; __vDSP_Kc: vDSP_Stride; __vDSP_ioData2: DSPDoubleSplitComplexPtr; __vDSP_Ir: vDSP_Stride; __vDSP_Ic: vDSP_Stride; __vDSP_temp: DSPDoubleSplitComplexPtr; __vDSP_log2nc: vDSP_Length; __vDSP_log2nr: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft2d_zroptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  In-place multiple complex Discrete Fourier Transform routines.

    fftm_zip    Single-precision without temporary memory.
    fftm_zipt   Single-precision with temporary memory.
    fftm_zipD   Double-precision without temporary memory.
    fftm_ziptD  Double-precision with temporary memory.
}
{
 *  vDSP_fftm_zip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zip( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zip';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zipt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zipt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_temp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zipt';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zipD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zipD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zipD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_ziptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_ziptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_temp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_ziptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Out-of-place multiple complex Discrete Fourier Transform routines.

    fftm_zop    Single-precision without temporary memory.
    fftm_zopt   Single-precision with temporary memory.
    fftm_zopD   Double-precision without temporary memory.
    fftm_zoptD  Double-precision with temporary memory.
}
{
 *  vDSP_fftm_zop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zop';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zopt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zopt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_temp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zopt';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zopD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zopD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zopD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zoptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zoptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_temp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zoptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  In-place multiple real-to-complex Discrete Fourier Transform routines.

    fftm_zip    Single-precision without temporary memory.
    fftm_zipt   Single-precision with temporary memory.
    fftm_zipD   Double-precision without temporary memory.
    fftm_ziptD  Double-precision with temporary memory.
}
{
 *  vDSP_fftm_zrip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zrip( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zrip';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zript()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zript( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_temp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zript';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zripD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zripD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zripD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zriptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zriptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_temp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zriptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Out-of-place multiple real-to-complex Discrete Fourier Transform routines.

    fftm_zrop   Single-precision without temporary memory.
    fftm_zropt  Single-precision with temporary memory.
    fftm_zropD  Double-precision without temporary memory.
    fftm_zroptD Double-precision with temporary memory.
}
{
 *  vDSP_fftm_zrop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zrop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zrop';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zropt()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zropt( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_temp: DSPSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zropt';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zropD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zropD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zropD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fftm_zroptD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fftm_zroptD( __vDSP_setup: FFTSetupD; __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_fftStride: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_rfftStride: vDSP_Stride; __vDSP_temp: DSPDoubleSplitComplexPtr; __vDSP_log2n: vDSP_Length; __vDSP_numFFT: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fftm_zroptD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{  Non-power-of-two out-of-place complex Discrete Fourier Transform routines.

    fft3_zop    3*2**n elements, single-precision without temporary memory.
    fft3_zopD   3*2**n elements, double-precision without temporary memory.
    fft5_zop    5*2**n elements, single-precision without temporary memory.
    fft5_zopD   5*2**n elements, double-precision without temporary memory.
}
{
 *  vDSP_fft3_zop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft3_zop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft3_zop';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft5_zop()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft5_zop( __vDSP_setup: FFTSetup; __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_resultStride: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft5_zop';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft3_zopD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft3_zopD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_ioData2: DSPDoubleSplitComplexPtr; __vDSP_L: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft3_zopD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  vDSP_fft5_zopD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_fft5_zopD( __vDSP_setup: FFTSetupD; __vDSP_ioData: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_ioData2: DSPDoubleSplitComplexPtr; __vDSP_L: vDSP_Stride; __vDSP_log2n: vDSP_Length; __vDSP_flag: FFTDirection ); external name '_vDSP_fft5_zopD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Convolution (or correlation), single-precision.}
{
 *  vDSP_conv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_conv( {const} __vDSP_signal: {variable-size-array} Float32Ptr; __vDSP_signalStride: vDSP_Stride; {const} __vDSP_filter: {variable-size-array} Float32Ptr; __vDSP_strideFilter: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_lenResult: vDSP_Length; __vDSP_lenFilter: vDSP_Length ); external name '_vDSP_conv';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Convolution (or correlation), double-precision.}
{
 *  vDSP_convD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_convD( {const} __vDSP_signal: {variable-size-array} Float64Ptr; __vDSP_signalStride: vDSP_Stride; {const} __vDSP_filter: {variable-size-array} Float64Ptr; __vDSP_strideFilter: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_lenResult: vDSP_Length; __vDSP_lenFilter: vDSP_Length ); external name '_vDSP_convD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ 3*3 filter convolution, single-precision.}
{
 *  vDSP_f3x3()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_f3x3( __vDSP_signal: Float32Ptr; __vDSP_rows: vDSP_Length; __vDSP_cols: vDSP_Length; __vDSP_filter: Float32Ptr; __vDSP_result: Float32Ptr ); external name '_vDSP_f3x3';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ 3*3 filter convolution, double-precision.}
{
 *  vDSP_f3x3D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_f3x3D( __vDSP_signal: Float64Ptr; __vDSP_rows: vDSP_Length; __vDSP_cols: vDSP_Length; __vDSP_filter: Float64Ptr; __vDSP_result: Float64Ptr ); external name '_vDSP_f3x3D';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ 5*5 filter convolution, single-precision.}
{
 *  vDSP_f5x5()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_f5x5( __vDSP_signal: Float32Ptr; __vDSP_rows: vDSP_Length; __vDSP_cols: vDSP_Length; __vDSP_filter: Float32Ptr; __vDSP_result: Float32Ptr ); external name '_vDSP_f5x5';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ 5*5 filter convolution, double-precision.}
{
 *  vDSP_f5x5D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_f5x5D( __vDSP_signal: Float64Ptr; __vDSP_rows: vDSP_Length; __vDSP_cols: vDSP_Length; __vDSP_filter: Float64Ptr; __vDSP_result: Float64Ptr ); external name '_vDSP_f5x5D';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ General two-dimensional (image) convolution, single-precision.}
{
 *  vDSP_imgfir()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_imgfir( __vDSP_signal: Float32Ptr; __vDSP_numRow: vDSP_Length; __vDSP_numCol: vDSP_Length; __vDSP_filter: Float32Ptr; __vDSP_result: Float32Ptr; __vDSP_fnumRow: vDSP_Length; __vDSP_fnumCol: vDSP_Length ); external name '_vDSP_imgfir';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ General two-dimensional (image) convolution, double-precision.}
{
 *  vDSP_imgfirD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_imgfirD( __vDSP_signal: Float64Ptr; __vDSP_numRow: vDSP_Length; __vDSP_numCol: vDSP_Length; __vDSP_filter: Float64Ptr; __vDSP_result: Float64Ptr; __vDSP_fnumRow: vDSP_Length; __vDSP_fnumCol: vDSP_Length ); external name '_vDSP_imgfirD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Matrix transpose, single-precision.}
{
 *  vDSP_mtrans()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mtrans( __vDSP_a: Float32Ptr; __vDSP_aStride: vDSP_Stride; __vDSP_c: Float32Ptr; __vDSP_cStride: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_mtrans';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Matrix transpose, double-precision.}
{
 *  vDSP_mtransD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mtransD( __vDSP_a: Float64Ptr; __vDSP_aStride: vDSP_Stride; __vDSP_c: Float64Ptr; __vDSP_cStride: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_mtransD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Matrix multiply, single-precision.}
{
 *  vDSP_mmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mmul( __vDSP_a: Float32Ptr; __vDSP_aStride: vDSP_Stride; __vDSP_b: Float32Ptr; __vDSP_bStride: vDSP_Stride; __vDSP_c: Float32Ptr; __vDSP_cStride: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_mmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Matrix multiply, double-precision.}
{
 *  vDSP_mmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mmulD( __vDSP_a: Float64Ptr; __vDSP_aStride: vDSP_Stride; __vDSP_b: Float64Ptr; __vDSP_bStride: vDSP_Stride; __vDSP_c: Float64Ptr; __vDSP_cStride: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_mmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix multiply and add, single-precision.}
{
 *  vDSP_zmma()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmma( __vDSP_a: DSPSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_d: DSPSplitComplexPtr; __vDSP_l: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmma';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix multiply and add, double-precision.}
{
 *  vDSP_zmmaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmmaD( __vDSP_a: DSPDoubleSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPDoubleSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPDoubleSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_d: DSPDoubleSplitComplexPtr; __vDSP_l: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmmaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix multiply and subtract, single-precision.}
{
 *  vDSP_zmms()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmms( __vDSP_a: DSPSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_d: DSPSplitComplexPtr; __vDSP_l: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmms';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix multiply and subtract, double-precision.}
{
 *  vDSP_zmmsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmmsD( __vDSP_a: DSPDoubleSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPDoubleSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPDoubleSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_d: DSPDoubleSplitComplexPtr; __vDSP_l: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmmsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix subtract and multiply, single-precision.}
{
 *  vDSP_zmsm()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmsm( __vDSP_a: DSPSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_d: DSPSplitComplexPtr; __vDSP_l: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmsm';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix subtract and multiply, double-precision.}
{
 *  vDSP_zmsmD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmsmD( __vDSP_a: DSPDoubleSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPDoubleSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPDoubleSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_d: DSPDoubleSplitComplexPtr; __vDSP_l: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmsmD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix multiply, single-precision.}
{
 *  vDSP_zmmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmmul( __vDSP_a: DSPSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split matrix multiply, double-precision.}
{
 *  vDSP_zmmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zmmulD( __vDSP_a: DSPDoubleSplitComplexPtr; __vDSP_i: vDSP_Stride; __vDSP_b: DSPDoubleSplitComplexPtr; __vDSP_j: vDSP_Stride; __vDSP_c: DSPDoubleSplitComplexPtr; __vDSP_k: vDSP_Stride; __vDSP_M: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_zmmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector add, single-precision.}
{
 *  vDSP_vadd()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vadd( {const} __vDSP_input1: {variable-size-array} Float32Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vadd';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector add, double-precision.}
{
 *  vDSP_vaddD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vaddD( {const} __vDSP_input1: {variable-size-array} Float64Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vaddD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector subtract, single-precision.}
{
 *  vDSP_vsub()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vsub( {const} __vDSP_input1: {variable-size-array} Float32Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vsub';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector subtract, double-precision.}
{
 *  vDSP_vsubD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsubD( {const} __vDSP_input1: {variable-size-array} Float64Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vsubD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector multiply, single-precision.}
{
 *  vDSP_vmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vmul( {const} __vDSP_input1: {variable-size-array} Float32Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector multiply, double-precision.}
{
 *  vDSP_vmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmulD( {const} __vDSP_input1: {variable-size-array} Float64Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector-scalar multiply, single-precision.}
{
 *  vDSP_vsmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vsmul( {const} __vDSP_input1: {variable-size-array} Float32Ptr; __vDSP_stride1: vDSP_Stride; const __vDSP_input2: Float32Ptr; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vsmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector-scalar multiply, double-precision.}
{
 *  vDSP_vsmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsmulD( {const} __vDSP_input1: {variable-size-array} Float64Ptr; __vDSP_stride1: vDSP_Stride; const __vDSP_input2: Float64Ptr; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vsmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector square, single-precision.}
{
 *  vDSP_vsq()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vsq( {const} __vDSP_input: {variable-size-array} Float32Ptr; __vDSP_strideInput: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vsq';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector square, double-precision.}
{
 *  vDSP_vsqD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsqD( {const} __vDSP_input: {variable-size-array} Float64Ptr; __vDSP_strideInput: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vsqD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector signed square, single-precision.}
{
 *  vDSP_vssq()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vssq( {const} __vDSP_input: {variable-size-array} Float32Ptr; __vDSP_strideInput: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vssq';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector signed square, double-precision.}
{
 *  vDSP_vssqD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vssqD( {const} __vDSP_input: {variable-size-array} Float64Ptr; __vDSP_strideInput: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vssqD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Dot product, single-precision.}
{
 *  vDSP_dotpr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_dotpr( {const} __vDSP_input1: {variable-size-array} Float32Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: Float32Ptr; __vDSP_size: vDSP_Length ); external name '_vDSP_dotpr';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Dot product, double-precision.}
{
 *  vDSP_dotprD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_dotprD( {const} __vDSP_input1: {variable-size-array} Float64Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: Float64Ptr; __vDSP_size: vDSP_Length ); external name '_vDSP_dotprD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector add and multiply, single-precision.}
{
 *  vDSP_vam()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_vam( {const} __vDSP_input1: {variable-size-array} Float32Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; {const} __vDSP_input3: {variable-size-array} Float32Ptr; __vDSP_stride3: vDSP_Stride; __vDSP_result: {variable-size-array} Float32Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vam';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Vector add and multiply, double-precision.}
{
 *  vDSP_vamD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vamD( {const} __vDSP_input1: {variable-size-array} Float64Ptr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; {const} __vDSP_input3: {variable-size-array} Float64Ptr; __vDSP_stride3: vDSP_Stride; __vDSP_result: {variable-size-array} Float64Ptr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_vamD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split convolution, single-precision.}
{
 *  vDSP_zconv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zconv( __vDSP_signal: DSPSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_filter: DSPSplitComplexPtr; __vDSP_strideFilter: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_lenResult: vDSP_Length; __vDSP_lenFilter: vDSP_Length ); external name '_vDSP_zconv';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split convolution, double-precision.}
{
 *  vDSP_zconvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zconvD( __vDSP_signal: DSPDoubleSplitComplexPtr; __vDSP_signalStride: vDSP_Stride; __vDSP_filter: DSPDoubleSplitComplexPtr; __vDSP_strideFilter: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_lenResult: vDSP_Length; __vDSP_lenFilter: vDSP_Length ); external name '_vDSP_zconvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split vector add, single-precision.}
{
 *  vDSP_zvadd()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zvadd( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zvadd';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split vector add, double-precision.}
{
 *  vDSP_zvaddD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvaddD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPDoubleSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zvaddD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split vector subtract, single-precision.}
{
 *  vDSP_zvsub()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zvsub( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zvsub';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split vector subtract, double-precision.}
{
 *  vDSP_zvsubD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvsubD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPDoubleSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zvsubD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split vector multiply, single-precision.}
{
 *  vDSP_zvmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zvmul( const __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; const __vDSP_input2: DSPSplitComplexPtr; __vDSP_stride2: vDSP_Stride; const __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length; __vDSP_conjugate: SInt32 ); external name '_vDSP_zvmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split vector multiply, double-precision.}
{
 *  vDSP_zvmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmulD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPDoubleSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length; __vDSP_conjugate: SInt32 ); external name '_vDSP_zvmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split dot product, single-precision.}
{
 *  vDSP_zdotpr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zdotpr( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_size: vDSP_Length ); external name '_vDSP_zdotpr';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split dot product, double-precision.}
{
 *  vDSP_zdotprD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zdotprD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPDoubleSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_size: vDSP_Length ); external name '_vDSP_zdotprD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split inner (conjugate) dot product, single-precision.}
{
 *  vDSP_zidotpr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zidotpr( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_size: vDSP_Length ); external name '_vDSP_zidotpr';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split inner (conjugate) dot product, double-precision.}
{
 *  vDSP_zidotprD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zidotprD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPDoubleSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_size: vDSP_Length ); external name '_vDSP_zidotprD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Dot product of complex-split with real, single-precision.}
{
 *  vDSP_zrdotpr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zrdotpr( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_size: vDSP_Length ); external name '_vDSP_zrdotpr';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Dot product of complex-split with real, double-precision.}
{
 *  vDSP_zrdotprD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrdotprD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_size: vDSP_Length ); external name '_vDSP_zrdotprD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Complex-split conjugate multiply and add, single-precision.}
{
 *  vDSP_zvcma()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zvcma( const __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; const __vDSP_input2: DSPSplitComplexPtr; __vDSP_stride2: vDSP_Stride; const __vDSP_input3: DSPSplitComplexPtr; __vDSP_stride3: vDSP_Stride; const __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zvcma';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Complex-split conjugate multiply and add, double-precision.}
{
 *  vDSP_zvcmaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvcmaD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; __vDSP_input2: DSPDoubleSplitComplexPtr; __vDSP_stride2: vDSP_Stride; __vDSP_input3: DSPDoubleSplitComplexPtr; __vDSP_stride3: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zvcmaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Add complex-split and real, single-precision.}
{
 *  vDSP_zrvadd()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zrvadd( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zrvadd';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Add complex-split and real, double-precision.}
{
 *  vDSP_zrvaddD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrvaddD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zrvaddD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Subtract real from complex-split, single-precision.}
{
 *  vDSP_zrvsub()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zrvsub( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zrvsub';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Subtract real from complex-split, double-precision.}
{
 *  vDSP_zrvsubD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrvsubD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zrvsubD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Multiply complex-split and real, single-precision.}
{
 *  vDSP_zrvmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in vecLib.framework
 *    CarbonLib:        not in Carbon, but vecLib is compatible with CarbonLib
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 }
procedure vDSP_zrvmul( __vDSP_input1: DSPSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float32Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zrvmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Multiply complex-split and real, double-precision.}
{
 *  vDSP_zrvmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrvmulD( __vDSP_input1: DSPDoubleSplitComplexPtr; __vDSP_stride1: vDSP_Stride; {const} __vDSP_input2: {variable-size-array} Float64Ptr; __vDSP_stride2: vDSP_Stride; __vDSP_result: DSPDoubleSplitComplexPtr; __vDSP_strideResult: vDSP_Stride; __vDSP_size: vDSP_Length ); external name '_vDSP_zrvmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Vector convert double-precision to single-precision.}
{
 *  vDSP_vdpsp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdpsp( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vdpsp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert single-precision to double-precision.}
{
 *  vDSP_vspdp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vspdp( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vspdp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector absolute value, integer.}
{
 *  vDSP_vabsi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vabsi( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vabsi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector (bit-wise) equivalence (not (A xor B)), integer.}
{
 *  vDSP_veqvi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_veqvi( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: SInt32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_veqvi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector divide, integer.}
{
 *  vDSP_vdivi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdivi( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: SInt32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vdivi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector fill, integer.}
{
 *  vDSP_vfilli()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfilli( __vDSP_A: SInt32Ptr; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfilli';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar add, integer.}
{
 *  vDSP_vsaddi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsaddi( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: SInt32Ptr; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsaddi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar divide, integer.}
{
 *  vDSP_vsdivi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsdivi( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: SInt32Ptr; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsdivi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split accumulating autospectrum, single-precision.}
{
 *  vDSP_zaspec()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zaspec( __vDSP_A: DSPSplitComplexPtr; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_zaspec';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split accumulating autospectrum, double-precision.}
{
 *  vDSP_zaspecD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zaspecD( var A: DSPDoubleSplitComplex; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_zaspecD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Create Blackman window, single-precision.}
{
 *  vDSP_blkman_window()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_blkman_window( __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length; __vDSP_FLAG: SInt32 ); external name '_vDSP_blkman_window';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Create Blackman window, double-precision.}
{
 *  vDSP_blkman_windowD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_blkman_windowD( __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length; __vDSP_FLAG: SInt32 ); external name '_vDSP_blkman_windowD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split coherence function, single-precision.}
{
 *  vDSP_zcoher()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zcoher( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_C: DSPSplitComplexPtr; __vDSP_D: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_zcoher';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split coherence function, double-precision.}
{
 *  vDSP_zcoherD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zcoherD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_D: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_zcoherD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split anti-aliasing down-sample with real filter, single-precision.}
{
 *  vDSP_zrdesamp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrdesamp( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: DSPSplitComplexPtr; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_zrdesamp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split anti-aliasing down-sample with real filter, double-precision.}
{
 *  vDSP_zrdesampD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrdesampD( var A: DSPDoubleSplitComplex; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_zrdesampD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector complex-split divide by real, single-precision.}
{
 *  vDSP_zrvdiv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrvdiv( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zrvdiv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector complex-split divide by real, double-precision.}
{
 *  vDSP_zrvdivD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zrvdivD( var A: DSPDoubleSplitComplex; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zrvdivD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Transfer function (B/A), single-precision.}
{
 *  vDSP_ztrans()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_ztrans( __vDSP_A: Float32Ptr; __vDSP_B: DSPSplitComplexPtr; __vDSP_C: DSPSplitComplexPtr; __vDSP_N: vDSP_Length ); external name '_vDSP_ztrans';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Transfer function (B/A), double-precision.}
{
 *  vDSP_ztransD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_ztransD( __vDSP_A: Float64Ptr; __vDSP_B: DSPDoubleSplitComplexPtr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_N: vDSP_Length ); external name '_vDSP_ztransD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector divide, single-precision.}
{
 *  vDSP_zvdiv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvdiv( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPSplitComplexPtr; __vDSP_J: vDSP_Stride; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvdiv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector divide, double-precision.}
{
 *  vDSP_zvdivD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvdivD( var A: DSPDoubleSplitComplex; __vDSP_I: vDSP_Stride; __vDSP_B: DSPDoubleSplitComplexPtr; __vDSP_J: vDSP_Stride; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvdivD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split accumulating cross-spectrum, single-precision.}
{
 *  vDSP_zcspec()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zcspec( __vDSP_A: DSPSplitComplexPtr; __vDSP_B: DSPSplitComplexPtr; __vDSP_C: DSPSplitComplexPtr; __vDSP_N: vDSP_Length ); external name '_vDSP_zcspec';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split accumulating cross-spectrum, double-precision.}
{
 *  vDSP_zcspecD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zcspecD( var A: DSPDoubleSplitComplex; __vDSP_B: DSPDoubleSplitComplexPtr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_N: vDSP_Length ); external name '_vDSP_zcspecD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector absolute value, single-precision.}
{
 *  vDSP_zvabs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvabs( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvabs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector absolute value, double-precision.}
{
 *  vDSP_zvabsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvabsD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvabsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector conjugate and multiply, single-precision.}
{
 *  vDSP_zvcmul()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvcmul( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPSplitComplexPtr; __vDSP_J: vDSP_Stride; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvcmul';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector conjugate and multiply, double-precision.}
{
 *  vDSP_zvcmulD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvcmulD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPDoubleSplitComplexPtr; __vDSP_J: vDSP_Stride; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvcmulD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector conjugate, single-precision.}
{
 *  vDSP_zvconj()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvconj( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvconj';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector conjugate, double-precision.}
{
 *  vDSP_zvconjD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvconjD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvconjD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector multiply with scalar, single-precision.}
{
 *  vDSP_zvzsml()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvzsml( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPSplitComplexPtr; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvzsml';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector multiply with scalar, double-precision.}
{
 *  vDSP_zvzsmlD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvzsmlD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPDoubleSplitComplexPtr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvzsmlD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector fill, single-precision.}
{
 *  vDSP_zvfill()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvfill( __vDSP_A: DSPSplitComplexPtr; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvfill';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector fill, double-precision.}
{
 *  vDSP_zvfillD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvfillD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvfillD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector magnitudes squared, single-precision.}
{
 *  vDSP_zvmags()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmags( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvmags';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector magnitudes squared, double-precision.}
{
 *  vDSP_zvmagsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmagsD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvmagsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector magnitudes square and add, single-precision.}
{
 *  vDSP_zvmgsa()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmgsa( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvmgsa';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector magnitudes square and add, double-precision.}
{
 *  vDSP_zvmgsaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmgsaD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvmgsaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector move, single-precision.}
{
 *  vDSP_zvmov()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmov( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvmov';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector move, double-precision.}
{
 *  vDSP_zvmovD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvmovD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvmovD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector negate, single-precision.}
{
 *  vDSP_zvneg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvneg( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvneg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector negate, double-precision.}
{
 *  vDSP_zvnegD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvnegD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvnegD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector phase, single-precision.}
{
 *  vDSP_zvphas()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvphas( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvphas';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector phase, double-precision.}
{
 *  vDSP_zvphasD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvphasD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvphasD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector multiply by scalar and add, single-precision.}
{
 *  vDSP_zvsma()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvsma( __vDSP_A: DSPSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPSplitComplexPtr; __vDSP_C: DSPSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_D: DSPSplitComplexPtr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvsma';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Complex-split vector multiply by scalar and add, double-precision.}
{
 *  vDSP_zvsmaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_zvsmaD( __vDSP_A: DSPDoubleSplitComplexPtr; __vDSP_I: vDSP_Stride; __vDSP_B: DSPDoubleSplitComplexPtr; __vDSP_C: DSPDoubleSplitComplexPtr; __vDSP_K: vDSP_Stride; __vDSP_D: DSPDoubleSplitComplexPtr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_zvsmaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Difference equation, 2 poles, 2 zeros, single-precision.}
{
 *  vDSP_deq22()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_deq22( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_deq22';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Difference equation, 2 poles, 2 zeros, double-precision.}
{
 *  vDSP_deq22D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_deq22D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_deq22D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Convolution with decimation (desampling), single-precision.}
{
 *  vDSP_desamp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_desamp( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_desamp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Convolution with decimation (desampling), double-precision.}
{
 *  vDSP_desampD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_desampD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_desampD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Create Hamming window, single-precision.}
{
 *  vDSP_hamm_window()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_hamm_window( __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length; __vDSP_FLAG: SInt32 ); external name '_vDSP_hamm_window';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Create Hamming window, double-precision.}
{
 *  vDSP_hamm_windowD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_hamm_windowD( __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length; __vDSP_FLAG: SInt32 ); external name '_vDSP_hamm_windowD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Create Hanning window, single-precision.}
{
 *  vDSP_hann_window()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_hann_window( __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length; __vDSP_FLAG: SInt32 ); external name '_vDSP_hann_window';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Create Hanning window, double-precision.}
{
 *  vDSP_hann_windowD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_hann_windowD( __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length; __vDSP_FLAG: SInt32 ); external name '_vDSP_hann_windowD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum magnitude of vector, single-precision.}
{
 *  vDSP_maxmgv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxmgv( const __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_maxmgv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum magnitude of vector, double-precision.}
{
 *  vDSP_maxmgvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxmgvD( const __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_maxmgvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum magnitude of vector, with index, single-precision.}
{
 *  vDSP_maxmgvi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxmgvi( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_maxmgvi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum magnitude of vector, with index, double-precision.}
{
 *  vDSP_maxmgviD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxmgviD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_maxmgviD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum value of vector, single-precision.}
{
 *  vDSP_maxv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_maxv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum value of vector, double-precision.}
{
 *  vDSP_maxvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_maxvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum value of vector, with index, single-precision.}
{
 *  vDSP_maxvi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxvi( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_maxvi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Maximum value of vector, with index, double-precision.}
{
 *  vDSP_maxviD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_maxviD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_maxviD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean magnitude of vector, single-precision.}
{
 *  vDSP_meamgv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_meamgv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_meamgv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean magnitude of vector, double-precision.}
{
 *  vDSP_meamgvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_meamgvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_meamgvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean of vector, single-precision.}
{
 *  vDSP_meanv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_meanv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_meanv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean of vector, double-precision.}
{
 *  vDSP_meanvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_meanvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_meanvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean square of vector, single-precision.}
{
 *  vDSP_measqv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_measqv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_measqv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean square of vector, double-precision.}
{
 *  vDSP_measqvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_measqvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_measqvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum magnitude of vector, single-precision.}
{
 *  vDSP_minmgv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minmgv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_minmgv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum magnitude of vector, double-precision.}
{
 *  vDSP_minmgvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minmgvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_minmgvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum magnitude of vector, with index, single-precision.}
{
 *  vDSP_minmgvi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minmgvi( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_minmgvi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum magnitude of vector, with index, double-precision.}
{
 *  vDSP_minmgviD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minmgviD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_minmgviD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum value of vector, single-precision.}
{
 *  vDSP_minv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_minv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum value of vector, double-precision.}
{
 *  vDSP_minvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_minvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum value of vector, with index, single-precision.}
{
 *  vDSP_minvi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minvi( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_minvi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Minimum value of vector, with index, double-precision.}
{
 *  vDSP_minviD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_minviD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; var __vDSP_IC: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_minviD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Matrix move, single-precision.}
{
 *  vDSP_mmov()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mmov( __vDSP_A: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_NC: vDSP_Length; __vDSP_NR: vDSP_Length; __vDSP_TCA: vDSP_Length; __vDSP_TCC: vDSP_Length ); external name '_vDSP_mmov';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Matrix move, double-precision.}
{
 *  vDSP_mmovD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mmovD( __vDSP_A: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_NC: vDSP_Length; __vDSP_NR: vDSP_Length; __vDSP_TCA: vDSP_Length; __vDSP_TCC: vDSP_Length ); external name '_vDSP_mmovD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean of signed squares of vector, single-precision.}
{
 *  vDSP_mvessq()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mvessq( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_mvessq';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Mean of signed squares of vector, double-precision.}
{
 *  vDSP_mvessqD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_mvessqD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_mvessqD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Find zero crossing, single-precision.}
{
 *  vDSP_nzcros()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_nzcros( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: vDSP_Length; var __vDSP_C: vDSP_Length; var __vDSP_D: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_nzcros';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Find zero crossing, double-precision.}
{
 *  vDSP_nzcrosD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_nzcrosD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: vDSP_Length; var __vDSP_C: vDSP_Length; var __vDSP_D: vDSP_Length; __vDSP_N: vDSP_Length ); external name '_vDSP_nzcrosD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Convert rectangular to polar, single-precision.}
{
 *  vDSP_polar()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_polar( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_polar';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Convert rectangular to polar, double-precision.}
{
 *  vDSP_polarD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_polarD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_polarD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Convert polar to rectangular, single-precision.}
{
 *  vDSP_rect()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_rect( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_rect';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Convert polar to rectangular, double-precision.}
{
 *  vDSP_rectD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_rectD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_rectD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Root-mean-square of vector, single-precision.}
{
 *  vDSP_rmsqv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_rmsqv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_rmsqv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Root-mean-square of vector, double-precision.}
{
 *  vDSP_rmsqvD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_rmsqvD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_rmsqvD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Scalar-vector divide, single-precision.}
{
 *  vDSP_svdiv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svdiv( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_svdiv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Scalar-vector divide, double-precision.}
{
 *  vDSP_svdivD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svdivD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_svdivD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements, single-precision.}
{
 *  vDSP_sve()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_sve( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_sve';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements, double-precision.}
{
 *  vDSP_sveD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_sveD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_sveD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements magnitudes, single-precision.}
{
 *  vDSP_svemg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svemg( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_svemg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements' magnitudes, double-precision.}
{
 *  vDSP_svemgD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svemgD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_svemgD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements' squares, single-precision.}
{
 *  vDSP_svesq()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svesq( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_svesq';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements' squares, double-precision.}
{
 *  vDSP_svesqD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svesqD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_svesqD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements' signed squares, single-precision.}
{
 *  vDSP_svs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svs( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_svs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Sum of vector elements' signed squares, double-precision.}
{
 *  vDSP_svsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_svsD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length ); external name '_vDSP_svsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector add, add, and multiply, single-precision.}
{
 *  vDSP_vaam()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vaam( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float32Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vaam';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector add, add, and multiply, double-precision.}
{
 *  vDSP_vaamD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vaamD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float64Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vaamD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector absolute value, single-precision.}
{
 *  vDSP_vabs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vabs( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vabs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector absolute value, double-precision.}
{
 *  vDSP_vabsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vabsD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vabsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector add, subtract, and multiply, single-precision.}
{
 *  vDSP_vasbm()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vasbm( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float32Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vasbm';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector add, subtract, and multiply, double-precision.}
{
 *  vDSP_vasbmD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vasbmD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float64Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vasbmD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector add and scalar multiply, single-precision.}
{
 *  vDSP_vasm()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vasm( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vasm';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector add and scalar multiply, double-precision.}
{
 *  vDSP_vasmD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vasmD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vasmD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector linear average, single-precision.}
{
 *  vDSP_vavlin()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vavlin( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vavlin';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector linear average, double-precision.}
{
 *  vDSP_vavlinD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vavlinD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vavlinD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector clip, single-precision.}
{
 *  vDSP_vclip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vclip( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vclip';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector clip, double-precision.}
{
 *  vDSP_vclipD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vclipD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vclipD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector clip and count, single-precision.}
{
 *  vDSP_vclipc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vclipc( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length; var __vDSP_NLOW: vDSP_Length; var __vDSP_NHI: vDSP_Length ); external name '_vDSP_vclipc';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector clip and count, double-precision.}
{
 *  vDSP_vclipcD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vclipcD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length; var __vDSP_NLOW: vDSP_Length; var __vDSP_NHI: vDSP_Length ); external name '_vDSP_vclipcD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector clear, single-precision.}
{
 *  vDSP_vclr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vclr( __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vclr';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector clear, double-precision.}
{
 *  vDSP_vclrD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vclrD( __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vclrD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector compress, single-precision.}
{
 *  vDSP_vcmprs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vcmprs( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vcmprs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector compress, double-precision.}
{
 *  vDSP_vcmprsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vcmprsD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vcmprsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to decibels, power, or amplitude, single-precision.}
{
 *  vDSP_vdbcon()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdbcon( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_F: UInt32 ); external name '_vDSP_vdbcon';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to decibels, power, or amplitude, double-precision.}
{
 *  vDSP_vdbconD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdbconD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_F: UInt32 ); external name '_vDSP_vdbconD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector distance, single-precision.}
{
 *  vDSP_vdist()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdist( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vdist';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector distance, double-precision.}
{
 *  vDSP_vdistD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdistD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vdistD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector divide, single-precision.}
{
 *  vDSP_vdiv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdiv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vdiv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector divide, double-precision.}
{
 *  vDSP_vdivD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vdivD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vdivD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector envelope, single-precision.}
{
 *  vDSP_venvlp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_venvlp( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_venvlp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector envelope, double-precision.}
{
 *  vDSP_venvlpD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_venvlpD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_venvlpD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector fill, single-precision.}
{
 *  vDSP_vfill()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfill( __vDSP_A: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfill';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector fill, double-precision.}
{
 *  vDSP_vfillD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfillD( __vDSP_A: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfillD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 8-bit integer, round toward zero, single-precision.}
{
 *  vDSP_vfix8()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfix8( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfix8';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 8-bit integer, round toward zero, double-precision.}
{
 *  vDSP_vfix8D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfix8D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfix8D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 16-bit integer, round toward zero, single-precision.}
{
 *  vDSP_vfix16()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfix16( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfix16';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 16-bit integer, round toward zero, double-precision.}
{
 *  vDSP_vfix16D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfix16D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfix16D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 32-bit integer, round toward zero, single-precision.}
{
 *  vDSP_vfix32()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfix32( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfix32';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 32-bit integer, round toward zero, double-precision.}
{
 *  vDSP_vfix32D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfix32D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfix32D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 8-bit integer, round to nearest, single-precision.}
{
 *  vDSP_vfixr8()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixr8( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixr8';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 8-bit integer, round to nearest, double-precision.}
{
 *  vDSP_vfixr8D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixr8D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixr8D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 16-bit integer, round to nearest, single-precision.}
{
 *  vDSP_vfixr16()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixr16( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixr16';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 16-bit integer, round to nearest, double-precision.}
{
 *  vDSP_vfixr16D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixr16D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixr16D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 32-bit integer, round to nearest, single-precision.}
{
 *  vDSP_vfixr32()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixr32( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixr32';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to 32-bit integer, round to nearest, double-precision.}
{
 *  vDSP_vfixr32D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixr32D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: SInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixr32D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 8-bit integer, toward zero, single-precision.}
{
 *  vDSP_vfixu8()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixu8( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixu8';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 8-bit integer, toward zero, double-precision.}
{
 *  vDSP_vfixu8D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixu8D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixu8D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 16-bit integer, toward zero, single-precision.}
{
 *  vDSP_vfixu16()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixu16( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixu16';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 16-bit integer, toward zero, double-precision.}
{
 *  vDSP_vfixu16D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixu16D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixu16D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 32-bit integer, toward zero, single-precision.}
{
 *  vDSP_vfixu32()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixu32( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixu32';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 32-bit integer, toward zero, double-precision.}
{
 *  vDSP_vfixu32D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixu32D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixu32D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 8-bit integer, to nearest, single-precision.}
{
 *  vDSP_vfixru8()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixru8( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixru8';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 8-bit integer, to nearest, double-precision.}
{
 *  vDSP_vfixru8D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixru8D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt8Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixru8D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 16-bit integer, to nearest, single-precision.}
{
 *  vDSP_vfixru16()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixru16( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixru16';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 16-bit integer, to nearest, double-precision.}
{
 *  vDSP_vfixru16D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixru16D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt16Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixru16D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 32-bit integer, to nearest, single-precision.}
{
 *  vDSP_vfixru32()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixru32( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixru32';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert to unsigned 32-bit integer, to nearest, double-precision.}
{
 *  vDSP_vfixru32D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfixru32D( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: UInt32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfixru32D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 8-bit integer, single-precision.}
{
 *  vDSP_vflt8()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vflt8( var A: char; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vflt8';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 8-bit integer, double-precision.}
{
 *  vDSP_vflt8D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vflt8D( var A: char; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vflt8D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 16-bit integer, single-precision.}
{
 *  vDSP_vflt16()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vflt16( var A: SInt16; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vflt16';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 16-bit integer, double-precision.}
{
 *  vDSP_vflt16D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vflt16D( var A: SInt16; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vflt16D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 32-bit integer, single-precision.}
{
 *  vDSP_vflt32()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vflt32( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vflt32';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 32-bit integer, double-precision.}
{
 *  vDSP_vflt32D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vflt32D( __vDSP_A: SInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vflt32D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 8-bit integer, single-precision.}
{
 *  vDSP_vfltu8()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfltu8( A: UInt8Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfltu8';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 8-bit integer, double-precision.}
{
 *  vDSP_vfltu8D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfltu8D( A: UInt8Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfltu8D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 16-bit integer, single-precision.}
{
 *  vDSP_vfltu16()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfltu16( A: UInt16Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfltu16';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 16-bit integer, double-precision.}
{
 *  vDSP_vfltu16D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfltu16D( A: UInt16Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfltu16D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 32-bit integer, single-precision.}
{
 *  vDSP_vfltu32()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfltu32( __vDSP_A: UInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfltu32';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector convert from 32-bit integer, double-precision.}
{
 *  vDSP_vfltu32D()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfltu32D( __vDSP_A: UInt32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfltu32D';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector fraction part (subtract integer toward zero), single-precision.}
{
 *  vDSP_vfrac()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfrac( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfrac';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector fraction part (subtract integer toward zero), double-precision.}
{
 *  vDSP_vfracD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vfracD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vfracD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector gather, single-precision.}
{
 *  vDSP_vgathr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgathr( __vDSP_A: Float32Ptr; var __vDSP_B: vDSP_Length; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vgathr';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector gather, double-precision.}
{
 *  vDSP_vgathrD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgathrD( __vDSP_A: Float64Ptr; var __vDSP_B: vDSP_Length; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vgathrD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector gather, absolute pointers, single-precision.}
{
 *  vDSP_vgathra()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgathra( var A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vgathra';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector gather, absolute pointers, double-precision.}
{
 *  vDSP_vgathraD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgathraD( var A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vgathraD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector generate tapered ramp, single-precision.}
{
 *  vDSP_vgen()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgen( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vgen';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector generate tapered ramp, double-precision.}
{
 *  vDSP_vgenD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgenD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vgenD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector generate by extrapolation and interpolation, single-precision.}
{
 *  vDSP_vgenp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgenp( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_vgenp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector generate by extrapolation and interpolation, double-precision.}
{
 *  vDSP_vgenpD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vgenpD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_vgenpD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector inverted clip, single-precision.}
{
 *  vDSP_viclip()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_viclip( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_viclip';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector inverted clip, double-precision.}
{
 *  vDSP_viclipD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_viclipD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_viclipD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector index (C[i] = A[truncate[B[i]]), single-precision.}
{
 *  vDSP_vindex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vindex( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vindex';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector index (C[i] = A[truncate[B[i]]), double-precision.}
{
 *  vDSP_vindexD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vindexD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vindexD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector interpolation between vectors, single-precision.}
{
 *  vDSP_vintb()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vintb( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vintb';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector interpolation between vectors, double-precision.}
{
 *  vDSP_vintbD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vintbD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vintbD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector test limit, single-precision.}
{
 *  vDSP_vlim()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vlim( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vlim';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector test limit, double-precision.}
{
 *  vDSP_vlimD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vlimD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vlimD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector linear interpolation, single-precision.}
{
 *  vDSP_vlint()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vlint( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_vlint';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector linear interpolation, double-precision.}
{
 *  vDSP_vlintD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vlintD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_vlintD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply and add, single-precision.}
{
 *  vDSP_vma()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vma( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vma';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply and add, double-precision.}
{
 *  vDSP_vmaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmaD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector maxima, single-precision.}
{
 *  vDSP_vmax()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmax( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmax';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector maxima, double-precision.}
{
 *  vDSP_vmaxD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmaxD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmaxD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector maximum magnitude, single-precision.}
{
 *  vDSP_vmaxmg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmaxmg( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmaxmg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector maximum magnitude, double-precision.}
{
 *  vDSP_vmaxmgD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmaxmgD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmaxmgD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector minima, single-precision.}
{
 *  vDSP_vmin()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmin( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmin';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector minima, double-precision.}
{
 *  vDSP_vminD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vminD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vminD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector minimum magnitude, single-precision.}
{
 *  vDSP_vminmg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vminmg( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vminmg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector minimum magnitude, double-precision.}
{
 *  vDSP_vminmgD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vminmgD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vminmgD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply, multiply, and add, single-precision.}
{
 *  vDSP_vmma()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmma( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float32Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmma';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply, multiply, and add, double-precision.}
{
 *  vDSP_vmmaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmmaD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float64Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmmaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply, multiply, and subtract, single-precision.}
{
 *  vDSP_vmmsb()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmmsb( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float32Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmmsb';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply, multiply, and subtract, double-precision.}
{
 *  vDSP_vmmsbD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmmsbD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float64Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmmsbD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply and scalar add, single-precision.}
{
 *  vDSP_vmsa()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmsa( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmsa';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply and scalar add, double-precision.}
{
 *  vDSP_vmsaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmsaD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmsaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply and subtract, single-precision.}
{
 *  vDSP_vmsb()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmsb( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmsb';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector multiply and subtract, double-precision.}
{
 *  vDSP_vmsbD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vmsbD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vmsbD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector negative absolute value, single-precision.}
{
 *  vDSP_vnabs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vnabs( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vnabs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector negative absolute value, double-precision.}
{
 *  vDSP_vnabsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vnabsD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vnabsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector negate, single-precision.}
{
 *  vDSP_vneg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vneg( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vneg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector negate, double-precision.}
{
 *  vDSP_vnegD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vnegD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vnegD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector polynomial, single-precision.}
{
 *  vDSP_vpoly()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vpoly( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_vpoly';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector polynomial, single-precision.}
{
 *  vDSP_vpolyD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vpolyD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_vpolyD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector Pythagoras, single-precision.}
{
 *  vDSP_vpythg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vpythg( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float32Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vpythg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector Pythagoras, double-precision.}
{
 *  vDSP_vpythgD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vpythgD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float64Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vpythgD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector quadratic interpolation, single-precision.}
{
 *  vDSP_vqint()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vqint( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_vqint';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector quadratic interpolation, double-precision.}
{
 *  vDSP_vqintD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vqintD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_M: vDSP_Length ); external name '_vDSP_vqintD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector build ramp, single-precision.}
{
 *  vDSP_vramp()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vramp( __vDSP_A: Float32Ptr; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vramp';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector build ramp, double-precision.}
{
 *  vDSP_vrampD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vrampD( __vDSP_A: Float64Ptr; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vrampD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector running sum integration, single-precision.}
{
 *  vDSP_vrsum()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vrsum( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_S: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vrsum';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector running sum integration, double-precision.}
{
 *  vDSP_vrsumD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vrsumD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_S: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vrsumD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector reverse order, in-place, single-precision.}
{
 *  vDSP_vrvrs()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vrvrs( __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vrvrs';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector reverse order, in-place, double-precision.}
{
 *  vDSP_vrvrsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vrvrsD( __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vrvrsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar add, single-precision.}
{
 *  vDSP_vsadd()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsadd( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsadd';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar add, double-precision.}
{
 *  vDSP_vsaddD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsaddD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsaddD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector subtract and multiply, single-precision.}
{
 *  vDSP_vsbm()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsbm( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsbm';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector subtract and multiply, double-precision.}
{
 *  vDSP_vsbmD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsbmD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsbmD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector subtract, subtract, and multiply, single-precision.}
{
 *  vDSP_vsbsbm()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsbsbm( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float32Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsbsbm';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector subtract, subtract, and multiply, double-precision.}
{
 *  vDSP_vsbsbmD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsbsbmD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_E: Float64Ptr; __vDSP_M: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsbsbmD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector subtract and scalar multiply, single-precision.}
{
 *  vDSP_vsbsm()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsbsm( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsbsm';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector subtract and scalar multiply, double-precision.}
{
 *  vDSP_vsbsmD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsbsmD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsbsmD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar divide, single-precision.}
{
 *  vDSP_vsdiv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsdiv( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsdiv';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar divide, double-precision.}
{
 *  vDSP_vsdivD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsdivD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsdivD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector Simpson integration, single-precision.}
{
 *  vDSP_vsimps()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsimps( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsimps';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector Simpson integration, double-precision.}
{
 *  vDSP_vsimpsD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsimpsD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsimpsD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar multiply and vector add, single-precision.}
{
 *  vDSP_vsma()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsma( const __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; const __vDSP_B: Float32Ptr; const __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsma';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar multiply and vector add, double-precision.}
{
 *  vDSP_vsmaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsmaD( const __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; const __vDSP_B: Float64Ptr; const __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsmaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar multiply and scalar add, single-precision.}
{
 *  vDSP_vsmsa()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsmsa( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsmsa';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector-scalar multiply and scalar add, double-precision.}
{
 *  vDSP_vsmsaD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsmsaD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsmsaD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector scalar multiply and vector subtract, single-precision.}
{
 *  vDSP_vsmsb()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsmsb( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsmsb';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector scalar multiply and vector subtract, double-precision.}
{
 *  vDSP_vsmsbD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsmsbD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vsmsbD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector sort, in-place, single-precision.}
{
 *  vDSP_vsort()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsort( __vDSP_C: Float32Ptr; __vDSP_N: vDSP_Length; __vDSP_OFLAG: SInt32 ); external name '_vDSP_vsort';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector sort, in-place, double-precision.}
{
 *  vDSP_vsortD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsortD( __vDSP_C: Float64Ptr; __vDSP_N: vDSP_Length; __vDSP_OFLAG: SInt32 ); external name '_vDSP_vsortD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector sort, in-place, integer, single-precision.}
{
 *  vDSP_vsorti()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsorti( __vDSP_C: Float32Ptr; var __vDSP_IC: vDSP_Length; var __vDSP_List_addr: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_OFLAG: SInt32 ); external name '_vDSP_vsorti';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector sort, in-place, integer, double-precision.}
{
 *  vDSP_vsortiD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vsortiD( __vDSP_C: Float64Ptr; var __vDSP_IC: vDSP_Length; var __vDSP_List_addr: vDSP_Length; __vDSP_N: vDSP_Length; __vDSP_OFLAG: SInt32 ); external name '_vDSP_vsortiD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector swap, single-precision.}
{
 *  vDSP_vswap()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vswap( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vswap';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector swap, double-precision.}
{
 *  vDSP_vswapD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vswapD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vswapD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector sliding window sum, single-precision.}
{
 *  vDSP_vswsum()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vswsum( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_vswsum';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector sliding window sum, double-precision.}
{
 *  vDSP_vswsumD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vswsumD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length; __vDSP_P: vDSP_Length ); external name '_vDSP_vswsumD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector table lookup and interpolation, single-precision.}
{
 *  vDSP_vtabi()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vtabi( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_S1: Float32Ptr; __vDSP_S2: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_M: vDSP_Length; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vtabi';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector table lookup and interpolation, double-precision.}
{
 *  vDSP_vtabiD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vtabiD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_S1: Float64Ptr; __vDSP_S2: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_M: vDSP_Length; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vtabiD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector threshold, single-precision.}
{
 *  vDSP_vthr()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vthr( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vthr';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector threshold, double-precision.}
{
 *  vDSP_vthrD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vthrD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vthrD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector threshold with zero fill, single-precision.}
{
 *  vDSP_vthres()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vthres( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vthres';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector threshold with zero fill, double-precision.}
{
 *  vDSP_vthresD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vthresD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vthresD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector threshold with signed constant, single-precision.}
{
 *  vDSP_vthrsc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vthrsc( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_D: Float32Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vthrsc';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector threshold with signed constant, double-precision.}
{
 *  vDSP_vthrscD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vthrscD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_D: Float64Ptr; __vDSP_L: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vthrscD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector tapered merge, single-precision.}
{
 *  vDSP_vtmerg()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vtmerg( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vtmerg';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector tapered merge, double-precision.}
{
 *  vDSP_vtmergD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vtmergD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_J: vDSP_Stride; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vtmergD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector trapezoidal integration, single-precision.}
{
 *  vDSP_vtrapz()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vtrapz( __vDSP_A: Float32Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vtrapz';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Vector trapezoidal integration, double-precision.}
{
 *  vDSP_vtrapzD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_vtrapzD( __vDSP_A: Float64Ptr; __vDSP_I: vDSP_Stride; __vDSP_B: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_K: vDSP_Stride; __vDSP_N: vDSP_Length ); external name '_vDSP_vtrapzD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Wiener Levinson, single-precision.}
{
 *  vDSP_wiener()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_wiener( __vDSP_L: vDSP_Length; __vDSP_A: Float32Ptr; __vDSP_C: Float32Ptr; __vDSP_F: Float32Ptr; __vDSP_P: Float32Ptr; __vDSP_IFLG: SInt32; var __vDSP_IERR: SInt32 ); external name '_vDSP_wiener';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{ Wiener Levinson, double-precision.}
{
 *  vDSP_wienerD()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in vecLib.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure vDSP_wienerD( __vDSP_L: vDSP_Length; __vDSP_A: Float64Ptr; __vDSP_C: Float64Ptr; __vDSP_F: Float64Ptr; __vDSP_P: Float64Ptr; __vDSP_IFLG: SInt32; var __vDSP_IERR: SInt32 ); external name '_vDSP_wienerD';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{$ifc undefined USE_NONE_APPLE_STANDARD_DATATYPES}
{$setc USE_NON_APPLE_STANDARD_DATATYPES := 1}
{$endc} {not defined USE_NON_APPLE_STANDARD_DATATYPES }

{$ifc USE_NON_APPLE_STANDARD_DATATYPES}
const
	FFT_FORWARD = kFFTDirection_Forward;
	FFT_INVERSE = kFFTDirection_Inverse;

const
	FFT_RADIX2 = kFFTRadix2;
	FFT_RADIX3 = kFFTRadix3;
	FFT_RADIX5 = kFFTRadix5;

type
	COMPLEX = DSPComplex;
	COMPLEX_SPLIT = DSPSplitComplex;
	DOUBLE_COMPLEX = DSPDoubleComplex;
	DOUBLE_COMPLEX_SPLIT = DSPDoubleSplitComplex;
{$endc} {USE_NON_APPLE_STANDARD_DATATYPES}


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
