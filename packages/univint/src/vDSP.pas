{
     File:       vDSP.p
 
     Contains:   AltiVec DSP Interfaces
 
     Version:    Technology: 1.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit vDSP;
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
uses MacTypes,ConditionalMacros;

{$ALIGN MAC68K}


type
	DSPComplexPtr = ^DSPComplex;
	DSPComplex = record
		real:					Single;
		imag:					Single;
	end;

	DSPSplitComplexPtr = ^DSPSplitComplex;
	DSPSplitComplex = record
		realp:					^Single;
		imagp:					^Single;
	end;

	FFTSetup    = ^SInt32; { an opaque 32-bit type }
	FFTSetupPtr = ^FFTSetup;  { when a var xx:FFTSetup parameter can be nil, it is changed to xx: FFTSetupPtr }
	FFTDirection 				= SInt32;
const
	kFFTDirection_Forward		= 1;
	kFFTDirection_Inverse		= -1;


type
	FFTRadix 					= SInt32;
const
	kFFTRadix2					= 0;
	kFFTRadix3					= 1;
	kFFTRadix5					= 2;

	{	
	ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
	    The criteria to invoke the PowerPC vector implementation is subject to     
	    change and become less restrictive in the future.                          
	ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
		}
	{	
	ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
	    Functions create_fftsetup and destroy_fftsetup.
	
	    create_fftsetup will allocate memory and setup a weight array used by      
	    the FFT. The call destroy_fftsetup will free the array.                    
	ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
		}
	{
	 *  create_fftsetup()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in vecLib 1.0 and later
	 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
	 *    Mac OS X:         in version 10.0 and later
	 	}
function create_fftsetup(log2n: UInt32; radix: FFTRadix): FFTSetup; external name '_create_fftsetup';

{
 *  destroy_fftsetup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure destroy_fftsetup(setup: FFTSetup); external name '_destroy_fftsetup';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions ctoz and ztoc.
    
    ctoz converts a complex array to a complex-split array
    ztoc converts a complex-split array to a complex array
    
    Criteria to invoke PowerPC vector code:    
        1. size > 3
        2. strideC = 2
        3. strideZ = 1
        4. Z.realp and Z.imagp are relatively aligned.
        5. C is 8-byte aligned if Z.realp and Z.imagp are 4-byte- aligned
           or C is 16-byte aligned if Z.realp and Z.imagp are at least
           8-byte aligned.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  ctoz()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure ctoz(const (*var*) C: DSPComplex; strideC: SInt32; var Z: DSPSplitComplex; strideZ: SInt32; size: UInt32); external name '_ctoz';

{
 *  ztoc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure ztoc(const (*var*) Z: DSPSplitComplex; strideZ: SInt32; var C: DSPComplex; strideC: SInt32; size: UInt32); external name '_ztoc';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft_zip and fft_zipt 
    
    In-place Split Complex Fourier Transform with or without temporary memory.
            
      Criteria to invoke PowerPC vector code:    
        
        1. ioData.realp and ioData.imagp must be 16-byte aligned.
        2. stride = 1
        3. 2 <= log2n <= 20
        4. bufferTemp.realp and bufferTemp.imagp must be 16-byte aligned.
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  Direction can be either
      kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft_zip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zip(setup: FFTSetup; var ioData: DSPSplitComplex; stride: SInt32; log2n: UInt32; direction: FFTDirection); external name '_fft_zip';

{
 *  fft_zipt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zipt(setup: FFTSetup; var ioData: DSPSplitComplex; stride: SInt32; var bufferTemp: DSPSplitComplex; log2n: UInt32; direction: FFTDirection); external name '_fft_zipt';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
     Functions fft_zop and fft_zopt
     
     Out-of-place Split Complex Fourier Transform with or without temporary
     memory
            
      Criteria to invoke PowerPC vector code:  
        
        1. signal.realp and signal.imagp must be 16-byte aligned.
        2. signalStride = 1
        3. result.realp and result.imagp must be 16-byte aligned.
        4. strideResult = 1
        5. 2 <= log2n <= 20
        6. bufferTemp.realp and bufferTemp.imagp must be 16-byte aligned.
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  Direction can be either
      kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft_zop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zop(setup: FFTSetup; var signal: DSPSplitComplex; signalStride: SInt32; var result: DSPSplitComplex; strideResult: SInt32; log2n: UInt32; direction: FFTDirection); external name '_fft_zop';

{
 *  fft_zopt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zopt(setup: FFTSetup; var signal: DSPSplitComplex; signalStride: SInt32; var result: DSPSplitComplex; strideResult: SInt32; var bufferTemp: DSPSplitComplex; log2n: UInt32; direction: FFTDirection); external name '_fft_zopt';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft_zrip and fft_zript
    
    In-Place Real Fourier Transform with or without temporary memory,
    split Complex Format
            
      Criteria to invoke PowerPC vector code:    
        1. ioData.realp and ioData.imagp must be 16-byte aligned.
        2. stride = 1
        3. 3 <= log2n <= 13
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  Direction can be either
      kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft_zrip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zrip(setup: FFTSetup; var ioData: DSPSplitComplex; stride: SInt32; log2n: UInt32; direction: FFTDirection); external name '_fft_zrip';

{
 *  fft_zript()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zript(setup: FFTSetup; var ioData: DSPSplitComplex; stride: SInt32; var bufferTemp: DSPSplitComplex; log2n: UInt32; direction: FFTDirection); external name '_fft_zript';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft_zrop and fft_zropt
    
    Out-of-Place Real Fourier Transform with or without temporary memory,
    split Complex Format
            
      Criteria to invoke PowerPC vector code:  
        1. signal.realp and signal.imagp must be 16-byte aligned.
        2. signalStride = 1
        3. result.realp and result.imagp must be be 16-byte aligned.
        4. strideResult = 1
        5. 3 <= log2n <= 13
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  Direction can be either
      kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft_zrop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zrop(setup: FFTSetup; var signal: DSPSplitComplex; signalStride: SInt32; var result: DSPSplitComplex; strideResult: SInt32; log2n: UInt32; direction: FFTDirection); external name '_fft_zrop';

{
 *  fft_zropt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft_zropt(setup: FFTSetup; var signal: DSPSplitComplex; signalStride: SInt32; var result: DSPSplitComplex; strideResult: SInt32; var bufferTemp: DSPSplitComplex; log2n: UInt32; direction: FFTDirection); external name '_fft_zropt';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft2d_zip and fft2d_zipt
    
    In-place two dimensional Split Complex Fourier Transform with or without
    temporary memory
            
      Criteria to invoke PowerPC vector code:  
        1. ioData.realp and ioData.imagp must be 16-byte aligned.
        2. strideInRow = 1;
        3. strideInCol must be a multiple of 4
        4. 2 <= log2nInRow <= 12
        5. 2 <= log2nInCol <= 12
        6. bufferTemp.realp and bufferTemp.imagp must be 16-byte aligned.
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  ( log2n = log2nInRow + log2nInCol ) 
      Direction can be either kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft2d_zip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zip(setup: FFTSetup; var ioData: DSPSplitComplex; strideInRow: SInt32; strideInCol: SInt32; log2nInCol: UInt32; log2nInRow: UInt32; direction: FFTDirection); external name '_fft2d_zip';

{
 *  fft2d_zipt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zipt(setup: FFTSetup; var ioData: DSPSplitComplex; strideInRow: SInt32; strideInCol: SInt32; var bufferTemp: DSPSplitComplex; log2nInCol: UInt32; log2nInRow: UInt32; direction: FFTDirection); external name '_fft2d_zipt';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft2d_zop and fft2d_zopt
    
    Out-of-Place two dimemsional Split Complex Fourier Transform with or
    without temporary memory
            
      Criteria to invoke PowerPC vector code:  
        
        1. signal.realp and signal.imagp must be 16-byte aligned.
        2. signalStrideInRow = 1;
        3. signalStrideInCol must be a multiple of 4
        4. result.realp and result.imagp must be 16-byte aligned.
        5. strideResultInRow = 1;
        6. strideResultInCol must be a multiple of 4
        7. 2 <= log2nInRow <= 12
        8. 2 <= log2nInCol <= 12
        9. bufferTemp.realp and bufferTemp.imagp must be 16-byte aligned.

      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  ( log2n = log2nInRow + log2nInCol ) 
      Direction can be either kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft2d_zop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zop(setup: FFTSetup; var signal: DSPSplitComplex; signalStrideInRow: SInt32; signalStrideInCol: SInt32; var result: DSPSplitComplex; strideResultInRow: SInt32; strideResultInCol: SInt32; log2nInCol: UInt32; log2nInRow: UInt32; flag: SInt32); external name '_fft2d_zop';

{
 *  fft2d_zopt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zopt(setup: FFTSetup; var signal: DSPSplitComplex; signalStrideInRow: SInt32; signalStrideInCol: SInt32; var result: DSPSplitComplex; strideResultInRow: SInt32; strideResultInCol: SInt32; var bufferTemp: DSPSplitComplex; log2nInCol: UInt32; log2nInRow: UInt32; flag: SInt32); external name '_fft2d_zopt';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft2d_zrip and fft2d_zript
    
    In-place two dimensional Real Fourier Transform with or without temporary
    memory, Split Complex Format
            
      Criteria to invoke PowerPC vector code:  
        1. ioData.realp and ioData.imagp must be 16-byte aligned.
        2. strideInRow = 1;
        3. strideInCol must be a multiple of 4
        4. 3 <= log2nInRow <= 12
        5. 3 <= log2nInCol <= 13
        6. bufferTemp.realp and bufferTemp.imagp must be 16-byte aligned.

      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  ( log2n = log2nInRow + log2nInCol ) 
      Direction can be either kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft2d_zrip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zrip(setup: FFTSetup; var ioData: DSPSplitComplex; strideInRow: SInt32; strideInCol: SInt32; log2nInCol: UInt32; log2nInRow: UInt32; direction: FFTDirection); external name '_fft2d_zrip';

{
 *  fft2d_zript()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zript(setup: FFTSetup; var ioData: DSPSplitComplex; strideInRow: SInt32; strideInCol: SInt32; var bufferTemp: DSPSplitComplex; log2nInCol: UInt32; log2nInRow: UInt32; direction: FFTDirection); external name '_fft2d_zript';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Functions fft2d_zrop and fft2d_zropt
    
    Out-of-Place Two-Dimemsional Real Fourier Transform with or without
    temporary memory, Split Complex Format
            
      Criteria to invoke PowerPC vector code:  
        1. signal.realp and signal.imagp must be 16-byte aligned.
        2. signalStrideInRow = 1;
        3. signalStrideInCol must be a multiple of 4
        4. result.realp and result.imagp must be 16-byte aligned.
        5. strideResultInRow = 1;
        6. strideResultInCol must be a multiple of 4
        7. 3 <= log2nInRow <= 12
        8. 3 <= log2nInCol <= 13
        9. bufferTemp.realp and bufferTemp.imagp must be 16-byte aligned.

      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The size of temporary memory for each part
      is the lower value of 4*n and 16k.  ( log2n = log2nInRow + log2nInCol ) 
      Direction can be either kFFTDirection_Forward or kFFTDirection_Inverse.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  fft2d_zrop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zrop(setup: FFTSetup; var signal: DSPSplitComplex; signalStrideInRow: SInt32; signalStrideInCol: SInt32; var result: DSPSplitComplex; strideResultInRow: SInt32; strideResultInCol: SInt32; log2nInCol: UInt32; log2nInRow: UInt32; flag: SInt32); external name '_fft2d_zrop';

{
 *  fft2d_zropt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure fft2d_zropt(setup: FFTSetup; var signal: DSPSplitComplex; signalStrideInRow: SInt32; signalStrideInCol: SInt32; var result: DSPSplitComplex; strideResultInRow: SInt32; strideResultInCol: SInt32; var bufferTemp: DSPSplitComplex; log2nInCol: UInt32; log2nInRow: UInt32; flag: SInt32); external name '_fft2d_zropt';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function conv
    
    Floating Point Convolution and Correlation
      
      Criteria to invoke PowerPC vector code:  
        1. signal and result must have relative alignement.
        2. 4 <= lenFilter <= 256
        3. lenResult > 36
        4. signalStride = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  strideFilter can be positive for
      correlation or negative for convolution.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  conv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure conv((*const*) var signal: Single; signalStride: SInt32; (*const*) var filter: Single; strideFilter: SInt32; var result: Single; strideResult: SInt32; lenResult: SInt32; lenFilter: SInt32); external name '_conv';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vadd
    
    Floating Point Add
    
      Criteria to invoke PowerPC vector code:  
        1. input1 and input2 and result are all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vadd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vadd((*const*) var input1: Single; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: Single; strideResult: SInt32; size: UInt32); external name '_vadd';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vsub
    
     Floating Point Substract
      
      Criteria to invoke PowerPC vector code:  
        1. input1 and input2 and result are all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vsub()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vsub((*const*) var input1: Single; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: Single; strideResult: SInt32; size: UInt32); external name '_vsub';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vmul
    
    Floating Point Multiply
    
      Criteria to invoke PowerPC vector code:  
        1. input1 and input2 and result must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vmul()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vmul((*const*) var input1: Single; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: Single; strideResult: SInt32; size: UInt32); external name '_vmul';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vsmul
    
    Floating Point - Scalar Multiply
    
      Criteria to invoke PowerPC vector code:  
        1. input1 and result are all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vsmul()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vsmul((*const*) var input1: Single; stride1: SInt32; (*const*) var input2: Single; var result: Single; strideResult: SInt32; size: UInt32); external name '_vsmul';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vsq
    
    Floating Point Square
      
      Criteria to invoke PowerPC vector code:  
        1. input and result are relatively aligned.
        2. size >= 8
        3. strideInput = 1
        4. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vsq()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vsq((*const*) var input: Single; strideInput: SInt32; var result: Single; strideResult: SInt32; size: UInt32); external name '_vsq';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vssq
    
    Floating Point Signed Square
      
      Criteria to invoke PowerPC vector code:  
        1. input and result must be all relatively aligned.
        2. size >= 8
        3. strideInput = 1
        4. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vssq()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vssq((*const*) var input: Single; strideInput: SInt32; var result: Single; strideResult: SInt32; size: UInt32); external name '_vssq';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function dotpr
    
    Floating Point Dot product
    
      Criteria to invoke PowerPC vector code:  
        1. input1 and input2 are relatively aligned.
        2. size >= 20
        3. stride1 = 1
        4. stride2 = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  dotpr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure dotpr((*const*) var input1: Single; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: Single; size: UInt32); external name '_dotpr';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function vam
    
    Floating Point vadd and Multiply
    
      Criteria to invoke PowerPC vector code:  
        1. input1, input2, input_3 and result are all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. stride_3 = 1
        6. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  vam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure vam((*const*) var input1: Single; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; (*const*) var input3: Single; stride3: SInt32; var result: Single; strideResult: SInt32; size: UInt32); external name '_vam';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zconv
    
    Split Complex Convolution and Correlation
      
      Criteria to invoke PowerPC vector code:  
        1. signal->realp, signal->imagp, result->realp, result->imagp
           must be relatively aligned.
        2. 4 <= lenFilter <= 128
        3. lenResult > 20
        4. signalStride = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  strideFilter can be positive for correlation
      or negative for convolution
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zconv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zconv(var signal: DSPSplitComplex; signalStride: SInt32; var filter: DSPSplitComplex; strideFilter: SInt32; var result: DSPSplitComplex; strideResult: SInt32; lenResult: SInt32; lenFilter: SInt32); external name '_zconv';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zvadd
    
    Split Complex vadd
      
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2.realp, input2.imagp,
           result.realp, result.imagp must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zvadd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zvadd(var input1: DSPSplitComplex; stride1: SInt32; var input2: DSPSplitComplex; stride2: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32); external name '_zvadd';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zvsub
    
    Split Complex Substract
      
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2.realp, input2.imagp,
           result.realp, result.imagp must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zvsub()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zvsub(var input1: DSPSplitComplex; stride1: SInt32; var input2: DSPSplitComplex; stride2: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32); external name '_zvsub';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zvmul
    
    Split Complex Multiply
      
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2.realp, input2.imagp,
           result.realp, result.imagp must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1

      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.  The conjugate value can be 1 or -1.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zvmul()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zvmul(var input1: DSPSplitComplex; stride1: SInt32; var input2: DSPSplitComplex; stride2: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32; conjugate: SInt32); external name '_zvmul';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zdotpr
    
    Split Complex Dot product
    
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2.realp, input2.imagp are all
           relatively aligned.
        2. size >= 20
        3. stride1 = 1
        4. stride2 = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zdotpr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zdotpr(var input1: DSPSplitComplex; stride1: SInt32; var input2: DSPSplitComplex; stride2: SInt32; var result: DSPSplitComplex; size: UInt32); external name '_zdotpr';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zidotpr
    
    Split Complex Inner Dot product
    
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2.realp, input2.imagp must be
           all relatively aligned.
        2. size >= 20
        3. stride1 = 1
        4. stride2 = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zidotpr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zidotpr(var input1: DSPSplitComplex; stride1: SInt32; var input2: DSPSplitComplex; stride2: SInt32; var result: DSPSplitComplex; size: UInt32); external name '_zidotpr';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zrdotpr
    
    Split Complex - Real Dot product
      
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2 are must be relatively aligned.
        2. size >= 16
        3. stride1 = 1
        4. stride2 = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zrdotpr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zrdotpr(var input1: DSPSplitComplex; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: DSPSplitComplex; size: UInt32); external name '_zrdotpr';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zvcma
    
    Split Complex Conjugate Multiply And vadd
    
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2.realp, input2.imagp,
          input_3.realp, input_3.imagp, result.realp, result.imagp
          must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. stride_3 = 1
        6. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zvcma()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zvcma(var input1: DSPSplitComplex; stride1: SInt32; var input2: DSPSplitComplex; stride2: SInt32; var input3: DSPSplitComplex; stride3: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32); external name '_zvcma';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zrvadd
    
    Split Complex - Real Add
      
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2, result.realp, result.imagp
           are all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zrvadd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zrvadd(var input1: DSPSplitComplex; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32); external name '_zrvadd';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zrvsub
    
    Split Complex - Real Substract
    
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2, result.realp, result.imagp
           must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zrvsub()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zrvsub(var input1: DSPSplitComplex; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32); external name '_zrvsub';


{
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
    Function zrvmul
    
    Split Complex - Real Multiply
    
      Criteria to invoke PowerPC vector code:  
        1. input1.realp, input1.imagp, input2, result.realp, result.imagp
           must be all relatively aligned.
        2. size >= 8
        3. stride1 = 1
        4. stride2 = 1
        5. strideResult = 1
      
      If any of the above criteria are not satisfied, the PowerPC scalor code
      implementation will be used.
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
}
{
 *  zrvmul()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in vecLib 1.0 and later
 *    CarbonLib:        not in Carbon, but vecLib is compatible with Carbon
 *    Mac OS X:         in version 10.0 and later
 }
procedure zrvmul(var input1: DSPSplitComplex; stride1: SInt32; (*const*) var input2: Single; stride2: SInt32; var result: DSPSplitComplex; strideResult: SInt32; size: UInt32); external name '_zrvmul';


{$ifc undefined USE_NONE_APPLE_STANDARD_DATATYPES}
{$setc USE_NONE_APPLE_STANDARD_DATATYPES := 1}
{$endc}

{$ifc USE_NONE_APPLE_STANDARD_DATATYPES}

const
	FFT_FORWARD					= 1;
	FFT_INVERSE					= -1;

	FFT_RADIX2					= 0;
	FFT_RADIX3					= 1;
	FFT_RADIX5					= 2;


type
	COMPLEX								= DSPComplex;
	COMPLEXPtr 							= ^COMPLEX;
	COMPLEX_SPLIT						= DSPSplitComplex;
	COMPLEX_SPLITPtr 					= ^COMPLEX_SPLIT;
{$endc}  {USE_NONE_APPLE_STANDARD_DATATYPES}

{$ALIGN MAC68K}


end.
