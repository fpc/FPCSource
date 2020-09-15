{!
	@file		AudioConverter.h
	@framework	AudioToolbox.framework
	@copyright	(c) 1985-2015 by Apple, Inc., all rights reserved.
    @abstract   API's to perform audio format conversions.
    
    @discussion
		AudioConverters convert between various linear PCM and compressed
		audio formats. Supported transformations include:

		- PCM float/integer/bit depth conversions
		- PCM sample rate conversion
		- PCM interleaving and deinterleaving
		- encoding PCM to compressed formats
		- decoding compressed formats to PCM

		A single AudioConverter may perform more than one
		of the above transformations.
}
{  Pascal Translation: Jonas Maebe <jonas@freepascal.org>, July 2019 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit AudioConverter;
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
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
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
uses MacTypes,CoreAudioTypes;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

//==================================================================================================

{!
    @header     AudioConverter.h
    
}

//=============================================================================
//  Includes
//=============================================================================


//CF_ASSUME_NONNULL_BEGIN


//=============================================================================
//  Theory of Operation
//=============================================================================

//=============================================================================
//  Types specific to the Audio Converter API
//=============================================================================

{!
    @typedef    AudioConverterRef
    @abstract   A reference to an AudioConverter object.
}
type
	AudioConverterRef = ^OpaqueAudioConverter; { an opaque type }
	OpaqueAudioConverter = record end;

type
	AudioConverterPropertyID = UInt32;

//=============================================================================
//  Standard Properties
//=============================================================================

{!
    @enum       AudioConverterPropertyID
    @abstract   The properties of an AudioConverter, accessible via AudioConverterGetProperty()
                and AudioConverterSetProperty().
    
    @constant   kAudioConverterPropertyMinimumInputBufferSize
                    a UInt32 that indicates the size in bytes of the smallest buffer of input
                    data that can be supplied via the AudioConverterInputProc or as the input to
                    AudioConverterConvertBuffer
    @constant   kAudioConverterPropertyMinimumOutputBufferSize
                    a UInt32 that indicates the size in bytes of the smallest buffer of output
                    data that can be supplied to AudioConverterFillComplexBuffer or as the output to
                    AudioConverterConvertBuffer
    @constant   kAudioConverterPropertyMaximumInputBufferSize
                    DEPRECATED. The AudioConverter input proc may be passed any number of packets of data.
                    If fewer are packets are returned than required, then the input proc will be called again.
                    If more packets are passed than required, they will remain in the client's buffer and be 
                    consumed as needed.
    @constant   kAudioConverterPropertyMaximumInputPacketSize
                    a UInt32 that indicates the size in bytes of the largest single packet of
                    data in the input format. This is mostly useful for variable bit rate
                    compressed data (decoders).
    @constant   kAudioConverterPropertyMaximumOutputPacketSize
                    a UInt32 that indicates the size in bytes of the largest single packet of
                    data in the output format. This is mostly useful for variable bit rate
                    compressed data (encoders).
    @constant   kAudioConverterPropertyCalculateInputBufferSize
                    a UInt32 that on input holds a size in bytes that is desired for the output
                    data. On output, it will hold the size in bytes of the input buffer required
                    to generate that much output data. Note that some converters cannot do this
                    calculation.
    @constant   kAudioConverterPropertyCalculateOutputBufferSize
                    a UInt32 that on input holds a size in bytes that is desired for the input
                    data. On output, it will hold the size in bytes of the output buffer
                    required to hold the output data that will be generated. Note that some
                    converters cannot do this calculation.
    @constant   kAudioConverterPropertyInputCodecParameters
                    The value of this property varies from format to format and is considered
                    private to the format. It is treated as a buffer of untyped data.
    @constant   kAudioConverterPropertyOutputCodecParameters
                    The value of this property varies from format to format and is considered
                    private to the format. It is treated as a buffer of untyped data.
    @constant   kAudioConverterSampleRateConverterAlgorithm
                    DEPRECATED: please use kAudioConverterSampleRateConverterComplexity instead
    @constant   kAudioConverterSampleRateConverterComplexity
                    An OSType that specifies the sample rate converter algorithm to use (as defined in
                    AudioToolbox/AudioUnitProperties.h)
    @constant   kAudioConverterSampleRateConverterQuality
                    A UInt32 that specifies rendering quality of the sample rate converter (see
                    enum constants below)
    @constant   kAudioConverterSampleRateConverterInitialPhase
                    A Float64 with value 0.0 <= x < 1.0 giving the initial subsample position of the 
                    sample rate converter.
    @constant   kAudioConverterCodecQuality
                    A UInt32 that specifies rendering quality of a codec (see enum constants
                    below)
    @constant   kAudioConverterPrimeMethod
                    a UInt32 specifying priming method (usually for sample-rate converter) see
                    explanation for struct AudioConverterPrimeInfo below along with enum
                    constants
    @constant   kAudioConverterPrimeInfo
                    A pointer to AudioConverterPrimeInfo (see explanation for struct
                    AudioConverterPrimeInfo below)
    @constant   kAudioConverterChannelMap
                    An array of SInt32's.  The size of the array is the number of output
                    channels, and each element specifies which input channel's data is routed to
                    that output channel (using a 0-based index of the input channels), or -1 if
                    no input channel is to be routed to that output channel.  The default
                    behavior is as follows. I = number of input channels, O = number of output
                    channels. When I > O, the first O inputs are routed to the first O outputs,
                    and the remaining puts discarded.  When O > I, the first I inputs are routed
                    to the first O outputs, and the remaining outputs are zeroed.
                    
                    A simple example for splitting mono input to stereo output (instead of routing
                    the input to only the first output channel):
                    
<pre>
   // this should be as large as the number of output channels:
  SInt32 channelMap[2] = ( 0, 0 );
  AudioConverterSetProperty(theConverter, kAudioConverterChannelMap, 
    sizeof(channelMap), channelMap);
</pre>
    @constant   kAudioConverterDecompressionMagicCookie
                    A void * pointing to memory set up by the caller. Required by some formats
                    in order to decompress the input data.
    @constant   kAudioConverterCompressionMagicCookie
                    A void * pointing to memory set up by the caller. Returned by the converter
                    so that it may be stored along with the output data. It can then be passed
                    back to the converter for decompression at a later time.
    @constant   kAudioConverterEncodeBitRate
                    A UInt32 containing the number of bits per second to aim for when encoding
                    data. Some decoders will also allow you to get this property to discover the bit rate.
    @constant   kAudioConverterEncodeAdjustableSampleRate
                    For encoders where the AudioConverter was created with an output sample rate
                    of zero, and the codec can do rate conversion on its input, this provides a
                    way to set the output sample rate. The property value is a Float64.
    @constant   kAudioConverterInputChannelLayout
                    The property value is an AudioChannelLayout.
    @constant   kAudioConverterOutputChannelLayout
                    The property value is an AudioChannelLayout.
    @constant   kAudioConverterApplicableEncodeBitRates
                    The property value is an array of AudioValueRange describing applicable bit
                    rates based on current settings.
    @constant   kAudioConverterAvailableEncodeBitRates
                    The property value is an array of AudioValueRange describing available bit
                    rates based on the input format. You can get all available bit rates from
                    the AudioFormat API.
    @constant   kAudioConverterApplicableEncodeSampleRates
                    The property value is an array of AudioValueRange describing applicable
                    sample rates based on current settings.
    @constant   kAudioConverterAvailableEncodeSampleRates
                    The property value is an array of AudioValueRange describing available
                    sample rates based on the input format. You can get all available sample
                    rates from the AudioFormat API.
    @constant   kAudioConverterAvailableEncodeChannelLayoutTags
                    The property value is an array of AudioChannelLayoutTags for the format and
                    number of channels specified in the input format going to the encoder.
    @constant   kAudioConverterCurrentOutputStreamDescription
                    Returns the current completely specified output AudioStreamBasicDescription.
                    For example when encoding to AAC, your original output stream description
                    will not have been completely filled out.
    @constant   kAudioConverterCurrentInputStreamDescription
                    Returns the current completely specified input AudioStreamBasicDescription.
    @constant   kAudioConverterPropertySettings
                    Returns the a CFArray of property settings for converters.
    @constant   kAudioConverterPropertyBitDepthHint
                    An SInt32 of the source bit depth to preserve. This is a hint to some
                    encoders like lossless about how many bits to preserve in the input. The
                    converter usually tries to preserve as many as possible, but a lossless
                    encoder will do poorly if more bits are supplied than are desired in the
                    output. The bit depth is expressed as a negative number if the source was floating point,
                    e.g. -32 for float, -64 for double.
    @constant   kAudioConverterPropertyFormatList
                    An array of AudioFormatListItem structs describing all the data formats produced by the
                    encoder end of the AudioConverter. If the ioPropertyDataSize parameter indicates that
                    outPropertyData is sizeof(AudioFormatListItem), then only the best format is returned.
                    This property may be used for example to discover all the data formats produced by the AAC_HE2
                    (AAC High Efficiency vers. 2) encoder.
}
const
	kAudioConverterPropertyMinimumInputBufferSize = FourCharCode('mibs');
	kAudioConverterPropertyMinimumOutputBufferSize = FourCharCode('mobs');
	kAudioConverterPropertyMaximumInputBufferSize = FourCharCode('xibs');
	kAudioConverterPropertyMaximumInputPacketSize = FourCharCode('xips');
	kAudioConverterPropertyMaximumOutputPacketSize = FourCharCode('xops');
	kAudioConverterPropertyCalculateInputBufferSize = FourCharCode('cibs');
	kAudioConverterPropertyCalculateOutputBufferSize = FourCharCode('cobs');
	kAudioConverterPropertyInputCodecParameters = FourCharCode('icdp');
	kAudioConverterPropertyOutputCodecParameters = FourCharCode('ocdp');
	kAudioConverterSampleRateConverterAlgorithm = FourCharCode('srci');
	kAudioConverterSampleRateConverterComplexity = FourCharCode('srca');
	kAudioConverterSampleRateConverterQuality = FourCharCode('srcq');
	kAudioConverterSampleRateConverterInitialPhase = FourCharCode('srcp');
	kAudioConverterCodecQuality = FourCharCode('cdqu');
	kAudioConverterPrimeMethod = FourCharCode('prmm');
	kAudioConverterPrimeInfo = FourCharCode('prim');
	kAudioConverterChannelMap = FourCharCode('chmp');
	kAudioConverterDecompressionMagicCookie = FourCharCode('dmgc');
	kAudioConverterCompressionMagicCookie = FourCharCode('cmgc');
	kAudioConverterEncodeBitRate = FourCharCode('brat');
	kAudioConverterEncodeAdjustableSampleRate = FourCharCode('ajsr');
	kAudioConverterInputChannelLayout = FourCharCode('icl ');
	kAudioConverterOutputChannelLayout = FourCharCode('ocl ');
	kAudioConverterApplicableEncodeBitRates = FourCharCode('aebr');
	kAudioConverterAvailableEncodeBitRates = FourCharCode('vebr');
	kAudioConverterApplicableEncodeSampleRates = FourCharCode('aesr');
	kAudioConverterAvailableEncodeSampleRates = FourCharCode('vesr');
	kAudioConverterAvailableEncodeChannelLayoutTags = FourCharCode('aecl');
	kAudioConverterCurrentOutputStreamDescription = FourCharCode('acod');
	kAudioConverterCurrentInputStreamDescription = FourCharCode('acid');
	kAudioConverterPropertySettings = FourCharCode('acps');
	kAudioConverterPropertyBitDepthHint = FourCharCode('acbd');
	kAudioConverterPropertyFormatList = FourCharCode('flst'); 


//=============================================================================
//  
//=============================================================================

{!
    @enum       Mac OS X AudioConverter Properties

    @constant   kAudioConverterPropertyDithering
					A UInt32. Set to a value from the enum of dithering algorithms below. 
					Zero means no dithering and is the default. (Mac OS X only.)
    @constant   kAudioConverterPropertyDitherBitDepth
					A UInt32. Dither is applied at this bit depth.  (Mac OS X only.)

}
const
	kAudioConverterPropertyDithering = FourCharCode('dith');
	kAudioConverterPropertyDitherBitDepth = FourCharCode('dbit'); 

{!
    @enum       Dithering algorithms

    @abstract   Constants to be used as the value for kAudioConverterPropertyDithering.

    @constant   kDitherAlgorithm_TPDF             Dither signal is generated by a white noise source with a triangular probability density function
    @constant   kDitherAlgorithm_NoiseShaping     Use a static, perceptually weighted noise shaped dither
}
const
	kDitherAlgorithm_TPDF = 1;
	kDitherAlgorithm_NoiseShaping = 2; 

{!
    @enum       Quality constants

    @abstract   Constants to be used with kAudioConverterSampleRateConverterQuality.

    @constant   kAudioConverterQuality_Max          maximum quality
    @constant   kAudioConverterQuality_High         high quality
    @constant   kAudioConverterQuality_Medium       medium quality
    @constant   kAudioConverterQuality_Low          low quality
    @constant   kAudioConverterQuality_Min          minimum quality
}
const
	kAudioConverterQuality_Max = $7F;
	kAudioConverterQuality_High = $60;
	kAudioConverterQuality_Medium = $40;
	kAudioConverterQuality_Low = $20;
	kAudioConverterQuality_Min = 0; 


{!
    @enum           Sample Rate Converter Complexity
    @constant       kAudioConverterSampleRateConverterComplexity_Linear
    @discussion         Linear interpolation. lowest quality, cheapest.
						InitialPhase and PrimeMethod properties are not operative with this mode.
    @constant       kAudioConverterSampleRateConverterComplexity_Normal
    @discussion         Normal quality sample rate conversion.
    @constant       kAudioConverterSampleRateConverterComplexity_Mastering
    @discussion         Mastering quality sample rate conversion. More expensive.
    @constant       kAudioConverterSampleRateConverterComplexity_MinimumPhase
    @discussion         Minimum phase impulse response. Stopband attenuation varies with quality setting.
                        The InitialPhase and PrimeMethod properties are not operative with this mode.
                        There are three levels of quality provided.
                            kAudioConverterQuality_Low (or Min)  : noise floor to -96 dB
                            kAudioConverterQuality_Medium        : noise floor to -144 dB
                            kAudioConverterQuality_High (or Max) : noise floor to -160 dB (this uses double precision internally)
						Quality equivalences to the other complexity modes are very roughly as follows:
							MinimumPhase Low    is somewhat better than Normal Medium.
							MinimumPhase Medium is similar to Normal Max.
							MinimumPhase High   is similar to Mastering Low.
						In general, MinimumPhase performs better than Normal and Mastering for the equivalent qualities listed above.
						MinimumPhase High is several times faster than Mastering Low.
 
}
const
	kAudioConverterSampleRateConverterComplexity_Linear = FourCharCode('line');    // linear interpolation
	kAudioConverterSampleRateConverterComplexity_Normal = FourCharCode('norm');    // normal quality range, the default
	kAudioConverterSampleRateConverterComplexity_Mastering = FourCharCode('bats');    // higher quality range, more expensive
	kAudioConverterSampleRateConverterComplexity_MinimumPhase = FourCharCode('minp'); 	// minimum phase impulse response.


{!
    @enum       Prime method constants

    @abstract   Constants to be used with kAudioConverterPrimeMethod.
    
    @constant   kConverterPrimeMethod_Pre
                    Primes with leading + trailing input frames.
    @constant   kConverterPrimeMethod_Normal
                    Only primes with trailing (zero latency). Leading frames are assumed to be
                    silence.
    @constant   kConverterPrimeMethod_None
                    Acts in "latency" mode. Both leading and trailing frames assumed to be
                    silence.
}
const
	kConverterPrimeMethod_Pre = 0;
	kConverterPrimeMethod_Normal = 1;
	kConverterPrimeMethod_None = 2; 

{!
    @struct     AudioConverterPrimeInfo
    @abstract   Specifies priming information.
    
    @field      leadingFrames
        Specifies the number of leading (previous) input frames, relative to the normal/desired
        start input frame, required by the converter to perform a high quality conversion. If
        using kConverterPrimeMethod_Pre, the client should "pre-seek" the input stream provided
        through the input proc by leadingFrames. If no frames are available previous to the
        desired input start frame (because, for example, the desired start frame is at the very
        beginning of available audio), then provide "leadingFrames" worth of initial zero frames
        in the input proc.  Do not "pre-seek" in the default case of
        kConverterPrimeMethod_Normal or when using kConverterPrimeMethod_None.

    @field      trailingFrames
        Specifies the number of trailing input frames (past the normal/expected end input frame)
        required by the converter to perform a high quality conversion.  The client should be
        prepared to provide this number of additional input frames except when using
        kConverterPrimeMethod_None. If no more frames of input are available in the input stream
        (because, for example, the desired end frame is at the end of an audio file), then zero
        (silent) trailing frames will be synthesized for the client.
            
    @discussion
        When using AudioConverterFillComplexBuffer() (either a single call or a series of calls), some
        conversions, particularly involving sample-rate conversion, ideally require a certain
        number of input frames previous to the normal start input frame and beyond the end of
        the last expected input frame in order to yield high-quality results.
        
        These are expressed in the leadingFrames and trailingFrames members of the structure.
        
        The very first call to AudioConverterFillComplexBuffer(), or first call after
        AudioConverterReset(), will request additional input frames beyond those normally
        expected in the input proc callback to fulfill this first AudioConverterFillComplexBuffer()
        request. The number of additional frames requested, depending on the prime method, will
        be approximately:

        <pre>
            kConverterPrimeMethod_Pre       leadingFrames + trailingFrames
            kConverterPrimeMethod_Normal    trailingFrames
            kConverterPrimeMethod_None      0
        </pre>

        Thus, in effect, the first input proc callback(s) may provide not only the leading
        frames, but also may "read ahead" by an additional number of trailing frames depending
        on the prime method.

        kConverterPrimeMethod_None is useful in a real-time application processing live input,
        in which case trailingFrames (relative to input sample rate) of through latency will be
        seen at the beginning of the output of the AudioConverter.  In other real-time
        applications such as DAW systems, it may be possible to provide these initial extra
        audio frames since they are stored on disk or in memory somewhere and
        kConverterPrimeMethod_Pre may be preferable.  The default method is
        kConverterPrimeMethod_Normal, which requires no pre-seeking of the input stream and
        generates no latency at the output.
}
type
	AudioConverterPrimeInfo = record
		leadingFrames: UInt32;
		trailingFrames: UInt32;
	end;
	AudioConverterPrimeInfoPtr = ^AudioConverterPrimeInfo;

//=============================================================================
//  Errors
//=============================================================================
const
	kAudioConverterErr_FormatNotSupported = FourCharCode('fmt?');
	kAudioConverterErr_OperationNotSupported = $6F703F3F;  // 'op??', integer used because of trigraph
	kAudioConverterErr_PropertyNotSupported = FourCharCode('prop');
	kAudioConverterErr_InvalidInputSize = FourCharCode('insz');
	kAudioConverterErr_InvalidOutputSize = FourCharCode('otsz'); 
        // e.g. byte size is not a multiple of the frame size
	kAudioConverterErr_UnspecifiedError = FourCharCode('what');
	kAudioConverterErr_BadPropertySizeError = FourCharCode('!siz');
	kAudioConverterErr_RequiresPacketDescriptionsError = FourCharCode('!pkd');
	kAudioConverterErr_InputSampleRateOutOfRange = FourCharCode('!isr');
	kAudioConverterErr_OutputSampleRateOutOfRange = FourCharCode('!osr'); 


//=============================================================================
//  Routines
//=============================================================================

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterNew
    @abstract   Create a new AudioConverter.

    @param      inSourceFormat
                    The format of the source audio to be converted.
    @param      inDestinationFormat
                    The destination format to which the audio is to be converted.
    @param      outAudioConverter
                    On successful return, points to a new AudioConverter instance.
    @result     An OSStatus result code.
    
    @discussion
                For a pair of linear PCM formats, the following conversions
                are supported:
                
                <ul>
                <li>addition and removal of channels, when the stream descriptions'
                mChannelsPerFrame does not match. Channels may also be reordered and removed
                using the kAudioConverterChannelMap property.</li>
                <li>sample rate conversion</li>
                <li>interleaving/deinterleaving, when the stream descriptions' (mFormatFlags &
                kAudioFormatFlagIsNonInterleaved) does not match.</li>
                <li>conversion between any pair of the following formats:</li>
                    <ul>
                    <li>8 bit integer, signed or unsigned</li>
                    <li>16, 24, or 32-bit integer, big- or little-endian. Other integral
                    bit depths, if high-aligned and non-packed, are also supported</li>
                    <li>32 and 64-bit float, big- or little-endian.</li>
                    </ul>
                </ul>
                
                Also, encoding and decoding between linear PCM and compressed formats is
                supported. Functions in AudioToolbox/AudioFormat.h return information about the
                supported formats. When using a codec, you can use any supported PCM format (as
                above); the converter will perform any necessary additional conversion between
                your PCM format and the one created or consumed by the codec.
}
function AudioConverterNew( const (*var*) inSourceFormat: AudioStreamBasicDescription; const (*var*) inDestinationFormat: AudioStreamBasicDescription; var outAudioConverter: AudioConverterRef {__nullable * __nonnull} ): OSStatus; external name '_AudioConverterNew';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)


//-----------------------------------------------------------------------------
{!
    @function   AudioConverterNewSpecific
    @abstract   Create a new AudioConverter using specific codecs.

    @param      inSourceFormat
                    The format of the source audio to be converted.
    @param      inDestinationFormat
                    The destination format to which the audio is to be converted.
    @param      inNumberClassDescriptions
                    The number of class descriptions.
    @param      inClassDescriptions
                    AudioClassDescriptions specifiying the codec to instantiate.
    @param      outAudioConverter
                    On successful return, points to a new AudioConverter instance.
    @result     An OSStatus result code.
    
    @discussion
                This function is identical to AudioConverterNew(), except that the client may
                explicitly choose which codec to instantiate if there is more than one choice.
}
function AudioConverterNewSpecific( const (*var*) inSourceFormat: AudioStreamBasicDescription; const (*var*) inDestinationFormat: AudioStreamBasicDescription; inNumberClassDescriptions: UInt32; const (*var*) inClassDescriptions: AudioClassDescription; var outAudioConverter: AudioConverterRef {__nullable * __nonnull} ): OSStatus; external name '_AudioConverterNewSpecific';
(* API_AVAILABLE(macos(10.4), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterDispose
    @abstract   Destroy an AudioConverter.

    @param      inAudioConverter
                    The AudioConverter to dispose.
    @result     An OSStatus result code.
}
function AudioConverterDispose( inAudioConverter: AudioConverterRef ): OSStatus; external name '_AudioConverterDispose';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterReset
    @abstract   Reset an AudioConverter

    @param      inAudioConverter
                    The AudioConverter to reset.
    @result     An OSStatus result code.
    
    @discussion
                Should be called whenever there is a discontinuity in the source audio stream
                being provided to the converter. This will flush any internal buffers in the
                converter.
}

function AudioConverterReset( inAudioConverter: AudioConverterRef ): OSStatus; external name '_AudioConverterReset';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterGetPropertyInfo
    @abstract   Returns information about an AudioConverter property.

    @param      inAudioConverter
                    The AudioConverter to query.
    @param      inPropertyID
                    The property to query.
    @param      outSize
                    If non-null, on exit, the maximum size of the property value in bytes.
    @param      outWritable
                    If non-null, on exit, indicates whether the property value is writable.
    @result     An OSStatus result code.
}
function AudioConverterGetPropertyInfo( inAudioConverter: AudioConverterRef; inPropertyID: AudioConverterPropertyID; outSize: UInt32Ptr {* __nullable}; outWritable: BooleanPtr {* __nullable} ): OSStatus; external name '_AudioConverterGetPropertyInfo';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterGetProperty
    @abstract   Returns an AudioConverter property value.

    @param      inAudioConverter
                    The AudioConverter to query.
    @param      inPropertyID
                    The property to fetch.
    @param      ioPropertyDataSize
                    On entry, the size of the memory pointed to by outPropertyData. On 
                    successful exit, the size of the property value.
    @param      outPropertyData
                    On exit, the property value.
    @result     An OSStatus result code.
}
function AudioConverterGetProperty( inAudioConverter: AudioConverterRef; inPropertyID: AudioConverterPropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioConverterGetProperty';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterSetProperty
    @abstract   Sets an AudioConverter property value.

    @param      inAudioConverter
                    The AudioConverter to modify.
    @param      inPropertyID
                    The property to set.
    @param      inPropertyDataSize
                    The size in bytes of the property value.
    @param      inPropertyData
                    Points to the new property value.
    @result     An OSStatus result code.
}
function AudioConverterSetProperty( inAudioConverter: AudioConverterRef; inPropertyID: AudioConverterPropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioConverterSetProperty';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @typedef    AudioConverterInputDataProc
    @abstract   Callback function for supplying input data to AudioConverterFillBuffer.

    @param      inAudioConverter
                    The AudioConverter requesting input.
    @param      ioDataSize
                    On entry, the minimum number of bytes of audio data the converter
                    would like in order to fulfill its current FillBuffer request.
                    On exit, the number of bytes of audio data actually being provided
                    for input, or 0 if there is no more input.
    @param      outData
                    On exit, *outData should point to the audio data being provided
                    for input.
    @param      inUserData
                    The inInputDataProcUserData parameter passed to AudioConverterFillBuffer().
    @result     An OSStatus result code.
    
    @discussion
                <b>NOTE:</b> This API is now deprecated, 
                use AudioConverterFillComplexBuffer instead.

                This callback function supplies input to AudioConverterFillBuffer.
                
                The AudioConverter requests a minimum amount of data (*ioDataSize). The callback
                may return any amount of data. If it is less than than the minimum, the callback
                will simply be called again in the near future.

                The callback supplies a pointer to a buffer of audio data. The callback is
                responsible for not freeing or altering this buffer until it is called again.
                
                If the callback returns an error, it must return zero bytes of data.
                AudioConverterFillBuffer will stop producing output and return whatever output
                has already been produced to its caller, along with the error code. This
                mechanism can be used when an input proc has temporarily run out of data, but
                has not yet reached end of stream.
}
type
	AudioConverterInputDataProc = function( inAudioConverter: AudioConverterRef; var ioDataSize: UInt32; var outData: UnivPtr {__nonnull * __nonnull}; inUserData: UnivPtr {__nullable} ): OSStatus;

//-----------------------------------------------------------------------------
(*
{!
    @function   AudioConverterFillBuffer
    @abstract   Converts data supplied by an input callback function.

    @param      inAudioConverter
                    The AudioConverter to use.
    @param      inInputDataProc
                    A callback function which supplies the input data.
    @param      inInputDataProcUserData
                    A value for the use of the callback function.
    @param      ioOutputDataSize
                    On entry, the size of the buffer pointed to by outOutputData.
                    On exit, the number of bytes written to outOutputData
    @param      outOutputData
                    The buffer into which the converted data is written.
    @result     An OSStatus result code.
    
    @discussion
                <b>NOTE:</b> This API is now deprecated, 
                use AudioConverterFillComplexBuffer instead.

                Produces a buffer of output data from an AudioConverter. The supplied input
                callback function is called whenever necessary.             
}
extern OSStatus
AudioConverterFillBuffer(   AudioConverterRef               inAudioConverter,
                            AudioConverterInputDataProc     inInputDataProc,
                            void * __nullable               inInputDataProcUserData,
                            UInt32 *                        ioOutputDataSize,
                            void *                          outOutputData)
                            
                                API_DEPRECATED("no longer supported", macos(10.1, 10.5)) API_UNAVAILABLE(ios, watchos, tvos);
*)
//-----------------------------------------------------------------------------
{!
    @function   AudioConverterConvertBuffer
    @abstract   Converts data from an input buffer to an output buffer.

    @param      inAudioConverter
                    The AudioConverter to use.
    @param      inInputDataSize
                    The size of the buffer inInputData.
    @param      inInputData
                    The input audio data buffer.
    @param      ioOutputDataSize
                    On entry, the size of the buffer outOutputData. On exit, the number of bytes
                    written to outOutputData.
    @param      outOutputData
                    The output data buffer.
    @result
                Produces a buffer of output data from an AudioConverter, using the supplied
                input buffer.
    @discussion
                <b>WARNING:</b> this function will fail for any conversion where there is a
                variable relationship between the input and output data buffer sizes. This
                includes sample rate conversions and most compressed formats. In these cases,
                use AudioConverterFillComplexBuffer. Generally this function is only appropriate for
                PCM-to-PCM conversions where there is no sample rate conversion.
}
function AudioConverterConvertBuffer( inAudioConverter: AudioConverterRef; inInputDataSize: UInt32; inInputData: {const} UnivPtr; var ioOutputDataSize: UInt32; outOutputData: UnivPtr ): OSStatus; external name '_AudioConverterConvertBuffer';
(* API_AVAILABLE(macos(10.1), ios(2.0), watchos(2.0), tvos(9.0)) *)

//-----------------------------------------------------------------------------
{!
    @typedef    AudioConverterComplexInputDataProc
    @abstract   Callback function for supplying input data to AudioConverterFillComplexBuffer.

    @param      inAudioConverter
                    The AudioConverter requesting input.
    @param      ioNumberDataPackets
                    On entry, the minimum number of packets of input audio data the converter
                    would like in order to fulfill its current FillBuffer request. On exit, the
                    number of packets of audio data actually being provided for input, or 0 if
                    there is no more input.
    @param      ioData
                    On exit, the members of ioData should be set to point to the audio data
                    being provided for input.
    @param      outDataPacketDescription
                    If non-null, on exit, the callback is expected to fill this in with
                    an AudioStreamPacketDescription for each packet of input data being provided.
    @param      inUserData
                    The inInputDataProcUserData parameter passed to AudioConverterFillComplexBuffer().
    @result     An OSStatus result code.
    
    @discussion
                This callback function supplies input to AudioConverterFillComplexBuffer.
                
                The AudioConverter requests a minimum number of packets (*ioNumberDataPackets).
                The callback may return one or more packets. If this is less than the minimum,
                the callback will simply be called again in the near future.

                The callback manipulates the members of ioData to point to one or more buffers
                of audio data (multiple buffers are used with non-interleaved PCM data). The
                callback is responsible for not freeing or altering this buffer until it is
                called again.

                If the callback returns an error, it must return zero packets of data.
                AudioConverterFillComplexBuffer will stop producing output and return whatever
                output has already been produced to its caller, along with the error code. This
                mechanism can be used when an input proc has temporarily run out of data, but
                has not yet reached end of stream.
}
type
	AudioConverterComplexInputDataProc = function( inAudioConverter: AudioConverterRef; var ioNumberDataPackets: UInt32; var ioData: AudioBufferList; outDataPacketDescription: AudioStreamPacketDescriptionPtrPtr {* __nullable * __nullable}; inUserData: UnivPtr {__nullable} ): OSStatus;

//-----------------------------------------------------------------------------
{!
    @function   AudioConverterFillComplexBuffer
    @abstract   Converts data supplied by an input callback function, supporting non-interleaved
                and packetized formats.

    @param      inAudioConverter
                    The AudioConverter to use.
    @param      inInputDataProc
                    A callback function which supplies the input data.
    @param      inInputDataProcUserData
                    A value for the use of the callback function.
    @param      ioOutputDataPacketSize
                    On entry, the capacity of outOutputData expressed in packets in the
                    converter's output format. On exit, the number of packets of converted
                    data that were written to outOutputData.
    @param      outOutputData
                    The converted output data is written to this buffer.
    @param      outPacketDescription
                    If non-null, and the converter's output uses packet descriptions, then
                    packet descriptions are written to this array. It must point to a memory
                    block capable of holding *ioOutputDataPacketSize packet descriptions.
                    (See AudioFormat.h for ways to determine whether an audio format
                    uses packet descriptions).
    @result     An OSStatus result code.

    @discussion
                Produces a buffer list of output data from an AudioConverter. The supplied input
                callback function is called whenever necessary.
}
function AudioConverterFillComplexBuffer( inAudioConverter: AudioConverterRef; inInputDataProc: AudioConverterComplexInputDataProc; inInputDataProcUserData: UnivPtr {__nullable}; var ioOutputDataPacketSize: UInt32; var outOutputData: AudioBufferList; outPacketDescription: AudioStreamPacketDescriptionPtr {* __nullable} ): OSStatus; external name '_AudioConverterFillComplexBuffer';
(* API_AVAILABLE(macos(10.2), ios(2.0), watchos(2.0), tvos(9.0)) *)


//-----------------------------------------------------------------------------
{!
    @function   AudioConverterConvertComplexBuffer
    @abstract   Converts PCM data from an input buffer list to an output buffer list.

    @param      inAudioConverter
                    The AudioConverter to use.
    @param      inNumberPCMFrames
                    The number of PCM frames to convert.
    @param      inInputData
                    The source audio buffer list.
    @param      outOutputData
                    The converted output data is written to this buffer list.
    @result     An OSStatus result code.
    @discussion
                <b>WARNING:</b> this function will fail for any conversion where there is a
                variable relationship between the input and output data buffer sizes. This
                includes sample rate conversions and most compressed formats. In these cases,
                use AudioConverterFillComplexBuffer. Generally this function is only appropriate for
                PCM-to-PCM conversions where there is no sample rate conversion.
}
function AudioConverterConvertComplexBuffer( inAudioConverter: AudioConverterRef; inNumberPCMFrames: UInt32; const (*var*) inInputData: AudioBufferList; var outOutputData: AudioBufferList ): OSStatus; external name '_AudioConverterConvertComplexBuffer';
(* API_AVAILABLE(macos(10.7), ios(5.0), watchos(2.0), tvos(9.0)) *)


//CF_ASSUME_NONNULL_END
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
