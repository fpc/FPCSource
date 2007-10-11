{
     File:       ImageCompression.p
 
     Contains:   QuickTime Image Compression Interfaces.
 
     Version:    Technology: QuickTime 6.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1990-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit ImageCompression;
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
uses MacTypes,Files,OSUtils,Quickdraw,QDOffscreen,Components,Dialogs;

{$ALIGN MAC68K}


type
	MatrixRecordPtr = ^MatrixRecord;
	MatrixRecord = record
		matrix:					array [0..2,0..2] of Fixed;
	end;


const
	kRawCodecType				= $72617720 (* 'raw ' *);
	kCinepakCodecType			= $63766964 (* 'cvid' *);
	kGraphicsCodecType			= $736D6320 (* 'smc ' *);
	kAnimationCodecType			= $726C6520 (* 'rle ' *);
	kVideoCodecType				= $72707A61 (* 'rpza' *);
	kComponentVideoCodecType	= $79757632 (* 'yuv2' *);
	kJPEGCodecType				= $6A706567 (* 'jpeg' *);
	kMotionJPEGACodecType		= $6D6A7061 (* 'mjpa' *);
	kMotionJPEGBCodecType		= $6D6A7062 (* 'mjpb' *);
	kSGICodecType				= $2E534749 (* '.SGI' *);
	kPlanarRGBCodecType			= $38425053 (* '8BPS' *);
	kMacPaintCodecType			= $504E5447 (* 'PNTG' *);
	kGIFCodecType				= $67696620 (* 'gif ' *);
	kPhotoCDCodecType			= $6B706364 (* 'kpcd' *);
	kQuickDrawGXCodecType		= $71646778 (* 'qdgx' *);
	kAVRJPEGCodecType			= $61767220 (* 'avr ' *);
	kOpenDMLJPEGCodecType		= $646D6231 (* 'dmb1' *);
	kBMPCodecType				= $57524C45 (* 'WRLE' *);
	kWindowsRawCodecType		= $57524157 (* 'WRAW' *);
	kVectorCodecType			= $70617468 (* 'path' *);
	kQuickDrawCodecType			= $71647277 (* 'qdrw' *);
	kWaterRippleCodecType		= $7269706C (* 'ripl' *);
	kFireCodecType				= $66697265 (* 'fire' *);
	kCloudCodecType				= $636C6F75 (* 'clou' *);
	kH261CodecType				= $68323631 (* 'h261' *);
	kH263CodecType				= $68323633 (* 'h263' *);
	kDVCNTSCCodecType			= $64766320 (* 'dvc ' *);						{  DV - NTSC and DVCPRO NTSC (available in QuickTime 6.0 or later) }
																{  NOTE: kDVCProNTSCCodecType is deprecated.   }
																{  Use kDVCNTSCCodecType instead -- as far as the codecs are concerned,  }
																{  the two data formats are identical. }
	kDVCPALCodecType			= $64766370 (* 'dvcp' *);
	kDVCProPALCodecType			= $64767070 (* 'dvpp' *);						{  available in QuickTime 6.0 or later }
	kBaseCodecType				= $62617365 (* 'base' *);
	kFLCCodecType				= $666C6963 (* 'flic' *);
	kTargaCodecType				= $74676120 (* 'tga ' *);
	kPNGCodecType				= $706E6720 (* 'png ' *);
	kTIFFCodecType				= $74696666 (* 'tiff' *);						{     NOTE: despite what might seem obvious from the two constants }
																{     below and their names, they really are correct. 'yuvu' really  }
																{     does mean signed, and 'yuvs' really does mean unsigned. Really.  }
	kComponentVideoSigned		= $79757675 (* 'yuvu' *);
	kComponentVideoUnsigned		= $79757673 (* 'yuvs' *);
	kCMYKCodecType				= $636D796B (* 'cmyk' *);
	kMicrosoftVideo1CodecType	= $6D737663 (* 'msvc' *);
	kSorensonCodecType			= $53565131 (* 'SVQ1' *);
	kSorenson3CodecType			= $53565133 (* 'SVQ3' *);						{  available in QuickTime 5 and later }
	kIndeo4CodecType			= $49563431 (* 'IV41' *);
	kMPEG4VisualCodecType		= $6D703476 (* 'mp4v' *);
	k64ARGBCodecType			= $62363461 (* 'b64a' *);
	k48RGBCodecType				= $62343872 (* 'b48r' *);
	k32AlphaGrayCodecType		= $62333261 (* 'b32a' *);
	k16GrayCodecType			= $62313667 (* 'b16g' *);
	kMpegYUV420CodecType		= $6D797576 (* 'myuv' *);
	kYUV420CodecType			= $79343230 (* 'y420' *);
	kSorensonYUV9CodecType		= $73797639 (* 'syv9' *);
	k422YpCbCr8CodecType		= $32767579 (* '2vuy' *);						{  Component Y'CbCr 8-bit 4:2:2   }
	k444YpCbCr8CodecType		= $76333038 (* 'v308' *);						{  Component Y'CbCr 8-bit 4:4:4   }
	k4444YpCbCrA8CodecType		= $76343038 (* 'v408' *);						{  Component Y'CbCrA 8-bit 4:4:4:4  }
	k422YpCbCr16CodecType		= $76323136 (* 'v216' *);						{  Component Y'CbCr 10,12,14,16-bit 4:2:2 }
	k422YpCbCr10CodecType		= $76323130 (* 'v210' *);						{  Component Y'CbCr 10-bit 4:2:2  }
	k444YpCbCr10CodecType		= $76343130 (* 'v410' *);						{  Component Y'CbCr 10-bit 4:4:4  }
	k4444YpCbCrA8RCodecType		= $72343038 (* 'r408' *);						{  Component Y'CbCrA 8-bit 4:4:4:4, rendering format. full range alpha, zero biased yuv }


	{	 one source effects 	}
	kBlurImageFilterType		= $626C7572 (* 'blur' *);
	kSharpenImageFilterType		= $73687270 (* 'shrp' *);
	kEdgeDetectImageFilterType	= $65646765 (* 'edge' *);
	kEmbossImageFilterType		= $656D6273 (* 'embs' *);
	kConvolveImageFilterType	= $67656E6B (* 'genk' *);
	kAlphaGainImageFilterType	= $6761696E (* 'gain' *);
	kRGBColorBalanceImageFilterType = $72676262 (* 'rgbb' *);
	kHSLColorBalanceImageFilterType = $68736C62 (* 'hslb' *);
	kColorSyncImageFilterType	= $73796E63 (* 'sync' *);
	kFilmNoiseImageFilterType	= $666D6E73 (* 'fmns' *);
	kSolarizeImageFilterType	= $736F6C72 (* 'solr' *);
	kColorTintImageFilterType	= $74696E74 (* 'tint' *);
	kLensFlareImageFilterType	= $6C656E73 (* 'lens' *);
	kBrightnessContrastImageFilterType = $6272636F (* 'brco' *);

	{	 two source effects 	}
	kAlphaCompositorTransitionType = $626C6E64 (* 'blnd' *);
	kCrossFadeTransitionType	= $64736C76 (* 'dslv' *);
	kChannelCompositeEffectType	= $6368616E (* 'chan' *);
	kChromaKeyTransitionType	= $636B6579 (* 'ckey' *);
	kImplodeTransitionType		= $6D706C6F (* 'mplo' *);
	kExplodeTransitionType		= $78706C6F (* 'xplo' *);
	kGradientTransitionType		= $6D617474 (* 'matt' *);
	kPushTransitionType			= $70757368 (* 'push' *);
	kSlideTransitionType		= $736C6964 (* 'slid' *);
	kWipeTransitionType			= $736D7074 (* 'smpt' *);
	kIrisTransitionType			= $736D7032 (* 'smp2' *);
	kRadialTransitionType		= $736D7033 (* 'smp3' *);
	kMatrixTransitionType		= $736D7034 (* 'smp4' *);
	kZoomTransitionType			= $7A6F6F6D (* 'zoom' *);

	{	 three source effects 	}
	kTravellingMatteEffectType	= $74726176 (* 'trav' *);


	{	 Supported by QTNewGWorld in QuickTime 4.0 and later 	}
	kCMYKPixelFormat			= $636D796B (* 'cmyk' *);						{  CMYK, 8-bit  }
	k64ARGBPixelFormat			= $62363461 (* 'b64a' *);						{  ARGB, 16-bit big-endian samples  }
	k48RGBPixelFormat			= $62343872 (* 'b48r' *);						{  RGB, 16-bit big-endian samples  }
	k32AlphaGrayPixelFormat		= $62333261 (* 'b32a' *);						{  AlphaGray, 16-bit big-endian samples  }
	k16GrayPixelFormat			= $62313667 (* 'b16g' *);						{  Grayscale, 16-bit big-endian samples  }
	k422YpCbCr8PixelFormat		= $32767579 (* '2vuy' *);						{  Component Y'CbCr 8-bit 4:2:2, ordered Cb Y'0 Cr Y'1  }

	{	 Supported by QTNewGWorld in QuickTime 4.1.2 and later 	}
	k4444YpCbCrA8PixelFormat	= $76343038 (* 'v408' *);						{  Component Y'CbCrA 8-bit 4:4:4:4, ordered Cb Y' Cr A  }
	k4444YpCbCrA8RPixelFormat	= $72343038 (* 'r408' *);						{  Component Y'CbCrA 8-bit 4:4:4:4, rendering format. full range alpha, zero biased yuv, ordered A Y' Cb Cr  }

	{	 Supported by QTNewGWorld in QuickTime 6.0 and later 	}
	kYUV420PixelFormat			= $79343230 (* 'y420' *);						{  Planar Component Y'CbCr 8-bit 4:2:0.  PixMap baseAddr points to a big-endian PlanarPixmapInfoYUV420 struct; see ImageCodec.i.  }


	{	 These are the bits that are set in the Component flags, and also in the codecInfo struct. 	}
	codecInfoDoes1				= $00000001;					{  codec can work with 1-bit pixels  }
	codecInfoDoes2				= $00000002;					{  codec can work with 2-bit pixels  }
	codecInfoDoes4				= $00000004;					{  codec can work with 4-bit pixels  }
	codecInfoDoes8				= $00000008;					{  codec can work with 8-bit pixels  }
	codecInfoDoes16				= $00000010;					{  codec can work with 16-bit pixels  }
	codecInfoDoes32				= $00000020;					{  codec can work with 32-bit pixels  }
	codecInfoDoesDither			= $00000040;					{  codec can do ditherMode  }
	codecInfoDoesStretch		= $00000080;					{  codec can stretch to arbitrary sizes  }
	codecInfoDoesShrink			= $00000100;					{  codec can shrink to arbitrary sizes  }
	codecInfoDoesMask			= $00000200;					{  codec can mask to clipping regions  }
	codecInfoDoesTemporal		= $00000400;					{  codec can handle temporal redundancy  }
	codecInfoDoesDouble			= $00000800;					{  codec can stretch to double size exactly  }
	codecInfoDoesQuad			= $00001000;					{  codec can stretch to quadruple size exactly  }
	codecInfoDoesHalf			= $00002000;					{  codec can shrink to half size  }
	codecInfoDoesQuarter		= $00004000;					{  codec can shrink to quarter size  }
	codecInfoDoesRotate			= $00008000;					{  codec can rotate on decompress  }
	codecInfoDoesHorizFlip		= $00010000;					{  codec can flip horizontally on decompress  }
	codecInfoDoesVertFlip		= $00020000;					{  codec can flip vertically on decompress  }
	codecInfoHasEffectParameterList = $00040000;				{  codec implements get effects parameter list call, once was codecInfoDoesSkew  }
	codecInfoDoesBlend			= $00080000;					{  codec can blend on decompress  }
	codecInfoDoesWarp			= $00100000;					{  codec can warp arbitrarily on decompress  }
	codecInfoDoesRecompress		= $00200000;					{  codec can recompress image without accumulating errors  }
	codecInfoDoesSpool			= $00400000;					{  codec can spool image data  }
	codecInfoDoesRateConstrain	= $00800000;					{  codec can data rate constrain  }


	codecInfoDepth1				= $00000001;					{  compressed data at 1 bpp depth available  }
	codecInfoDepth2				= $00000002;					{  compressed data at 2 bpp depth available  }
	codecInfoDepth4				= $00000004;					{  compressed data at 4 bpp depth available  }
	codecInfoDepth8				= $00000008;					{  compressed data at 8 bpp depth available  }
	codecInfoDepth16			= $00000010;					{  compressed data at 16 bpp depth available  }
	codecInfoDepth32			= $00000020;					{  compressed data at 32 bpp depth available  }
	codecInfoDepth24			= $00000040;					{  compressed data at 24 bpp depth available  }
	codecInfoDepth33			= $00000080;					{  compressed data at 1 bpp monochrome depth  available  }
	codecInfoDepth34			= $00000100;					{  compressed data at 2 bpp grayscale depth available  }
	codecInfoDepth36			= $00000200;					{  compressed data at 4 bpp grayscale depth available  }
	codecInfoDepth40			= $00000400;					{  compressed data at 8 bpp grayscale depth available  }
	codecInfoStoresClut			= $00000800;					{  compressed data can have custom cluts  }
	codecInfoDoesLossless		= $00001000;					{  compressed data can be stored in lossless format  }
	codecInfoSequenceSensitive	= $00002000;					{  compressed data is sensitive to out of sequence decoding  }


	{  input sequence flags }
	codecFlagUseImageBuffer		= $00000001;					{  decompress }
	codecFlagUseScreenBuffer	= $00000002;					{  decompress }
	codecFlagUpdatePrevious		= $00000004;					{  compress }
	codecFlagNoScreenUpdate		= $00000008;					{  decompress }
	codecFlagWasCompressed		= $00000010;					{  compress }
	codecFlagDontOffscreen		= $00000020;					{  decompress }
	codecFlagUpdatePreviousComp	= $00000040;					{  compress }
	codecFlagForceKeyFrame		= $00000080;					{  compress }
	codecFlagOnlyScreenUpdate	= $00000100;					{  decompress }
	codecFlagLiveGrab			= $00000200;					{  compress }
	codecFlagDiffFrame			= $00000200;					{  decompress }
	codecFlagDontUseNewImageBuffer = $00000400;					{  decompress }
	codecFlagInterlaceUpdate	= $00000800;					{  decompress }
	codecFlagCatchUpDiff		= $00001000;					{  decompress }
	codecFlagSupportDisable		= $00002000;					{  decompress }
	codecFlagReenable			= $00004000;					{  decompress }


	{  output sequence flags }
	codecFlagOutUpdateOnNextIdle = $00000200;
	codecFlagOutUpdateOnDataSourceChange = $00000400;
	codecFlagSequenceSensitive	= $00000800;
	codecFlagOutUpdateOnTimeChange = $00001000;
	codecFlagImageBufferNotSourceImage = $00002000;
	codecFlagUsedNewImageBuffer	= $00004000;
	codecFlagUsedImageBuffer	= $00008000;


																{  The minimum data size for spooling in or out data  }
	codecMinimumDataSize		= 32768;


	compressorComponentType		= $696D636F (* 'imco' *);						{  the type for "Components" which compress images  }
	decompressorComponentType	= $696D6463 (* 'imdc' *);						{  the type for "Components" which decompress images  }


type
	CompressorComponent					= Component;
	DecompressorComponent				= Component;
	CodecComponent						= Component;

const
	anyCodec					= 0;							{  take first working codec of given type  }
	bestSpeedCodec				= -1;							{  take fastest codec of given type  }
	bestFidelityCodec			= -2;							{  take codec which is most accurate  }
	bestCompressionCodec		= -3;							{  take codec of given type that is most accurate  }


type
	CodecType							= OSType;
	CodecType_fix                       = CodecType; { used as field type when a record declaration contains a CodecType field identifier }
	CodecFlags							= UInt16;
	CodecQ								= UInt32;

const
	codecLosslessQuality		= $00000400;
	codecMaxQuality				= $000003FF;
	codecMinQuality				= $00000000;
	codecLowQuality				= $00000100;
	codecNormalQuality			= $00000200;
	codecHighQuality			= $00000300;

	codecLockBitsShieldCursor	= $01;							{  shield cursor  }

	codecCompletionSource		= $01;							{  asynchronous codec is done with source data  }
	codecCompletionDest			= $02;							{  asynchronous codec is done with destination data  }
	codecCompletionDontUnshield	= $04;							{  on dest complete don't unshield cursor  }
	codecCompletionWentOffscreen = $08;							{  codec used offscreen buffer  }
	codecCompletionUnlockBits	= $10;							{  on dest complete, call ICMSequenceUnlockBits  }
	codecCompletionForceChainFlush = $20;						{  ICM needs to flush the whole chain  }
	codecCompletionDropped		= $40;							{  codec decided to drop this frame  }

	codecProgressOpen			= 0;
	codecProgressUpdatePercent	= 1;
	codecProgressClose			= 2;


type
{$ifc TYPED_FUNCTION_POINTERS}
	ICMDataProcPtr = function(var dataP: Ptr; bytesNeeded: SInt32; refcon: SInt32): OSErr;
{$elsec}
	ICMDataProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ICMFlushProcPtr = function(data: Ptr; bytesAdded: SInt32; refcon: SInt32): OSErr;
{$elsec}
	ICMFlushProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ICMCompletionProcPtr = procedure(result: OSErr; flags: SInt16; refcon: SInt32);
{$elsec}
	ICMCompletionProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ICMProgressProcPtr = function(message: SInt16; completeness: Fixed; refcon: SInt32): OSErr;
{$elsec}
	ICMProgressProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	StdPixProcPtr = procedure(var src: PixMap; var srcRect: Rect; var matrix: MatrixRecord; mode: SInt16; mask: RgnHandle; var matte: PixMap; var matteRect: Rect; flags: SInt16);
{$elsec}
	StdPixProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	QDPixProcPtr = procedure(var src: PixMap; var srcRect: Rect; var matrix: MatrixRecord; mode: SInt16; mask: RgnHandle; var matte: PixMap; var matteRect: Rect; flags: SInt16);
{$elsec}
	QDPixProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ICMAlignmentProcPtr = procedure(var rp: Rect; refcon: SInt32);
{$elsec}
	ICMAlignmentProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ICMCursorShieldedProcPtr = procedure(const (*var*) r: Rect; refcon: UnivPtr; flags: SInt32);
{$elsec}
	ICMCursorShieldedProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ICMMemoryDisposedProcPtr = procedure(memoryBlock: Ptr; refcon: UnivPtr);
{$elsec}
	ICMMemoryDisposedProcPtr = ProcPtr;
{$endc}

	ICMCursorNotify						= Ptr;
{$ifc TYPED_FUNCTION_POINTERS}
	ICMConvertDataFormatProcPtr = function(refCon: UnivPtr; flags: SInt32; desiredFormat: Handle; sourceDataFormat: Handle; srcData: UnivPtr; srcDataSize: SInt32; var dstData: UnivPtr; var dstDataSize: SInt32): OSErr;
{$elsec}
	ICMConvertDataFormatProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	ICMDataUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMDataUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMFlushUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMFlushUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMCompletionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMCompletionUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMProgressUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMProgressUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	StdPixUPP = ^SInt32; { an opaque UPP }
{$elsec}
	StdPixUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	QDPixUPP = ^SInt32; { an opaque UPP }
{$elsec}
	QDPixUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMAlignmentUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMAlignmentUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMCursorShieldedUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMCursorShieldedUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMMemoryDisposedUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMMemoryDisposedUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	ICMConvertDataFormatUPP = ^SInt32; { an opaque UPP }
{$elsec}
	ICMConvertDataFormatUPP = UniversalProcPtr;
{$endc}	
	ImageSequence						= SInt32;
	ImageSequenceDataSource				= SInt32;
	ImageTranscodeSequence				= SInt32;
	ImageFieldSequence					= SInt32;
	ICMProgressProcRecordPtr = ^ICMProgressProcRecord;
	ICMProgressProcRecord = record
		progressProc:			ICMProgressUPP;
		progressRefCon:			SInt32;
	end;

	ICMCompletionProcRecordPtr = ^ICMCompletionProcRecord;
	ICMCompletionProcRecord = record
		completionProc:			ICMCompletionUPP;
		completionRefCon:		SInt32;
	end;

	ICMDataProcRecordPtr = ^ICMDataProcRecord;
	ICMDataProcRecord = record
		dataProc:				ICMDataUPP;
		dataRefCon:				SInt32;
	end;

	ICMFlushProcRecordPtr = ^ICMFlushProcRecord;
	ICMFlushProcRecord = record
		flushProc:				ICMFlushUPP;
		flushRefCon:			SInt32;
	end;

	ICMAlignmentProcRecordPtr = ^ICMAlignmentProcRecord;
	ICMAlignmentProcRecord = record
		alignmentProc:			ICMAlignmentUPP;
		alignmentRefCon:		SInt32;
	end;

	DataRateParamsPtr = ^DataRateParams;
	DataRateParams = record
		dataRate:				SInt32;
		dataOverrun:			SInt32;
		frameDuration:			SInt32;
		keyFrameRate:			SInt32;
		minSpatialQuality:		CodecQ;
		minTemporalQuality:		CodecQ;
	end;

	ImageDescriptionPtr = ^ImageDescription;
	ImageDescription = packed record
		idSize:					SInt32;								{  total size of ImageDescription including extra data ( CLUTs and other per sequence data )  }
		cType:					CodecType;								{  what kind of codec compressed this data  }
		resvd1:					SInt32;								{  reserved for Apple use  }
		resvd2:					SInt16;								{  reserved for Apple use  }
		dataRefIndex:			SInt16;								{  set to zero   }
		version:				SInt16;								{  which version is this data  }
		revisionLevel:			SInt16;								{  what version of that codec did this  }
		vendor:					SInt32;								{  whose  codec compressed this data  }
		temporalQuality:		CodecQ;									{  what was the temporal quality factor   }
		spatialQuality:			CodecQ;									{  what was the spatial quality factor  }
		width:					SInt16;								{  how many pixels wide is this data  }
		height:					SInt16;								{  how many pixels high is this data  }
		hRes:					Fixed;									{  horizontal resolution  }
		vRes:					Fixed;									{  vertical resolution  }
		dataSize:				SInt32;								{  if known, the size of data for this image descriptor  }
		frameCount:				SInt16;								{  number of frames this description applies to  }
		name:					Str31;									{  name of codec ( in case not installed )   }
		depth:					SInt16;								{  what depth is this data (1-32) or ( 33-40 grayscale )  }
		clutID:					SInt16;								{  clut id or if 0 clut follows  or -1 if no clut  }
	end;

	ImageDescriptionHandle				= ^ImageDescriptionPtr;
	CodecInfoPtr = ^CodecInfo;
	CodecInfo = packed record
		typeName:				Str31;									{  name of the codec type i.e.: 'Apple Image Compression'  }
		version:				SInt16;								{  version of the codec data that this codec knows about  }
		revisionLevel:			SInt16;								{  revision level of this codec i.e: 0x00010001 (1.0.1)  }
		vendor:					SInt32;								{  Maker of this codec i.e: 'appl'  }
		decompressFlags:		SInt32;								{  codecInfo flags for decompression capabilities  }
		compressFlags:			SInt32;								{  codecInfo flags for compression capabilities  }
		formatFlags:			SInt32;								{  codecInfo flags for compression format details  }
		compressionAccuracy:	UInt8;									{  measure (1-255) of accuracy of this codec for compress (0 if unknown)  }
		decompressionAccuracy:	UInt8;									{  measure (1-255) of accuracy of this codec for decompress (0 if unknown)  }
		compressionSpeed:		UInt16;									{  ( millisecs for compressing 320x240 on base mac II) (0 if unknown)   }
		decompressionSpeed:		UInt16;									{  ( millisecs for decompressing 320x240 on mac II)(0 if unknown)   }
		compressionLevel:		UInt8;									{  measure (1-255) of compression level of this codec (0 if unknown)   }
		resvd:					UInt8;									{  pad  }
		minimumHeight:			SInt16;								{  minimum height of image (block size)  }
		minimumWidth:			SInt16;								{  minimum width of image (block size)  }
		decompressPipelineLatency: SInt16;								{  in milliseconds ( for asynchronous codecs )  }
		compressPipelineLatency: SInt16;								{  in milliseconds ( for asynchronous codecs )  }
		privateData:			SInt32;
	end;

	CodecNameSpecPtr = ^CodecNameSpec;
	CodecNameSpec = record
		codec:					CodecComponent;
		cType:					CodecType;
		typeName:				Str31;
		name:					Handle;
	end;

	CodecNameSpecListPtr = ^CodecNameSpecList;
	CodecNameSpecList = record
		count:					SInt16;
		list:					array [0..0] of CodecNameSpec;
	end;


const
	defaultDither				= 0;
	forceDither					= 1;
	suppressDither				= 2;
	useColorMatching			= 4;

	callStdBits					= 1;
	callOldBits					= 2;
	noDefaultOpcodes			= 4;

	graphicsModeStraightAlpha	= 256;
	graphicsModePreWhiteAlpha	= 257;
	graphicsModePreBlackAlpha	= 258;
	graphicsModeComposition		= 259;
	graphicsModeStraightAlphaBlend = 260;
	graphicsModePreMulColorAlpha = 261;

	evenField1ToEvenFieldOut	= $01;
	evenField1ToOddFieldOut		= $02;
	oddField1ToEvenFieldOut		= $04;
	oddField1ToOddFieldOut		= $08;
	evenField2ToEvenFieldOut	= $10;
	evenField2ToOddFieldOut		= $20;
	oddField2ToEvenFieldOut		= $40;
	oddField2ToOddFieldOut		= $80;

	icmFrameTimeHasVirtualStartTimeAndDuration = $01;


type
	ICMFrameTimeRecordPtr = ^ICMFrameTimeRecord;
	ICMFrameTimeRecord = record
		value:					wide;									{  frame time }
		scale:					SInt32;								{  timescale of value/duration fields }
		base:					Ptr;									{  timebase }
		duration:				SInt32;								{  duration frame is to be displayed (0 if unknown) }
		rate:					Fixed;									{  rate of timebase relative to wall-time }
		recordSize:				SInt32;								{  total number of bytes in ICMFrameTimeRecord }
		frameNumber:			SInt32;								{  number of frame, zero if not known }
		flags:					SInt32;
		virtualStartTime:		wide;									{  conceptual start time }
		virtualDuration:		SInt32;								{  conceptual duration }
	end;

	ICMFrameTimePtr						= ^ICMFrameTimeRecord;
{$ifc undefined __QTUUID__}
{$setc __QTUUID__ := 1}
	{  QuickTime flavor of universally unique identifier (uuid) }
	QTUUIDPtr = ^QTUUID;
	QTUUID = record
		data1:					UInt32;
		data2:					UInt16;
		data3:					UInt16;
		data4:					packed array [0..7] of UInt8;
	end;

	QTMediaContextID					= QTUUID;
	QTMediaContextIDPtr 				= ^QTMediaContextID;
{$endc}


const
	uppICMDataProcInfo = $00000FE0;
	uppICMFlushProcInfo = $00000FE0;
	uppICMCompletionProcInfo = $00000E80;
	uppICMProgressProcInfo = $00000FA0;
	uppStdPixProcInfo = $002FEFC0;
	uppQDPixProcInfo = $002FEFC0;
	uppICMAlignmentProcInfo = $000003C0;
	uppICMCursorShieldedProcInfo = $00000FC0;
	uppICMMemoryDisposedProcInfo = $000003C0;
	uppICMConvertDataFormatProcInfo = $003FFFE0;
	{
	 *  NewICMDataUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewICMDataUPP(userRoutine: ICMDataProcPtr): ICMDataUPP; external name '_NewICMDataUPP'; { old name was NewICMDataProc }
{
 *  NewICMFlushUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMFlushUPP(userRoutine: ICMFlushProcPtr): ICMFlushUPP; external name '_NewICMFlushUPP'; { old name was NewICMFlushProc }
{
 *  NewICMCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMCompletionUPP(userRoutine: ICMCompletionProcPtr): ICMCompletionUPP; external name '_NewICMCompletionUPP'; { old name was NewICMCompletionProc }
{
 *  NewICMProgressUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMProgressUPP(userRoutine: ICMProgressProcPtr): ICMProgressUPP; external name '_NewICMProgressUPP'; { old name was NewICMProgressProc }
{
 *  NewStdPixUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewStdPixUPP(userRoutine: StdPixProcPtr): StdPixUPP; external name '_NewStdPixUPP'; { old name was NewStdPixProc }
{
 *  NewQDPixUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewQDPixUPP(userRoutine: QDPixProcPtr): QDPixUPP; external name '_NewQDPixUPP'; { old name was NewQDPixProc }
{
 *  NewICMAlignmentUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMAlignmentUPP(userRoutine: ICMAlignmentProcPtr): ICMAlignmentUPP; external name '_NewICMAlignmentUPP'; { old name was NewICMAlignmentProc }
{
 *  NewICMCursorShieldedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMCursorShieldedUPP(userRoutine: ICMCursorShieldedProcPtr): ICMCursorShieldedUPP; external name '_NewICMCursorShieldedUPP'; { old name was NewICMCursorShieldedProc }
{
 *  NewICMMemoryDisposedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMMemoryDisposedUPP(userRoutine: ICMMemoryDisposedProcPtr): ICMMemoryDisposedUPP; external name '_NewICMMemoryDisposedUPP'; { old name was NewICMMemoryDisposedProc }
{
 *  NewICMConvertDataFormatUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewICMConvertDataFormatUPP(userRoutine: ICMConvertDataFormatProcPtr): ICMConvertDataFormatUPP; external name '_NewICMConvertDataFormatUPP'; { old name was NewICMConvertDataFormatProc }
{
 *  DisposeICMDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMDataUPP(userUPP: ICMDataUPP); external name '_DisposeICMDataUPP';
{
 *  DisposeICMFlushUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMFlushUPP(userUPP: ICMFlushUPP); external name '_DisposeICMFlushUPP';
{
 *  DisposeICMCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMCompletionUPP(userUPP: ICMCompletionUPP); external name '_DisposeICMCompletionUPP';
{
 *  DisposeICMProgressUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMProgressUPP(userUPP: ICMProgressUPP); external name '_DisposeICMProgressUPP';
{
 *  DisposeStdPixUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeStdPixUPP(userUPP: StdPixUPP); external name '_DisposeStdPixUPP';
{
 *  DisposeQDPixUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeQDPixUPP(userUPP: QDPixUPP); external name '_DisposeQDPixUPP';
{
 *  DisposeICMAlignmentUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMAlignmentUPP(userUPP: ICMAlignmentUPP); external name '_DisposeICMAlignmentUPP';
{
 *  DisposeICMCursorShieldedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMCursorShieldedUPP(userUPP: ICMCursorShieldedUPP); external name '_DisposeICMCursorShieldedUPP';
{
 *  DisposeICMMemoryDisposedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMMemoryDisposedUPP(userUPP: ICMMemoryDisposedUPP); external name '_DisposeICMMemoryDisposedUPP';
{
 *  DisposeICMConvertDataFormatUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeICMConvertDataFormatUPP(userUPP: ICMConvertDataFormatUPP); external name '_DisposeICMConvertDataFormatUPP';
{
 *  InvokeICMDataUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeICMDataUPP(var dataP: Ptr; bytesNeeded: SInt32; refcon: SInt32; userRoutine: ICMDataUPP): OSErr; external name '_InvokeICMDataUPP'; { old name was CallICMDataProc }
{
 *  InvokeICMFlushUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeICMFlushUPP(data: Ptr; bytesAdded: SInt32; refcon: SInt32; userRoutine: ICMFlushUPP): OSErr; external name '_InvokeICMFlushUPP'; { old name was CallICMFlushProc }
{
 *  InvokeICMCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeICMCompletionUPP(result: OSErr; flags: SInt16; refcon: SInt32; userRoutine: ICMCompletionUPP); external name '_InvokeICMCompletionUPP'; { old name was CallICMCompletionProc }
{
 *  InvokeICMProgressUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeICMProgressUPP(message: SInt16; completeness: Fixed; refcon: SInt32; userRoutine: ICMProgressUPP): OSErr; external name '_InvokeICMProgressUPP'; { old name was CallICMProgressProc }
{
 *  InvokeStdPixUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeStdPixUPP(var src: PixMap; var srcRect: Rect; var matrix: MatrixRecord; mode: SInt16; mask: RgnHandle; var matte: PixMap; var matteRect: Rect; flags: SInt16; userRoutine: StdPixUPP); external name '_InvokeStdPixUPP'; { old name was CallStdPixProc }
{
 *  InvokeQDPixUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeQDPixUPP(var src: PixMap; var srcRect: Rect; var matrix: MatrixRecord; mode: SInt16; mask: RgnHandle; var matte: PixMap; var matteRect: Rect; flags: SInt16; userRoutine: QDPixUPP); external name '_InvokeQDPixUPP'; { old name was CallQDPixProc }
{
 *  InvokeICMAlignmentUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeICMAlignmentUPP(var rp: Rect; refcon: SInt32; userRoutine: ICMAlignmentUPP); external name '_InvokeICMAlignmentUPP'; { old name was CallICMAlignmentProc }
{
 *  InvokeICMCursorShieldedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeICMCursorShieldedUPP(const (*var*) r: Rect; refcon: UnivPtr; flags: SInt32; userRoutine: ICMCursorShieldedUPP); external name '_InvokeICMCursorShieldedUPP'; { old name was CallICMCursorShieldedProc }
{
 *  InvokeICMMemoryDisposedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeICMMemoryDisposedUPP(memoryBlock: Ptr; refcon: UnivPtr; userRoutine: ICMMemoryDisposedUPP); external name '_InvokeICMMemoryDisposedUPP'; { old name was CallICMMemoryDisposedProc }
{
 *  InvokeICMConvertDataFormatUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeICMConvertDataFormatUPP(refCon: UnivPtr; flags: SInt32; desiredFormat: Handle; sourceDataFormat: Handle; srcData: UnivPtr; srcDataSize: SInt32; var dstData: UnivPtr; var dstDataSize: SInt32; userRoutine: ICMConvertDataFormatUPP): OSErr; external name '_InvokeICMConvertDataFormatUPP'; { old name was CallICMConvertDataFormatProc }
{
 *  CodecManagerVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CodecManagerVersion(var version: SInt32): OSErr; external name '_CodecManagerVersion';
{
 *  GetCodecNameList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCodecNameList(var list: CodecNameSpecListPtr; showAll: SInt16): OSErr; external name '_GetCodecNameList';
{
 *  DisposeCodecNameList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DisposeCodecNameList(list: CodecNameSpecListPtr): OSErr; external name '_DisposeCodecNameList';
{
 *  GetCodecInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCodecInfo(var info: CodecInfo; cType: CodecType; codec: CodecComponent): OSErr; external name '_GetCodecInfo';
{
 *  GetMaxCompressionSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetMaxCompressionSize(src: PixMapHandle; const (*var*) srcRect: Rect; colorDepth: SInt16; quality: CodecQ; cType: CodecType; codec: CompressorComponent; var size: SInt32): OSErr; external name '_GetMaxCompressionSize';
{
 *  GetCSequenceMaxCompressionSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCSequenceMaxCompressionSize(seqID: ImageSequence; src: PixMapHandle; var size: SInt32): OSErr; external name '_GetCSequenceMaxCompressionSize';
{
 *  GetCompressionTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCompressionTime(src: PixMapHandle; const (*var*) srcRect: Rect; colorDepth: SInt16; cType: CodecType; codec: CompressorComponent; var spatialQuality: CodecQ; var temporalQuality: CodecQ; var compressTime: UInt32): OSErr; external name '_GetCompressionTime';
{
 *  CompressImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompressImage(src: PixMapHandle; const (*var*) srcRect: Rect; quality: CodecQ; cType: CodecType; desc: ImageDescriptionHandle; data: Ptr): OSErr; external name '_CompressImage';
{
 *  FCompressImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FCompressImage(src: PixMapHandle; const (*var*) srcRect: Rect; colorDepth: SInt16; quality: CodecQ; cType: CodecType; codec: CompressorComponent; ctable: CTabHandle; flags: CodecFlags; bufferSize: SInt32; flushProc: ICMFlushProcRecordPtr; progressProc: ICMProgressProcRecordPtr; desc: ImageDescriptionHandle; data: Ptr): OSErr; external name '_FCompressImage';
{
 *  DecompressImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DecompressImage(data: Ptr; desc: ImageDescriptionHandle; dst: PixMapHandle; const (*var*) srcRect: Rect; const (*var*) dstRect: Rect; mode: SInt16; mask: RgnHandle): OSErr; external name '_DecompressImage';
{
 *  FDecompressImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FDecompressImage(data: Ptr; desc: ImageDescriptionHandle; dst: PixMapHandle; const (*var*) srcRect: Rect; matrix: MatrixRecordPtr; mode: SInt16; mask: RgnHandle; matte: PixMapHandle; const (*var*) matteRect: Rect; accuracy: CodecQ; codec: DecompressorComponent; bufferSize: SInt32; dataProc: ICMDataProcRecordPtr; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_FDecompressImage';
{
 *  CompressSequenceBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompressSequenceBegin(var seqID: ImageSequence; src: PixMapHandle; prev: PixMapHandle; const (*var*) srcRect: Rect; const (*var*) prevRect: Rect; colorDepth: SInt16; cType: CodecType; codec: CompressorComponent; spatialQuality: CodecQ; temporalQuality: CodecQ; keyFrameRate: SInt32; ctable: CTabHandle; flags: CodecFlags; desc: ImageDescriptionHandle): OSErr; external name '_CompressSequenceBegin';
{
 *  CompressSequenceFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompressSequenceFrame(seqID: ImageSequence; src: PixMapHandle; const (*var*) srcRect: Rect; flags: CodecFlags; data: Ptr; var dataSize: SInt32; var similarity: UInt8; asyncCompletionProc: ICMCompletionProcRecordPtr): OSErr; external name '_CompressSequenceFrame';
{
 *  DecompressSequenceBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DecompressSequenceBegin(var seqID: ImageSequence; desc: ImageDescriptionHandle; port: CGrafPtr; gdh: GDHandle; const (*var*) srcRect: Rect; matrix: MatrixRecordPtr; mode: SInt16; mask: RgnHandle; flags: CodecFlags; accuracy: CodecQ; codec: DecompressorComponent): OSErr; external name '_DecompressSequenceBegin';
{
 *  DecompressSequenceBeginS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DecompressSequenceBeginS(var seqID: ImageSequence; desc: ImageDescriptionHandle; data: Ptr; dataSize: SInt32; port: CGrafPtr; gdh: GDHandle; const (*var*) srcRect: Rect; matrix: MatrixRecordPtr; mode: SInt16; mask: RgnHandle; flags: CodecFlags; accuracy: CodecQ; codec: DecompressorComponent): OSErr; external name '_DecompressSequenceBeginS';
{
 *  DecompressSequenceFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DecompressSequenceFrame(seqID: ImageSequence; data: Ptr; inFlags: CodecFlags; var outFlags: CodecFlags; asyncCompletionProc: ICMCompletionProcRecordPtr): OSErr; external name '_DecompressSequenceFrame';
{
 *  DecompressSequenceFrameS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DecompressSequenceFrameS(seqID: ImageSequence; data: Ptr; dataSize: SInt32; inFlags: CodecFlags; var outFlags: CodecFlags; asyncCompletionProc: ICMCompletionProcRecordPtr): OSErr; external name '_DecompressSequenceFrameS';
{
 *  DecompressSequenceFrameWhen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DecompressSequenceFrameWhen(seqID: ImageSequence; data: Ptr; dataSize: SInt32; inFlags: CodecFlags; var outFlags: CodecFlags; asyncCompletionProc: ICMCompletionProcRecordPtr; const (*var*) frameTime: ICMFrameTimeRecord): OSErr; external name '_DecompressSequenceFrameWhen';
{
 *  CDSequenceFlush()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceFlush(seqID: ImageSequence): OSErr; external name '_CDSequenceFlush';
{
 *  SetDSequenceMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceMatrix(seqID: ImageSequence; matrix: MatrixRecordPtr): OSErr; external name '_SetDSequenceMatrix';
{
 *  GetDSequenceMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GetDSequenceMatrix(seqID: ImageSequence; matrix: MatrixRecordPtr): OSErr; external name '_GetDSequenceMatrix';
{
 *  SetDSequenceMatte()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceMatte(seqID: ImageSequence; matte: PixMapHandle; const (*var*) matteRect: Rect): OSErr; external name '_SetDSequenceMatte';
{
 *  SetDSequenceMask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceMask(seqID: ImageSequence; mask: RgnHandle): OSErr; external name '_SetDSequenceMask';
{
 *  SetDSequenceTransferMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceTransferMode(seqID: ImageSequence; mode: SInt16; const (*var*) opColor: RGBColor): OSErr; external name '_SetDSequenceTransferMode';
{
 *  SetDSequenceDataProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceDataProc(seqID: ImageSequence; dataProc: ICMDataProcRecordPtr; bufferSize: SInt32): OSErr; external name '_SetDSequenceDataProc';
{
 *  SetDSequenceAccuracy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceAccuracy(seqID: ImageSequence; accuracy: CodecQ): OSErr; external name '_SetDSequenceAccuracy';
{
 *  SetDSequenceSrcRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceSrcRect(seqID: ImageSequence; const (*var*) srcRect: Rect): OSErr; external name '_SetDSequenceSrcRect';
{
 *  SetDSequenceFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function SetDSequenceFlags(seqID: ImageSequence; flags: SInt32; flagsMask: SInt32): OSErr; external name '_SetDSequenceFlags';
const
	codecDSequenceDisableOverlaySurface = $00000020;
	codecDSequenceSingleField	= $00000040;
	codecDSequenceBidirectionalPrediction = $00000080;
	codecDSequenceFlushInsteadOfDirtying = $00000100;
	codecDSequenceEnableSubPixelPositioning = $00000200;


type
	CodecComponentPtr					= ^CodecComponent;
	CodecComponentHandle				= ^CodecComponentPtr;
	{  selectors for ICMSequenceGet/SetInfo }

const
	kICMSequenceTaskWeight		= $74776569 (* 'twei' *);						{  data is pointer to UInt32 }
	kICMSequenceTaskName		= $746E616D (* 'tnam' *);						{  data is pointer to OSType }
	kICMSequenceUserPreferredCodecs = $70756E74 (* 'punt' *);					{  data is pointer to CodecComponentHandle }

	{
	 *  ICMSequenceGetInfo()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function ICMSequenceGetInfo(seqID: ImageSequence; which: OSType; data: UnivPtr): OSErr; external name '_ICMSequenceGetInfo';
{
 *  ICMSequenceSetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ICMSequenceSetInfo(seqID: ImageSequence; which: OSType; data: UnivPtr; dataSize: Size): OSErr; external name '_ICMSequenceSetInfo';
{
 *  GetDSequenceImageBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetDSequenceImageBuffer(seqID: ImageSequence; var gworld: GWorldPtr): OSErr; external name '_GetDSequenceImageBuffer';
{
 *  GetDSequenceScreenBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetDSequenceScreenBuffer(seqID: ImageSequence; var gworld: GWorldPtr): OSErr; external name '_GetDSequenceScreenBuffer';
{
 *  SetCSequenceQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequenceQuality(seqID: ImageSequence; spatialQuality: CodecQ; temporalQuality: CodecQ): OSErr; external name '_SetCSequenceQuality';
{
 *  SetCSequencePrev()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequencePrev(seqID: ImageSequence; prev: PixMapHandle; const (*var*) prevRect: Rect): OSErr; external name '_SetCSequencePrev';
{
 *  SetCSequenceFlushProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequenceFlushProc(seqID: ImageSequence; flushProc: ICMFlushProcRecordPtr; bufferSize: SInt32): OSErr; external name '_SetCSequenceFlushProc';
{
 *  SetCSequenceKeyFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequenceKeyFrameRate(seqID: ImageSequence; keyFrameRate: SInt32): OSErr; external name '_SetCSequenceKeyFrameRate';
{
 *  GetCSequenceKeyFrameRate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCSequenceKeyFrameRate(seqID: ImageSequence; var keyFrameRate: SInt32): OSErr; external name '_GetCSequenceKeyFrameRate';
{
 *  GetCSequencePrevBuffer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCSequencePrevBuffer(seqID: ImageSequence; var gworld: GWorldPtr): OSErr; external name '_GetCSequencePrevBuffer';
{
 *  CDSequenceBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceBusy(seqID: ImageSequence): OSErr; external name '_CDSequenceBusy';
{
 *  CDSequenceEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceEnd(seqID: ImageSequence): OSErr; external name '_CDSequenceEnd';
{
 *  CDSequenceEquivalentImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceEquivalentImageDescription(seqID: ImageSequence; newDesc: ImageDescriptionHandle; var equivalent: boolean): OSErr; external name '_CDSequenceEquivalentImageDescription';
{
 *  CDSequenceEquivalentImageDescriptionS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function CDSequenceEquivalentImageDescriptionS(seqID: ImageSequence; newDesc: ImageDescriptionHandle; var equivalent: boolean; var canSwitch: boolean): OSErr; external name '_CDSequenceEquivalentImageDescriptionS';
{
 *  ReplaceDSequenceImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function ReplaceDSequenceImageDescription(seqID: ImageSequence; newDesc: ImageDescriptionHandle): OSErr; external name '_ReplaceDSequenceImageDescription';
{
 *  GetCompressedImageSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCompressedImageSize(desc: ImageDescriptionHandle; data: Ptr; bufferSize: SInt32; dataProc: ICMDataProcRecordPtr; var dataSize: SInt32): OSErr; external name '_GetCompressedImageSize';
{
 *  GetSimilarity()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetSimilarity(src: PixMapHandle; const (*var*) srcRect: Rect; desc: ImageDescriptionHandle; data: Ptr; var similarity: Fixed): OSErr; external name '_GetSimilarity';
const
	kImageDescriptionSampleFormat = $6964666D (* 'idfm' *);						{  image description extension describing sample format }
	kImageDescriptionClassicAtomFormat = $61746F6D (* 'atom' *);				{  sample contains classic atom structure (ie, GX codec and Curve codec) }
	kImageDescriptionQTAtomFormat = $71746174 (* 'qtat' *);						{  sample contains QT atom structure }
	kImageDescriptionEffectDataFormat = $66786174 (* 'fxat' *);					{  sample describes an effect (as QTAtoms) }
	kImageDescriptionPrivateDataFormat = $70726976 (* 'priv' *);				{  sample is in a private codec specific format }
	kImageDescriptionAlternateCodec = $73756273 (* 'subs' *);					{  image description extension containing the OSType of a substitute codec should the main codec not be available }
	kImageDescriptionColorSpace	= $63737063 (* 'cspc' *);						{  image description extension containing an OSType naming the native pixel format of an image (only used for pixel formats not supported by classic Color QuickDraw) }

	{
	 *  GetImageDescriptionCTable()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function GetImageDescriptionCTable(desc: ImageDescriptionHandle; var ctable: CTabHandle): OSErr; external name '_GetImageDescriptionCTable';
{
 *  SetImageDescriptionCTable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetImageDescriptionCTable(desc: ImageDescriptionHandle; ctable: CTabHandle): OSErr; external name '_SetImageDescriptionCTable';
{
 *  GetImageDescriptionExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetImageDescriptionExtension(desc: ImageDescriptionHandle; var extension: Handle; idType: SInt32; index: SInt32): OSErr; external name '_GetImageDescriptionExtension';
{
 *  AddImageDescriptionExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddImageDescriptionExtension(desc: ImageDescriptionHandle; extension: Handle; idType: SInt32): OSErr; external name '_AddImageDescriptionExtension';
{
 *  RemoveImageDescriptionExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function RemoveImageDescriptionExtension(desc: ImageDescriptionHandle; idType: SInt32; index: SInt32): OSErr; external name '_RemoveImageDescriptionExtension';
{
 *  CountImageDescriptionExtensionType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CountImageDescriptionExtensionType(desc: ImageDescriptionHandle; idType: SInt32; var count: SInt32): OSErr; external name '_CountImageDescriptionExtensionType';
{
 *  GetNextImageDescriptionExtensionType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetNextImageDescriptionExtensionType(desc: ImageDescriptionHandle; var idType: SInt32): OSErr; external name '_GetNextImageDescriptionExtensionType';
{
 *  FindCodec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FindCodec(cType: CodecType; specCodec: CodecComponent; var compressor: CompressorComponent; var decompressor: DecompressorComponent): OSErr; external name '_FindCodec';
{
 *  CompressPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompressPicture(srcPicture: PicHandle; dstPicture: PicHandle; quality: CodecQ; cType: CodecType): OSErr; external name '_CompressPicture';
{
 *  FCompressPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FCompressPicture(srcPicture: PicHandle; dstPicture: PicHandle; colorDepth: SInt16; ctable: CTabHandle; quality: CodecQ; doDither: SInt16; compressAgain: SInt16; progressProc: ICMProgressProcRecordPtr; cType: CodecType; codec: CompressorComponent): OSErr; external name '_FCompressPicture';
{
 *  CompressPictureFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompressPictureFile(srcRefNum: SInt16; dstRefNum: SInt16; quality: CodecQ; cType: CodecType): OSErr; external name '_CompressPictureFile';
{
 *  FCompressPictureFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FCompressPictureFile(srcRefNum: SInt16; dstRefNum: SInt16; colorDepth: SInt16; ctable: CTabHandle; quality: CodecQ; doDither: SInt16; compressAgain: SInt16; progressProc: ICMProgressProcRecordPtr; cType: CodecType; codec: CompressorComponent): OSErr; external name '_FCompressPictureFile';
{
 *  GetPictureFileHeader()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetPictureFileHeader(refNum: SInt16; var frame: Rect; var header: OpenCPicParams): OSErr; external name '_GetPictureFileHeader';
{
 *  DrawPictureFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DrawPictureFile(refNum: SInt16; const (*var*) frame: Rect; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_DrawPictureFile';
{
 *  DrawTrimmedPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DrawTrimmedPicture(srcPicture: PicHandle; const (*var*) frame: Rect; trimMask: RgnHandle; doDither: SInt16; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_DrawTrimmedPicture';
{
 *  DrawTrimmedPictureFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DrawTrimmedPictureFile(srcRefnum: SInt16; const (*var*) frame: Rect; trimMask: RgnHandle; doDither: SInt16; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_DrawTrimmedPictureFile';
{
 *  MakeThumbnailFromPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeThumbnailFromPicture(picture: PicHandle; colorDepth: SInt16; thumbnail: PicHandle; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_MakeThumbnailFromPicture';
{
 *  MakeThumbnailFromPictureFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeThumbnailFromPictureFile(refNum: SInt16; colorDepth: SInt16; thumbnail: PicHandle; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_MakeThumbnailFromPictureFile';
{
 *  MakeThumbnailFromPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeThumbnailFromPixMap(src: PixMapHandle; const (*var*) srcRect: Rect; colorDepth: SInt16; thumbnail: PicHandle; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_MakeThumbnailFromPixMap';
{
 *  TrimImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TrimImage(desc: ImageDescriptionHandle; inData: Ptr; inBufferSize: SInt32; dataProc: ICMDataProcRecordPtr; outData: Ptr; outBufferSize: SInt32; flushProc: ICMFlushProcRecordPtr; var trimRect: Rect; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_TrimImage';
{
 *  ConvertImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ConvertImage(srcDD: ImageDescriptionHandle; srcData: Ptr; colorDepth: SInt16; ctable: CTabHandle; accuracy: CodecQ; quality: CodecQ; cType: CodecType; codec: CodecComponent; dstDD: ImageDescriptionHandle; dstData: Ptr): OSErr; external name '_ConvertImage';
{
 *  GetCompressedPixMapInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCompressedPixMapInfo(pix: PixMapPtr; var desc: ImageDescriptionHandle; var data: Ptr; var bufferSize: SInt32; var dataProc: ICMDataProcRecord; var progressProc: ICMProgressProcRecord): OSErr; external name '_GetCompressedPixMapInfo';
{
 *  SetCompressedPixMapInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCompressedPixMapInfo(pix: PixMapPtr; desc: ImageDescriptionHandle; data: Ptr; bufferSize: SInt32; dataProc: ICMDataProcRecordPtr; progressProc: ICMProgressProcRecordPtr): OSErr; external name '_SetCompressedPixMapInfo';
{
 *  StdPix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure StdPix(src: PixMapPtr; const (*var*) srcRect: Rect; matrix: MatrixRecordPtr; mode: SInt16; mask: RgnHandle; matte: PixMapPtr; const (*var*) matteRect: Rect; flags: SInt16); external name '_StdPix';
{
 *  TransformRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TransformRgn(matrix: MatrixRecordPtr; rgn: RgnHandle): OSErr; external name '_TransformRgn';
{**********
    preview stuff
**********}
{$ifc CALL_NOT_IN_CARBON}
{
 *  SFGetFilePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SFGetFilePreview(where: Point; const (*var*) prompt: Str255; fileFilter: FileFilterUPP; numTypes: SInt16; typeList: ConstSFTypeListPtr; dlgHook: DlgHookUPP; var reply: SFReply); external name '_SFGetFilePreview';
{
 *  SFPGetFilePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SFPGetFilePreview(where: Point; const (*var*) prompt: Str255; fileFilter: FileFilterUPP; numTypes: SInt16; typeList: ConstSFTypeListPtr; dlgHook: DlgHookUPP; var reply: SFReply; dlgID: SInt16; filterProc: ModalFilterUPP); external name '_SFPGetFilePreview';
{
 *  StandardGetFilePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure StandardGetFilePreview(fileFilter: FileFilterUPP; numTypes: SInt16; typeList: ConstSFTypeListPtr; var reply: StandardFileReply); external name '_StandardGetFilePreview';
{
 *  CustomGetFilePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CustomGetFilePreview(fileFilter: FileFilterYDUPP; numTypes: SInt16; typeList: ConstSFTypeListPtr; var reply: StandardFileReply; dlgID: SInt16; where: Point; dlgHook: DlgHookYDUPP; filterProc: ModalFilterYDUPP; activeList: ActivationOrderListPtr; activateProc: ActivateYDUPP; yourDataPtr: UnivPtr); external name '_CustomGetFilePreview';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  MakeFilePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeFilePreview(resRefNum: SInt16; progress: ICMProgressProcRecordPtr): OSErr; external name '_MakeFilePreview';
{
 *  AddFilePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function AddFilePreview(resRefNum: SInt16; previewType: OSType; previewData: Handle): OSErr; external name '_AddFilePreview';
const
	sfpItemPreviewAreaUser		= 11;
	sfpItemPreviewStaticText	= 12;
	sfpItemPreviewDividerUser	= 13;
	sfpItemCreatePreviewButton	= 14;
	sfpItemShowPreviewButton	= 15;


type
	PreviewResourceRecordPtr = ^PreviewResourceRecord;
	PreviewResourceRecord = record
		modDate:				UInt32;
		version:				SInt16;
		resType:				OSType;
		resID:					SInt16;
	end;

	PreviewResourcePtr					= ^PreviewResourceRecord;
	PreviewResource						= ^PreviewResourcePtr;
	{
	 *  AlignScreenRect()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
procedure AlignScreenRect(var rp: Rect; alignmentProc: ICMAlignmentProcRecordPtr); external name '_AlignScreenRect';
{
 *  AlignWindow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure AlignWindow(wp: WindowRef; front: boolean; const (*var*) alignmentRect: Rect; alignmentProc: ICMAlignmentProcRecordPtr); external name '_AlignWindow';
{
 *  DragAlignedWindow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DragAlignedWindow(wp: WindowRef; startPt: Point; var boundsRect: Rect; var alignmentRect: Rect; alignmentProc: ICMAlignmentProcRecordPtr); external name '_DragAlignedWindow';
{
 *  DragAlignedGrayRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function DragAlignedGrayRgn(theRgn: RgnHandle; startPt: Point; var boundsRect: Rect; var slopRect: Rect; axis: SInt16; actionProc: UniversalProcPtr; var alignmentRect: Rect; alignmentProc: ICMAlignmentProcRecordPtr): SInt32; external name '_DragAlignedGrayRgn';
{
 *  SetCSequenceDataRateParams()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequenceDataRateParams(seqID: ImageSequence; params: DataRateParamsPtr): OSErr; external name '_SetCSequenceDataRateParams';
{
 *  SetCSequenceFrameNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequenceFrameNumber(seqID: ImageSequence; frameNumber: SInt32): OSErr; external name '_SetCSequenceFrameNumber';
{
 *  SetCSequencePreferredPacketSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetCSequencePreferredPacketSize(seqID: ImageSequence; preferredPacketSizeInBytes: SInt32): OSErr; external name '_SetCSequencePreferredPacketSize';
{
 *  NewImageGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NewImageGWorld(var gworld: GWorldPtr; idh: ImageDescriptionHandle; flags: GWorldFlags): OSErr; external name '_NewImageGWorld';
{
 *  GetCSequenceDataRateParams()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCSequenceDataRateParams(seqID: ImageSequence; params: DataRateParamsPtr): OSErr; external name '_GetCSequenceDataRateParams';
{
 *  GetCSequenceFrameNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetCSequenceFrameNumber(seqID: ImageSequence; var frameNumber: SInt32): OSErr; external name '_GetCSequenceFrameNumber';
{
 *  GetBestDeviceRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetBestDeviceRect(var gdh: GDHandle; var rp: Rect): OSErr; external name '_GetBestDeviceRect';
{
 *  SetSequenceProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetSequenceProgressProc(seqID: ImageSequence; var progressProc: ICMProgressProcRecord): OSErr; external name '_SetSequenceProgressProc';
{
 *  GDHasScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GDHasScale(gdh: GDHandle; depth: SInt16; var scale: Fixed): OSErr; external name '_GDHasScale';
{
 *  GDGetScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GDGetScale(gdh: GDHandle; var scale: Fixed; var flags: SInt16): OSErr; external name '_GDGetScale';
{
 *  GDSetScale()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GDSetScale(gdh: GDHandle; scale: Fixed; flags: SInt16): OSErr; external name '_GDSetScale';
{
 *  ICMShieldSequenceCursor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ICMShieldSequenceCursor(seqID: ImageSequence): OSErr; external name '_ICMShieldSequenceCursor';
{
 *  ICMDecompressComplete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ICMDecompressComplete(seqID: ImageSequence; err: OSErr; flag: SInt16; completionRtn: ICMCompletionProcRecordPtr); external name '_ICMDecompressComplete';
{
 *  ICMDecompressCompleteS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ICMDecompressCompleteS(seqID: ImageSequence; err: OSErr; flag: SInt16; completionRtn: ICMCompletionProcRecordPtr): OSErr; external name '_ICMDecompressCompleteS';
{
 *  ICMSequenceLockBits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ICMSequenceLockBits(seqID: ImageSequence; dst: PixMapPtr; flags: SInt32): OSErr; external name '_ICMSequenceLockBits';
{
 *  ICMSequenceUnlockBits()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ICMSequenceUnlockBits(seqID: ImageSequence; flags: SInt32): OSErr; external name '_ICMSequenceUnlockBits';
const
	kICMPixelFormatIsPlanarMask	= $0F;							{  these bits in formatFlags indicate how many planes there are; they're 0 if chunky }
	kICMPixelFormatIsIndexed	= $00000010;
	kICMPixelFormatIsSupportedByQD = $00000020;
	kICMPixelFormatIsMonochrome	= $00000040;
	kICMPixelFormatHasAlphaChannel = $00000080;


type
	ICMPixelFormatInfoPtr = ^ICMPixelFormatInfo;
	ICMPixelFormatInfo = record
		size:					SInt32;								{  caller MUST fill this in with sizeof(ICMPixelFormatInfo) before calling ICMGet/SetPixelFormatInfo }
		formatFlags:			UInt32;
		bitsPerPixel:			array [0..13] of SInt16;				{  list each plane's bits per pixel separately if planar }
																		{  new field for QuickTime 4.1 }
		defaultGammaLevel:		Fixed;
																		{  new fields for QuickTime 6.0 }
		horizontalSubsampling:	array [0..13] of SInt16;				{  per plane; use 1 if plane is not subsampled }
		verticalSubsampling:	array [0..13] of SInt16;				{  per plane; use 1 if plane is not subsampled }
	end;

	{	 IMPORTANT: Fill in theInfo->size with sizeof(ICMPixelFormatInfo) before calling ICMGetPixelFormatInfo 	}
	{
	 *  ICMGetPixelFormatInfo()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function ICMGetPixelFormatInfo(PixelFormat: OSType; theInfo: ICMPixelFormatInfoPtr): OSErr; external name '_ICMGetPixelFormatInfo';
{ IMPORTANT: Fill in theInfo->size with sizeof(ICMPixelFormatInfo) before calling ICMSetPixelFormatInfo }
{
 *  ICMSetPixelFormatInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ICMSetPixelFormatInfo(PixelFormat: OSType; theInfo: ICMPixelFormatInfoPtr): OSErr; external name '_ICMSetPixelFormatInfo';
const
	kICMGetChainUltimateParent	= 0;
	kICMGetChainParent			= 1;
	kICMGetChainChild			= 2;
	kICMGetChainUltimateChild	= 3;

	{
	 *  ICMSequenceGetChainMember()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function ICMSequenceGetChainMember(seqID: ImageSequence; var retSeqID: ImageSequence; flags: SInt32): OSErr; external name '_ICMSequenceGetChainMember';
{
 *  SetDSequenceTimeCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function SetDSequenceTimeCode(seqID: ImageSequence; timeCodeFormat: UnivPtr; timeCodeTime: UnivPtr): OSErr; external name '_SetDSequenceTimeCode';
{
 *  CDSequenceNewMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceNewMemory(seqID: ImageSequence; var data: Ptr; dataSize: Size; dataUse: SInt32; memoryGoneProc: ICMMemoryDisposedUPP; refCon: UnivPtr): OSErr; external name '_CDSequenceNewMemory';
{
 *  CDSequenceDisposeMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceDisposeMemory(seqID: ImageSequence; data: Ptr): OSErr; external name '_CDSequenceDisposeMemory';
{
 *  CDSequenceNewDataSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceNewDataSource(seqID: ImageSequence; var sourceID: ImageSequenceDataSource; sourceType: OSType; sourceInputNumber: SInt32; dataDescription: Handle; transferProc: ICMConvertDataFormatUPP; refCon: UnivPtr): OSErr; external name '_CDSequenceNewDataSource';
{
 *  CDSequenceDisposeDataSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceDisposeDataSource(sourceID: ImageSequenceDataSource): OSErr; external name '_CDSequenceDisposeDataSource';
{
 *  CDSequenceSetSourceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceSetSourceData(sourceID: ImageSequenceDataSource; data: UnivPtr; dataSize: SInt32): OSErr; external name '_CDSequenceSetSourceData';
{
 *  CDSequenceChangedSourceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceChangedSourceData(sourceID: ImageSequenceDataSource): OSErr; external name '_CDSequenceChangedSourceData';
{
 *  CDSequenceSetSourceDataQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceSetSourceDataQueue(sourceID: ImageSequenceDataSource; dataQueue: QHdrPtr): OSErr; external name '_CDSequenceSetSourceDataQueue';
{
 *  CDSequenceGetDataSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceGetDataSource(seqID: ImageSequence; var sourceID: ImageSequenceDataSource; sourceType: OSType; sourceInputNumber: SInt32): OSErr; external name '_CDSequenceGetDataSource';
{
 *  PtInDSequenceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function PtInDSequenceData(seqID: ImageSequence; data: UnivPtr; dataSize: Size; where: Point; var hit: boolean): OSErr; external name '_PtInDSequenceData';
{
 *  HitTestDSequenceData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function HitTestDSequenceData(seqID: ImageSequence; data: UnivPtr; dataSize: Size; where: Point; var hit: SInt32; hitFlags: SInt32): OSErr; external name '_HitTestDSequenceData';
{
 *  GetGraphicsImporterForFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetGraphicsImporterForFile(const (*var*) theFile: FSSpec; var gi: ComponentInstance): OSErr; external name '_GetGraphicsImporterForFile';
{
 *  GetGraphicsImporterForDataRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetGraphicsImporterForDataRef(dataRef: Handle; dataRefType: OSType; var gi: ComponentInstance): OSErr; external name '_GetGraphicsImporterForDataRef';
const
	kDontUseValidateToFindGraphicsImporter = $00000001;

	{
	 *  GetGraphicsImporterForFileWithFlags()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function GetGraphicsImporterForFileWithFlags(const (*var*) theFile: FSSpec; var gi: ComponentInstance; flags: SInt32): OSErr; external name '_GetGraphicsImporterForFileWithFlags';
{
 *  GetGraphicsImporterForDataRefWithFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GetGraphicsImporterForDataRefWithFlags(dataRef: Handle; dataRefType: OSType; var gi: ComponentInstance; flags: SInt32): OSErr; external name '_GetGraphicsImporterForDataRefWithFlags';
{
 *  QTGetFileNameExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetFileNameExtension(const (*var*) fileName: StrFileName; fileType: OSType; var extension: OSType): OSErr; external name '_QTGetFileNameExtension';
{
 *  ImageTranscodeSequenceBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscodeSequenceBegin(var its: ImageTranscodeSequence; srcDesc: ImageDescriptionHandle; destType: OSType; var dstDesc: ImageDescriptionHandle; data: UnivPtr; dataSize: SInt32): OSErr; external name '_ImageTranscodeSequenceBegin';
{
 *  ImageTranscodeSequenceEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscodeSequenceEnd(its: ImageTranscodeSequence): OSErr; external name '_ImageTranscodeSequenceEnd';
{
 *  ImageTranscodeFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscodeFrame(its: ImageTranscodeSequence; srcData: UnivPtr; srcDataSize: SInt32; var dstData: UnivPtr; var dstDataSize: SInt32): OSErr; external name '_ImageTranscodeFrame';
{
 *  ImageTranscodeDisposeFrameData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscodeDisposeFrameData(its: ImageTranscodeSequence; dstData: UnivPtr): OSErr; external name '_ImageTranscodeDisposeFrameData';
{
 *  CDSequenceInvalidate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceInvalidate(seqID: ImageSequence; invalRgn: RgnHandle): OSErr; external name '_CDSequenceInvalidate';
{
 *  CDSequenceSetTimeBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CDSequenceSetTimeBase(seqID: ImageSequence; base: UnivPtr): OSErr; external name '_CDSequenceSetTimeBase';
{
 *  ImageFieldSequenceBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageFieldSequenceBegin(var ifs: ImageFieldSequence; desc1: ImageDescriptionHandle; desc2: ImageDescriptionHandle; descOut: ImageDescriptionHandle): OSErr; external name '_ImageFieldSequenceBegin';
{
 *  ImageFieldSequenceExtractCombine()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageFieldSequenceExtractCombine(ifs: ImageFieldSequence; fieldFlags: SInt32; data1: UnivPtr; dataSize1: SInt32; data2: UnivPtr; dataSize2: SInt32; outputData: UnivPtr; var outDataSize: SInt32): OSErr; external name '_ImageFieldSequenceExtractCombine';
{
 *  ImageFieldSequenceEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageFieldSequenceEnd(ifs: ImageFieldSequence): OSErr; external name '_ImageFieldSequenceEnd';
const
	kICMTempThenAppMemory		= $00001000;
	kICMAppThenTempMemory		= $00002000;

	{
	 *  QTNewGWorld()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTNewGWorld(var offscreenGWorld: GWorldPtr; PixelFormat: OSType; const (*var*) boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags): OSErr; external name '_QTNewGWorld';
{
 *  QTNewGWorldFromPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTNewGWorldFromPtr(var gw: GWorldPtr; pixelFormat: OSType; const (*var*) boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags; baseAddr: UnivPtr; rowBytes: SInt32): OSErr; external name '_QTNewGWorldFromPtr';
{
 *  QTUpdateGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTUpdateGWorld(var offscreenGWorld: GWorldPtr; PixelFormat: OSType; const (*var*) boundsRect: Rect; cTable: CTabHandle; aGDevice: GDHandle; flags: GWorldFlags): GWorldFlags; external name '_QTUpdateGWorld';
{
 *  MakeImageDescriptionForPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function MakeImageDescriptionForPixMap(pixmap: PixMapHandle; var idh: ImageDescriptionHandle): OSErr; external name '_MakeImageDescriptionForPixMap';
{
 *  MakeImageDescriptionForEffect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function MakeImageDescriptionForEffect(effectType: OSType; var idh: ImageDescriptionHandle): OSErr; external name '_MakeImageDescriptionForEffect';
{
 *  QTGetPixelSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTGetPixelSize(PixelFormat: OSType): SInt16; external name '_QTGetPixelSize';
{
 *  QTGetPixelFormatDepthForImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function QTGetPixelFormatDepthForImageDescription(PixelFormat: OSType): SInt16; external name '_QTGetPixelFormatDepthForImageDescription';
{
 *  QTGetPixMapPtrRowBytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTGetPixMapPtrRowBytes(pm: PixMapPtr): SInt32; external name '_QTGetPixMapPtrRowBytes';
{
 *  QTGetPixMapHandleRowBytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTGetPixMapHandleRowBytes(pm: PixMapHandle): SInt32; external name '_QTGetPixMapHandleRowBytes';
{
 *  QTSetPixMapPtrRowBytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTSetPixMapPtrRowBytes(pm: PixMapPtr; rowBytes: SInt32): OSErr; external name '_QTSetPixMapPtrRowBytes';
{
 *  QTSetPixMapHandleRowBytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function QTSetPixMapHandleRowBytes(pm: PixMapHandle; rowBytes: SInt32): OSErr; external name '_QTSetPixMapHandleRowBytes';
const
	kQTUsePlatformDefaultGammaLevel = 0;						{  When decompressing into this PixMap, gamma-correct to the platform's standard gamma.  }
	kQTUseSourceGammaLevel		= -1;							{  When decompressing into this PixMap, don't perform gamma-correction.  }
	kQTCCIR601VideoGammaLevel	= $00023333;					{  2.2, standard television video gamma. }

	{
	 *  QTGetPixMapPtrGammaLevel()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 5.0 and later
	 	}
function QTGetPixMapPtrGammaLevel(pm: PixMapPtr): Fixed; external name '_QTGetPixMapPtrGammaLevel';
{
 *  QTSetPixMapPtrGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTSetPixMapPtrGammaLevel(pm: PixMapPtr; gammaLevel: Fixed): OSErr; external name '_QTSetPixMapPtrGammaLevel';
{
 *  QTGetPixMapHandleGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTGetPixMapHandleGammaLevel(pm: PixMapHandle): Fixed; external name '_QTGetPixMapHandleGammaLevel';
{
 *  QTSetPixMapHandleGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTSetPixMapHandleGammaLevel(pm: PixMapHandle; gammaLevel: Fixed): OSErr; external name '_QTSetPixMapHandleGammaLevel';
{
 *  QTGetPixMapPtrRequestedGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTGetPixMapPtrRequestedGammaLevel(pm: PixMapPtr): Fixed; external name '_QTGetPixMapPtrRequestedGammaLevel';
{
 *  QTSetPixMapPtrRequestedGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTSetPixMapPtrRequestedGammaLevel(pm: PixMapPtr; requestedGammaLevel: Fixed): OSErr; external name '_QTSetPixMapPtrRequestedGammaLevel';
{
 *  QTGetPixMapHandleRequestedGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTGetPixMapHandleRequestedGammaLevel(pm: PixMapHandle): Fixed; external name '_QTGetPixMapHandleRequestedGammaLevel';
{
 *  QTSetPixMapHandleRequestedGammaLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0 and later
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTSetPixMapHandleRequestedGammaLevel(pm: PixMapHandle; requestedGammaLevel: Fixed): OSErr; external name '_QTSetPixMapHandleRequestedGammaLevel';
{
 *  QuadToQuadMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
 {
QA 1227: This call takes four initial points (FixedPoint), four final points (FixedPoint), and defines a matrix that will map between them.
 }
type
	FixedPoint4 = array[0..3] of FixedPoint;
function QuadToQuadMatrix(const (*var*) source: FixedPoint4; const (*var*) dest: FixedPoint4; var map: MatrixRecord): OSErr; external name '_QuadToQuadMatrix';
const
	identityMatrixType			= $00;							{  result if matrix is identity  }
	translateMatrixType			= $01;							{  result if matrix translates  }
	scaleMatrixType				= $02;							{  result if matrix scales  }
	scaleTranslateMatrixType	= $03;							{  result if matrix scales and translates  }
	linearMatrixType			= $04;							{  result if matrix is general 2 x 2  }
	linearTranslateMatrixType	= $05;							{  result if matrix is general 2 x 2 and translates  }
	perspectiveMatrixType		= $06;							{  result if matrix is general 3 x 3  }


type
	MatrixFlags							= UInt16;
	{
	 *  GetMatrixType()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function GetMatrixType(const (*var*) m: MatrixRecord): SInt16; external name '_GetMatrixType';
{
 *  CopyMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CopyMatrix(const (*var*) m1: MatrixRecord; var m2: MatrixRecord); external name '_CopyMatrix';
{
 *  EqualMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function EqualMatrix(const (*var*) m1: MatrixRecord; const (*var*) m2: MatrixRecord): boolean; external name '_EqualMatrix';
{
 *  SetIdentityMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SetIdentityMatrix(var matrix: MatrixRecord); external name '_SetIdentityMatrix';
{
 *  TranslateMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure TranslateMatrix(var m: MatrixRecord; deltaH: Fixed; deltaV: Fixed); external name '_TranslateMatrix';
{
 *  RotateMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure RotateMatrix(var m: MatrixRecord; degrees: Fixed; aboutX: Fixed; aboutY: Fixed); external name '_RotateMatrix';
{
 *  ScaleMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ScaleMatrix(var m: MatrixRecord; scaleX: Fixed; scaleY: Fixed; aboutX: Fixed; aboutY: Fixed); external name '_ScaleMatrix';
{
 *  SkewMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure SkewMatrix(var m: MatrixRecord; skewX: Fixed; skewY: Fixed; aboutX: Fixed; aboutY: Fixed); external name '_SkewMatrix';
{
 *  TransformFixedPoints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TransformFixedPoints(const (*var*) m: MatrixRecord; var fpt: FixedPoint; count: SInt32): OSErr; external name '_TransformFixedPoints';
{
 *  TransformPoints()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TransformPoints(const (*var*) mp: MatrixRecord; var pt1: Point; count: SInt32): OSErr; external name '_TransformPoints';
{
 *  TransformFixedRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TransformFixedRect(const (*var*) m: MatrixRecord; var fr: FixedRect; var fpp: FixedPoint): boolean; external name '_TransformFixedRect';
{
 *  TransformRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function TransformRect(const (*var*) m: MatrixRecord; var r: Rect; var fpp: FixedPoint): boolean; external name '_TransformRect';
{
 *  InverseMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function InverseMatrix(const (*var*) m: MatrixRecord; var im: MatrixRecord): boolean; external name '_InverseMatrix';
{
 *  ConcatMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ConcatMatrix(const (*var*) a: MatrixRecord; var b: MatrixRecord); external name '_ConcatMatrix';
{
 *  RectMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure RectMatrix(var matrix: MatrixRecord; const (*var*) srcRect: Rect; const (*var*) dstRect: Rect); external name '_RectMatrix';
{
 *  MapMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure MapMatrix(var matrix: MatrixRecord; const (*var*) fromRect: Rect; const (*var*) toRect: Rect); external name '_MapMatrix';
{
 *  CompAdd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompAdd(var src: wide; var dst: wide); external name '_CompAdd';
{
 *  CompSub()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompSub(var src: wide; var dst: wide); external name '_CompSub';
{
 *  CompNeg()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompNeg(var dst: wide); external name '_CompNeg';
{
 *  CompShift()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompShift(var src: wide; shift: SInt16); external name '_CompShift';
{
 *  CompMul()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompMul(src1: SInt32; src2: SInt32; var dst: wide); external name '_CompMul';
{
 *  CompDiv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompDiv(var numerator: wide; denominator: SInt32; var remainder: SInt32): SInt32; external name '_CompDiv';
{
 *  CompFixMul()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompFixMul(var compSrc: wide; fixSrc: Fixed; var compDst: wide); external name '_CompFixMul';
{
 *  CompMulDiv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompMulDiv(var co: wide; mul: SInt32; divisor: SInt32); external name '_CompMulDiv';
{
 *  CompMulDivTrunc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure CompMulDivTrunc(var co: wide; mul: SInt32; divisor: SInt32; var remainder: SInt32); external name '_CompMulDivTrunc';
{
 *  CompCompare()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompCompare(const (*var*) a: wide; const (*var*) minusb: wide): SInt32; external name '_CompCompare';
{
 *  CompSquareRoot()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CompSquareRoot(const (*var*) src: wide): UInt32; external name '_CompSquareRoot';
{
 *  FixMulDiv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FixMulDiv(src: Fixed; mul: Fixed; divisor: Fixed): Fixed; external name '_FixMulDiv';
{
 *  UnsignedFixMulDiv()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function UnsignedFixMulDiv(src: Fixed; mul: Fixed; divisor: Fixed): Fixed; external name '_UnsignedFixMulDiv';
{
 *  FracSinCos()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FracSinCos(degree: Fixed; var cosOut: Fract): Fract; external name '_FracSinCos';
{
 *  FixExp2()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FixExp2(src: Fixed): Fixed; external name '_FixExp2';
{
 *  FixLog2()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FixLog2(src: Fixed): Fixed; external name '_FixLog2';
{
 *  FixPow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function FixPow(base: Fixed; exp: Fixed): Fixed; external name '_FixPow';
type
	GraphicsImportComponent				= ComponentInstance;

const
	GraphicsImporterComponentType = $67726970 (* 'grip' *);

	graphicsImporterUsesImageDecompressor = $00800000;

	quickTimeImageFileImageDescriptionAtom = $69647363 (* 'idsc' *);
	quickTimeImageFileImageDataAtom = $69646174 (* 'idat' *);
	quickTimeImageFileMetaDataAtom = $6D657461 (* 'meta' *);
	quickTimeImageFileColorSyncProfileAtom = $69696363 (* 'iicc' *);

	graphicsImporterDrawsAllPixels = 0;
	graphicsImporterDoesntDrawAllPixels = 1;
	graphicsImporterDontKnowIfDrawAllPixels = 2;

	{	 Flags for GraphicsImportSetFlags 	}
	kGraphicsImporterDontDoGammaCorrection = $00000001;
	kGraphicsImporterTrustResolutionFromFile = $00000002;
	kGraphicsImporterEnableSubPixelPositioning = $00000004;

	kGraphicsExportGroup		= $6578706F (* 'expo' *);
	kGraphicsExportFileType		= $66747970 (* 'ftyp' *);
	kGraphicsExportMIMEType		= $6D696D65 (* 'mime' *);
	kGraphicsExportExtension	= $65787420 (* 'ext ' *);
	kGraphicsExportDescription	= $64657363 (* 'desc' *);

	{	 User data types for layers of Photoshop files 	}
	kQTPhotoshopLayerMode		= $6C6D6F64 (* 'lmod' *);						{  OSType  }
	kQTPhotoshopLayerOpacity	= $6C6F7061 (* 'lopa' *);						{  UInt8, 0 = transparent .. 255 = opaque  }
	kQTPhotoshopLayerClipping	= $6C636C70 (* 'lclp' *);						{  UInt8, 0 = base, 1 = non-base  }
	kQTPhotoshopLayerFlags		= $6C666C67 (* 'lflg' *);						{  UInt8  }
	kQTPhotoshopLayerName		= $C2A96C6E (* '©lnm' *);						{  Text  }
	kQTPhotoshopLayerUnicodeName = $6C756E69 (* 'luni' *);						{  Unicode characters, not terminated  }

	{	 User data returned by graphics importers to suggest intended use for indexed images 	}
	kQTIndexedImageType			= $6E74683F (* 'nth?' *);						{  1 or more OSTypes, such as the following values:  }
	kQTIndexedImageIsThumbnail	= $6E3D7468 (* 'n=th' *);						{  The image at this index is a thumbnail.  }
	kQTIndexedImageIsLayer		= $6E3D6C79 (* 'n=ly' *);						{  The image at this index is a layer.  }
	kQTIndexedImageIsPage		= $6E3D7067 (* 'n=pg' *);						{  The image at this index is a page.  }
	kQTIndexedImageIsMultiResolution = $6E3D7273 (* 'n=rs' *);					{  The image at this index is one of several identical images at different resolutions.  }

	{	 Other user data types returned by graphics importers 	}
	kQTTIFFUserDataPrefix		= $74690000;					{  Added to some tag values in TIFF IFDs to generate user data codes.  (0x7469 is 'ti'.)  }
																{  For example, YCbCrPositioning is tag 0x0213, so its user data code is 0x74690213.  }
	kQTTIFFExifUserDataPrefix	= $65780000;					{  Added to tag values in Exif IFDs to generate user data codes.  (0x6578 is 'ex'.)  }
																{  For example, DateTimeOriginal is tag 0x9003, so its user data code is 0x65789003.  }
	kQTTIFFExifGPSUserDataPrefix = $67700000;					{  Added to tag values in Exif GPS IFDs to generate user data codes.  (0x6770 is 'gp'.)  }
																{  For example, GPSAltitude is tag 0x0006, so its user data code is 0x6770006.  }
	kQTAlphaMode				= $616C6D6F (* 'almo' *);						{  UInt32; eg, graphicsModeStraightAlpha or graphicsModePreBlackAlpha  }
	kQTAlphaModePreMulColor		= $616C6D70 (* 'almp' *);						{  RGBColor; used if kQTAlphaMode is graphicsModePreMulColorAlpha  }
	kUserDataIPTC				= $69707463 (* 'iptc' *);

	{	 Found in TIFF and Exif JPEG files 	}
	kQTTIFFUserDataOrientation	= $74690112;					{  1 SHORT  }
	kQTTIFFUserDataTransferFunction = $7469012D;				{  n SHORTs  }
	kQTTIFFUserDataWhitePoint	= $7469013E;					{  2 RATIONALs  }
	kQTTIFFUserDataPrimaryChromaticities = $7469013F;			{  6 RATIONALs  }
	kQTTIFFUserDataTransferRange = $74690156;					{  6 SHORTs  }
	kQTTIFFUserDataYCbCrPositioning = $74690213;				{  1 SHORT  }
	kQTTIFFUserDataReferenceBlackWhite = $74690214;				{  n LONGs  }

	{	 Found in GeoTIFF files; defined in the GeoTIFF 1.0 spec 	}
	kQTTIFFUserDataModelPixelScale = $7469830E;					{  3 DOUBLEs  }
	kQTTIFFUserDataModelTransformation = $746985D8;				{  16 DOUBLEs  }
	kQTTIFFUserDataModelTiepoint = $74698482;					{  n DOUBLEs  }
	kQTTIFFUserDataGeoKeyDirectory = $746987AF;					{  n SHORTs  }
	kQTTIFFUserDataGeoDoubleParams = $746987B0;					{  n DOUBLEs  }
	kQTTIFFUserDataGeoAsciiParams = $746987B1;					{  n ASCIIs  }
	kQTTIFFUserDataIntergraphMatrix = $74698480;				{  16 or 17 DOUBLEs  }

	{	 Found in Exif TIFF and Exif JPEG files; defined in the Exif 2.1 spec 	}
	kQTExifUserDataExifVersion	= $65789000;					{  4 bytes (import only)  }
	kQTExifUserDataFlashPixVersion = $6578A000;					{  4 bytes  }
	kQTExifUserDataColorSpace	= $6578A001;					{  1 SHORT  }
	kQTExifUserDataComponentsConfiguration = $65789101;			{  4 bytes  }
	kQTExifUserDataCompressedBitsPerPixel = $65789102;			{  1 RATIONAL  }
	kQTExifUserDataPixelXDimension = $6578A002;					{  1 SHORT or LONG  }
	kQTExifUserDataPixelYDimension = $6578A003;					{  1 SHORT or LONG  }
	kQTExifUserDataMakerNote	= $6578927C;					{  n bytes  }
	kQTExifUserDataUserComment	= $6578928C;					{  n bytes  }
	kQTExifUserDataRelatedSoundFile = $6578A004;				{  13 ASCIIs }
	kQTExifUserDataDateTimeOriginal = $65789003;				{  20 ASCIIs  }
	kQTExifUserDataDateTimeDigitized = $65789004;				{  20 ASCIIs  }
	kQTExifUserDataSubSecTime	= $65789290;					{  n ASCIIs  }
	kQTExifUserDataSubSecTimeOriginal = $65789291;				{  n ASCIIs  }
	kQTExifUserDataSubSecTimeDigitized = $65789292;				{  n ASCIIs  }
	kQTExifUserDataExposureTime	= $6578829A;					{  1 RATIONAL  }
	kQTExifUserDataFNumber		= $6578829D;					{  1 RATIONAL  }
	kQTExifUserDataExposureProgram = $65788822;					{  1 SHORT  }
	kQTExifUserDataSpectralSensitivity = $65788824;				{  n ASCIIs  }
	kQTExifUserDataISOSpeedRatings = $65788827;					{  n SHORTs  }
	kQTExifUserDataShutterSpeedValue = $65789201;				{  1 SIGNED RATIONAL  }
	kQTExifUserDataApertureValue = $65789202;					{  1 RATIONAL  }
	kQTExifUserDataBrightnessValue = $65789203;					{  1 SIGNED RATIONAL  }
	kQTExifUserDataExposureBiasValue = $65789204;				{  1 SIGNED RATIONAL  }
	kQTExifUserDataMaxApertureValue = $65789205;				{  1 RATIONAL  }
	kQTExifUserDataSubjectDistance = $65789206;					{  1 RATIONAL  }
	kQTExifUserDataMeteringMode	= $65789207;					{  1 SHORT  }
	kQTExifUserDataLightSource	= $65789208;					{  1 SHORT  }
	kQTExifUserDataFlash		= $65789209;					{  1 SHORT  }
	kQTExifUserDataFocalLength	= $6578920A;					{  1 RATIONAL  }
	kQTExifUserDataFlashEnergy	= $6578A20B;					{  1 RATIONAL  }
	kQTExifUserDataFocalPlaneXResolution = $6578A20E;			{  1 RATIONAL  }
	kQTExifUserDataFocalPlaneYResolution = $6578A20F;			{  1 RATIONAL  }
	kQTExifUserDataFocalPlaneResolutionUnit = $6578A210;		{  1 SHORT  }
	kQTExifUserDataSubjectLocation = $6578A214;					{  1 SHORT  }
	kQTExifUserDataExposureIndex = $6578A215;					{  1 RATIONAL  }
	kQTExifUserDataSensingMethod = $6578A217;					{  1 SHORT  }
	kQTExifUserDataFileSource	= $6578A300;					{  1 UNDEFINED  }
	kQTExifUserDataSceneType	= $6578A301;					{  1 UNDEFINED  }

	{	 Found in some Exif TIFF and Exif JPEG files; defined in the Exif 2.1 spec 	}
	kQTExifUserDataGPSVersionID	= $06770000;					{  4 BYTEs  }
	kQTExifUserDataGPSLatitudeRef = $06770001;					{  2 ASCIIs }
	kQTExifUserDataGPSLatitude	= $06770002;					{  3 RATIONALs  }
	kQTExifUserDataGPSLongitudeRef = $06770003;					{  2 ASCIIs  }
	kQTExifUserDataGPSLongitude	= $06770004;					{  3 RATIONALs  }
	kQTExifUserDataGPSAltitudeRef = $06770005;					{  1 BYTE  }
	kQTExifUserDataGPSAltitude	= $06770006;					{  1 RATIONAL  }
	kQTExifUserDataGPSTimeStamp	= $06770007;					{  3 RATIONALs  }
	kQTExifUserDataGPSSatellites = $06770008;					{  n ASCIIs  }
	kQTExifUserDataGPSStatus	= $06770009;					{  2 ASCIIs  }
	kQTExifUserDataGPSMeasureMode = $0677000A;					{  2 ASCIIs  }
	kQTExifUserDataGPSDOP		= $0677000B;					{  1 RATIONAL  }
	kQTExifUserDataGPSSpeedRef	= $0677000C;					{  2 ASCIIs  }
	kQTExifUserDataGPSSpeed		= $0677000D;					{  1 RATIONAL  }
	kQTExifUserDataGPSTrackRef	= $0677000E;					{  2 ASCIIs  }
	kQTExifUserDataGPSTrack		= $0677000F;					{  1 RATIONAL  }
	kQTExifUserDataGPSImgDirectionRef = $06770010;				{  2 ASCIIs  }
	kQTExifUserDataGPSImgDirection = $06770011;					{  1 RATIONAL  }
	kQTExifUserDataGPSMapDatum	= $06770012;					{  n ASCII  }
	kQTExifUserDataGPSDestLatitudeRef = $06770013;				{  2 ASCIIs  }
	kQTExifUserDataGPSDestLatitude = $06770014;					{  3 RATIONALs  }
	kQTExifUserDataGPSDestLongitudeRef = $06770015;				{  2 ASCIIs  }
	kQTExifUserDataGPSDestLongitude = $06770016;				{  3 RATIONALs  }
	kQTExifUserDataGPSDestBearingRef = $06770017;				{  2 ASCIIs  }
	kQTExifUserDataGPSDestBearing = $06770018;					{  1 RATIONAL  }
	kQTExifUserDataGPSDestDistanceRef = $06770019;				{  2 ASCIIs  }
	kQTExifUserDataGPSDestDistance = $0677001A;					{  1 RATIONAL  }


	{	* These are GraphicsImport procedures *	}
	{
	 *  GraphicsImportSetDataReference()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function GraphicsImportSetDataReference(ci: GraphicsImportComponent; dataRef: Handle; dataReType: OSType): ComponentResult; external name '_GraphicsImportSetDataReference';
{
 *  GraphicsImportGetDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetDataReference(ci: GraphicsImportComponent; var dataRef: Handle; var dataReType: OSType): ComponentResult; external name '_GraphicsImportGetDataReference';
{
 *  GraphicsImportSetDataFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetDataFile(ci: GraphicsImportComponent; const (*var*) theFile: FSSpec): ComponentResult; external name '_GraphicsImportSetDataFile';
{
 *  GraphicsImportGetDataFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetDataFile(ci: GraphicsImportComponent; var theFile: FSSpec): ComponentResult; external name '_GraphicsImportGetDataFile';
{
 *  GraphicsImportSetDataHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetDataHandle(ci: GraphicsImportComponent; h: Handle): ComponentResult; external name '_GraphicsImportSetDataHandle';
{
 *  GraphicsImportGetDataHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetDataHandle(ci: GraphicsImportComponent; var h: Handle): ComponentResult; external name '_GraphicsImportGetDataHandle';
{
 *  GraphicsImportGetImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetImageDescription(ci: GraphicsImportComponent; var desc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsImportGetImageDescription';
{
 *  GraphicsImportGetDataOffsetAndSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetDataOffsetAndSize(ci: GraphicsImportComponent; var offset: UInt32; var size: UInt32): ComponentResult; external name '_GraphicsImportGetDataOffsetAndSize';
{
 *  GraphicsImportReadData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportReadData(ci: GraphicsImportComponent; dataPtr: UnivPtr; dataOffset: UInt32; dataSize: UInt32): ComponentResult; external name '_GraphicsImportReadData';
{
 *  GraphicsImportSetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetClip(ci: GraphicsImportComponent; clipRgn: RgnHandle): ComponentResult; external name '_GraphicsImportSetClip';
{
 *  GraphicsImportGetClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetClip(ci: GraphicsImportComponent; var clipRgn: RgnHandle): ComponentResult; external name '_GraphicsImportGetClip';
{
 *  GraphicsImportSetSourceRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetSourceRect(ci: GraphicsImportComponent; const (*var*) sourceRect: Rect): ComponentResult; external name '_GraphicsImportSetSourceRect';
{
 *  GraphicsImportGetSourceRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetSourceRect(ci: GraphicsImportComponent; var sourceRect: Rect): ComponentResult; external name '_GraphicsImportGetSourceRect';
{
 *  GraphicsImportGetNaturalBounds()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetNaturalBounds(ci: GraphicsImportComponent; var naturalBounds: Rect): ComponentResult; external name '_GraphicsImportGetNaturalBounds';
{
 *  GraphicsImportDraw()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportDraw(ci: GraphicsImportComponent): ComponentResult; external name '_GraphicsImportDraw';
{
 *  GraphicsImportSetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetGWorld(ci: GraphicsImportComponent; port: CGrafPtr; gd: GDHandle): ComponentResult; external name '_GraphicsImportSetGWorld';
{
 *  GraphicsImportGetGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetGWorld(ci: GraphicsImportComponent; var port: CGrafPtr; var gd: GDHandle): ComponentResult; external name '_GraphicsImportGetGWorld';
{
 *  GraphicsImportSetMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetMatrix(ci: GraphicsImportComponent; const (*var*) matrix: MatrixRecord): ComponentResult; external name '_GraphicsImportSetMatrix';
{
 *  GraphicsImportGetMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetMatrix(ci: GraphicsImportComponent; var matrix: MatrixRecord): ComponentResult; external name '_GraphicsImportGetMatrix';
{
 *  GraphicsImportSetBoundsRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetBoundsRect(ci: GraphicsImportComponent; const (*var*) bounds: Rect): ComponentResult; external name '_GraphicsImportSetBoundsRect';
{
 *  GraphicsImportGetBoundsRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetBoundsRect(ci: GraphicsImportComponent; var bounds: Rect): ComponentResult; external name '_GraphicsImportGetBoundsRect';
{
 *  GraphicsImportSaveAsPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSaveAsPicture(ci: GraphicsImportComponent; const (*var*) fss: FSSpec; scriptTag: ScriptCode): ComponentResult; external name '_GraphicsImportSaveAsPicture';
{
 *  GraphicsImportSetGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetGraphicsMode(ci: GraphicsImportComponent; graphicsMode: SInt32; const (*var*) opColor: RGBColor): ComponentResult; external name '_GraphicsImportSetGraphicsMode';
{
 *  GraphicsImportGetGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetGraphicsMode(ci: GraphicsImportComponent; var graphicsMode: SInt32; var opColor: RGBColor): ComponentResult; external name '_GraphicsImportGetGraphicsMode';
{
 *  GraphicsImportSetQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetQuality(ci: GraphicsImportComponent; quality: CodecQ): ComponentResult; external name '_GraphicsImportSetQuality';
{
 *  GraphicsImportGetQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetQuality(ci: GraphicsImportComponent; var quality: CodecQ): ComponentResult; external name '_GraphicsImportGetQuality';
{
 *  GraphicsImportSaveAsQuickTimeImageFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSaveAsQuickTimeImageFile(ci: GraphicsImportComponent; const (*var*) fss: FSSpec; scriptTag: ScriptCode): ComponentResult; external name '_GraphicsImportSaveAsQuickTimeImageFile';
{
 *  GraphicsImportSetDataReferenceOffsetAndLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetDataReferenceOffsetAndLimit(ci: GraphicsImportComponent; offset: UInt32; limit: UInt32): ComponentResult; external name '_GraphicsImportSetDataReferenceOffsetAndLimit';
{
 *  GraphicsImportGetDataReferenceOffsetAndLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetDataReferenceOffsetAndLimit(ci: GraphicsImportComponent; var offset: UInt32; var limit: UInt32): ComponentResult; external name '_GraphicsImportGetDataReferenceOffsetAndLimit';
{
 *  GraphicsImportGetAliasedDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetAliasedDataReference(ci: GraphicsImportComponent; var dataRef: Handle; var dataRefType: OSType): ComponentResult; external name '_GraphicsImportGetAliasedDataReference';
{
 *  GraphicsImportValidate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportValidate(ci: GraphicsImportComponent; var valid: boolean): ComponentResult; external name '_GraphicsImportValidate';
{
 *  GraphicsImportGetMetaData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetMetaData(ci: GraphicsImportComponent; userData: UnivPtr): ComponentResult; external name '_GraphicsImportGetMetaData';
{
 *  GraphicsImportGetMIMETypeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetMIMETypeList(ci: GraphicsImportComponent; qtAtomContainerPtr: UnivPtr): ComponentResult; external name '_GraphicsImportGetMIMETypeList';
{
 *  GraphicsImportDoesDrawAllPixels()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportDoesDrawAllPixels(ci: GraphicsImportComponent; var drawsAllPixels: SInt16): ComponentResult; external name '_GraphicsImportDoesDrawAllPixels';
{
 *  GraphicsImportGetAsPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetAsPicture(ci: GraphicsImportComponent; var picture: PicHandle): ComponentResult; external name '_GraphicsImportGetAsPicture';
{
 *  GraphicsImportExportImageFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportExportImageFile(ci: GraphicsImportComponent; fileType: OSType; fileCreator: OSType; const (*var*) fss: FSSpec; scriptTag: ScriptCode): ComponentResult; external name '_GraphicsImportExportImageFile';
{
 *  GraphicsImportGetExportImageTypeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetExportImageTypeList(ci: GraphicsImportComponent; qtAtomContainerPtr: UnivPtr): ComponentResult; external name '_GraphicsImportGetExportImageTypeList';
{
 *  GraphicsImportDoExportImageFileDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportDoExportImageFileDialog(ci: GraphicsImportComponent; const (*var*) inDefaultSpec: FSSpec; prompt: StringPtr; filterProc: ModalFilterYDUPP; var outExportedType: OSType; var outExportedSpec: FSSpec; var outScriptTag: ScriptCode): ComponentResult; external name '_GraphicsImportDoExportImageFileDialog';
{
 *  GraphicsImportGetExportSettingsAsAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetExportSettingsAsAtomContainer(ci: GraphicsImportComponent; qtAtomContainerPtr: UnivPtr): ComponentResult; external name '_GraphicsImportGetExportSettingsAsAtomContainer';
{
 *  GraphicsImportSetExportSettingsFromAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetExportSettingsFromAtomContainer(ci: GraphicsImportComponent; qtAtomContainer: UnivPtr): ComponentResult; external name '_GraphicsImportSetExportSettingsFromAtomContainer';
{
 *  GraphicsImportSetProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportSetProgressProc(ci: GraphicsImportComponent; progressProc: ICMProgressProcRecordPtr): ComponentResult; external name '_GraphicsImportSetProgressProc';
{
 *  GraphicsImportGetProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 3.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function GraphicsImportGetProgressProc(ci: GraphicsImportComponent; progressProc: ICMProgressProcRecordPtr): ComponentResult; external name '_GraphicsImportGetProgressProc';
{
 *  GraphicsImportGetImageCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetImageCount(ci: GraphicsImportComponent; var imageCount: UInt32): ComponentResult; external name '_GraphicsImportGetImageCount';
{
 *  GraphicsImportSetImageIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportSetImageIndex(ci: GraphicsImportComponent; imageIndex: UInt32): ComponentResult; external name '_GraphicsImportSetImageIndex';
{
 *  GraphicsImportGetImageIndex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetImageIndex(ci: GraphicsImportComponent; var imageIndex: UInt32): ComponentResult; external name '_GraphicsImportGetImageIndex';
{
 *  GraphicsImportGetDataOffsetAndSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDataOffsetAndSize64(ci: GraphicsImportComponent; var offset: wide; var size: wide): ComponentResult; external name '_GraphicsImportGetDataOffsetAndSize64';
{
 *  GraphicsImportReadData64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportReadData64(ci: GraphicsImportComponent; dataPtr: UnivPtr; const (*var*) dataOffset: wide; dataSize: UInt32): ComponentResult; external name '_GraphicsImportReadData64';
{
 *  GraphicsImportSetDataReferenceOffsetAndLimit64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportSetDataReferenceOffsetAndLimit64(ci: GraphicsImportComponent; const (*var*) offset: wide; const (*var*) limit: wide): ComponentResult; external name '_GraphicsImportSetDataReferenceOffsetAndLimit64';
{
 *  GraphicsImportGetDataReferenceOffsetAndLimit64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDataReferenceOffsetAndLimit64(ci: GraphicsImportComponent; var offset: wide; var limit: wide): ComponentResult; external name '_GraphicsImportGetDataReferenceOffsetAndLimit64';
{
 *  GraphicsImportGetDefaultMatrix()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDefaultMatrix(ci: GraphicsImportComponent; var defaultMatrix: MatrixRecord): ComponentResult; external name '_GraphicsImportGetDefaultMatrix';
{
 *  GraphicsImportGetDefaultClip()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDefaultClip(ci: GraphicsImportComponent; var defaultRgn: RgnHandle): ComponentResult; external name '_GraphicsImportGetDefaultClip';
{
 *  GraphicsImportGetDefaultGraphicsMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDefaultGraphicsMode(ci: GraphicsImportComponent; var defaultGraphicsMode: SInt32; var defaultOpColor: RGBColor): ComponentResult; external name '_GraphicsImportGetDefaultGraphicsMode';
{
 *  GraphicsImportGetDefaultSourceRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDefaultSourceRect(ci: GraphicsImportComponent; var defaultSourceRect: Rect): ComponentResult; external name '_GraphicsImportGetDefaultSourceRect';
{
 *  GraphicsImportGetColorSyncProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetColorSyncProfile(ci: GraphicsImportComponent; var profile: Handle): ComponentResult; external name '_GraphicsImportGetColorSyncProfile';
{
 *  GraphicsImportSetDestRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportSetDestRect(ci: GraphicsImportComponent; const (*var*) destRect: Rect): ComponentResult; external name '_GraphicsImportSetDestRect';
{
 *  GraphicsImportGetDestRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetDestRect(ci: GraphicsImportComponent; var destRect: Rect): ComponentResult; external name '_GraphicsImportGetDestRect';
{
 *  GraphicsImportSetFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportSetFlags(ci: GraphicsImportComponent; flags: SInt32): ComponentResult; external name '_GraphicsImportSetFlags';
{
 *  GraphicsImportGetFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsImportGetFlags(ci: GraphicsImportComponent; var flags: SInt32): ComponentResult; external name '_GraphicsImportGetFlags';
{ 2 private selectors }
{
 *  GraphicsImportGetBaseDataOffsetAndSize64()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0.2 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in qtmlClient.lib 5.0.2 and later
 }
function GraphicsImportGetBaseDataOffsetAndSize64(ci: GraphicsImportComponent; var offset: wide; var size: wide): ComponentResult; external name '_GraphicsImportGetBaseDataOffsetAndSize64';
{
 *  GraphicsImportSetImageIndexToThumbnail()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 6.0 and later
 *    CarbonLib:        in CarbonLib 1.6 and later
 *    Mac OS X:         in version 10.2 and later
 *    Windows:          in qtmlClient.lib 6.0 and later
 }
function GraphicsImportSetImageIndexToThumbnail(ci: GraphicsImportComponent): ComponentResult; external name '_GraphicsImportSetImageIndexToThumbnail';
type
	GraphicsExportComponent				= ComponentInstance;

const
	GraphicsExporterComponentType = $67726578 (* 'grex' *);
	kBaseGraphicsExporterSubType = $62617365 (* 'base' *);

	graphicsExporterIsBaseExporter = $00000001;
	graphicsExporterCanTranscode = $00000002;
	graphicsExporterUsesImageCompressor = $00000004;


type
	QTResolutionSettingsPtr = ^QTResolutionSettings;
	QTResolutionSettings = record
		horizontalResolution:	Fixed;
		verticalResolution:		Fixed;
	end;

	QTTargetDataSizePtr = ^QTTargetDataSize;
	QTTargetDataSize = record
		targetDataSize:			UInt32;
	end;

	QTThumbnailSettingsPtr = ^QTThumbnailSettings;
	QTThumbnailSettings = record
		enableThumbnail:		SInt32;								{  a thoroughly padded Boolean }
		maxThumbnailWidth:		SInt32;								{  set to zero to let someone else decide }
		maxThumbnailHeight:		SInt32;								{  set to zero to let someone else decide }
	end;


const
	kQTResolutionSettings		= $7265736F (* 'reso' *);
	kQTTargetDataSize			= $6461737A (* 'dasz' *);
	kQTDontRecompress			= $646E7472 (* 'dntr' *);
	kQTInterlaceStyle			= $696C6163 (* 'ilac' *);
	kQTColorSyncProfile			= $69636370 (* 'iccp' *);
	kQTThumbnailSettings		= $7468756D (* 'thum' *);
	kQTEnableExif				= $65786966 (* 'exif' *);						{  UInt8 (boolean) }
	kQTMetaData					= $6D657461 (* 'meta' *);

	kQTTIFFCompressionMethod	= $74696663 (* 'tifc' *);						{  UInt32 }
	kQTTIFFCompression_None		= 1;
	kQTTIFFCompression_PackBits	= 32773;
	kQTTIFFLittleEndian			= $74696665 (* 'tife' *);						{  UInt8 (boolean) }

	kQTPNGFilterPreference		= $706E6766 (* 'pngf' *);						{  UInt32 }
	kQTPNGFilterBestForColorType = $62666C74 (* 'bflt' *);
	kQTPNGFilterNone			= 0;
	kQTPNGFilterSub				= 1;
	kQTPNGFilterUp				= 2;
	kQTPNGFilterAverage			= 3;
	kQTPNGFilterPaeth			= 4;
	kQTPNGFilterAdaptivePerRow	= $61666C74 (* 'aflt' *);
	kQTPNGInterlaceStyle		= $696C6163 (* 'ilac' *);						{  UInt32 }
	kQTPNGInterlaceNone			= 0;
	kQTPNGInterlaceAdam7		= 1;


	{	* These are GraphicsExport procedures *	}
	{	 To use: set the input and output (and other settings as desired) and call GEDoExport. 	}
	{
	 *  GraphicsExportDoExport()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 4.0 and later
	 	}
function GraphicsExportDoExport(ci: GraphicsExportComponent; var actualSizeWritten: UInt32): ComponentResult; external name '_GraphicsExportDoExport';
{ Used for internal communication between the base and format-specific graphics exporter: }
{
 *  GraphicsExportCanTranscode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportCanTranscode(ci: GraphicsExportComponent; var canTranscode: boolean): ComponentResult; external name '_GraphicsExportCanTranscode';
{
 *  GraphicsExportDoTranscode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportDoTranscode(ci: GraphicsExportComponent): ComponentResult; external name '_GraphicsExportDoTranscode';
{
 *  GraphicsExportCanUseCompressor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportCanUseCompressor(ci: GraphicsExportComponent; var canUseCompressor: boolean; codecSettingsAtomContainerPtr: UnivPtr): ComponentResult; external name '_GraphicsExportCanUseCompressor';
{
 *  GraphicsExportDoUseCompressor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportDoUseCompressor(ci: GraphicsExportComponent; codecSettingsAtomContainer: UnivPtr; var outDesc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsExportDoUseCompressor';
{
 *  GraphicsExportDoStandaloneExport()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportDoStandaloneExport(ci: GraphicsExportComponent): ComponentResult; external name '_GraphicsExportDoStandaloneExport';
{ Queries applications can make of a format-specific graphics exporter: }
{
 *  GraphicsExportGetDefaultFileTypeAndCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetDefaultFileTypeAndCreator(ci: GraphicsExportComponent; var fileType: OSType; var fileCreator: OSType): ComponentResult; external name '_GraphicsExportGetDefaultFileTypeAndCreator';
{
 *  GraphicsExportGetDefaultFileNameExtension()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetDefaultFileNameExtension(ci: GraphicsExportComponent; var fileNameExtension: OSType): ComponentResult; external name '_GraphicsExportGetDefaultFileNameExtension';
{
 *  GraphicsExportGetMIMETypeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetMIMETypeList(ci: GraphicsExportComponent; qtAtomContainerPtr: UnivPtr): ComponentResult; external name '_GraphicsExportGetMIMETypeList';
{ GraphicsExportIsTranscodePossibleFromCurrentInput is removed; call GraphicsExportCanTranscode instead }
{ Graphics exporter settings: }
{
 *  GraphicsExportRequestSettings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportRequestSettings(ci: GraphicsExportComponent; filterProc: ModalFilterYDUPP; yourDataProc: UnivPtr): ComponentResult; external name '_GraphicsExportRequestSettings';
{
 *  GraphicsExportSetSettingsFromAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetSettingsFromAtomContainer(ci: GraphicsExportComponent; qtAtomContainer: UnivPtr): ComponentResult; external name '_GraphicsExportSetSettingsFromAtomContainer';
{
 *  GraphicsExportGetSettingsAsAtomContainer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetSettingsAsAtomContainer(ci: GraphicsExportComponent; qtAtomContainerPtr: UnivPtr): ComponentResult; external name '_GraphicsExportGetSettingsAsAtomContainer';
{
 *  GraphicsExportGetSettingsAsText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetSettingsAsText(ci: GraphicsExportComponent; var theText: Handle): ComponentResult; external name '_GraphicsExportGetSettingsAsText';
{ Graphics exporters may implement some or none of the following: }
{
 *  GraphicsExportSetDontRecompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetDontRecompress(ci: GraphicsExportComponent; dontRecompress: boolean): ComponentResult; external name '_GraphicsExportSetDontRecompress';
{
 *  GraphicsExportGetDontRecompress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetDontRecompress(ci: GraphicsExportComponent; var dontRecompress: boolean): ComponentResult; external name '_GraphicsExportGetDontRecompress';
{
 *  GraphicsExportSetInterlaceStyle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInterlaceStyle(ci: GraphicsExportComponent; interlaceStyle: UInt32): ComponentResult; external name '_GraphicsExportSetInterlaceStyle';
{
 *  GraphicsExportGetInterlaceStyle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInterlaceStyle(ci: GraphicsExportComponent; var interlaceStyle: UInt32): ComponentResult; external name '_GraphicsExportGetInterlaceStyle';
{
 *  GraphicsExportSetMetaData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetMetaData(ci: GraphicsExportComponent; userData: UnivPtr): ComponentResult; external name '_GraphicsExportSetMetaData';
{
 *  GraphicsExportGetMetaData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetMetaData(ci: GraphicsExportComponent; userData: UnivPtr): ComponentResult; external name '_GraphicsExportGetMetaData';
{
 *  GraphicsExportSetTargetDataSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetTargetDataSize(ci: GraphicsExportComponent; targetDataSize: UInt32): ComponentResult; external name '_GraphicsExportSetTargetDataSize';
{
 *  GraphicsExportGetTargetDataSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetTargetDataSize(ci: GraphicsExportComponent; var targetDataSize: UInt32): ComponentResult; external name '_GraphicsExportGetTargetDataSize';
{
 *  GraphicsExportSetCompressionMethod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetCompressionMethod(ci: GraphicsExportComponent; compressionMethod: SInt32): ComponentResult; external name '_GraphicsExportSetCompressionMethod';
{
 *  GraphicsExportGetCompressionMethod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetCompressionMethod(ci: GraphicsExportComponent; var compressionMethod: SInt32): ComponentResult; external name '_GraphicsExportGetCompressionMethod';
{
 *  GraphicsExportSetCompressionQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetCompressionQuality(ci: GraphicsExportComponent; spatialQuality: CodecQ): ComponentResult; external name '_GraphicsExportSetCompressionQuality';
{
 *  GraphicsExportGetCompressionQuality()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetCompressionQuality(ci: GraphicsExportComponent; var spatialQuality: CodecQ): ComponentResult; external name '_GraphicsExportGetCompressionQuality';
{
 *  GraphicsExportSetResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetResolution(ci: GraphicsExportComponent; horizontalResolution: Fixed; verticalResolution: Fixed): ComponentResult; external name '_GraphicsExportSetResolution';
{
 *  GraphicsExportGetResolution()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetResolution(ci: GraphicsExportComponent; var horizontalResolution: Fixed; var verticalResolution: Fixed): ComponentResult; external name '_GraphicsExportGetResolution';
{
 *  GraphicsExportSetDepth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetDepth(ci: GraphicsExportComponent; depth: SInt32): ComponentResult; external name '_GraphicsExportSetDepth';
{
 *  GraphicsExportGetDepth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetDepth(ci: GraphicsExportComponent; var depth: SInt32): ComponentResult; external name '_GraphicsExportGetDepth';
{ (2 unused selectors) }
{
 *  GraphicsExportSetColorSyncProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetColorSyncProfile(ci: GraphicsExportComponent; colorSyncProfile: Handle): ComponentResult; external name '_GraphicsExportSetColorSyncProfile';
{
 *  GraphicsExportGetColorSyncProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetColorSyncProfile(ci: GraphicsExportComponent; var colorSyncProfile: Handle): ComponentResult; external name '_GraphicsExportGetColorSyncProfile';
{ Always implemented by the base graphics exporter: }
{
 *  GraphicsExportSetProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetProgressProc(ci: GraphicsExportComponent; progressProc: ICMProgressProcRecordPtr): ComponentResult; external name '_GraphicsExportSetProgressProc';
{
 *  GraphicsExportGetProgressProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetProgressProc(ci: GraphicsExportComponent; progressProc: ICMProgressProcRecordPtr): ComponentResult; external name '_GraphicsExportGetProgressProc';
{ Sources for the input image: }
{
 *  GraphicsExportSetInputDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputDataReference(ci: GraphicsExportComponent; dataRef: Handle; dataRefType: OSType; desc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsExportSetInputDataReference';
{
 *  GraphicsExportGetInputDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputDataReference(ci: GraphicsExportComponent; var dataRef: Handle; var dataRefType: OSType): ComponentResult; external name '_GraphicsExportGetInputDataReference';
{
 *  GraphicsExportSetInputFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputFile(ci: GraphicsExportComponent; const (*var*) theFile: FSSpec; desc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsExportSetInputFile';
{
 *  GraphicsExportGetInputFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputFile(ci: GraphicsExportComponent; var theFile: FSSpec): ComponentResult; external name '_GraphicsExportGetInputFile';
{
 *  GraphicsExportSetInputHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputHandle(ci: GraphicsExportComponent; h: Handle; desc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsExportSetInputHandle';
{
 *  GraphicsExportGetInputHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputHandle(ci: GraphicsExportComponent; var h: Handle): ComponentResult; external name '_GraphicsExportGetInputHandle';
{
 *  GraphicsExportSetInputPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputPtr(ci: GraphicsExportComponent; p: Ptr; size: UInt32; desc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsExportSetInputPtr';
{
 *  GraphicsExportGetInputPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputPtr(ci: GraphicsExportComponent; var p: Ptr; var size: UInt32): ComponentResult; external name '_GraphicsExportGetInputPtr';
{
 *  GraphicsExportSetInputGraphicsImporter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputGraphicsImporter(ci: GraphicsExportComponent; grip: GraphicsImportComponent): ComponentResult; external name '_GraphicsExportSetInputGraphicsImporter';
{
 *  GraphicsExportGetInputGraphicsImporter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputGraphicsImporter(ci: GraphicsExportComponent; var grip: GraphicsImportComponent): ComponentResult; external name '_GraphicsExportGetInputGraphicsImporter';
{
 *  GraphicsExportSetInputPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputPicture(ci: GraphicsExportComponent; picture: PicHandle): ComponentResult; external name '_GraphicsExportSetInputPicture';
{
 *  GraphicsExportGetInputPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputPicture(ci: GraphicsExportComponent; var picture: PicHandle): ComponentResult; external name '_GraphicsExportGetInputPicture';
{
 *  GraphicsExportSetInputGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputGWorld(ci: GraphicsExportComponent; gworld: GWorldPtr): ComponentResult; external name '_GraphicsExportSetInputGWorld';
{
 *  GraphicsExportGetInputGWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputGWorld(ci: GraphicsExportComponent; var gworld: GWorldPtr): ComponentResult; external name '_GraphicsExportGetInputGWorld';
{
 *  GraphicsExportSetInputPixmap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputPixmap(ci: GraphicsExportComponent; pixmap: PixMapHandle): ComponentResult; external name '_GraphicsExportSetInputPixmap';
{
 *  GraphicsExportGetInputPixmap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputPixmap(ci: GraphicsExportComponent; var pixmap: PixMapHandle): ComponentResult; external name '_GraphicsExportGetInputPixmap';
{ Only applicable when the input is a data reference, file, handle or ptr: }
{
 *  GraphicsExportSetInputOffsetAndLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetInputOffsetAndLimit(ci: GraphicsExportComponent; offset: UInt32; limit: UInt32): ComponentResult; external name '_GraphicsExportSetInputOffsetAndLimit';
{
 *  GraphicsExportGetInputOffsetAndLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputOffsetAndLimit(ci: GraphicsExportComponent; var offset: UInt32; var limit: UInt32): ComponentResult; external name '_GraphicsExportGetInputOffsetAndLimit';
{ Used by format-specific graphics exporters when transcoding: }
{
 *  GraphicsExportMayExporterReadInputData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportMayExporterReadInputData(ci: GraphicsExportComponent; var mayReadInputData: boolean): ComponentResult; external name '_GraphicsExportMayExporterReadInputData';
{
 *  GraphicsExportGetInputDataSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputDataSize(ci: GraphicsExportComponent; var size: UInt32): ComponentResult; external name '_GraphicsExportGetInputDataSize';
{
 *  GraphicsExportReadInputData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportReadInputData(ci: GraphicsExportComponent; dataPtr: UnivPtr; dataOffset: UInt32; dataSize: UInt32): ComponentResult; external name '_GraphicsExportReadInputData';
{ Used by format-specific graphics exporters, especially when doing standalone export: }
{
 *  GraphicsExportGetInputImageDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputImageDescription(ci: GraphicsExportComponent; var desc: ImageDescriptionHandle): ComponentResult; external name '_GraphicsExportGetInputImageDescription';
{
 *  GraphicsExportGetInputImageDimensions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputImageDimensions(ci: GraphicsExportComponent; var dimensions: Rect): ComponentResult; external name '_GraphicsExportGetInputImageDimensions';
{
 *  GraphicsExportGetInputImageDepth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetInputImageDepth(ci: GraphicsExportComponent; var inputDepth: SInt32): ComponentResult; external name '_GraphicsExportGetInputImageDepth';
{
 *  GraphicsExportDrawInputImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportDrawInputImage(ci: GraphicsExportComponent; gw: CGrafPtr; gd: GDHandle; const (*var*) srcRect: Rect; const (*var*) dstRect: Rect): ComponentResult; external name '_GraphicsExportDrawInputImage';
{ Destinations for the output image: }
{
 *  GraphicsExportSetOutputDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetOutputDataReference(ci: GraphicsExportComponent; dataRef: Handle; dataRefType: OSType): ComponentResult; external name '_GraphicsExportSetOutputDataReference';
{
 *  GraphicsExportGetOutputDataReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetOutputDataReference(ci: GraphicsExportComponent; var dataRef: Handle; var dataRefType: OSType): ComponentResult; external name '_GraphicsExportGetOutputDataReference';
{
 *  GraphicsExportSetOutputFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetOutputFile(ci: GraphicsExportComponent; const (*var*) theFile: FSSpec): ComponentResult; external name '_GraphicsExportSetOutputFile';
{
 *  GraphicsExportGetOutputFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetOutputFile(ci: GraphicsExportComponent; var theFile: FSSpec): ComponentResult; external name '_GraphicsExportGetOutputFile';
{
 *  GraphicsExportSetOutputHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetOutputHandle(ci: GraphicsExportComponent; h: Handle): ComponentResult; external name '_GraphicsExportSetOutputHandle';
{
 *  GraphicsExportGetOutputHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetOutputHandle(ci: GraphicsExportComponent; var h: Handle): ComponentResult; external name '_GraphicsExportGetOutputHandle';
{
 *  GraphicsExportSetOutputOffsetAndMaxSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetOutputOffsetAndMaxSize(ci: GraphicsExportComponent; offset: UInt32; maxSize: UInt32; truncateFile: boolean): ComponentResult; external name '_GraphicsExportSetOutputOffsetAndMaxSize';
{
 *  GraphicsExportGetOutputOffsetAndMaxSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetOutputOffsetAndMaxSize(ci: GraphicsExportComponent; var offset: UInt32; var maxSize: UInt32; var truncateFile: boolean): ComponentResult; external name '_GraphicsExportGetOutputOffsetAndMaxSize';
{
 *  GraphicsExportSetOutputFileTypeAndCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetOutputFileTypeAndCreator(ci: GraphicsExportComponent; fileType: OSType; fileCreator: OSType): ComponentResult; external name '_GraphicsExportSetOutputFileTypeAndCreator';
{
 *  GraphicsExportGetOutputFileTypeAndCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetOutputFileTypeAndCreator(ci: GraphicsExportComponent; var fileType: OSType; var fileCreator: OSType): ComponentResult; external name '_GraphicsExportGetOutputFileTypeAndCreator';
{ Used by format-specific graphics exporters: }
{
 *  GraphicsExportWriteOutputData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportWriteOutputData(ci: GraphicsExportComponent; dataPtr: UnivPtr; dataSize: UInt32): ComponentResult; external name '_GraphicsExportWriteOutputData';
{
 *  GraphicsExportSetOutputMark()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportSetOutputMark(ci: GraphicsExportComponent; mark: UInt32): ComponentResult; external name '_GraphicsExportSetOutputMark';
{
 *  GraphicsExportGetOutputMark()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportGetOutputMark(ci: GraphicsExportComponent; var mark: UInt32): ComponentResult; external name '_GraphicsExportGetOutputMark';
{
 *  GraphicsExportReadOutputData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 4.0 and later
 }
function GraphicsExportReadOutputData(ci: GraphicsExportComponent; dataPtr: UnivPtr; dataOffset: UInt32; dataSize: UInt32): ComponentResult; external name '_GraphicsExportReadOutputData';
{ Allows embedded thumbnail creation, if supported. }
{
 *  GraphicsExportSetThumbnailEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0.2 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in qtmlClient.lib 5.0.2 and later
 }
function GraphicsExportSetThumbnailEnabled(ci: GraphicsExportComponent; enableThumbnail: boolean; maxThumbnailWidth: SInt32; maxThumbnailHeight: SInt32): ComponentResult; external name '_GraphicsExportSetThumbnailEnabled';
{
 *  GraphicsExportGetThumbnailEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0.2 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in qtmlClient.lib 5.0.2 and later
 }
function GraphicsExportGetThumbnailEnabled(ci: GraphicsExportComponent; var thumbnailEnabled: boolean; var maxThumbnailWidth: SInt32; var maxThumbnailHeight: SInt32): ComponentResult; external name '_GraphicsExportGetThumbnailEnabled';
{ Allows export of Exif files, if supported.  This disables Exif-incompatible settings such as grayscale JPEG and compressed TIFF, and enables export of Exif metadata. }
{
 *  GraphicsExportSetExifEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0.2 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in qtmlClient.lib 5.0.2 and later
 }
function GraphicsExportSetExifEnabled(ci: GraphicsExportComponent; enableExif: boolean): ComponentResult; external name '_GraphicsExportSetExifEnabled';
{
 *  GraphicsExportGetExifEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 5.0.2 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 *    Windows:          in qtmlClient.lib 5.0.2 and later
 }
function GraphicsExportGetExifEnabled(ci: GraphicsExportComponent; var exifEnabled: boolean): ComponentResult; external name '_GraphicsExportGetExifEnabled';
type
	ImageTranscoderComponent			= ComponentInstance;

const
	ImageTranscodererComponentType = $696D7463 (* 'imtc' *);


	{	* These are ImageTranscoder procedures *	}
	{
	 *  ImageTranscoderBeginSequence()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function ImageTranscoderBeginSequence(itc: ImageTranscoderComponent; srcDesc: ImageDescriptionHandle; var dstDesc: ImageDescriptionHandle; data: UnivPtr; dataSize: SInt32): ComponentResult; external name '_ImageTranscoderBeginSequence';
{
 *  ImageTranscoderConvert()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscoderConvert(itc: ImageTranscoderComponent; srcData: UnivPtr; srcDataSize: SInt32; var dstData: UnivPtr; var dstDataSize: SInt32): ComponentResult; external name '_ImageTranscoderConvert';
{
 *  ImageTranscoderDisposeData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscoderDisposeData(itc: ImageTranscoderComponent; dstData: UnivPtr): ComponentResult; external name '_ImageTranscoderDisposeData';
{
 *  ImageTranscoderEndSequence()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickTimeLib 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function ImageTranscoderEndSequence(itc: ImageTranscoderComponent): ComponentResult; external name '_ImageTranscoderEndSequence';
{ UPP call backs }
{$ALIGN MAC68K}


end.
