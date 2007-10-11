{
     File:       ColorSync/CMICCProfile.h
 
     Contains:   ICC Profile Format Definitions
 
     Version:    ColorSync-174.1~229
 
     Copyright:  © 1994-2006 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2007 }

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

unit CMICCProfile;
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
uses MacTypes;


{$ALIGN MAC68K}

{ ICC Profile version constants  }

const
	cmICCProfileVersion4        = $04000000;
	cmICCProfileVersion2		= $02000000;
	cmICCProfileVersion21		= $02100000;
	cmCS2ProfileVersion			= $02000000;
	cmCS1ProfileVersion			= $00000100;					{  ColorSync 1.0 profile version  }

	{	 Current Major version number 	}
	cmProfileMajorVersionMask	= $FF000000;
	cmCurrentProfileMajorVersion = $02000000;

	{	 magic cookie number for anonymous file ID 	}
	cmMagicNumber				= $61637370 (* 'acsp' *);


	{	**********************************************************************	}
	{	************** ColorSync 2.0 profile specification *******************	}
	{	**********************************************************************	}
	{	*** flags field  ***	}
	cmICCReservedFlagsMask		= $0000FFFF;					{  these bits of the flags field are defined and reserved by ICC  }
	cmEmbeddedMask				= $00000001;					{  if bit 0 is 0 then not embedded profile, if 1 then embedded profile  }
	cmEmbeddedUseMask			= $00000002;					{  if bit 1 is 0 then ok to use anywhere, if 1 then ok to use as embedded profile only  }
	cmBlackPointCompensationMask = $00000004;                   { if bit 1 is 0 then ok to use anywhere, if 1 then ok to use as embedded profile only }
	cmCMSReservedFlagsMask		= $FFFF0000;					{  these bits of the flags field are defined and reserved by CMS vendor  }
	cmQualityMask				= $00030000;					{  if bits 16-17 is 0 then normal, if 1 then draft, if 2 then best  }
	cmInterpolationMask			= $00040000;					{  if bit 18 is 0 then interpolation, if 1 then lookup only  }
	cmGamutCheckingMask			= $00080000;					{  if bit 19 is 0 then create gamut checking info, if 1 then no gamut checking info  }

	{	 copyright-protection flag options 	}
	cmEmbeddedProfile			= 0;							{  0 is not embedded profile, 1 is embedded profile  }
	cmEmbeddedUse				= 1;							{  0 is to use anywhere, 1 is to use as embedded profile only  }

	{	 speed and quality flag options 	}
	cmNormalMode				= 0;							{  it uses the least significent two bits in the high word of flag  }
	cmDraftMode					= 1;							{  it should be evaulated like this: right shift 16 bits first, mask off the  }
	cmBestMode					= 2;							{  high 14 bits, and then compare with the enum to determine the option value  }

    { black point compensation flag option }
	cmBlackPointCompensation    = 1;                            { 0 do not apply Black Point Compensation, 1 apply }

	{	*** deviceAttributes fields ***	}
	{	 deviceAttributes[0] is defined by and reserved for device vendors 	}
	{	 deviceAttributes[1] is defined by and reserved for ICC 	}
	{	 The following bits of deviceAttributes[1] are currently defined 	}
	cmReflectiveTransparentMask	= $00000001;					{  if bit 0 is 0 then reflective media, if 1 then transparency media  }
	cmGlossyMatteMask			= $00000002;					{  if bit 1 is 0 then glossy, if 1 then matte  }

	{	 device/media attributes element values  	}
	cmReflective				= 0;							{  if bit 0 is 0 then reflective media, if 1 then transparency media  }
	cmGlossy					= 1;							{  if bit 1 is 0 then glossy, if 1 then matte  }


	{	*** renderingIntent field ***	}
	cmPerceptual				= 0;							{  Photographic images  }
	cmRelativeColorimetric		= 1;							{  Logo Colors  }
	cmSaturation				= 2;							{  Business graphics  }
	cmAbsoluteColorimetric		= 3;							{  Logo Colors  }


	{	 data type element values 	}
	cmAsciiData					= 0;
	cmBinaryData				= 1;

	{	 screen encodings  	}
	cmPrtrDefaultScreens		= 0;							{  Use printer default screens.  0 is false, 1 is ture  }
	cmLinesPer					= 1;							{  0 is LinesPerCm, 1 is LinesPerInch  }

	{	 2.0 tag type information 	}
	cmNumHeaderElements			= 10;

	{	 public tags 	}
	cmAToB0Tag					= $41324230 (* 'A2B0' *);
	cmAToB1Tag					= $41324231 (* 'A2B1' *);
	cmAToB2Tag					= $41324232 (* 'A2B2' *);
	cmBlueColorantTag			= $6258595A (* 'bXYZ' *);
	cmBlueTRCTag				= $62545243 (* 'bTRC' *);
	cmBToA0Tag					= $42324130 (* 'B2A0' *);
	cmBToA1Tag					= $42324131 (* 'B2A1' *);
	cmBToA2Tag					= $42324132 (* 'B2A2' *);
	cmCalibrationDateTimeTag	= $63616C74 (* 'calt' *);
	cmChromaticAdaptationTag	= $63686164 (* 'chad' *);
	cmCharTargetTag				= $74617267 (* 'targ' *);
	cmCopyrightTag				= $63707274 (* 'cprt' *);
	cmDeviceMfgDescTag			= $646D6E64 (* 'dmnd' *);
	cmDeviceModelDescTag		= $646D6464 (* 'dmdd' *);
	cmGamutTag					= $67616D74 (* 'gamt' *);
	cmGrayTRCTag				= $6B545243 (* 'kTRC' *);
	cmGreenColorantTag			= $6758595A (* 'gXYZ' *);
	cmGreenTRCTag				= $67545243 (* 'gTRC' *);
	cmLuminanceTag				= $6C756D69 (* 'lumi' *);
	cmMeasurementTag			= $6D656173 (* 'meas' *);
	cmMediaBlackPointTag		= $626B7074 (* 'bkpt' *);
	cmMediaWhitePointTag		= $77747074 (* 'wtpt' *);
	cmNamedColorTag				= $6E636F6C (* 'ncol' *);
	cmNamedColor2Tag			= $6E636C32 (* 'ncl2' *);
	cmPreview0Tag				= $70726530 (* 'pre0' *);
	cmPreview1Tag				= $70726531 (* 'pre1' *);
	cmPreview2Tag				= $70726532 (* 'pre2' *);
	cmProfileDescriptionTag		= $64657363 (* 'desc' *);
	cmProfileSequenceDescTag	= $70736571 (* 'pseq' *);
	cmPS2CRD0Tag				= $70736430 (* 'psd0' *);
	cmPS2CRD1Tag				= $70736431 (* 'psd1' *);
	cmPS2CRD2Tag				= $70736432 (* 'psd2' *);
	cmPS2CRD3Tag				= $70736433 (* 'psd3' *);
	cmPS2CSATag					= $70733273 (* 'ps2s' *);
	cmPS2RenderingIntentTag		= $70733269 (* 'ps2i' *);
	cmRedColorantTag			= $7258595A (* 'rXYZ' *);
	cmRedTRCTag					= $72545243 (* 'rTRC' *);
	cmScreeningDescTag			= $73637264 (* 'scrd' *);
	cmScreeningTag				= $7363726E (* 'scrn' *);
	cmTechnologyTag				= $74656368 (* 'tech' *);
	cmUcrBgTag					= $62666420 (* 'bfd ' *);
	cmViewingConditionsDescTag	= $76756564 (* 'vued' *);
	cmViewingConditionsTag		= $76696577 (* 'view' *);

	{	 custom tags 	}
	cmPS2CRDVMSizeTag			= $7073766D (* 'psvm' *);
	cmVideoCardGammaTag			= $76636774 (* 'vcgt' *);
	cmMakeAndModelTag			= $6D6D6F64 (* 'mmod' *);
	cmProfileDescriptionMLTag	= $6473636D (* 'dscm' *);
	cmNativeDisplayInfoTag      = $6E64696E (* 'ndin' *);

	{	 public type signatures 	}
	cmSigCrdInfoType            = $63726469 (* 'crdi' *);
	cmSigCurveType				= $63757276 (* 'curv' *);
	cmSigDataType				= $64617461 (* 'data' *);
	cmSigDateTimeType			= $6474696D (* 'dtim' *);
	cmSigLut16Type				= $6D667432 (* 'mft2' *);
	cmSigLut8Type				= $6D667431 (* 'mft1' *);
	cmSigMeasurementType		= $6D656173 (* 'meas' *);
	cmSigMultiFunctA2BType      = $6D414220 (* 'mAB ' *);
	cmSigMultiFunctB2AType      = $6D424120 (* 'mBA ' *);
	cmSigNamedColorType			= $6E636F6C (* 'ncol' *);
	cmSigNamedColor2Type		= $6E636C32 (* 'ncl2' *);
	cmSigParametricCurveType    = $70617261 (* 'para' *);
	cmSigProfileDescriptionType	= $64657363 (* 'desc' *);
	cmSigProfileSequenceDescType = $70736571 (* 'pseq' *);
	cmSigScreeningType			= $7363726E (* 'scrn' *);
	cmSigS15Fixed16Type			= $73663332 (* 'sf32' *);
	cmSigSignatureType			= $73696720 (* 'sig ' *);
	cmSigTextType				= $74657874 (* 'text' *);
	cmSigU16Fixed16Type			= $75663332 (* 'uf32' *);
	cmSigU1Fixed15Type			= $75663136 (* 'uf16' *);
	cmSigUInt8Type				= $75693038 (* 'ui08' *);
	cmSigUInt16Type				= $75693136 (* 'ui16' *);
	cmSigUInt32Type				= $75693332 (* 'ui32' *);
	cmSigUInt64Type				= $75693634 (* 'ui64' *);
	cmSigUcrBgType				= $62666420 (* 'bfd ' *);
	cmSigUnicodeTextType		= $75747874 (* 'utxt' *);
	cmSigViewingConditionsType	= $76696577 (* 'view' *);
	cmSigXYZType				= $58595A20 (* 'XYZ ' *);

	{	 custom type signatures 	}
	cmSigPS2CRDVMSizeType		= $7073766D (* 'psvm' *);
	cmSigVideoCardGammaType		= $76636774 (* 'vcgt' *);
	cmSigMakeAndModelType		= $6D6D6F64 (* 'mmod' *);
	cmSigNativeDisplayInfoType  = $6E64696E (* 'ndin' *);
	cmSigMultiLocalizedUniCodeType = $6D6C7563 (* 'mluc' *);

	{	 technology tag descriptions 	}
	cmTechnologyDigitalCamera   = $6463616D (* 'dcam' *);
	cmTechnologyFilmScanner		= $6673636E (* 'fscn' *);
	cmTechnologyReflectiveScanner = $7273636E (* 'rscn' *);
	cmTechnologyInkJetPrinter	= $696A6574 (* 'ijet' *);
	cmTechnologyThermalWaxPrinter = $74776178 (* 'twax' *);
	cmTechnologyElectrophotographicPrinter = $6570686F (* 'epho' *);
	cmTechnologyElectrostaticPrinter = $65737461 (* 'esta' *);
	cmTechnologyDyeSublimationPrinter = $64737562 (* 'dsub' *);
	cmTechnologyPhotographicPaperPrinter = $7270686F (* 'rpho' *);
	cmTechnologyFilmWriter		= $6670726E (* 'fprn' *);
	cmTechnologyVideoMonitor	= $7669646D (* 'vidm' *);
	cmTechnologyVideoCamera		= $76696463 (* 'vidc' *);
	cmTechnologyProjectionTelevision = $706A7476 (* 'pjtv' *);
	cmTechnologyCRTDisplay		= $43525420 (* 'CRT ' *);
	cmTechnologyPMDisplay		= $504D4420 (* 'PMD ' *);
	cmTechnologyAMDisplay		= $414D4420 (* 'AMD ' *);
	cmTechnologyPhotoCD			= $4B504344 (* 'KPCD' *);
	cmTechnologyPhotoImageSetter = $696D6773 (* 'imgs' *);
	cmTechnologyGravure			= $67726176 (* 'grav' *);
	cmTechnologyOffsetLithography = $6F666673 (* 'offs' *);
	cmTechnologySilkscreen		= $73696C6B (* 'silk' *);
	cmTechnologyFlexography		= $666C6578 (* 'flex' *);


	{	 Measurement type encodings 	}
	{	 Measurement Flare 	}
	cmFlare0					= $00000000;
	cmFlare100					= $00000001;

	{	 Measurement Geometry 	}
	cmGeometryUnknown			= $00000000;
	cmGeometry045or450			= $00000001;
	cmGeometry0dord0			= $00000002;

	{	 Standard Observer    	}
	cmStdobsUnknown				= $00000000;
	cmStdobs1931TwoDegrees		= $00000001;
	cmStdobs1964TenDegrees		= $00000002;

	{	 Standard Illuminant 	}
	cmIlluminantUnknown			= $00000000;
	cmIlluminantD50				= $00000001;
	cmIlluminantD65				= $00000002;
	cmIlluminantD93				= $00000003;
	cmIlluminantF2				= $00000004;
	cmIlluminantD55				= $00000005;
	cmIlluminantA				= $00000006;
	cmIlluminantEquiPower		= $00000007;
	cmIlluminantF8				= $00000008;

	{	 Spot Function Value 	}
	cmSpotFunctionUnknown		= 0;
	cmSpotFunctionDefault		= 1;
	cmSpotFunctionRound			= 2;
	cmSpotFunctionDiamond		= 3;
	cmSpotFunctionEllipse		= 4;
	cmSpotFunctionLine			= 5;
	cmSpotFunctionSquare		= 6;
	cmSpotFunctionCross			= 7;

	{	 Color Space Signatures 	}
	cmXYZData					= $58595A20 (* 'XYZ ' *);
	cmLabData					= $4C616220 (* 'Lab ' *);
	cmLuvData					= $4C757620 (* 'Luv ' *);
	cmYCbCrData                 = $59436272 (* 'YCbr' *);
	cmYxyData					= $59787920 (* 'Yxy ' *);
	cmRGBData					= $52474220 (* 'RGB ' *);
	cmSRGBData					= $73524742 (* 'sRGB' *);
	cmGrayData					= $47524159 (* 'GRAY' *);
	cmHSVData					= $48535620 (* 'HSV ' *);
	cmHLSData					= $484C5320 (* 'HLS ' *);
	cmCMYKData					= $434D594B (* 'CMYK' *);
	cmCMYData					= $434D5920 (* 'CMY ' *);
	cmMCH5Data					= $4D434835 (* 'MCH5' *);
	cmMCH6Data					= $4D434836 (* 'MCH6' *);
	cmMCH7Data					= $4D434837 (* 'MCH7' *);
	cmMCH8Data					= $4D434838 (* 'MCH8' *);
	cm3CLRData					= $33434C52 (* '3CLR' *);
	cm4CLRData					= $34434C52 (* '4CLR' *);
	cm5CLRData					= $35434C52 (* '5CLR' *);
	cm6CLRData					= $36434C52 (* '6CLR' *);
	cm7CLRData					= $37434C52 (* '7CLR' *);
	cm8CLRData					= $38434C52 (* '8CLR' *);
	cm9CLRData                  = $39434C52 (* '9CLR' *);
	cm10CLRData                 = $41434C52 (* 'ACLR' *);
	cm11CLRData                 = $42434C52 (* 'BCLR' *);
	cm12CLRData                 = $43434C52 (* 'CCLR' *);
	cm13CLRData                 = $44434C52 (* 'DCLR' *);
	cm14CLRData                 = $45434C52 (* 'ECLR' *);
	cm15CLRData                 = $46434C52 (* 'FCLR' *);
	cmNamedData					= $4E414D45 (* 'NAME' *);

	{	 profileClass enumerations 	}
	cmInputClass				= $73636E72 (* 'scnr' *);
	cmDisplayClass				= $6D6E7472 (* 'mntr' *);
	cmOutputClass				= $70727472 (* 'prtr' *);
	cmLinkClass					= $6C696E6B (* 'link' *);
	cmAbstractClass				= $61627374 (* 'abst' *);
	cmColorSpaceClass			= $73706163 (* 'spac' *);
	cmNamedColorClass			= $6E6D636C (* 'nmcl' *);

	{	 platform enumerations 	}
	cmMacintosh					= $4150504C (* 'APPL' *);
	cmMicrosoft					= $4D534654 (* 'MSFT' *);
	cmSolaris					= $53554E57 (* 'SUNW' *);
	cmSiliconGraphics			= $53474920 (* 'SGI ' *);
	cmTaligent					= $54474E54 (* 'TGNT' *);

	{ parametric curve type enumerations }
	cmParametricType0           = 0;    { Y = X^gamma }
	cmParametricType1           = 1;    { Y = (aX+b)^gamma     [X>=-b/a],  Y = 0    [X<-b/a] }
	cmParametricType2           = 2;    { Y = (aX+b)^gamma + c [X>=-b/a],  Y = c    [X<-b/a] }
	cmParametricType3           = 3;    { Y = (aX+b)^gamma     [X>=d],     Y = cX   [X<d]    }
	cmParametricType4           = 4;     { Y = (aX+b)^gamma + e [X>=d],     Y = cX+f [X<d]    }


	{ ColorSync 1.0 elements }
	cmCS1ChromTag				= $6368726D (* 'chrm' *);
	cmCS1TRCTag					= $74726320 (* 'trc ' *);
	cmCS1NameTag				= $6E616D65 (* 'name' *);
	cmCS1CustTag				= $63757374 (* 'cust' *);

	{	 General element data types 	}

type
	CMDateTimePtr = ^CMDateTime;
	CMDateTime = record
		year:					UInt16;
		month:					UInt16;
		dayOfTheMonth:			UInt16;
		hours:					UInt16;
		minutes:				UInt16;
		seconds:				UInt16;
	end;

	CMFixedXYColorPtr = ^CMFixedXYColor;
	CMFixedXYColor = record
		x: Fixed;
		y: Fixed;
	end;

	CMFixedXYZColorPtr = ^CMFixedXYZColor;
	CMFixedXYZColor = record
		X:						Fixed;
		Y:						Fixed;
		Z:						Fixed;
	end;

	CMXYZComponent						= UInt16;
	CMXYZColorPtr = ^CMXYZColor;
	CMXYZColor = record
		X:						CMXYZComponent;
		Y:						CMXYZComponent;
		Z:						CMXYZComponent;
	end;

{ Type for Profile MD5 message digest }
{ Derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm }

     CMProfileMD5 = packed array[0..15] of UInt8;
     CMProfileMD5Ptr = ^CMProfileMD5;

{
 *  CMProfileMD5AreEqual()
 *  
 *  Availability:       available as macro/inline
 }
//  #define CMProfileMD5AreEqual(a, b) (\
//    ((long*)a)[0]==((long*)b)[0] && ((long*)a)[1]==((long*)b)[1] && \
//  ((long*)a)[2]==((long*)b)[2] && ((long*)a)[3]==((long*)b)[3])


	CM2HeaderPtr = ^CM2Header;
	CM2Header = record
		size:					UInt32;									{  This is the total size of the Profile  }
		CMMType:				OSType;									{  CMM signature,  Registered with CS2 consortium   }
		profileVersion:			UInt32;									{  Version of CMProfile format  }
		profileClass:			OSType;									{  input, display, output, devicelink, abstract, or color conversion profile type  }
		dataColorSpace:			OSType;									{  color space of data  }
		profileConnectionSpace:	OSType;									{  profile connection color space  }
		dateTime:				CMDateTime;								{  date and time of profile creation  }
		CS2profileSignature:	OSType;									{  'acsp' constant ColorSync 2.0 file ID  }
		platform:				OSType;									{  primary profile platform, Registered with CS2 consortium  }
		flags:					UInt32;									{  profile flags  }
		deviceManufacturer:		OSType;									{  Registered with ICC consortium  }
		deviceModel:			UInt32;									{  Registered with ICC consortium  }
		deviceAttributes:		array [0..1] of UInt32;					{  Attributes[0] is for device vendors, [1] is for ICC  }
		renderingIntent:		UInt32;									{  preferred rendering intent of tagged object  }
		white:					CMFixedXYZColor;						{  profile illuminant  }
		creator:				OSType;									{  profile creator  }
		reserved:				packed array [0..43] of char;			{  reserved for future use  }
	end;

	CM4HeaderPtr = ^ CM4Header;
	CM4Header = record
		size:                   UInt32;                                 { This is the total size of the Profile }
		CMMType:                OSType;                                 { CMM signature,  Registered with CS2 consortium  }
		profileVersion:         UInt32;                                 { Version of CMProfile format }
		profileClass:           OSType;                                 { input, display, output, devicelink, abstract, or color conversion profile type }
		dataColorSpace:         OSType;                                 { color space of data }
		profileConnectionSpace: OSType;                                 { profile connection color space }
		dateTime:               CMDateTime;                             { date and time of profile creation }
		CS2profileSignature:    OSType;                                 { 'acsp' constant ColorSync 2.0 file ID }
		platform:               OSType;                                 { primary profile platform, Registered with CS2 consortium }
		flags:                  UInt32;                                 { profile flags }
		deviceManufacturer:     OSType;                                 { Registered with ICC consortium }
		deviceModel:            UInt32;                                 { Registered with ICC consortium }
		deviceAttributes:       array[0..1] of UInt32;                  { Attributes[0] is for device vendors, [1] is for ICC }
		renderingIntent:        UInt32;                                 { preferred rendering intent of tagged object }
		white:                  CMFixedXYZColor;                        { profile illuminant }
		creator:                OSType;                                 { profile creator }
		digest:                 CMProfileMD5;                           { Profile message digest }
		reserved:               packed array[0..27] of char;            { reserved for future use }
    end;
    
	CMTagRecordPtr = ^CMTagRecord;
	CMTagRecord = record
		tag:					OSType;									{  Registered with CS2 consortium  }
		elementOffset:			UInt32;									{  Relative to start of CMProfile  }
		elementSize:			UInt32;
	end;

	CMTagElemTablePtr = ^CMTagElemTable;
	CMTagElemTable = record
		count:					UInt32;
		tagList:				array [0..0] of CMTagRecord;			{  variable size, determined by count  }
	end;

	CM2ProfilePtr = ^CM2Profile;
	CM2Profile = record
		header:					CM2Header;
		tagTable:				CMTagElemTable;
		elemData:				SInt8;									{  variable size data for tagged element storage  }
	end;

	CM2ProfileHandle					= ^CM2ProfilePtr;
	{	 Tag Type Definitions 	}
	CMAdaptationMatrixTypePtr = ^CMAdaptationMatrixType;
	CMAdaptationMatrixType = record
		typeDescriptor:			OSType;									{  'sf32' = cmSigS15Fixed16Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		adaptationMatrix:		array [0..8] of Fixed;					{  fixed size of nine matrix entries  }
	end;

	CMCurveTypePtr = ^CMCurveType;
	CMCurveType = record
		typeDescriptor:			OSType;									{  'curv' = cmSigCurveType  }
		reserved:				UInt32;									{  fill with 0x00  }
		countValue:				UInt32;									{  number of entries in table that follows  }
		data:					array [0..0] of UInt16;					{  variable size, determined by countValue  }
	end;

	CMDataTypePtr = ^CMDataType;
	CMDataType = record
		typeDescriptor:			OSType;									{  'data' = cmSigDataType }
		reserved:				UInt32;									{  fill with 0x00  }
		dataFlag:				UInt32;									{  0 = ASCII, 1 = binary  }
		data:					SInt8;									{  variable size, determined by tag element size  }
	end;

	CMDateTimeTypePtr = ^CMDateTimeType;
	CMDateTimeType = record
		typeDescriptor:			OSType;									{  'dtim' = cmSigDateTimeType  }
		reserved:				UInt32;									{  fill with 0x00  }
		dateTime:				CMDateTime;								{   }
	end;

	CMLut16TypePtr = ^CMLut16Type;
	CMLut16Type = record
		typeDescriptor:			OSType;									{  'mft2' = cmSigLut16Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		inputChannels:			SInt8;									{  Number of input channels  }
		outputChannels:			SInt8;									{  Number of output channels  }
		gridPoints:				SInt8;									{  Number of clutTable grid points  }
		reserved2:				SInt8;									{  fill with 0x00  }
		matrix:					array [0..2,0..2] of Fixed;				{   }
		inputTableEntries:		UInt16;									{  Number of entries in 1-D input luts  }
		outputTableEntries:		UInt16;									{  Number of entries in 1-D output luts  }
		inputTable:				array [0..0] of UInt16;					{  variable size, determined by inputChannels*inputTableEntries  }
	end;

	CMLut8TypePtr = ^CMLut8Type;
	CMLut8Type = record
		typeDescriptor:			OSType;									{  'mft1' = cmSigLut8Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		inputChannels:			SInt8;									{  Number of input channels  }
		outputChannels:			SInt8;									{  Number of output channels  }
		gridPoints:				SInt8;									{  Number of clutTable grid points  }
		reserved2:				SInt8;									{  fill with 0x00  }
		matrix:					array [0..2,0..2] of Fixed;				{   }
		inputTable:				SInt8;									{  variable size, determined by inputChannels*256  }
	end;

    CMMultiFunctLutTypePtr = ^CMMultiFunctLutType;
	CMMultiFunctLutType = record
		typeDescriptor:         OSType;                                 { 'mAB ' = cmSigMultiFunctA2BType or 'mBA ' = cmSigMultiFunctB2AType }
		reserved:               UInt32;                                 { fill with 0x00 }
		inputChannels:          SInt8;                                  { Number of input channels }
		outputChannels:         SInt8;                                  { Number of output channels }
		reserved2:              UInt16;                                 { fill with 0x00 }
		offsetBcurves:          UInt32;                                 { offset to first "B" curve }
		offsetMatrix:           UInt32;                                 { offset to 3x4 matrix }
		offsetMcurves:          UInt32;                                 { offset to first "M" curve }
		offsetCLUT:             UInt32;                                 { offset to multi-dimensional LUT of type CMMultiFunctCLUTType }
		offsetAcurves:          UInt32;                                 { offset to first "A" curve }
		data:                   SInt8;                                  { variable size }
	end;

	CMMultiFunctLutA2BType = CMMultiFunctLutType;
	CMMultiFunctLutA2BTypePtr = ^CMMultiFunctLutA2BType;
	CMMultiFunctLutB2AType = CMMultiFunctLutType;
	CMMultiFunctLutB2ATypePtr = ^CMMultiFunctLutB2AType;
	
	CMMultiFunctCLUTTypePtr = ^CMMultiFunctCLUTType;
	CMMultiFunctCLUTType = record
		gridPoints:             packed array[0..15] of UInt8;           { grigpoints for each input channel dimension (remaining are 0) }
		entrySize:              SInt8;                                  { bytes per lut enrty (1 or 2) }
		reserved:               array[0..2] of SInt8;                   { fill with 0x00 }
		data:                   SInt8;                                  { variable size, determined by above }
        pad:                    SInt8;                                  { pad byte needed for correct record size. Critical to accessing CMMultiFunctLutType's variable sized data field contents. }
	end;


	CMMeasurementTypePtr = ^CMMeasurementType;
	CMMeasurementType = record
		typeDescriptor:			OSType;									{  'meas' = cmSigMeasurementType  }
		reserved:				UInt32;									{  fill with 0x00  }
		standardObserver:		UInt32;									{  cmStdobsUnknown, cmStdobs1931TwoDegrees, cmStdobs1964TenDegrees  }
		backingXYZ:				CMFixedXYZColor;						{  absolute XYZ values of backing  }
		geometry:				UInt32;									{  cmGeometryUnknown, cmGeometry045or450 (0/45), cmGeometry0dord0 (0/d or d/0)  }
		flare:					UInt32;									{  cmFlare0, cmFlare100  }
		illuminant:				UInt32;									{  cmIlluminantUnknown, cmIlluminantD50, ...  }
	end;

	CMNamedColorTypePtr = ^CMNamedColorType;
	CMNamedColorType = record
		typeDescriptor:			OSType;									{  'ncol' = cmSigNamedColorType  }
		reserved:				UInt32;									{  fill with 0x00  }
		vendorFlag:				UInt32;									{   }
		count:					UInt32;									{  count of named colors in array that follows  }
		prefixName:				SInt8;									{  variable size, max = 32  }
	end;

	CMNamedColor2EntryTypePtr = ^CMNamedColor2EntryType;
	CMNamedColor2EntryType = record
		rootName:				packed array [0..31] of UInt8;			{  32 byte field.  7 bit ASCII null terminated  }
		PCSColorCoords:			array [0..2] of UInt16;					{  Lab or XYZ color  }
		DeviceColorCoords:		array [0..0] of UInt16;					{  variable size  }
	end;

	CMNamedColor2TypePtr = ^CMNamedColor2Type;
	CMNamedColor2Type = record
		typeDescriptor:			OSType;									{  'ncl2' = cmSigNamedColor2Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		vendorFlag:				UInt32;									{  lower 16 bits reserved for ICC use  }
		count:					UInt32;									{  count of named colors in array that follows  }
		deviceChannelCount:		UInt32;									{  number of device channels, 0 indicates no device value available  }
		prefixName:				packed array [0..31] of UInt8;			{  Fixed 32 byte size.  7 bit ASCII null terminated  }
		suffixName:				packed array [0..31] of UInt8;			{  Fixed 32 byte size.  7 bit ASCII null terminated  }
		data:					SInt8;									{  variable size data for CMNamedColor2EntryType  }
	end;

	CMNativeDisplayInfoPtr = ^CMNativeDisplayInfo;
	CMNativeDisplayInfo = record
		dataSize:               UInt32;                                 { Size of this structure }
		redPhosphor:            CMFixedXYColor;                         { Phosphors - native cromaticity values of the display  }
		greenPhosphor:          CMFixedXYColor;
		bluePhosphor:           CMFixedXYColor;
		whitePoint:             CMFixedXYColor;
		redGammaValue:          Fixed;                                  { Gammas - native gamma values of the display }
		greenGammaValue:        Fixed;
		blueGammaValue:         Fixed;
                                                                        {  Gamma tables - if if gammaChannels is not zero, }
                                                                        {  native gamma tables are preferred over values }
                                                                        {  redGammaValue, greenGammaValue, blueGammaValue }
		gammaChannels:          UInt16;                                 { # of gamma channels (1 or 3) }
		gammaEntryCount:        UInt16;                                 { 1-based number of entries per channel }
		gammaEntrySize:         UInt16;                                 { size in bytes of each entry }
		gammaData:              SInt8;                                  { variable size, determined by channels*entryCount*entrySize }
	end;

	CMNativeDisplayInfoTypePtr = ^CMNativeDisplayInfoType;
	CMNativeDisplayInfoType = record
		typeDescriptor:         OSType;                                 { 'ndin' = cmSigNativeDisplayInfoType }
		reserved:               UInt32;                                 { fill with 0x00 }
		nativeDisplayInfo:      CMNativeDisplayInfo;                    { data of type CMNativeDisplayInfo }
	end;

	CMParametricCurveTypePtr = ^CMParametricCurveType;
	CMParametricCurveType = record
		typeDescriptor:         OSType;                                 { 'para' = cmSigParametricCurveType }
		reserved:               UInt32;                                 { fill with 0x00 }
		functionType:           UInt16;                                 { cmParametricType0, cmParametricType1, etc. }
		reserved2:              UInt16;                                 { fill with 0x00 }
		value:                  array[0..0] of Fixed;                   { variable size, determined by functionType }
	end;

	CMTextDescriptionTypePtr = ^CMTextDescriptionType;
	CMTextDescriptionType = packed record
		typeDescriptor:			OSType;									{  'desc' = cmSigProfileDescriptionType  }
		reserved:				UInt32;									{  fill with 0x00  }
		ASCIICount:				UInt32;									{  Count of bytes (including null terminator)   }
		ASCIIName:				packed array [0..1] of UInt8;			{  variable size, determined by ASCIICount.  7 bit ASCII null terminated  }
	end;

	CMTextTypePtr = ^CMTextType;
	CMTextType = record
		typeDescriptor:			OSType;									{  'text' = cmSigTextType  }
		reserved:				UInt32;									{  fill with 0x00  }
		text:					SInt8;									{  variable size, determined by tag element size  }
	end;

	CMUnicodeTextTypePtr = ^CMUnicodeTextType;
	CMUnicodeTextType = record
		typeDescriptor:			OSType;									{  'utxt' = cmSigUnicodeTextType  }
		reserved:				UInt32;									{  fill with 0x00  }
		text:					array [0..0] of UniChar;				{  variable size, determined by tag element size   }
	end;

	CMScreeningChannelRecPtr = ^CMScreeningChannelRec;
	CMScreeningChannelRec = record
		frequency:				Fixed;
		angle:					Fixed;
		spotFunction:			UInt32;
	end;

	CMScreeningTypePtr = ^CMScreeningType;
	CMScreeningType = record
		typeDescriptor:			OSType;									{  'scrn' = cmSigScreeningType  }
		reserved:				UInt32;									{  fill with 0x00  }
		screeningFlag:			UInt32;									{  bit 0 : use printer default screens, bit 1 : inch/cm  }
		channelCount:			UInt32;									{   }
		channelInfo:			array [0..0] of CMScreeningChannelRec;	{  variable size, determined by channelCount  }
	end;

	CMSignatureTypePtr = ^CMSignatureType;
	CMSignatureType = record
		typeDescriptor:			OSType;									{  'sig ' = cmSigSignatureType  }
		reserved:				UInt32;									{  fill with 0x00  }
		signature:				OSType;
	end;

	CMS15Fixed16ArrayTypePtr = ^CMS15Fixed16ArrayType;
	CMS15Fixed16ArrayType = record
		typeDescriptor:			OSType;									{  'sf32' = cmSigS15Fixed16Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		value:					array [0..0] of Fixed;					{  variable size, determined by tag element size  }
	end;

	CMU16Fixed16ArrayTypePtr = ^CMU16Fixed16ArrayType;
	CMU16Fixed16ArrayType = record
		typeDescriptor:			OSType;									{  'uf32' = cmSigU16Fixed16Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		value:					array [0..0] of UInt32;					{  variable size, determined by tag element size  }
	end;

	CMUInt8ArrayTypePtr = ^CMUInt8ArrayType;
	CMUInt8ArrayType = record
		typeDescriptor:			OSType;									{  'ui08' = cmSigUInt8Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		value:					SInt8;									{  variable size, determined by tag element size  }
	end;

	CMUInt16ArrayTypePtr = ^CMUInt16ArrayType;
	CMUInt16ArrayType = record
		typeDescriptor:			OSType;									{  'ui16' = cmSigUInt16Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		value:					array [0..0] of UInt16;					{  variable size, determined by tag element size  }
	end;

	CMUInt32ArrayTypePtr = ^CMUInt32ArrayType;
	CMUInt32ArrayType = record
		typeDescriptor:			OSType;									{  'ui32' = cmSigUInt32Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		value:					array [0..0] of UInt32;					{  variable size, determined by tag element size  }
	end;

	CMUInt64ArrayTypePtr = ^CMUInt64ArrayType;
	CMUInt64ArrayType = record
		typeDescriptor:			OSType;									{  'ui64' = cmSigUInt64Type  }
		reserved:				UInt32;									{  fill with 0x00  }
		value:					array [0..0] of UInt32;					{  variable size, determined by tag element size  }
	end;

	CMViewingConditionsTypePtr = ^CMViewingConditionsType;
	CMViewingConditionsType = record
		typeDescriptor:			OSType;									{  'view' = cmSigViewingConditionsType  }
		reserved:				UInt32;									{  fill with 0x00  }
		illuminant:				CMFixedXYZColor;						{  absolute XYZs of illuminant  in cd/m^2  }
		surround:				CMFixedXYZColor;						{  absolute XYZs of surround in cd/m^2  }
		stdIlluminant:			UInt32;									{  see definitions of std illuminants  }
	end;

	CMXYZTypePtr = ^CMXYZType;
	CMXYZType = record
		typeDescriptor:			OSType;									{  'XYZ ' = cmSigXYZType  }
		reserved:				UInt32;									{  fill with 0x00  }
		XYZ:					array [0..0] of CMFixedXYZColor;		{  variable size, determined by tag element size  }
	end;

	CMProfileSequenceDescTypePtr = ^CMProfileSequenceDescType;
	CMProfileSequenceDescType = record
		typeDescriptor:			OSType;									{  'pseq' = cmProfileSequenceDescTag  }
		reserved:				UInt32;									{  fill with 0x00  }
		count:					UInt32;									{  Number of descriptions  }
		data:					SInt8;									{  variable size data explained in ICC spec  }
	end;

	CMUcrBgTypePtr = ^CMUcrBgType;
	CMUcrBgType = record
		typeDescriptor:			OSType;									{  'bfd ' = cmSigUcrBgType  }
		reserved:				UInt32;									{  fill with 0x00  }
		ucrCount:				UInt32;									{  Number of UCR entries  }
		ucrValues:				array [0..0] of UInt16;					{  variable size, determined by ucrCount  }
	end;

	{	 Private Tag Type Definitions 	}
	CMIntentCRDVMSizePtr = ^CMIntentCRDVMSize;
	CMIntentCRDVMSize = record
		renderingIntent:		SInt32;								{  rendering intent  }
		VMSize:					UInt32;									{  VM size taken up by the CRD  }
	end;

	CMPS2CRDVMSizeTypePtr = ^CMPS2CRDVMSizeType;
	CMPS2CRDVMSizeType = record
		typeDescriptor:			OSType;									{  'psvm' = cmSigPS2CRDVMSizeType  }
		reserved:				UInt32;									{  fill with 0x00  }
		count:					UInt32;									{  number of intent entries  }
		intentCRD:				array [0..0] of CMIntentCRDVMSize;		{  variable size, determined by count  }
	end;


const
	cmVideoCardGammaTableType	= 0;
	cmVideoCardGammaFormulaType	= 1;


type
	CMVideoCardGammaTablePtr = ^CMVideoCardGammaTable;
	CMVideoCardGammaTable = record
		channels:				UInt16;									{  # of gamma channels (1 or 3)  }
		entryCount:				UInt16;									{  1-based number of entries per channel  }
		entrySize:				UInt16;									{  size in bytes of each entry  }
		data:					SInt8;									{  variable size, determined by channels*entryCount*entrySize  }
	end;

	CMVideoCardGammaFormulaPtr = ^CMVideoCardGammaFormula;
	CMVideoCardGammaFormula = record
		redGamma:				Fixed;									{  must be > 0.0  }
		redMin:					Fixed;									{  must be > 0.0 and < 1.0  }
		redMax:					Fixed;									{  must be > 0.0 and < 1.0  }
		greenGamma:				Fixed;									{  must be > 0.0  }
		greenMin:				Fixed;									{  must be > 0.0 and < 1.0  }
		greenMax:				Fixed;									{  must be > 0.0 and < 1.0  }
		blueGamma:				Fixed;									{  must be > 0.0  }
		blueMin:				Fixed;									{  must be > 0.0 and < 1.0  }
		blueMax:				Fixed;									{  must be > 0.0 and < 1.0  }
	end;

	CMVideoCardGammaPtr = ^CMVideoCardGamma;
	CMVideoCardGamma = record
		tagType:				UInt32;
		case SInt16 of
		0: (
			table:				CMVideoCardGammaTable;
			);
		1: (
			formula:			CMVideoCardGammaFormula;
			);
	end;

	CMVideoCardGammaTypePtr = ^CMVideoCardGammaType;
	CMVideoCardGammaType = record
		typeDescriptor:			OSType;									{  'vcgt' = cmSigVideoCardGammaType  }
		reserved:				UInt32;									{  fill with 0x00  }
		gamma:					CMVideoCardGamma;
	end;

	CMMakeAndModelPtr = ^CMMakeAndModel;
	CMMakeAndModel = record
		manufacturer:			OSType;
		model:					UInt32;
		serialNumber:			UInt32;
		manufactureDate:		UInt32;
		reserved1:				UInt32;									{  fill with 0x00  }
		reserved2:				UInt32;									{  fill with 0x00  }
		reserved3:				UInt32;									{  fill with 0x00  }
		reserved4:				UInt32;									{  fill with 0x00  }
	end;

	CMMakeAndModelTypePtr = ^CMMakeAndModelType;
	CMMakeAndModelType = record
		typeDescriptor:			OSType;									{  'mmod' = cmSigMakeAndModelType  }
		reserved:				UInt32;									{  fill with 0x00  }
		makeAndModel:			CMMakeAndModel;
	end;

	CMMultiLocalizedUniCodeEntryRecPtr = ^CMMultiLocalizedUniCodeEntryRec;
	CMMultiLocalizedUniCodeEntryRec = record
		languageCode:			packed array [0..1] of char;			{  language code from ISO-639  }
		regionCode:				packed array [0..1] of char;			{  region code from ISO-3166  }
		textLength:				UInt32;									{  the length in bytes of the string  }
		textOffset:				UInt32;									{  the offset from the start of tag in bytes  }
	end;

	CMMultiLocalizedUniCodeTypePtr = ^CMMultiLocalizedUniCodeType;
	CMMultiLocalizedUniCodeType = record
		typeDescriptor:			OSType;									{  'mluc' = cmSigMultiLocalizedUniCodeType  }
		reserved:				UInt32;									{  fill with 0x00  }
		entryCount:				UInt32;									{  1-based number of name records that follow  }
		entrySize:				UInt32;									{  size in bytes of name records that follow  }
																		{  variable-length data for storage of CMMultiLocalizedUniCodeEntryRec  }
																		{  variable-length data for storage of Unicode strings }
	end;

	{	**********************************************************************	}
	{	************** ColorSync 1.0 profile specification *******************	}
	{	**********************************************************************	}

const
	cmGrayResponse				= 0;
	cmRedResponse				= 1;
	cmGreenResponse				= 2;
	cmBlueResponse				= 3;
	cmCyanResponse				= 4;
	cmMagentaResponse			= 5;
	cmYellowResponse			= 6;
	cmUcrResponse				= 7;
	cmBgResponse				= 8;
	cmOnePlusLastResponse		= 9;


	{	 Device types 	}
	cmMonitorDevice				= $6D6E7472 (* 'mntr' *);
	cmScannerDevice				= $73636E72 (* 'scnr' *);
	cmPrinterDevice				= $70727472 (* 'prtr' *);


type
	CMIStringPtr = ^CMIString;
	CMIString = record
		theScript:				ScriptCode;
		theString:				Str63;
	end;

	{	 Profile options 	}

const
	cmPerceptualMatch			= $0000;						{  Default. For photographic images  }
	cmColorimetricMatch			= $0001;						{  Exact matching when possible  }
	cmSaturationMatch			= $0002;						{  For solid colors  }

	{	 Profile flags 	}
	cmNativeMatchingPreferred	= $00000001;					{  Default to native not preferred  }
	cmTurnOffCache				= $00000002;					{  Default to turn on CMM cache  }


type
	CMMatchOption						= SInt32;
	CMMatchFlag							= SInt32;
	CMHeaderPtr = ^CMHeader;
	CMHeader = record
		size:					UInt32;
		CMMType:				OSType;
		applProfileVersion:		UInt32;
		dataType:				OSType;
		deviceType:				OSType;
		deviceManufacturer:		OSType;
		deviceModel:			UInt32;
		deviceAttributes:		array [0..1] of UInt32;
		profileNameOffset:		UInt32;
		customDataOffset:		UInt32;
		flags:					CMMatchFlag;
		options:				CMMatchOption;
		white:					CMXYZColor;
		black:					CMXYZColor;
	end;

	CMProfileChromaticitiesPtr = ^CMProfileChromaticities;
	CMProfileChromaticities = record
		red:					CMXYZColor;
		green:					CMXYZColor;
		blue:					CMXYZColor;
		cyan:					CMXYZColor;
		magenta:				CMXYZColor;
		yellow:					CMXYZColor;
	end;

	CMProfileResponsePtr = ^CMProfileResponse;
	CMProfileResponse = record
		counts:					array [0..8] of UInt16;
		data:					array [0..0] of UInt16;					{  Variable size  }
	end;

	CMProfilePtr = ^CMProfile;
	CMProfile = record
		header:					CMHeader;
		profile:				CMProfileChromaticities;
		response:				CMProfileResponse;
		profileName:			CMIString;
		customData:				SInt8;									{  Variable size  }
	end;

	CMProfileHandle						= ^CMProfilePtr;
{$ifc OLDROUTINENAMES}

const
	kCMApplProfileVersion		= $00000100;

	grayResponse				= 0;
	redResponse					= 1;
	greenResponse				= 2;
	blueResponse				= 3;
	cyanResponse				= 4;
	magentaResponse				= 5;
	yellowResponse				= 6;
	ucrResponse					= 7;
	bgResponse					= 8;
	onePlusLastResponse			= 9;

	rgbData						= $52474220 (* 'RGB ' *);
	cmykData					= $434D594B (* 'CMYK' *);
	grayData					= $47524159 (* 'GRAY' *);
	xyzData						= $58595A20 (* 'XYZ ' *);

	monitorDevice				= $6D6E7472 (* 'mntr' *);
	scannerDevice				= $73636E72 (* 'scnr' *);
	printerDevice				= $70727472 (* 'prtr' *);


type
	XYZComponent						= UInt16;
	XYZColor							= CMXYZColor;
	XYZColorPtr 						= ^XYZColor;
	CMResponseData						= UInt16;
	IString								= CMIString;
	IStringPtr 							= ^IString;
	CMResponseColor						= SInt32;
	responseColor						= CMResponseColor;
{$endc}  {OLDROUTINENAMES}


{$ALIGN MAC68K}


end.
