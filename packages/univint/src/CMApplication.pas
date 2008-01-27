{
     File:       ColorSync/CMApplication.h
 
     Contains:   Color Matching Interfaces
 
     Version:    ColorSync-174.1~229
 
     Copyright:  © 1992-2006 by Apple Computer, Inc., all rights reserved.
 
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

unit CMApplication;
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
uses MacTypes,CFBase,CFData,Files,CMICCProfile,MacErrors,CMTypes,CFString,CFDictionary,Quickdraw,Printing;


{$setc _DECLARE_CS_QD_API_ := 0} { Mac OS X ColorSync QuickDraw API are located in QuickDraw.p[.pas] }
{$ifc TARGET_API_MAC_OS8}
{$endc}  {TARGET_API_MAC_OS8}


{$ALIGN MAC68K}


const
	kDefaultCMMSignature		= $6170706C (* 'appl' *);

	{	 Macintosh 68K trap word 	}
	cmTrap						= $ABEE;


	{	 PicComment IDs 	}
	cmBeginProfile				= 220;
	cmEndProfile				= 221;
	cmEnableMatching			= 222;
	cmDisableMatching			= 223;
	cmComment					= 224;

	{	 PicComment selectors for cmComment 	}
	cmBeginProfileSel			= 0;
	cmContinueProfileSel		= 1;
	cmEndProfileSel				= 2;
	cmProfileIdentifierSel		= 3;


	{	 Defines for version 1.0 CMProfileSearchRecord.fieldMask 	}
	cmMatchCMMType				= $00000001;
	cmMatchApplProfileVersion	= $00000002;
	cmMatchDataType				= $00000004;
	cmMatchDeviceType			= $00000008;
	cmMatchDeviceManufacturer	= $00000010;
	cmMatchDeviceModel			= $00000020;
	cmMatchDeviceAttributes		= $00000040;
	cmMatchFlags				= $00000080;
	cmMatchOptions				= $00000100;
	cmMatchWhite				= $00000200;
	cmMatchBlack				= $00000400;

	{	 Defines for version 2.0 CMSearchRecord.searchMask 	}
	cmMatchAnyProfile			= $00000000;
	cmMatchProfileCMMType		= $00000001;
	cmMatchProfileClass			= $00000002;
	cmMatchDataColorSpace		= $00000004;
	cmMatchProfileConnectionSpace = $00000008;
	cmMatchManufacturer			= $00000010;
	cmMatchModel				= $00000020;
	cmMatchAttributes			= $00000040;
	cmMatchProfileFlags			= $00000080;


	{	 Flags for PostScript-related functions 	}
	cmPS7bit					= 1;
	cmPS8bit					= 2;

	{	 Flags for profile embedding functions 	}
	cmEmbedWholeProfile			= $00000000;
	cmEmbedProfileIdentifier	= $00000001;

	{	 Commands for CMFlattenUPP() 	}
	cmOpenReadSpool				= 1;
	cmOpenWriteSpool			= 2;
	cmReadSpool					= 3;
	cmWriteSpool				= 4;
	cmCloseSpool				= 5;

	{	 Commands for CMAccessUPP() 	}
	cmOpenReadAccess			= 1;
	cmOpenWriteAccess			= 2;
	cmReadAccess				= 3;
	cmWriteAccess				= 4;
	cmCloseAccess				= 5;
	cmCreateNewAccess			= 6;
	cmAbortWriteAccess			= 7;
	cmBeginAccess				= 8;
	cmEndAccess					= 9;


	{	 Use types for CMGet/SetDefaultProfileByUse() 	}
	cmInputUse					= $696E7074 (* 'inpt' *);
	cmOutputUse					= $6F757470 (* 'outp' *);
	cmDisplayUse				= $64706C79 (* 'dply' *);
	cmProofUse					= $70727566 (* 'pruf' *);


	{	 Union of 1.0, 2.0, and 4.0 profile header variants 	}

type
	CMAppleProfileHeaderPtr = ^CMAppleProfileHeader;
	CMAppleProfileHeader = record
		case SInt16 of
		0: (
			cm1:				CMHeader;
			);
		1: (
			cm2:				CM2Header;
			);
		2: (
			cm4:				CM4Header;
			);
	end;

	{	 CWConcatColorWorld() definitions 	}
	CMConcatProfileSetPtr = ^CMConcatProfileSet;
	CMConcatProfileSet = record
		keyIndex:				UInt16;									{  Zero-based  }
		count:					UInt16;									{  Min 1  }
		profileSet:				array [0..0] of CMProfileRef;			{  Variable. Ordered from Source -> Dest  }
	end;

	{	 NCWConcatColorWorld() definitions 	}
	NCMConcatProfileSpecPtr = ^NCMConcatProfileSpec;
	NCMConcatProfileSpec = record
		renderingIntent:		UInt32;									{  renderingIntent override  }
		transformTag:			UInt32;									{  transform enumerations defined below  }
		profile:				CMProfileRef;							{  profile  }
	end;

	NCMConcatProfileSetPtr = ^NCMConcatProfileSet;
	NCMConcatProfileSet = record
		cmm:					OSType;									{  e.g. 'KCMS', 'appl', ...  uniquely ids the cmm, or 0000  }
		flags:					UInt32;									{  specify quality, lookup only, no gamut checking ...  }
		flagsMask:				UInt32;									{  which bits of 'flags' to use to override profile  }
		profileCount:			UInt32;									{  how many ProfileSpecs in the following set  }
		profileSpecs:			array [0..0] of NCMConcatProfileSpec;	{  Variable. Ordered from Source -> Dest  }
	end;


const
	kNoTransform				= 0;							{  Not used  }
	kUseAtoB					= 1;							{  Use 'A2B*' tag from this profile or equivalent  }
	kUseBtoA					= 2;							{  Use 'B2A*' tag from this profile or equivalent  }
	kUseBtoB					= 3;							{  Use 'pre*' tag from this profile or equivalent  }
																{  For typical device profiles the following synonyms may be useful  }
	kDeviceToPCS				= 1;							{  Device Dependent to Device Independent  }
	kPCSToDevice				= 2;							{  Device Independent to Device Dependent  }
	kPCSToPCS					= 3;							{  Independent, through device's gamut  }
	kUseProfileIntent			= $FFFFFFFF;					{  For renderingIntent in NCMConcatProfileSpec     }


	{	 ColorSync color data types 	}

type
	CMRGBColorPtr = ^CMRGBColor;
	CMRGBColor = record
		red:					UInt16;									{  0..65535  }
		green:					UInt16;
		blue:					UInt16;
	end;

	CMCMYKColorPtr = ^CMCMYKColor;
	CMCMYKColor = record
		cyan:					UInt16;									{  0..65535  }
		magenta:				UInt16;
		yellow:					UInt16;
		black:					UInt16;
	end;

	CMCMYColorPtr = ^CMCMYColor;
	CMCMYColor = record
		cyan:					UInt16;									{  0..65535  }
		magenta:				UInt16;
		yellow:					UInt16;
	end;

	CMHLSColorPtr = ^CMHLSColor;
	CMHLSColor = record
		hue:					UInt16;									{  0..65535. Fraction of circle. Red at 0  }
		lightness:				UInt16;									{  0..65535  }
		saturation:				UInt16;									{  0..65535  }
	end;

	CMHSVColorPtr = ^CMHSVColor;
	CMHSVColor = record
		hue:					UInt16;									{  0..65535. Fraction of circle. Red at 0  }
		saturation:				UInt16;									{  0..65535  }
		value:					UInt16;									{  0..65535  }
	end;

	CMLabColorPtr = ^CMLabColor;
	CMLabColor = record
		L:						UInt16;									{  0..65535 maps to 0..100  }
		a:						UInt16;									{  0..65535 maps to -128..127.996  }
		b:						UInt16;									{  0..65535 maps to -128..127.996  }
	end;

	CMLuvColorPtr = ^CMLuvColor;
	CMLuvColor = record
		L:						UInt16;									{  0..65535 maps to 0..100  }
		u:						UInt16;									{  0..65535 maps to -128..127.996  }
		v:						UInt16;									{  0..65535 maps to -128..127.996  }
	end;

	CMYxyColorPtr = ^CMYxyColor;
	CMYxyColor = record
		capY:					UInt16;									{  0..65535 maps to 0..1  }
		x:						UInt16;									{  0..65535 maps to 0..1  }
		y:						UInt16;									{  0..65535 maps to 0..1  }
	end;

	CMGrayColorPtr = ^CMGrayColor;
	CMGrayColor = record
		gray:					UInt16;									{  0..65535  }
	end;

	CMMultichannel5ColorPtr = ^CMMultichannel5Color;
	CMMultichannel5Color = record
		components:				packed array [0..4] of UInt8;			{  0..255  }
		pad: UInt8; {pad byte so record size equals Apple gcc struct size}
	end;

	CMMultichannel6ColorPtr = ^CMMultichannel6Color;
	CMMultichannel6Color = record
		components:				packed array [0..5] of UInt8;			{  0..255  }
	end;

	CMMultichannel7ColorPtr = ^CMMultichannel7Color;
	CMMultichannel7Color = record
		components:				packed array [0..6] of UInt8;			{  0..255  }
		pad: UInt8; {pad byte so record size equals Apple gcc struct size}
	end;

	CMMultichannel8ColorPtr = ^CMMultichannel8Color;
	CMMultichannel8Color = record
		components:				packed array [0..7] of UInt8;			{  0..255  }
	end;

	CMNamedColorPtr = ^CMNamedColor;
	CMNamedColor = record
		namedColorIndex:		UInt32;									{  0..a lot  }
	end;

	CMColorPtr = ^CMColor;
	CMColor = record
		case SInt16 of
		0: (
			rgb:				CMRGBColor;
			);
		1: (
			hsv:				CMHSVColor;
			);
		2: (
			hls:				CMHLSColor;
			);
		3: (
			XYZ:				CMXYZColor;
			);
		4: (
			Lab:				CMLabColor;
			);
		5: (
			Luv:				CMLuvColor;
			);
		6: (
			Yxy:				CMYxyColor;
			);
		7: (
			cmyk:				CMCMYKColor;
			);
		8: (
			cmy:				CMCMYColor;
			);
		9: (
			gray:				CMGrayColor;
			);
		10: (
			mc5:				CMMultichannel5Color;
			);
		11: (
			mc6:				CMMultichannel6Color;
			);
		12: (
			mc7:				CMMultichannel7Color;
			);
		13: (
			mc8:				CMMultichannel8Color;
			);
		14: (
			namedColor:			CMNamedColor;
			);
	end;

	{	 GetIndexedProfile() search definition 	}
	CMProfileSearchRecordPtr = ^CMProfileSearchRecord;
	CMProfileSearchRecord = record
		header:					CMHeader;
		fieldMask:				UInt32;
		reserved:				array [0..1] of UInt32;
	end;

	CMProfileSearchRecordHandle			= ^CMProfileSearchRecordPtr;
	{	 CMNewProfileSearch() search definition 	}
	CMSearchRecordPtr = ^CMSearchRecord;
	CMSearchRecord = record
		CMMType:				OSType;
		profileClass:			OSType;
		dataColorSpace:			OSType;
		profileConnectionSpace:	OSType;
		deviceManufacturer:		UInt32;
		deviceModel:			UInt32;
		deviceAttributes:		array [0..1] of UInt32;
		profileFlags:			UInt32;
		searchMask:				UInt32;
		filter:					CMProfileFilterUPP;
	end;

	{	 CMMIterateUPP() structure 	}
	CMMInfoPtr = ^CMMInfo;
	CMMInfo = record
		dataSize:				UInt32;									{  Size of this structure - compatibility }
		CMMType:				OSType;									{  Signature, e.g. 'appl', 'HDM ' or 'KCMS' }
		CMMMfr:					OSType;									{  Vendor, e.g. 'appl' }
		CMMVersion:				UInt32;									{  CMM version number }
		ASCIIName:				packed array [0..31] of UInt8;			{  pascal string - name }
		ASCIIDesc:				packed array [0..255] of UInt8;			{  pascal string - description or copyright }
		UniCodeNameCount:		UniCharCount;							{  count of UniChars in following array }
		UniCodeName:			array [0..31] of UniChar;				{  the name in UniCode chars }
		UniCodeDescCount:		UniCharCount;							{  count of UniChars in following array }
		UniCodeDesc:			array [0..255] of UniChar;				{  the description in UniCode chars }
	end;

	{	 GetCWInfo() structures 	}
	CMMInfoRecordPtr = ^CMMInfoRecord;
	CMMInfoRecord = record
		CMMType:				OSType;
		CMMVersion:				SInt32;
	end;

	CMCWInfoRecordPtr = ^CMCWInfoRecord;
	CMCWInfoRecord = record
		cmmCount:				UInt32;
		cmmInfo:				array [0..1] of CMMInfoRecord;
	end;

	{	 profile identifier structures 	}
	CMProfileIdentifierPtr = ^CMProfileIdentifier;
	CMProfileIdentifier = record
		profileHeader:			CM2Header;
		calibrationDate:		CMDateTime;
		ASCIIProfileDescriptionLen: UInt32;
		ASCIIProfileDescription: SInt8;									{  variable length  }
	end;

	{	 colorspace masks 	}

const
	cmColorSpaceSpaceMask		= $0000003F;
	cmColorSpacePremulAlphaMask	= $00000040;
	cmColorSpaceAlphaMask		= $00000080;
	cmColorSpaceSpaceAndAlphaMask = $000000FF;
	cmColorSpacePackingMask		= $0000FF00;
	cmColorSpaceEncodingMask	= $000F0000;
	cmColorSpaceReservedMask	= $FFF00000;

	{	 packing formats 	}
	cmNoColorPacking			= $0000;
	cmWord5ColorPacking			= $0500;
	cmWord565ColorPacking		= $0600;
	cmLong8ColorPacking			= $0800;
	cmLong10ColorPacking		= $0A00;
	cmAlphaFirstPacking			= $1000;
	cmOneBitDirectPacking		= $0B00;
	cmAlphaLastPacking			= $0000;
	cm8_8ColorPacking			= $2800;
	cm16_8ColorPacking			= $2000;
	cm24_8ColorPacking			= $2100;
	cm32_8ColorPacking			= $0800;
	cm40_8ColorPacking			= $2200;
	cm48_8ColorPacking			= $2300;
	cm56_8ColorPacking			= $2400;
	cm64_8ColorPacking			= $2500;
	cm32_16ColorPacking			= $2600;
	cm48_16ColorPacking			= $2900;
	cm64_16ColorPacking			= $2A00;
	cm32_32ColorPacking			= $2700;
	cmLittleEndianPacking		= $4000;
	cmReverseChannelPacking		= $8000;

	{	 channel encoding format 	}
	cmSRGB16ChannelEncoding		= $00010000;					{  used for sRGB64 encoding ( ±3.12 format) }

	{	 general colorspaces 	}
	cmNoSpace					= $0000;
	cmRGBSpace					= $0001;
	cmCMYKSpace					= $0002;
	cmHSVSpace					= $0003;
	cmHLSSpace					= $0004;
	cmYXYSpace					= $0005;
	cmXYZSpace					= $0006;
	cmLUVSpace					= $0007;
	cmLABSpace					= $0008;
	cmReservedSpace1			= $0009;
	cmGraySpace					= $000A;
	cmReservedSpace2			= $000B;
	cmGamutResultSpace			= $000C;
	cmNamedIndexedSpace			= $0010;
	cmMCFiveSpace				= $0011;
	cmMCSixSpace				= $0012;
	cmMCSevenSpace				= $0013;
	cmMCEightSpace				= $0014;
	cmAlphaPmulSpace			= $0040;
	cmAlphaSpace				= $0080;
	cmRGBASpace					= $0081;
	cmGrayASpace				= $008A;
	cmRGBAPmulSpace				= $00C1;
	cmGrayAPmulSpace			= $00CA;

	{	 supported CMBitmapColorSpaces - Each of the following is a 	}
	{	 combination of a general colospace and a packing formats. 	}
	{	 Each can also be or'd with cmReverseChannelPacking. 	}
	cmGray8Space				= $280A;
	cmGray16Space				= $000A;
	cmGray16LSpace				= $400A;
	cmGrayA16Space				= $208A;
	cmGrayA32Space				= $008A;
	cmGrayA32LSpace				= $408A;
	cmGrayA16PmulSpace			= $20CA;
	cmGrayA32PmulSpace			= $00CA;
	cmGrayA32LPmulSpace			= $40CA;
	cmRGB16Space				= $0501;
	cmRGB16LSpace				= $4501;
	cmRGB565Space				= $0601;
	cmRGB565LSpace				= $4601;
	cmRGB24Space				= $2101;
	cmRGB32Space				= $0801;
	cmRGB48Space				= $2901;
	cmRGB48LSpace				= $6901;
	cmARGB32Space				= $1881;
	cmARGB64Space				= $3A81;
	cmARGB64LSpace				= $7A81;
	cmRGBA32Space				= $0881;
	cmRGBA64Space				= $2A81;
	cmRGBA64LSpace				= $6A81;
	cmARGB32PmulSpace			= $18C1;
	cmARGB64PmulSpace			= $3AC1;
	cmARGB64LPmulSpace			= $7AC1;
	cmRGBA32PmulSpace			= $08C1;
	cmRGBA64PmulSpace			= $2AC1;
	cmRGBA64LPmulSpace			= $6AC1;
	cmCMYK32Space				= $0802;
	cmCMYK64Space				= $2A02;
	cmCMYK64LSpace				= $6A02;
	cmHSV32Space				= $0A03;
	cmHLS32Space				= $0A04;
	cmYXY32Space				= $0A05;
	cmXYZ24Space				= $2106;
	cmXYZ32Space				= $0A06;
	cmXYZ48Space				= $2906;
	cmXYZ48LSpace				= $6906;
	cmLUV32Space				= $0A07;
	cmLAB24Space				= $2108;
	cmLAB32Space				= $0A08;
	cmLAB48Space				= $2908;
	cmLAB48LSpace				= $6908;
	cmGamutResult1Space			= $0B0C;
	cmNamedIndexed32Space		= $2710;
	cmNamedIndexed32LSpace		= $6710;
	cmMCFive8Space				= $2211;
	cmMCSix8Space				= $2312;
	cmMCSeven8Space				= $2413;
	cmMCEight8Space				= $2514;


type
	CMBitmapColorSpace					= UInt32;
	CMBitmapPtr = ^CMBitmap;
	CMBitmap = record
		image:					CStringPtr;
		width:					SInt32;
		height:					SInt32;
		rowBytes:				SInt32;
		pixelSize:				SInt32;
		space:					CMBitmapColorSpace;
		user1:					SInt32;
		user2:					SInt32;
	end;

const
	{	 Profile Locations 	}
	CS_MAX_PATH					= 256;

	cmNoProfileBase				= 0;
	cmFileBasedProfile			= 1;
	cmHandleBasedProfile		= 2;
	cmPtrBasedProfile			= 3;
	cmProcedureBasedProfile		= 4;
	cmPathBasedProfile			= 5;
	cmBufferBasedProfile		= 6;


type
	CMFileLocationPtr = ^CMFileLocation;
	CMFileLocation = record
		spec:					FSSpec;
	end;

	CMHandleLocationPtr = ^CMHandleLocation;
	CMHandleLocation = record
		h:						Handle;
	end;

	CMPtrLocationPtr = ^CMPtrLocation;
	CMPtrLocation = record
		p:						Ptr;
	end;

	CMProcedureLocationPtr = ^CMProcedureLocation;
	CMProcedureLocation = record
		proc:					CMProfileAccessUPP;
		refCon:					Ptr;
	end;

	CMPathLocationPtr = ^CMPathLocation;
	CMPathLocation = record
		path:					packed array [0..255] of char;
	end;

	CMBufferLocationPtr = ^CMBufferLocation;
	CMBufferLocation = record
		buffer:					Ptr;
		size:					UInt32;
	end;

	CMProfLocPtr = ^CMProfLoc;
	CMProfLoc = record
		case SInt16 of
		0: (
			fileLoc:			CMFileLocation;
			);
		1: (
			handleLoc:			CMHandleLocation;
			);
		2: (
			ptrLoc:				CMPtrLocation;
			);
		3: (
			procLoc:			CMProcedureLocation;
			);
		4: (
			pathLoc:			CMPathLocation;
			);
		5: (
			bufferLoc:			CMBufferLocation;
			);
	end;

	CMProfileLocationPtr = ^CMProfileLocation;
	CMProfileLocation = record
		locType:				SInt16;
		u:						CMProfLoc;
	end;

{$ifc TARGET_OS_MAC}

const
	cmOriginalProfileLocationSize = 72;
	cmCurrentProfileLocationSize = 258;

{$elsec}

const
	cmOriginalProfileLocationSize = 258;
	cmCurrentProfileLocationSize = 258;

{$endc}  {TARGET_OS_MAC}

	{	 Struct and enums used for Profile iteration 	}

const
	cmProfileIterateDataVersion1 = $00010000;
	cmProfileIterateDataVersion2 = $00020000;					{  Added makeAndModel }
	cmProfileIterateDataVersion3 = $00030000;					{  Added MD5 digest }


type
	CMProfileIterateDataPtr = ^CMProfileIterateData;
	CMProfileIterateData = record
		dataVersion:			UInt32;									{  cmProfileIterateDataVersion2  }
		header:					CM2Header;
		code:					ScriptCode;
		name:					Str255;
		location:				CMProfileLocation;
		uniCodeNameCount:		UniCharCount;
		uniCodeName:			UniCharPtr;
		asciiName:				Ptr;
		makeAndModel:			CMMakeAndModelPtr;
		digest:					CMProfileMD5Ptr;
	end;

	{	 Caller-supplied callback function for Profile & CMM iteration 	}
{$ifc TYPED_FUNCTION_POINTERS}
	CMProfileIterateProcPtr = function(var iterateData: CMProfileIterateData; refCon: UnivPtr): OSErr;
{$elsec}
	CMProfileIterateProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CMMIterateProcPtr = function(var iterateData: CMMInfo; refCon: UnivPtr): OSErr;
{$elsec}
	CMMIterateProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	CMProfileIterateUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMProfileIterateUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CMMIterateUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CMMIterateUPP = UniversalProcPtr;
{$endc}	

const
	uppCMProfileIterateProcInfo = $000003E0;
	uppCMMIterateProcInfo = $000003E0;
{
 *  NewCMProfileIterateUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMProfileIterateUPP(userRoutine: CMProfileIterateProcPtr): CMProfileIterateUPP; external name '_NewCMProfileIterateUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewCMMIterateUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCMMIterateUPP(userRoutine: CMMIterateProcPtr): CMMIterateUPP; external name '_NewCMMIterateUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMProfileIterateUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMProfileIterateUPP(userUPP: CMProfileIterateUPP); external name '_DisposeCMProfileIterateUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeCMMIterateUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCMMIterateUPP(userUPP: CMMIterateUPP); external name '_DisposeCMMIterateUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMProfileIterateUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMProfileIterateUPP(var iterateData: CMProfileIterateData; refCon: UnivPtr; userRoutine: CMProfileIterateUPP): OSErr; external name '_InvokeCMProfileIterateUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeCMMIterateUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCMMIterateUPP(var iterateData: CMMInfo; refCon: UnivPtr; userRoutine: CMMIterateUPP): OSErr; external name '_InvokeCMMIterateUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ CMLabToLabProcPtr isn't used in any official Apple headers and no documentation exists on it. }
//type
//	CMLabToLabProcPtr = procedure( var L: Float32; var a: Float32; var b: Float32; refcon: UnivPtr );

{ Creating Profiles }
{ Profile file and element access }
{
 *  CMNewProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMNewProfile(var prof: CMProfileRef; const (*var*) theProfile: CMProfileLocation): CMError; external name '_CMNewProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWNewLinkProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CWNewLinkProfile( var prof: CMProfileRef; const (*var*) targetLocation: CMProfileLocation; var profileSet: CMConcatProfileSet ): CMError; external name '_CWNewLinkProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NCWNewLinkProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function NCWNewLinkProfile( var prof: CMProfileRef; const (*var*) targetLocation: CMProfileLocation; var profileSet: NCMConcatProfileSet; proc: CMConcatCallBackUPP; refCon: UnivPtr ): CMError; external name '_NCWNewLinkProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMMakeProfile()
 *  
 *  Summary:
 *    Make a display or abstract profile.
 *  
 *  Discussion:
 *    Adds appropriate tags to a profile to make display or abstract
 *    profile based on an specification dictionary. 
 *    
 *    One key in the specification dictionary must be "profileType" 
 *    which must have a CFString value of "abstractLab", "displayRGB" 
 *    or "displayID".  It can also contain the keys/values: 
 *      "description"  CFString (optional) 
 *      "copyright"    CFString (optional) 
 *    
 *    For profileType of "abstractLab", the dictionary 
 *    should also contain the keys/values: 
 *      "gridPoints"   CFNumber(SInt32) (should be odd) 
 *      "proc"         CFNumber(SInt64) 
 *                     (coerced from a LabToLabProcPtr) 
 *      "refcon"       CFNumber(SInt64) (optional) 
 *                     (coerced from a void*) 
 *    
 *    For profileType of "displayRGB", the dictionary 
 *    should also contain the keys/values: 
 *      "targetGamma"  CFNumber(Float)  (e.g. 1.8)  (optional) 
 *      "targetWhite"  CFNumber(SInt32) (e.g. 6500) (optional) 
 *      "gammaR"       CFNumber(Float)  (e.g. 2.5) 
 *      "gammaG"       CFNumber(Float)  (e.g. 2.5) 
 *      "gammaB"       CFNumber(Float)  (e.g. 2.5) 
 *      "tableChans"   CFNumber(SInt32) (1 or 3) (optional) 
 *      "tableEntries" CFNumber(SInt32) (e.g 16 or 255) (optional) 
 *      "tableEntrySize" CFNumber(SInt32) (1 or 2) (optional) 
 *      "tableData"    CFData (lut in RRRGGGBBB order) (optional) 
 *     either 
 *      "phosphorRx"   CFNumber(Float) 
 *      "phosphorRy"   CFNumber(Float) 
 *      "phosphorGx"   CFNumber(Float) 
 *      "phosphorGy"   CFNumber(Float) 
 *      "phosphorBx"   CFNumber(Float) 
 *      "phosphorBy"   CFNumber(Float) 
 *      or 
 *      "phosphorSet"  CFString ("WideRGB", "700/525/450nm", 
 *                      "P22-EBU", "HDTV", "CCIR709", "sRGB", 
 *                      "AdobeRGB98" or "Trinitron") 
 *     either 
 *      "whitePointx"  CFNumber(Float) 
 *      "whitePointy"  CFNumber(Float) 
 *      or 
 *      "whiteTemp"    CFNumber(SInt32)  (e.g. 5000, 6500, 9300) 
 *    
 *    For profileType of "displayID", the dictionary 
 *    should also contain the keys/values: 
 *      "targetGamma"  CFNumber(Float)  (e.g. 1.8)  (optional) 
 *      "targetWhite"  CFNumber(SInt32) (e.g. 6500) (optional) 
 *      "displayID     CFNumber(SInt32) 
 *    Optionally, the keys/values for "displayRGB" can be 
 *    provided to override the valuses from the display.
 *  
 *  Parameters:
 *    
 *    prof:
 *      (in) the profile to modify
 *    
 *    spec:
 *      (in) specification dictionary
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CMMakeProfile( prof: CMProfileRef; spec: CFDictionaryRef ): CMError; external name '_CMMakeProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Accessing Profiles }
{
 *  CMOpenProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMOpenProfile(var prof: CMProfileRef; const (*var*) theProfile: CMProfileLocation): CMError; external name '_CMOpenProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMCloseProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMCloseProfile(prof: CMProfileRef): CMError; external name '_CMCloseProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMUpdateProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMUpdateProfile(prof: CMProfileRef): CMError; external name '_CMUpdateProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMCopyProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMCopyProfile(var targetProf: CMProfileRef; const (*var*) targetLocation: CMProfileLocation; srcProf: CMProfileRef): CMError; external name '_CMCopyProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMValidateProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMValidateProfile(prof: CMProfileRef; var valid: boolean; var preferredCMMnotfound: boolean): CMError; external name '_CMValidateProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetProfileLocation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetProfileLocation(prof: CMProfileRef; var theProfile: CMProfileLocation): CMError; external name '_CMGetProfileLocation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NCMGetProfileLocation()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function NCMGetProfileLocation(prof: CMProfileRef; var theProfile: CMProfileLocation; var locationSize: UInt32): CMError; external name '_NCMGetProfileLocation';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMProfileCopyICCData()
 *  
 *  Summary:
 *    Return a copy of the icc data specified by `prof'.
 *  
 *  Parameters:
 *    
 *    allocator:
 *      (in) The object to be used to allocate memory for the data
 *    
 *    prof:
 *      (in) The profile to query
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CMProfileCopyICCData( allocator: CFAllocatorRef; prof: CMProfileRef ): CFDataRef; external name '_CMProfileCopyICCData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{
 *  CMFlattenProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMFlattenProfile(prof: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr; var preferredCMMnotfound: boolean): CMError; external name '_CMFlattenProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NCMUnflattenProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function NCMUnflattenProfile( var targetLocation: CMProfileLocation; proc: CMFlattenUPP; refCon: UnivPtr; var preferredCMMnotfound: Boolean ): CMError; external name '_NCMUnflattenProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$ifc TARGET_OS_MAC}
{$ifc CALL_NOT_IN_CARBON}
{
 *  CMUnflattenProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMUnflattenProfile(var resultFileSpec: FSSpec; proc: CMFlattenUPP; refCon: UnivPtr; var preferredCMMnotfound: boolean): CMError; external name '_CMUnflattenProfile';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_OS_MAC}

{
 *  CMGetProfileHeader()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetProfileHeader(prof: CMProfileRef; var header: CMAppleProfileHeader): CMError; external name '_CMGetProfileHeader';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetProfileHeader()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSetProfileHeader(prof: CMProfileRef; const (*var*) header: CMAppleProfileHeader): CMError; external name '_CMSetProfileHeader';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMCloneProfileRef()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMCloneProfileRef( prof: CMProfileRef ): CMError; external name '_CMCloneProfileRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetProfileRefCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMGetProfileRefCount( prof: CMProfileRef; var count: SInt32 ): CMError; external name '_CMGetProfileRefCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMProfileModified()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMProfileModified( prof: CMProfileRef; var modified: Boolean ): CMError; external name '_CMProfileModified';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetProfileMD5()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CMGetProfileMD5( prof: CMProfileRef; digest: CMProfileMD5 ): CMError; external name '_CMGetProfileMD5';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{ Accessing Profile Elements }
{
 *  CMCountProfileElements()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMCountProfileElements( prof: CMProfileRef; var elementCount: UInt32 ): CMError; external name '_CMCountProfileElements';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMProfileElementExists()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMProfileElementExists(prof: CMProfileRef; tag: OSType; var found: boolean): CMError; external name '_CMProfileElementExists';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetProfileElement()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetProfileElement(prof: CMProfileRef; tag: OSType; var elementSize: UInt32; elementData: UnivPtr): CMError; external name '_CMGetProfileElement';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetProfileElement()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSetProfileElement(prof: CMProfileRef; tag: OSType; elementSize: UInt32; elementData: UnivPtr): CMError; external name '_CMSetProfileElement';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetProfileElementSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSetProfileElementSize(prof: CMProfileRef; tag: OSType; elementSize: UInt32): CMError; external name '_CMSetProfileElementSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetProfileElementReference()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSetProfileElementReference(prof: CMProfileRef; elementTag: OSType; referenceTag: OSType): CMError; external name '_CMSetProfileElementReference';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetPartialProfileElement()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetPartialProfileElement(prof: CMProfileRef; tag: OSType; offset: UInt32; var byteCount: UInt32; elementData: UnivPtr): CMError; external name '_CMGetPartialProfileElement';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetPartialProfileElement()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSetPartialProfileElement(prof: CMProfileRef; tag: OSType; offset: UInt32; byteCount: UInt32; elementData: {const} UnivPtr): CMError; external name '_CMSetPartialProfileElement';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetIndProfileElementInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetIndProfileElementInfo(prof: CMProfileRef; index: UInt32; var tag: OSType; var elementSize: UInt32; var refs: boolean): CMError; external name '_CMGetIndProfileElementInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetIndProfileElement()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetIndProfileElement(prof: CMProfileRef; index: UInt32; var elementSize: UInt32; elementData: UnivPtr): CMError; external name '_CMGetIndProfileElement';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMRemoveProfileElement()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMRemoveProfileElement(prof: CMProfileRef; tag: OSType): CMError; external name '_CMRemoveProfileElement';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ Accessing Profile Descriptions }
{
 *  CMGetScriptProfileDescription()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetScriptProfileDescription(prof: CMProfileRef; var name: Str255; var code: ScriptCode): CMError; external name '_CMGetScriptProfileDescription';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetProfileDescriptions()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function CMGetProfileDescriptions(prof: CMProfileRef; aName: CStringPtr; var aCount: UInt32; var mName: Str255; var mCode: ScriptCode; var uName: UniChar; var uCount: UniCharCount): CMError; external name '_CMGetProfileDescriptions';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetProfileDescriptions()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function CMSetProfileDescriptions(prof: CMProfileRef; aName: ConstCStringPtr; aCount: UInt32; const (*var*) mName: Str255; mCode: ScriptCode; uName: ConstUniCharPtr; uCount: UniCharCount): CMError; external name '_CMSetProfileDescriptions';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMCopyProfileLocalizedStringDictionary()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in ColorSyncLib 3.1 and later
 }
function CMCopyProfileLocalizedStringDictionary(prof: CMProfileRef; tag: OSType; var theDict: CFDictionaryRef): CMError; external name '_CMCopyProfileLocalizedStringDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{
 *  CMSetProfileLocalizedStringDictionary()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in ColorSyncLib 3.1 and later
 }
function CMSetProfileLocalizedStringDictionary(prof: CMProfileRef; tag: OSType; theDict: CFDictionaryRef): CMError; external name '_CMSetProfileLocalizedStringDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{
 *  CMCopyProfileLocalizedString()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in ColorSyncLib 3.1 and later
 }
function CMCopyProfileLocalizedString(prof: CMProfileRef; tag: OSType; reqLocale: CFStringRef; var locale: CFStringRef; var str: CFStringRef): CMError; external name '_CMCopyProfileLocalizedString';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{
 *  CMCopyProfileDescriptionString()
 *  
 *  Summary:
 *    Returns the name of a profile as a CFString.
 *  
 *  Discussion:
 *    If the profile is multi-localized, the best localized name for
 *    the current process is returned.
 *  
 *  Parameters:
 *    
 *    prof:
 *      (in) the profile to query
 *    
 *    str:
 *      (out) returns the name
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CMCopyProfileDescriptionString( prof: CMProfileRef; var str: CFStringRef ): CMError; external name '_CMCopyProfileDescriptionString';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{ Accessing Name-Class Profiles }
{
 *  CMGetNamedColorInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMGetNamedColorInfo(prof: CMProfileRef; var deviceChannels: UInt32; var deviceColorSpace: OSType; var PCSColorSpace: OSType; var count: UInt32; prefix: StringPtr; suffix: StringPtr): CMError; external name '_CMGetNamedColorInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetNamedColorValue()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMGetNamedColorValue(prof: CMProfileRef; name: StringPtr; var deviceColor: CMColor; var PCSColor: CMColor): CMError; external name '_CMGetNamedColorValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetIndNamedColorValue()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMGetIndNamedColorValue(prof: CMProfileRef; index: UInt32; var deviceColor: CMColor; var PCSColor: CMColor): CMError; external name '_CMGetIndNamedColorValue';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetNamedColorIndex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMGetNamedColorIndex(prof: CMProfileRef; name: StringPtr; var index: UInt32): CMError; external name '_CMGetNamedColorIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetNamedColorName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMGetNamedColorName(prof: CMProfileRef; index: UInt32; name: StringPtr): CMError; external name '_CMGetNamedColorName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ General-purpose matching functions }
{ Working with ColorWorlds }
{
 *  NCWNewColorWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function NCWNewColorWorld(var cw: CMWorldRef; src: CMProfileRef; dst: CMProfileRef): CMError; external name '_NCWNewColorWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWConcatColorWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CWConcatColorWorld(var cw: CMWorldRef; var profileSet: CMConcatProfileSet): CMError; external name '_CWConcatColorWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NCWConcatColorWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function NCWConcatColorWorld(var cw: CMWorldRef; var profileSet: NCMConcatProfileSet; proc: CMConcatCallBackUPP; refCon: UnivPtr): CMError; external name '_NCWConcatColorWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetCWInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 }
function CMGetCWInfo( cw: CMWorldRef; var info: CMCWInfoRecord ): CMError; external name '_CMGetCWInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWDisposeColorWorld()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 }
procedure CWDisposeColorWorld(cw: CMWorldRef); external name '_CWDisposeColorWorld';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWMatchColors()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 }
function CWMatchColors(cw: CMWorldRef; var myColors: CMColor; count: UInt32): CMError; external name '_CWMatchColors';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWCheckColors()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 }
function CWCheckColors(cw: CMWorldRef; var myColors: CMColor; count: UInt32; var result: SInt8): CMError; external name '_CWCheckColors';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWMatchBitmap()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CWMatchBitmap(cw: CMWorldRef; var bitmap: CMBitmap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr; var matchedBitmap: CMBitmap): CMError; external name '_CWMatchBitmap';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CWCheckBitmap()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CWCheckBitmap(cw: CMWorldRef; const (*var*) bitmap: CMBitmap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr; var resultBitmap: CMBitmap): CMError; external name '_CWCheckBitmap';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ OpenGL support }
const
	cmTextureRGBtoRGBX8 = 0;     { RGB to 8-bit RGBx texture}

{
 *  CWFillLookupTexture()
 *  
 *  Summary:
 *    Fills a 3d lookup texture from a colorworld.
 *  
 *  Discussion:
 *    The resulting table is suitable for use in OpenGL to accelerate
 *    color management in hardware.
 *  
 *  Parameters:
 *    
 *    cw:
 *      (in) the colorworld to use
 *    
 *    gridPoints:
 *      (in) number of grid points per channel in the texture
 *    
 *    format:
 *      (in) format of pixels in texture (e.g. cmTextureRGBtoRGBX8)
 *    
 *    dataSize:
 *      (in) size in bytes of texture data to fill
 *    
 *    data:
 *      (in/out) pointer to texture data to fill
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CWFillLookupTexture( cw: CMWorldRef; gridPoints: UInt32; format: UInt32; dataSize: UInt32; data: UnivPtr ): CMError; external name '_CWFillLookupTexture';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Quickdraw-specific matching }
{ Mac OS X active declarations are in QuickDraw.p[.pas] }

{$ifc TARGET_OS_MAC AND _DECLARE_CS_QD_API_}

{
 *  CWMatchPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function CWMatchPixMap(cw: CMWorldRef; var myPixMap: PixMap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr): CMError; external name '_CWMatchPixMap';
{
 *  CWCheckPixMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function CWCheckPixMap(cw: CMWorldRef; var myPixMap: PixMap; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr; var resultBitMap: BitMap): CMError; external name '_CWCheckPixMap';
{
 *  NCMBeginMatching()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NCMBeginMatching(src: CMProfileRef; dst: CMProfileRef; var myRef: CMMatchRef): CMError; external name '_NCMBeginMatching';
{
 *  CMEndMatching()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure CMEndMatching(myRef: CMMatchRef); external name '_CMEndMatching';
{
 *  NCMDrawMatchedPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure NCMDrawMatchedPicture(myPicture: PicHandle; dst: CMProfileRef; var myRect: Rect); external name '_NCMDrawMatchedPicture';
{
 *  CMEnableMatchingComment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure CMEnableMatchingComment(enableIt: boolean); external name '_CMEnableMatchingComment';
{
 *  NCMUseProfileComment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NCMUseProfileComment(prof: CMProfileRef; flags: UInt32): CMError; external name '_NCMUseProfileComment';
{$endc}

{$ifc TARGET_OS_WIN32}
{$ifc CALL_NOT_IN_CARBON}
{
 *  CWMatchHBITMAP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CWMatchHBITMAP(cw: CMWorldRef; hBitmap_: HBITMAP; progressProc: CMBitmapCallBackUPP; refCon: UnivPtr): CMError; external name '_CWMatchHBITMAP';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_OS_WIN32}

{ Accessing Special Profiles }
{ System Profile access }
{
 *  CMGetSystemProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetSystemProfile(var prof: CMProfileRef): CMError; external name '_CMGetSystemProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetSystemProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSetSystemProfile(const (*var*) profileFileSpec: FSSpec): CMError; external name '_CMSetSystemProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NCMSetSystemProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function NCMSetSystemProfile(const (*var*) profLoc: CMProfileLocation): CMError; external name '_NCMSetSystemProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetDefaultProfileBySpace()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function CMGetDefaultProfileBySpace(dataColorSpace: OSType; var prof: CMProfileRef): CMError; external name '_CMGetDefaultProfileBySpace';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetDefaultProfileBySpace()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function CMSetDefaultProfileBySpace(dataColorSpace: OSType; prof: CMProfileRef): CMError; external name '_CMSetDefaultProfileBySpace';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ Profile access by Use }
{
 *  CMGetDefaultProfileByUse()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 3.0 and later
 }
function CMGetDefaultProfileByUse(use: OSType; var prof: CMProfileRef): CMError; external name '_CMGetDefaultProfileByUse';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetDefaultProfileByUse()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 3.0 and later
 }
function CMSetDefaultProfileByUse(use: OSType; prof: CMProfileRef): CMError; external name '_CMSetDefaultProfileByUse';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetProfileByAVID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function CMGetProfileByAVID(theID: CMDisplayIDType; var prof: CMProfileRef): CMError; external name '_CMGetProfileByAVID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetProfileByAVID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function CMSetProfileByAVID(theID: CMDisplayIDType; prof: CMProfileRef): CMError; external name '_CMSetProfileByAVID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetGammaByAVID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 3.0 and later
 }
function CMGetGammaByAVID(theID: CMDisplayIDType; var gamma: CMVideoCardGamma; var size: UInt32): CMError; external name '_CMGetGammaByAVID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSetGammaByAVID()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 3.0 and later
 }
function CMSetGammaByAVID(theID: CMDisplayIDType; var gamma: CMVideoCardGamma): CMError; external name '_CMSetGammaByAVID';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Profile Management }
{ Searching for Profiles }
{
 *  CMIterateColorSyncFolder()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function CMIterateColorSyncFolder(proc: CMProfileIterateUPP; var seed: UInt32; var count: UInt32; refCon: UnivPtr): CMError; external name '_CMIterateColorSyncFolder';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetColorSyncFolderSpec()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetColorSyncFolderSpec(vRefNum: SInt16; createFolder: boolean; var foundVRefNum: SInt16; var foundDirID: SInt32): CMError; external name '_CMGetColorSyncFolderSpec';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMNewProfileSearch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMNewProfileSearch(var searchSpec: CMSearchRecord; refCon: UnivPtr; var count: UInt32; var searchResult: CMProfileSearchRef): CMError; external name '_CMNewProfileSearch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMUpdateProfileSearch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMUpdateProfileSearch(search: CMProfileSearchRef; refCon: UnivPtr; var count: UInt32): CMError; external name '_CMUpdateProfileSearch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMDisposeProfileSearch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
procedure CMDisposeProfileSearch(search: CMProfileSearchRef); external name '_CMDisposeProfileSearch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSearchGetIndProfile()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSearchGetIndProfile(search: CMProfileSearchRef; index: UInt32; var prof: CMProfileRef): CMError; external name '_CMSearchGetIndProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMSearchGetIndProfileFileSpec()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMSearchGetIndProfileFileSpec(search: CMProfileSearchRef; index: UInt32; var profileFile: FSSpec): CMError; external name '_CMSearchGetIndProfileFileSpec';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMCreateProfileIdentifier()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMCreateProfileIdentifier(prof: CMProfileRef; ident: CMProfileIdentifierPtr; var size: UInt32): CMError; external name '_CMCreateProfileIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMProfileIdentifierFolderSearch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMProfileIdentifierFolderSearch(ident: CMProfileIdentifierPtr; var matchedCount: UInt32; var searchResult: CMProfileSearchRef): CMError; external name '_CMProfileIdentifierFolderSearch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMProfileIdentifierListSearch()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMProfileIdentifierListSearch(ident: CMProfileIdentifierPtr; var profileList: CMProfileRef; listSize: UInt32; var matchedCount: UInt32; var matchedList: CMProfileRef): CMError; external name '_CMProfileIdentifierListSearch';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$ifc TARGET_API_MAC_OS8}
{$ifc CALL_NOT_IN_CARBON}
{
 *  CMConvertProfile2to1()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMConvertProfile2to1(profv2: CMProfileRef; var profv1: CMProfileHandle): CMError; external name '_CMConvertProfile2to1';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_API_MAC_OS8}

{ Utilities }
{
 *  CMGetPreferredCMM()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.5 and later
 }
function CMGetPreferredCMM(var cmmType: OSType; var preferredCMMnotfound: boolean): CMError; external name '_CMGetPreferredCMM';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMIterateCMMInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function CMIterateCMMInfo(proc: CMMIterateUPP; var count: UInt32; refCon: UnivPtr): CMError; external name '_CMIterateCMMInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetColorSyncVersion()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.6 and later
 }
function CMGetColorSyncVersion(var version: UInt32): CMError; external name '_CMGetColorSyncVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMLaunchControlPanel()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 3.0 and later
 }
function CMLaunchControlPanel(flags: UInt32): CMError; external name '_CMLaunchControlPanel';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ ColorSpace conversion functions }
{ Converting Colors }
{
 *  CMConvertXYZToLab()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertXYZToLab(const (*var*) src: CMColor; const (*var*) white: CMXYZColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertXYZToLab';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertLabToXYZ()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertLabToXYZ(const (*var*) src: CMColor; const (*var*) white: CMXYZColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertLabToXYZ';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertXYZToLuv()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertXYZToLuv(const (*var*) src: CMColor; const (*var*) white: CMXYZColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertXYZToLuv';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertLuvToXYZ()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertLuvToXYZ(const (*var*) src: CMColor; const (*var*) white: CMXYZColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertLuvToXYZ';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertXYZToYxy()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertXYZToYxy(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertXYZToYxy';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertYxyToXYZ()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertYxyToXYZ(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertYxyToXYZ';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertRGBToHLS()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertRGBToHLS(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertRGBToHLS';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertHLSToRGB()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertHLSToRGB(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertHLSToRGB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertRGBToHSV()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertRGBToHSV(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertRGBToHSV';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertHSVToRGB()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertHSVToRGB(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertHSVToRGB';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertRGBToGray()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertRGBToGray(const (*var*) src: CMColor; var dst: CMColor; count: UInt32): CMError; external name '_CMConvertRGBToGray';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertXYZToFixedXYZ()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertXYZToFixedXYZ(const (*var*) src: CMXYZColor; var dst: CMFixedXYZColor; count: UInt32): CMError; external name '_CMConvertXYZToFixedXYZ';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertFixedXYZToXYZ()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.1 and later
 }
function CMConvertFixedXYZToXYZ(const (*var*) src: CMFixedXYZColor; var dst: CMXYZColor; count: UInt32): CMError; external name '_CMConvertFixedXYZToXYZ';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMConvertXYZToXYZ()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function CMConvertXYZToXYZ(const (*var*) src: CMColor; const (*var*) srcIlluminant: CMXYZColor; var dst: CMColor; const (*var*) dstIlluminant: CMXYZColor; method: CMChromaticAdaptation; count: UInt32): CMError; external name '_CMConvertXYZToXYZ';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{ PS-related }
{ Working with PostScript }
{
 *  CMGetPS2ColorSpace()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetPS2ColorSpace(srcProf: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr; var preferredCMMnotfound: boolean): CMError; external name '_CMGetPS2ColorSpace';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetPS2ColorRenderingIntent()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetPS2ColorRenderingIntent(srcProf: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr; var preferredCMMnotfound: boolean): CMError; external name '_CMGetPS2ColorRenderingIntent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetPS2ColorRendering()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetPS2ColorRendering(srcProf: CMProfileRef; dstProf: CMProfileRef; flags: UInt32; proc: CMFlattenUPP; refCon: UnivPtr; var preferredCMMnotfound: boolean): CMError; external name '_CMGetPS2ColorRendering';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  CMGetPS2ColorRenderingVMSize()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 }
function CMGetPS2ColorRenderingVMSize(srcProf: CMProfileRef; dstProf: CMProfileRef; var vmSize: UInt32; var preferredCMMnotfound: boolean): CMError; external name '_CMGetPS2ColorRenderingVMSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{ Notifications }

{
 *  Clients can register for notifications of ColorSync preference changes by
 *  using the kCMPrefsChangedNotification key. This notification will be sent
 *  if the user changes ColorSync preferences such as:
 *      the default profile by colors space, (CMSetDefaultProfileBySpace)
 *      the default profile by device useage, (CMSetDefaultProfileByUse)
 *      or the preferred CMM.
 *  See <CMDeviceIntegration.h> for more notifications that can be sent.
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kCMPrefsChangedNotification CFSTRP('AppleColorSyncPreferencesChangedNotification')}
{$endc}

{ ColorSync 1.0 functions which have parallel 2.0 counterparts }
{$ifc TARGET_API_MAC_OS8}
{$ifc CALL_NOT_IN_CARBON}
{
 *  CWNewColorWorld()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CWNewColorWorld(var cw: CMWorldRef; src: CMProfileHandle; dst: CMProfileHandle): CMError; external name '_CWNewColorWorld';
{
 *  ConcatenateProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ConcatenateProfiles(thru: CMProfileHandle; dst: CMProfileHandle; var newDst: CMProfileHandle): CMError; external name '_ConcatenateProfiles';
{
 *  CMBeginMatching()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMBeginMatching(src: CMProfileHandle; dst: CMProfileHandle; var myRef: CMMatchRef): CMError; external name '_CMBeginMatching';
{
 *  CMDrawMatchedPicture()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure CMDrawMatchedPicture(myPicture: PicHandle; dst: CMProfileHandle; var myRect: Rect); external name '_CMDrawMatchedPicture';
{
 *  CMUseProfileComment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMUseProfileComment(profile: CMProfileHandle): CMError; external name '_CMUseProfileComment';
{
 *  CMGetProfileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure CMGetProfileName(myProfile: CMProfileHandle; var IStringResult: CMIString); external name '_CMGetProfileName';
{
 *  CMGetProfileAdditionalDataOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMGetProfileAdditionalDataOffset(myProfile: CMProfileHandle): SInt32; external name '_CMGetProfileAdditionalDataOffset';
{ ProfileResponder definitions }
{$endc}  {CALL_NOT_IN_CARBON}

const
	cmSystemDevice				= $73797320 (* 'sys ' *);
	cmGDevice					= $67646576 (* 'gdev' *);

	{	 ProfileResponder functions 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  GetProfile()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function GetProfile(deviceType: OSType; refNum: SInt32; aProfile: CMProfileHandle; var returnedProfile: CMProfileHandle): CMError; external name '_GetProfile';
{
 *  SetProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetProfile(deviceType: OSType; refNum: SInt32; newProfile: CMProfileHandle): CMError; external name '_SetProfile';
{
 *  SetProfileDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetProfileDescription(deviceType: OSType; refNum: SInt32; deviceData: SInt32; hProfile: CMProfileHandle): CMError; external name '_SetProfileDescription';
{
 *  GetIndexedProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetIndexedProfile(deviceType: OSType; refNum: SInt32; search: CMProfileSearchRecordHandle; var returnProfile: CMProfileHandle; var index: SInt32): CMError; external name '_GetIndexedProfile';
{
 *  DeleteDeviceProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DeleteDeviceProfile(deviceType: OSType; refNum: SInt32; deleteMe: CMProfileHandle): CMError; external name '_DeleteDeviceProfile';
{$endc}  {CALL_NOT_IN_CARBON}
{$ifc OLDROUTINENAMES}
{ old constants }

type
	CMFlattenProc						= CMFlattenProcPtr;
	CMBitmapCallBackProc				= CMBitmapCallBackProcPtr;
	CMProfileFilterProc					= CMProfileFilterProcPtr;

const
	qdSystemDevice				= $73797320 (* 'sys ' *);
	qdGDevice					= $67646576 (* 'gdev' *);


	kMatchCMMType				= $00000001;
	kMatchApplProfileVersion	= $00000002;
	kMatchDataType				= $00000004;
	kMatchDeviceType			= $00000008;
	kMatchDeviceManufacturer	= $00000010;
	kMatchDeviceModel			= $00000020;
	kMatchDeviceAttributes		= $00000040;
	kMatchFlags					= $00000080;
	kMatchOptions				= $00000100;
	kMatchWhite					= $00000200;
	kMatchBlack					= $00000400;

	{	 old types 	}

type
	CMYKColor							= CMCMYKColor;
	CMYKColorPtr 						= ^CMYKColor;
	CWorld								= CMWorldRef;
	CMGamutResult						= ^SInt32;
	{	 old functions 	}
{$endc}  {OLDROUTINENAMES}
{  Deprecated stuff }
{ PrGeneral parameter blocks }

const
	enableColorMatchingOp		= 12;
	registerProfileOp			= 13;


type
	TEnableColorMatchingBlkPtr = ^TEnableColorMatchingBlk;
	TEnableColorMatchingBlk = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;
		hPrint:					THPrint;
		fEnableIt:				boolean;
		filler:					SInt8;
	end;

	TRegisterProfileBlkPtr = ^TRegisterProfileBlk;
	TRegisterProfileBlk = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;
		hPrint:					THPrint;
		fRegisterIt:			boolean;
		filler:					SInt8;
	end;

{$endc}  {TARGET_API_MAC_OS8}

{$ALIGN MAC68K}


end.
