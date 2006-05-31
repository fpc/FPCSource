{
     File:       SFNTTypes.p
 
     Contains:   Font file structures.
 
     Version:    Technology: Mac OS 9 / Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1994-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit SFNTTypes;
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


type
	sfntDirectoryEntryPtr = ^sfntDirectoryEntry;
	sfntDirectoryEntry = record
		tableTag:				FourCharCode;
		checkSum:				UInt32;
		offset:					UInt32;
		length:					UInt32;
	end;

	{	 The search fields limits numOffsets to 4096. 	}
	sfntDirectoryPtr = ^sfntDirectory;
	sfntDirectory = record
		format:					FourCharCode;
		numOffsets:				UInt16;									{  number of tables  }
		searchRange:			UInt16;									{  (max2 <= numOffsets)*16  }
		entrySelector:			UInt16;									{  log2(max2 <= numOffsets)  }
		rangeShift:				UInt16;									{  numOffsets*16-searchRange }
		table:					array [0..0] of sfntDirectoryEntry;		{  table[numOffsets]  }
	end;


const
	sizeof_sfntDirectory		= 12;

	{	 Cmap - character id to glyph id mapping 	}
	cmapFontTableTag			= $636D6170 (* 'cmap' *);

	kFontUnicodePlatform		= 0;
	kFontMacintoshPlatform		= 1;
	kFontReservedPlatform		= 2;
	kFontMicrosoftPlatform		= 3;
	kFontCustomPlatform			= 4;

	kFontUnicodeDefaultSemantics = 0;
	kFontUnicodeV1_1Semantics	= 1;
	kFontISO10646_1993Semantics	= 2;

	kFontRomanScript			= 0;
	kFontJapaneseScript			= 1;
	kFontTraditionalChineseScript = 2;
	kFontChineseScript			= 2;
	kFontKoreanScript			= 3;
	kFontArabicScript			= 4;
	kFontHebrewScript			= 5;
	kFontGreekScript			= 6;
	kFontCyrillicScript			= 7;
	kFontRussian				= 7;
	kFontRSymbolScript			= 8;
	kFontDevanagariScript		= 9;
	kFontGurmukhiScript			= 10;
	kFontGujaratiScript			= 11;
	kFontOriyaScript			= 12;
	kFontBengaliScript			= 13;
	kFontTamilScript			= 14;
	kFontTeluguScript			= 15;
	kFontKannadaScript			= 16;
	kFontMalayalamScript		= 17;
	kFontSinhaleseScript		= 18;
	kFontBurmeseScript			= 19;
	kFontKhmerScript			= 20;
	kFontThaiScript				= 21;
	kFontLaotianScript			= 22;
	kFontGeorgianScript			= 23;
	kFontArmenianScript			= 24;
	kFontSimpleChineseScript	= 25;
	kFontTibetanScript			= 26;
	kFontMongolianScript		= 27;
	kFontGeezScript				= 28;
	kFontEthiopicScript			= 28;
	kFontAmharicScript			= 28;
	kFontSlavicScript			= 29;
	kFontEastEuropeanRomanScript = 29;
	kFontVietnameseScript		= 30;
	kFontExtendedArabicScript	= 31;
	kFontSindhiScript			= 31;
	kFontUninterpretedScript	= 32;

	kFontMicrosoftSymbolScript	= 0;
	kFontMicrosoftStandardScript = 1;
	kFontMicrosoftUCS4Script	= 10;


	kFontCustom8BitScript		= 0;
	kFontCustom816BitScript		= 1;
	kFontCustom16BitScript		= 2;

	{	 Language codes are zero based everywhere but within a 'cmap' table 	}
	kFontEnglishLanguage		= 0;
	kFontFrenchLanguage			= 1;
	kFontGermanLanguage			= 2;
	kFontItalianLanguage		= 3;
	kFontDutchLanguage			= 4;
	kFontSwedishLanguage		= 5;
	kFontSpanishLanguage		= 6;
	kFontDanishLanguage			= 7;
	kFontPortugueseLanguage		= 8;
	kFontNorwegianLanguage		= 9;
	kFontHebrewLanguage			= 10;
	kFontJapaneseLanguage		= 11;
	kFontArabicLanguage			= 12;
	kFontFinnishLanguage		= 13;
	kFontGreekLanguage			= 14;
	kFontIcelandicLanguage		= 15;
	kFontMalteseLanguage		= 16;
	kFontTurkishLanguage		= 17;
	kFontCroatianLanguage		= 18;
	kFontTradChineseLanguage	= 19;
	kFontUrduLanguage			= 20;
	kFontHindiLanguage			= 21;
	kFontThaiLanguage			= 22;
	kFontKoreanLanguage			= 23;
	kFontLithuanianLanguage		= 24;
	kFontPolishLanguage			= 25;
	kFontHungarianLanguage		= 26;
	kFontEstonianLanguage		= 27;
	kFontLettishLanguage		= 28;
	kFontLatvianLanguage		= 28;
	kFontSaamiskLanguage		= 29;
	kFontLappishLanguage		= 29;
	kFontFaeroeseLanguage		= 30;
	kFontFarsiLanguage			= 31;
	kFontPersianLanguage		= 31;
	kFontRussianLanguage		= 32;
	kFontSimpChineseLanguage	= 33;
	kFontFlemishLanguage		= 34;
	kFontIrishLanguage			= 35;
	kFontAlbanianLanguage		= 36;
	kFontRomanianLanguage		= 37;
	kFontCzechLanguage			= 38;
	kFontSlovakLanguage			= 39;
	kFontSlovenianLanguage		= 40;
	kFontYiddishLanguage		= 41;
	kFontSerbianLanguage		= 42;
	kFontMacedonianLanguage		= 43;
	kFontBulgarianLanguage		= 44;
	kFontUkrainianLanguage		= 45;
	kFontByelorussianLanguage	= 46;
	kFontUzbekLanguage			= 47;
	kFontKazakhLanguage			= 48;
	kFontAzerbaijaniLanguage	= 49;
	kFontAzerbaijanArLanguage	= 50;
	kFontArmenianLanguage		= 51;
	kFontGeorgianLanguage		= 52;
	kFontMoldavianLanguage		= 53;
	kFontKirghizLanguage		= 54;
	kFontTajikiLanguage			= 55;
	kFontTurkmenLanguage		= 56;
	kFontMongolianLanguage		= 57;
	kFontMongolianCyrLanguage	= 58;
	kFontPashtoLanguage			= 59;
	kFontKurdishLanguage		= 60;
	kFontKashmiriLanguage		= 61;
	kFontSindhiLanguage			= 62;
	kFontTibetanLanguage		= 63;
	kFontNepaliLanguage			= 64;
	kFontSanskritLanguage		= 65;
	kFontMarathiLanguage		= 66;
	kFontBengaliLanguage		= 67;
	kFontAssameseLanguage		= 68;
	kFontGujaratiLanguage		= 69;
	kFontPunjabiLanguage		= 70;
	kFontOriyaLanguage			= 71;
	kFontMalayalamLanguage		= 72;
	kFontKannadaLanguage		= 73;
	kFontTamilLanguage			= 74;
	kFontTeluguLanguage			= 75;
	kFontSinhaleseLanguage		= 76;
	kFontBurmeseLanguage		= 77;
	kFontKhmerLanguage			= 78;
	kFontLaoLanguage			= 79;
	kFontVietnameseLanguage		= 80;
	kFontIndonesianLanguage		= 81;
	kFontTagalogLanguage		= 82;
	kFontMalayRomanLanguage		= 83;
	kFontMalayArabicLanguage	= 84;
	kFontAmharicLanguage		= 85;
	kFontTigrinyaLanguage		= 86;
	kFontGallaLanguage			= 87;
	kFontOromoLanguage			= 87;
	kFontSomaliLanguage			= 88;
	kFontSwahiliLanguage		= 89;
	kFontRuandaLanguage			= 90;
	kFontRundiLanguage			= 91;
	kFontChewaLanguage			= 92;
	kFontMalagasyLanguage		= 93;
	kFontEsperantoLanguage		= 94;
	kFontWelshLanguage			= 128;
	kFontBasqueLanguage			= 129;
	kFontCatalanLanguage		= 130;
	kFontLatinLanguage			= 131;
	kFontQuechuaLanguage		= 132;
	kFontGuaraniLanguage		= 133;
	kFontAymaraLanguage			= 134;
	kFontTatarLanguage			= 135;
	kFontUighurLanguage			= 136;
	kFontDzongkhaLanguage		= 137;
	kFontJavaneseRomLanguage	= 138;
	kFontSundaneseRomLanguage	= 139;

	{	 The following are special "don't care" values to be used in interfaces 	}
	kFontNoPlatform				= $FFFFFFFF;
	kFontNoScript				= $FFFFFFFF;
	kFontNoLanguage				= $FFFFFFFF;


type
	sfntCMapSubHeaderPtr = ^sfntCMapSubHeader;
	sfntCMapSubHeader = record
		format:					UInt16;
		length:					UInt16;
		languageID:				UInt16;									{  base-1  }
	end;


const
	sizeof_sfntCMapSubHeader	= 6;


type
	sfntCMapEncodingPtr = ^sfntCMapEncoding;
	sfntCMapEncoding = record
		platformID:				UInt16;									{  base-0  }
		scriptID:				UInt16;									{  base-0  }
		offset:					UInt32;
	end;


const
	sizeof_sfntCMapEncoding		= 8;


type
	sfntCMapHeaderPtr = ^sfntCMapHeader;
	sfntCMapHeader = record
		version:				UInt16;
		numTables:				UInt16;
		encoding:				array [0..0] of sfntCMapEncoding;
	end;


const
	sizeof_sfntCMapHeader		= 4;

	{	 Name table 	}
	nameFontTableTag			= $6E616D65 (* 'name' *);

	kFontCopyrightName			= 0;
	kFontFamilyName				= 1;
	kFontStyleName				= 2;
	kFontUniqueName				= 3;
	kFontFullName				= 4;
	kFontVersionName			= 5;
	kFontPostscriptName			= 6;
	kFontTrademarkName			= 7;
	kFontManufacturerName		= 8;
	kFontDesignerName			= 9;
	kFontDescriptionName		= 10;
	kFontVendorURLName			= 11;
	kFontDesignerURLName		= 12;
	kFontLicenseDescriptionName	= 13;
	kFontLicenseInfoURLName		= 14;
	kFontLastReservedName		= 255;

	{	 The following is a special "don't care" value to be used in interfaces 	}
	kFontNoName					= $FFFFFFFF;


type
	sfntNameRecordPtr = ^sfntNameRecord;
	sfntNameRecord = record
		platformID:				UInt16;									{  base-0  }
		scriptID:				UInt16;									{  base-0  }
		languageID:				UInt16;									{  base-0  }
		nameID:					UInt16;									{  base-0  }
		length:					UInt16;
		offset:					UInt16;
	end;


const
	sizeof_sfntNameRecord		= 12;


type
	sfntNameHeaderPtr = ^sfntNameHeader;
	sfntNameHeader = record
		format:					UInt16;
		count:					UInt16;
		stringOffset:			UInt16;
		rec:					array [0..0] of sfntNameRecord;
	end;


const
	sizeof_sfntNameHeader		= 6;

	{	 Fvar table - font variations 	}
	variationFontTableTag		= $66766172 (* 'fvar' *);

	{	 These define each font variation 	}

type
	sfntVariationAxisPtr = ^sfntVariationAxis;
	sfntVariationAxis = record
		axisTag:				FourCharCode;
		minValue:				Fixed;
		defaultValue:			Fixed;
		maxValue:				Fixed;
		flags:					SInt16;
		nameID:					SInt16;
	end;


const
	sizeof_sfntVariationAxis	= 20;

	{	 These are named locations in style-space for the user 	}

type
	sfntInstancePtr = ^sfntInstance;
	sfntInstance = record
		nameID:					SInt16;
		flags:					SInt16;
		coord:					array [0..0] of Fixed;					{  [axisCount]  }
																		{  room to grow since the header carries a tupleSize field  }
	end;


const
	sizeof_sfntInstance			= 4;


type
	sfntVariationHeaderPtr = ^sfntVariationHeader;
	sfntVariationHeader = record
		version:				Fixed;									{  1.0 Fixed  }
		offsetToData:			UInt16;									{  to first axis = 16 }
		countSizePairs:			UInt16;									{  axis+inst = 2  }
		axisCount:				UInt16;
		axisSize:				UInt16;
		instanceCount:			UInt16;
		instanceSize:			UInt16;
																		{  Éother <count,size> pairs  }
		axis:					array [0..0] of sfntVariationAxis;		{  [axisCount]  }
		instance:				array [0..0] of sfntInstance;			{  [instanceCount]  Éother arrays of data  }
	end;


const
	sizeof_sfntVariationHeader	= 16;

	{	 Fdsc table - font descriptor 	}
	descriptorFontTableTag		= $66647363 (* 'fdsc' *);


type
	sfntFontDescriptorPtr = ^sfntFontDescriptor;
	sfntFontDescriptor = record
		name:					FourCharCode;
		value:					Fixed;
	end;

	sfntDescriptorHeaderPtr = ^sfntDescriptorHeader;
	sfntDescriptorHeader = record
		version:				Fixed;									{  1.0 in Fixed  }
		descriptorCount:		SInt32;
		descriptor:				array [0..0] of sfntFontDescriptor;
	end;


const
	sizeof_sfntDescriptorHeader	= 8;

	{	 Feat Table - layout feature table 	}
	featureFontTableTag			= $66656174 (* 'feat' *);


type
	sfntFeatureNamePtr = ^sfntFeatureName;
	sfntFeatureName = record
		featureType:			UInt16;
		settingCount:			UInt16;
		offsetToSettings:		SInt32;
		featureFlags:			UInt16;
		nameID:					UInt16;
	end;

	sfntFontFeatureSettingPtr = ^sfntFontFeatureSetting;
	sfntFontFeatureSetting = record
		setting:				UInt16;
		nameID:					UInt16;
	end;

	sfntFontRunFeaturePtr = ^sfntFontRunFeature;
	sfntFontRunFeature = record
		featureType:			UInt16;
		setting:				UInt16;
	end;

	sfntFeatureHeaderPtr = ^sfntFeatureHeader;
	sfntFeatureHeader = record
		version:				SInt32;									{  1.0  }
		featureNameCount:		UInt16;
		featureSetCount:		UInt16;
		reserved:				SInt32;									{  set to 0  }
		names:					array [0..0] of sfntFeatureName;
		settings:				array [0..0] of sfntFontFeatureSetting;
		runs:					array [0..0] of sfntFontRunFeature;
	end;

	{	 OS/2 Table 	}

const
	os2FontTableTag				= $4F532F32 (* 'OS/2' *);

	{	  Special invalid glyph ID value, useful as a sentinel value, for example 	}
	nonGlyphID					= 65535;

	{	  Data type used to access names from font name table 	}

type
	FontNameCode						= UInt32;
	FontNameCodePtr						= ^FontNameCode; { when a VAR xx: FontNameCode parameter can be nil, it is changed to xx: FontNameCodePtr }
	{	 Data types for encoding components as used in interfaces 	}
	FontPlatformCode					= UInt32;
	FontPlatformCodePtr					= ^FontPlatformCode; { when a VAR xx: FontPlatformCode parameter can be nil, it is changed to xx: FontPlatformCodePtr }
	FontScriptCode						= UInt32;
	FontScriptCodePtr					= ^FontScriptCode; { when a VAR xx: FontScriptCode parameter can be nil, it is changed to xx: FontScriptCodePtr }
	FontLanguageCode					= UInt32;
	FontLanguageCodePtr					= ^FontLanguageCode; { when a VAR xx: FontLanguageCode parameter can be nil, it is changed to xx: FontLanguageCodePtr }
	{	
	**  FontVariation is used to specify a coordinate along a variation axis. The name
	**  identifies the axes to be applied, and value is the setting to be used.
		}
	FontVariationPtr = ^FontVariation;
	FontVariation = record
		name:					FourCharCode;
		value:					Fixed;
	end;

{$ALIGN MAC68K}


end.
