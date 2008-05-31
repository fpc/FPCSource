{	CFStringEncodingExt.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
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

unit CFStringEncodingExt;
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
uses MacTypes,CFBase;
{$ALIGN POWER}


type
	CFStringEncodings = SInt32;
const
{   kCFStringEncodingMacRoman = 0L, defined in CoreFoundation/CFString.h  }
	kCFStringEncodingMacJapanese = 1;
	kCFStringEncodingMacChineseTrad = 2;
	kCFStringEncodingMacKorean = 3;
	kCFStringEncodingMacArabic = 4;
	kCFStringEncodingMacHebrew = 5;
	kCFStringEncodingMacGreek = 6;
	kCFStringEncodingMacCyrillic = 7;
	kCFStringEncodingMacDevanagari = 9;
	kCFStringEncodingMacGurmukhi = 10;
	kCFStringEncodingMacGujarati = 11;
	kCFStringEncodingMacOriya = 12;
	kCFStringEncodingMacBengali = 13;
	kCFStringEncodingMacTamil = 14;
	kCFStringEncodingMacTelugu = 15;
	kCFStringEncodingMacKannada = 16;
	kCFStringEncodingMacMalayalam = 17;
	kCFStringEncodingMacSinhalese = 18;
	kCFStringEncodingMacBurmese = 19;
	kCFStringEncodingMacKhmer = 20;
	kCFStringEncodingMacThai = 21;
	kCFStringEncodingMacLaotian = 22;
	kCFStringEncodingMacGeorgian = 23;
	kCFStringEncodingMacArmenian = 24;
	kCFStringEncodingMacChineseSimp = 25;
	kCFStringEncodingMacTibetan = 26;
	kCFStringEncodingMacMongolian = 27;
	kCFStringEncodingMacEthiopic = 28;
	kCFStringEncodingMacCentralEurRoman = 29;
	kCFStringEncodingMacVietnamese = 30;
	kCFStringEncodingMacExtArabic = 31;
	{  The following use script code 0, smRoman  }
	kCFStringEncodingMacSymbol = 33;
	kCFStringEncodingMacDingbats = 34;
	kCFStringEncodingMacTurkish = 35;
	kCFStringEncodingMacCroatian = 36;
	kCFStringEncodingMacIcelandic = 37;
	kCFStringEncodingMacRomanian = 38;
	kCFStringEncodingMacCeltic = 39;
	kCFStringEncodingMacGaelic = 40;
	{  The following use script code 4, smArabic  }
	kCFStringEncodingMacFarsi = $8C; {  Like MacArabic but uses Farsi digits  }
	{  The following use script code 7, smCyrillic  }
	kCFStringEncodingMacUkrainian = $98;
	{  The following use script code 32, smUnimplemented  }
	kCFStringEncodingMacInuit = $EC;
	kCFStringEncodingMacVT100 = $FC; {  VT100/102 font from Comm Toolbox: Latin-1 repertoire + box drawing etc  }
	{  Special Mac OS encodings }
	kCFStringEncodingMacHFS = $FF; {  Meta-value, should never appear in a table  }
	
	{  Unicode & ISO UCS encodings begin at 0x100  }
	{  We don't use Unicode variations defined in TextEncoding; use the ones in CFString.h, instead.  }
	
	{  ISO 8-bit and 7-bit encodings begin at 0x200  }
	{   kCFStringEncodingISOLatin1 = 0x0201, defined in CoreFoundation/CFString.h  }
	kCFStringEncodingISOLatin2 = $0202;						{  ISO 8859-2  }
	kCFStringEncodingISOLatin3 = $0203;						{  ISO 8859-3  }
	kCFStringEncodingISOLatin4 = $0204;						{  ISO 8859-4  }
	kCFStringEncodingISOLatinCyrillic = $0205;					{  ISO 8859-5  }
	kCFStringEncodingISOLatinArabic = $0206;					{  ISO 8859-6, =ASMO 708, =DOS CP 708  }
	kCFStringEncodingISOLatinGreek = $0207;						{  ISO 8859-7  }
	kCFStringEncodingISOLatinHebrew = $0208;					{  ISO 8859-8  }
	kCFStringEncodingISOLatin5 = $0209;						{  ISO 8859-9  }
	kCFStringEncodingISOLatin6 = $020A;						{  ISO 8859-10  }
	kCFStringEncodingISOLatinThai = $020B;						{  ISO 8859-11  }
	kCFStringEncodingISOLatin7 = $020D;						{  ISO 8859-13  }
	kCFStringEncodingISOLatin8 = $020E;						{  ISO 8859-14  }
	kCFStringEncodingISOLatin9 = $020F;						{  ISO 8859-15  }
{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    kCFStringEncodingISOLatin10 = $0210;	{ ISO 8859-16 }
{#endif} { MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }

	{  MS-DOS & Windows encodings begin at 0x400  }
	kCFStringEncodingDOSLatinUS = $0400;						{  code page 437  }
	kCFStringEncodingDOSGreek = $0405;						{  code page 737 (formerly code page 437G)  }
	kCFStringEncodingDOSBalticRim = $0406;						{  code page 775  }
	kCFStringEncodingDOSLatin1 = $0410;						{  code page 850, "Multilingual"  }
	kCFStringEncodingDOSGreek1 = $0411;						{  code page 851  }
	kCFStringEncodingDOSLatin2 = $0412;						{  code page 852, Slavic  }
	kCFStringEncodingDOSCyrillic = $0413;						{  code page 855, IBM Cyrillic  }
	kCFStringEncodingDOSTurkish = $0414;						{  code page 857, IBM Turkish  }
	kCFStringEncodingDOSPortuguese = $0415;						{  code page 860  }
	kCFStringEncodingDOSIcelandic = $0416;						{  code page 861  }
	kCFStringEncodingDOSHebrew = $0417;						{  code page 862  }
	kCFStringEncodingDOSCanadianFrench = $0418;					{  code page 863  }
	kCFStringEncodingDOSArabic = $0419;						{  code page 864  }
	kCFStringEncodingDOSNordic = $041A;						{  code page 865  }
	kCFStringEncodingDOSRussian = $041B;						{  code page 866  }
	kCFStringEncodingDOSGreek2 = $041C;						{  code page 869, IBM Modern Greek  }
	kCFStringEncodingDOSThai = $041D;						{  code page 874, also for Windows  }
	kCFStringEncodingDOSJapanese = $0420;						{  code page 932, also for Windows  }
	kCFStringEncodingDOSChineseSimplif = $0421;					{  code page 936, also for Windows  }
	kCFStringEncodingDOSKorean = $0422;						{  code page 949, also for Windows; Unified Hangul Code  }
	kCFStringEncodingDOSChineseTrad = $0423;					{  code page 950, also for Windows  }
	{   kCFStringEncodingWindowsLatin1 = 0x0500, defined in CoreFoundation/CFString.h  }
	kCFStringEncodingWindowsLatin2 = $0501;						{  code page 1250, Central Europe  }
	kCFStringEncodingWindowsCyrillic = $0502;					{  code page 1251, Slavic Cyrillic  }
	kCFStringEncodingWindowsGreek = $0503;						{  code page 1253  }
	kCFStringEncodingWindowsLatin5 = $0504;						{  code page 1254, Turkish  }
	kCFStringEncodingWindowsHebrew = $0505;						{  code page 1255  }
	kCFStringEncodingWindowsArabic = $0506;						{  code page 1256  }
	kCFStringEncodingWindowsBalticRim = $0507;					{  code page 1257  }
	kCFStringEncodingWindowsVietnamese = $0508;					{  code page 1258  }
	kCFStringEncodingWindowsKoreanJohab = $0510;				{  code page 1361, for Windows NT  }

	{  Various national standards begin at 0x600  }
	{   kCFStringEncodingASCII = 0x0600, defined in CoreFoundation/CFString.h  }
{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    kCFStringEncodingANSEL = $0601;	{ ANSEL (ANSI Z39.47) }
{#endif} { MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }
	kCFStringEncodingJIS_X0201_76 = $0620;
	kCFStringEncodingJIS_X0208_83 = $0621;
	kCFStringEncodingJIS_X0208_90 = $0622;
	kCFStringEncodingJIS_X0212_90 = $0623;
	kCFStringEncodingJIS_C6226_78 = $0624;
	kCFStringEncodingShiftJIS_X0213_00 = $0628; { Shift-JIS format encoding of JIS X0213 planes 1 and 2}
{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    kCFStringEncodingShiftJIS_X0213_MenKuTen = $0629;	{ JIS X0213 in plane-row-column notation }
{#endif} { MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }
	kCFStringEncodingGB_2312_80 = $0630;
	kCFStringEncodingGBK_95 = $0631;						{  annex to GB 13000-93; for Windows 95  }
	kCFStringEncodingKSC_5601_87 = $0640;						{  same as KSC 5601-92 without Johab annex  }
	kCFStringEncodingKSC_5601_92_Johab = $0641;					{  KSC 5601-92 Johab annex  }
	kCFStringEncodingCNS_11643_92_P1 = $0651;					{  CNS 11643-1992 plane 1  }
	kCFStringEncodingCNS_11643_92_P2 = $0652;					{  CNS 11643-1992 plane 2  }
	kCFStringEncodingCNS_11643_92_P3 = $0653;					{  CNS 11643-1992 plane 3 (was plane 14 in 1986 version)  }

	{  ISO 2022 collections begin at 0x800  }
	kCFStringEncodingISO_2022_JP = $0820;
	kCFStringEncodingISO_2022_JP_2 = $0821;
	kCFStringEncodingISO_2022_JP_1 = $0822; { RFC 2237}
	kCFStringEncodingISO_2022_JP_3 = $0823; { JIS X0213}
	kCFStringEncodingISO_2022_CN = $0830;
	kCFStringEncodingISO_2022_CN_EXT = $0831;
	kCFStringEncodingISO_2022_KR = $0840;
	
	{  EUC collections begin at 0x900  }
	kCFStringEncodingEUC_JP = $0920;						{  ISO 646, 1-byte katakana, JIS 208, JIS 212  }
	kCFStringEncodingEUC_CN = $0930;						{  ISO 646, GB 2312-80  }
	kCFStringEncodingEUC_TW = $0931;						{  ISO 646, CNS 11643-1992 Planes 1-16  }
	kCFStringEncodingEUC_KR = $0940;						{  ISO 646, KS C 5601-1987  }

	{  Misc standards begin at 0xA00  }
	kCFStringEncodingShiftJIS = $0A01;						{  plain Shift-JIS  }
	kCFStringEncodingKOI8_R = $0A02;						{  Russian internet standard  }
	kCFStringEncodingBig5 = $0A03;						{  Big-5 (has variants)  }
	kCFStringEncodingMacRomanLatin1 = $0A04;					{  Mac OS Roman permuted to align with ISO Latin-1  }
	kCFStringEncodingHZ_GB_2312 = $0A05;						{  HZ (RFC 1842, for Chinese mail & news)  }
	kCFStringEncodingBig5_HKSCS_1999 = $0A06; { Big-5 with Hong Kong special char set supplement}
{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
	kCFStringEncodingVISCII = $0A07;	{ RFC 1456, Vietnamese }
	kCFStringEncodingKOI8_U = $0A08;	{ RFC 2319, Ukrainian }
	kCFStringEncodingBig5_E = $0A09;	{ Taiwan Big-5E standard }
{#endif} { MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }

	{  Other platform encodings }
{  kCFStringEncodingNextStepLatin = 0x0B01, defined in CoreFoundation/CFString.h }
{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
	kCFStringEncodingNextStepJapanese = $0B02;	{ NextStep Japanese encoding }
{#endif} { MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }
																{  EBCDIC & IBM host encodings begin at 0xC00  }
	kCFStringEncodingEBCDIC_US = $0C01;						{  basic EBCDIC-US  }
	kCFStringEncodingEBCDIC_CP037 = $0C02;						{  code page 037, extended EBCDIC (Latin-1 set) for US,Canada...  }


end.
