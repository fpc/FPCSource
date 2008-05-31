{
     File:       MacLocales.p
 
     Contains:   Types & prototypes for locale functions
 
     Version:    Technology: Mac OS 9.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit MacLocales;
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
uses MacTypes,MacErrors;


{$ALIGN MAC68K}


{
   -------------------------------------------------------------------------------------------------
   TYPES & CONSTANTS
   -------------------------------------------------------------------------------------------------
}


type
	LocaleRef    = ^SInt32; { an opaque 32-bit type }
	LocaleRefPtr = ^LocaleRef;  { when a var xx:LocaleRef parameter can be nil, it is changed to xx: LocaleRefPtr }
	LocalePartMask						= UInt32;

const
																{  bit set requests the following: }
	kLocaleLanguageMask			= $00000001;					{  ISO 639-1 or -2 language code (2 or 3 letters) }
	kLocaleLanguageVariantMask	= $00000002;					{  custom string for language variant }
	kLocaleScriptMask			= $00000004;					{  ISO 15924 script code (2 letters) }
	kLocaleScriptVariantMask	= $00000008;					{  custom string for script variant }
	kLocaleRegionMask			= $00000010;					{  ISO 3166 country/region code (2 letters) }
	kLocaleRegionVariantMask	= $00000020;					{  custom string for region variant }
	kLocaleAllPartsMask			= $0000003F;					{  all of the above }


type
	LocaleOperationClass				= FourCharCode;
	{  constants for LocaleOperationClass are in UnicodeUtilities interfaces }
	LocaleOperationVariant				= FourCharCode;
	LocaleAndVariantPtr = ^LocaleAndVariant;
	LocaleAndVariant = record
		locale:					LocaleRef;
		opVariant:				LocaleOperationVariant;
	end;

	LocaleNameMask						= UInt32;

const
																{  bit set requests the following: }
	kLocaleNameMask				= $00000001;					{  name of locale }
	kLocaleOperationVariantNameMask = $00000002;				{  name of LocaleOperationVariant }
	kLocaleAndVariantNameMask	= $00000003;					{  all of the above }

	{
	   -------------------------------------------------------------------------------------------------
	   function PROTOTYPES
	   -------------------------------------------------------------------------------------------------
	}

	{  Convert to or from LocaleRefs (and related utilities) }

	{
	 *  LocaleRefFromLangOrRegionCode()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function LocaleRefFromLangOrRegionCode(lang: LangCode; region: RegionCode; var locale: LocaleRef): OSStatus; external name '_LocaleRefFromLangOrRegionCode';

{
 *  LocaleRefFromLocaleString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleRefFromLocaleString(localeString: ConstCStringPtr; var locale: LocaleRef): OSStatus; external name '_LocaleRefFromLocaleString';

{
 *  LocaleRefGetPartString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleRefGetPartString(locale: LocaleRef; partMask: LocalePartMask; maxStringLen: ByteCount; var partString: char): OSStatus; external name '_LocaleRefGetPartString';

{
 *  LocaleStringToLangAndRegionCodes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleStringToLangAndRegionCodes(localeString: ConstCStringPtr; var lang: LangCode; var region: RegionCode): OSStatus; external name '_LocaleStringToLangAndRegionCodes';

{  Enumerate locales for a LocaleOperationClass  }
{
 *  LocaleOperationCountLocales()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleOperationCountLocales(opClass: LocaleOperationClass; var localeCount: ItemCount): OSStatus; external name '_LocaleOperationCountLocales';

{
 *  LocaleOperationGetLocales()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleOperationGetLocales(opClass: LocaleOperationClass; maxLocaleCount: ItemCount; var actualLocaleCount: ItemCount; var localeVariantList: LocaleAndVariant): OSStatus; external name '_LocaleOperationGetLocales';

{  Get names for a locale (or a region's language) }

{
 *  LocaleGetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleGetName(locale: LocaleRef; opVariant: LocaleOperationVariant; nameMask: LocaleNameMask; displayLocale: LocaleRef; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: UniCharPtr): OSStatus; external name '_LocaleGetName';

{
 *  LocaleCountNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleCountNames(locale: LocaleRef; opVariant: LocaleOperationVariant; nameMask: LocaleNameMask; var nameCount: ItemCount): OSStatus; external name '_LocaleCountNames';

{
 *  LocaleGetIndName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleGetIndName(locale: LocaleRef; opVariant: LocaleOperationVariant; nameMask: LocaleNameMask; nameIndex: ItemCount; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: UniCharPtr; var displayLocale: LocaleRef): OSStatus; external name '_LocaleGetIndName';

{
 *  LocaleGetRegionLanguageName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleGetRegionLanguageName(region: RegionCode; var languageName: Str255): OSStatus; external name '_LocaleGetRegionLanguageName';

{  Get names for a LocaleOperationClass }
{
 *  LocaleOperationGetName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleOperationGetName(opClass: LocaleOperationClass; displayLocale: LocaleRef; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: UniCharPtr): OSStatus; external name '_LocaleOperationGetName';

{
 *  LocaleOperationCountNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleOperationCountNames(opClass: LocaleOperationClass; var nameCount: ItemCount): OSStatus; external name '_LocaleOperationCountNames';

{
 *  LocaleOperationGetIndName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LocaleOperationGetIndName(opClass: LocaleOperationClass; nameIndex: ItemCount; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: UniCharPtr; var displayLocale: LocaleRef): OSStatus; external name '_LocaleOperationGetIndName';

{$ALIGN MAC68K}


end.
