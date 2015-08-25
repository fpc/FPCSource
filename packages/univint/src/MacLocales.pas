{
     File:       CarbonCore/MacLocales.h
 
     Contains:   Types & prototypes for locale functions
 
     Copyright:  © 1998-2012 by Apple Inc. All rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
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

unit MacLocales;
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
{$ifc defined(iphonesim)}
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
{$ifc defined(iphonesim)}
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
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
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
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
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}


{
   -------------------------------------------------------------------------------------------------
   TYPES & CONSTANTS
   -------------------------------------------------------------------------------------------------
}

type
	LocaleRef = ^OpaqueLocaleRef; { an opaque type }
	OpaqueLocaleRef = record end;
	LocalePartMask = UInt32;
const
{ bit set requests the following:}
	kLocaleLanguageMask = 1 shl 0; { ISO 639-1 or -2 language code (2 or 3 letters)}
	kLocaleLanguageVariantMask = 1 shl 1; { custom string for language variant}
	kLocaleScriptMask = 1 shl 2; { ISO 15924 script code (2 letters)}
	kLocaleScriptVariantMask = 1 shl 3; { custom string for script variant}
	kLocaleRegionMask = 1 shl 4; { ISO 3166 country/region code (2 letters)}
	kLocaleRegionVariantMask = 1 shl 5; { custom string for region variant}
	kLocaleAllPartsMask = $0000003F; { all of the above}

type
	LocaleOperationClass = FourCharCode;
{ constants for LocaleOperationClass are in UnicodeUtilities interfaces}
type
	LocaleAndVariantPtr = ^LocaleAndVariant;
	LocaleOperationVariant = FourCharCode;
	LocaleAndVariant = record
		locale: LocaleRef;
		opVariant: LocaleOperationVariant;
	end;

type
	LocaleNameMask = UInt32;
const
{ bit set requests the following:}
	kLocaleNameMask = 1 shl 0; { name of locale}
	kLocaleOperationVariantNameMask = 1 shl 1; { name of LocaleOperationVariant}
	kLocaleAndVariantNameMask = $00000003; { all of the above}

{
   -------------------------------------------------------------------------------------------------
   FUNCTION PROTOTYPES
   -------------------------------------------------------------------------------------------------
}

{ Convert to or from LocaleRefs (and related utilities)}

{
 *  LocaleRefFromLangOrRegionCode()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleRefFromLangOrRegionCode( lang: LangCode; region: RegionCode; var locale: LocaleRef ): OSStatus; external name '_LocaleRefFromLangOrRegionCode';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  LocaleRefFromLocaleString()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleRefFromLocaleString( localeString: ConstCStringPtr; var locale: LocaleRef ): OSStatus; external name '_LocaleRefFromLocaleString';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  LocaleRefGetPartString()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleRefGetPartString( locale: LocaleRef; partMask: LocalePartMask; maxStringLen: ByteCount; partString: CStringPtr ): OSStatus; external name '_LocaleRefGetPartString';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  LocaleStringToLangAndRegionCodes()
 *  
 *  Summary:
 *    Map a CFLocale-style locale string to old Script Manager LangCode
 *    and RegionCode values.
 *  
 *  Parameters:
 *    
 *    localeString:
 *      A null-terminated C-string version of a CFLocale-style locale
 *      identifier of the sort that could be passed to
 *      CFLocaleCreateCanonicalLocaleIdentifierFromString, based on BCP
 *      47 language tags: <http://www.rfc-editor.org/rfc/bcp/bcp47.txt>.
 *    
 *    lang:
 *      A pointer to a LangCode to be set from the locale identifier;
 *      will be set to langUnspecified or -1 if no language code can be
 *      derived from the identifier. May be NULL if region is not also
 *      NULL.
 *    
 *    region:
 *      A pointer to a RegionCode to be set from the locale identifier;
 *      will be set to -1 if no language code can be derived from the
 *      identifier. May be NULL if lang is not also NULL.
 *  
 *  Result:
 *    OSStatus, noErr if operation successful, otherwise paramErr or
 *    possibly other errors.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 9.0 and later
 }
function LocaleStringToLangAndRegionCodes( localeString: ConstCStringPtr; var lang: LangCode; var region: RegionCode ): OSStatus; external name '_LocaleStringToLangAndRegionCodes';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{ Enumerate locales for a LocaleOperationClass }

{
 *  LocaleOperationCountLocales()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFLocaleCopyAvailableLocaleIdentifiers instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFLocaleCopyAvailableLocaleIdentifiers instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.8
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleOperationCountLocales( opClass: LocaleOperationClass; var localeCount: ItemCount ): OSStatus; external name '_LocaleOperationCountLocales';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LocaleOperationGetLocales()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFXxxxx instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFLocaleCopyAvailableLocaleIdentifiers instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.8
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleOperationGetLocales( opClass: LocaleOperationClass; maxLocaleCount: ItemCount; var actualLocaleCount: ItemCount; localeVariantList: {variable-size-array} LocaleAndVariantPtr ): OSStatus; external name '_LocaleOperationGetLocales';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ Get names for a locale (or a region's language)}

{
 *  LocaleGetName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFLocaleCopyDisplayNameForPropertyValue instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFLocaleCopyDisplayNameForPropertyValue instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.8
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleGetName( locale: LocaleRef; opVariant: LocaleOperationVariant; nameMask: LocaleNameMask; displayLocale: LocaleRef; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: {variable-size-array} UniCharPtr ): OSStatus; external name '_LocaleGetName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LocaleCountNames()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFLocaleCopyAvailableLocaleIdentifiers instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFLocaleCopyAvailableLocaleIdentifiers instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.8
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleCountNames( locale: LocaleRef; opVariant: LocaleOperationVariant; nameMask: LocaleNameMask; var nameCount: ItemCount ): OSStatus; external name '_LocaleCountNames';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  LocaleGetIndName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFLocaleCopyAvailableLocaleIdentifiers and
 *    CFLocaleCopyDisplayNameForPropertyValue instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFLocaleCopyAvailableLocaleIdentifiers and
 *    CFLocaleCopyDisplayNameForPropertyValue instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework but deprecated in 10.8
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleGetIndName( locale: LocaleRef; opVariant: LocaleOperationVariant; nameMask: LocaleNameMask; nameIndex: ItemCount; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: {variable-size-array} UniCharPtr; var displayLocale: LocaleRef ): OSStatus; external name '_LocaleGetIndName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc TARGET_CPU_64}
{
 *  LocaleGetRegionLanguageName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    use CFLocaleCreateCanonicalLocaleIdentifierFromScriptManagerCodes
 *    and CFLocaleCopyDisplayNameForPropertyValue instead.
 *  
 *  Discussion:
 *    This function is no longer recommended. Please use
 *    CFLocaleCreateCanonicalLocaleIdentifierFromScriptManagerCodes and
 *    CFLocaleCopyDisplayNameForPropertyValue instead.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 9.0 and later
 }
function LocaleGetRegionLanguageName( region: RegionCode; var languageName: Str255 ): OSStatus; external name '_LocaleGetRegionLanguageName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ Get names for a LocaleOperationClass}

{$endc} {TARGET_CPU_64}

{
 *  LocaleOperationGetName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleOperationGetName( opClass: LocaleOperationClass; displayLocale: LocaleRef; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: {variable-size-array} UniCharPtr ): OSStatus; external name '_LocaleOperationGetName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  LocaleOperationCountNames()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleOperationCountNames( opClass: LocaleOperationClass; var nameCount: ItemCount ): OSStatus; external name '_LocaleOperationCountNames';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)


{
 *  LocaleOperationGetIndName()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in LocalesLib 8.6 and later
 }
function LocaleOperationGetIndName( opClass: LocaleOperationClass; nameIndex: ItemCount; maxNameLen: UniCharCount; var actualNameLen: UniCharCount; displayName: {variable-size-array} UniCharPtr; var displayLocale: LocaleRef ): OSStatus; external name '_LocaleOperationGetIndName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_0, __IPHONE_NA) *)



{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
