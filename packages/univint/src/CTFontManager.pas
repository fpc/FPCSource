{
 *  CTFontManager.h
 *  CoreText
 *
 *  Copyright (c) 2008 Apple Inc. All rights reserved.
 *
 }
{       Initial Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CTFontManager;
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
	{$setc TARGET_CPU_PPC := TFALSE}
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
uses MacTypes,CTFontDescriptor,CTFontManagerErrors,CFBase,CFArray,CFError;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{!
    @header

    Thread Safety Information

    All functions in this header are thread safe unless otherwise specified.
}


{!
	@constant	CTRegisterBundleFonts
	@discussion If this key is defined in the application bundle info dictionary with a boolean value of true, CTFontManager will register all fonts in the Fonts subdirectory of the bundle's Resources directory in the process scope.  
 }

{!
	@function   CTFontManagerCopyAvailablePostScriptNames
	@abstract   Returns an array of unique PostScript font names.
 
	@result     This function returns a retained reference to a CFArray of CFString references, or NULL on error. The caller is responsible for releasing the array.
 }
function CTFontManagerCopyAvailablePostScriptNames: CFArrayRef; external name '_CTFontManagerCopyAvailablePostScriptNames';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerCopyAvailableFontFamilyNames
    @abstract   Returns an array of visible font family names sorted for UI display.

    @result     This function returns a retained reference to a CFArray of CFString references, or NULL on error. The caller is responsible for releasing the array.
}
function CTFontManagerCopyAvailableFontFamilyNames: CFArrayRef; external name '_CTFontManagerCopyAvailableFontFamilyNames';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerCopyAvailableFontURLs
    @abstract   Returns an array of font URLs.

    @result     This function returns a retained reference to a CFArray of CFURL references, or NULL on error. The caller is responsible for releasing the array.
}
function CTFontManagerCopyAvailableFontURLs: CFArrayRef; external name '_CTFontManagerCopyAvailableFontURLs';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerCompareFontFamilyNames
    @abstract   A CFComparatorFunction to compare font family names and sort them according to Apple guidelines.
    @discussion This function compares font family names and sorts them in the Apple preferred order, accounting for foundry prefix. Family names with recognized prefixes are sorted after the un-prefixed names in prefix order.
    @param      family1
                The first localized font family name, as CFStringRef.
    @param      family2
                The second localized font family name, as CFStringRef.
    @param      context
                Unused. Can be NULL.
    @result     A CFComparisonResult value indicating the sort order for the two family names. kCFComparisonResultGreatherThan if family1 is greater than family2, kCFComparisonResultLessThan if family1 is less than family2, and kCFComparisonResultEqualTo if they are equal.
}
function CTFontManagerCompareFontFamilyNames( family1: {const} UnivPtr; family2: {const} UnivPtr; context: UnivPtr ): CFComparisonResult; external name '_CTFontManagerCompareFontFamilyNames';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerCreateFontDescriptorsFromURL
    @abstract   Returns an array of font descriptors representing each of the fonts in the specified URL.
				Note: these font descriptors are not availabe through font descriptor matching.

    @param      fileURL
                A file system URL referencing a valid font file.

    @result     This function returns a retained reference to a CFArray, or NULL on error. The caller is responsible for releasing the array.
}
function CTFontManagerCreateFontDescriptorsFromURL( fileURL: CFURLRef ): CFArrayRef; external name '_CTFontManagerCreateFontDescriptorsFromURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @enum       CTFontManagerScope
    @abstract   Scope for font registration.
    @constant   kCTFontManagerScopeProcess
                The font is available to the current process for the duration of the process unless directly unregistered.
    @constant   kCTFontManagerScopeUser
                The font is available to all processes for the current user session and will be available in subsequent sessions unless unregistered.
    @constant   kCTFontManagerScopeSession
                The font is available to the current user session, and will not be available in subsequent sessions.
}
const
	kCTFontManagerScopeNone = 0;
	kCTFontManagerScopeProcess = 1;
	kCTFontManagerScopeUser = 2;
	kCTFontManagerScopeSession = 3;
type
	CTFontManagerScope = UInt32;

{!
    @function   CTFontManagerRegisterFontsForURL
    @abstract   Registers fonts from the specified font URL with the font manager. Registered fonts are discoverable through font descriptor matching.

    @param      fontURL
                Font URL.

    @param      scope
                Scope constant defining the availability and lifetime of the registration. See scope constants for more details.

    @param      error
                Pointer to receive CFError in the case of failed registration.

    @result     Returns true if registration of the fonts was successful.
}
function CTFontManagerRegisterFontsForURL( fontURL: CFURLRef; scope: CTFontManagerScope; var error: CFErrorRef ): CBool; external name '_CTFontManagerRegisterFontsForURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerUnregisterFontsForURL
    @abstract   Unregisters fonts from the specified font URL with the font manager. Unregistered fonts are no longer discoverable through font descriptor matching.

    @param      fontURL
                Font URL.

    @param      scope
                Scope constant defining the availability and lifetime of the registration. Should match the scope the fonts are registered in. See scope constants for more details.

    @param      error
                Pointer to receive CFError in the case of failed unregistration.

    @result     Returns true if unregistration of the fonts was successful.

}
function CTFontManagerUnregisterFontsForURL( fontURL: CFURLRef; scope: CTFontManagerScope; var error: CFErrorRef ): CBool; external name '_CTFontManagerUnregisterFontsForURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerRegisterFontsForURLs
    @abstract   Registers fonts from the specified font URLs with the font manager. Registered fonts are discoverable through font descriptor matching.

    @param      fontURLs
                Array of font URLs.

    @param      scope
                Scope constant defining the availability and lifetime of the registration. See scope constants for more details.

    @param      errors
                Pointer to CFArrayRef to receive array of CFError references. Each error will contain a CFArray of font URLs corresponding to kCTFontManagerErrorFontURLsKey. These URLs represent the font files that caused the error, and were not successfully registered. Must be released by caller. Can be NULL.

    @result     Returns true if registration of all font URLs was successful. Otherwise false.
}
function CTFontManagerRegisterFontsForURLs( fontURLs: CFArrayRef; scope: CTFontManagerScope; errors: CFArrayRefPtr {can be null} ): CBool; external name '_CTFontManagerRegisterFontsForURLs';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerUnregisterFontsForURLs
    @abstract   Unregisters fonts from the specified font URLs with the font manager. Unregistered fonts are no longer discoverable through font descriptor matching.

    @param      fontURLs
                Array of font URLs.

    @param      scope
                Scope constant defining the availability and lifetime of the registration. Should match the scope the fonts are registered in. See scope constants for more details.

    @param      errors
                Pointer to CFArrayRef to receive array of CFError references. Each error will contain a CFArray of font URLs corresponding to kCTFontManagerErrorFontURLsKey. These URLs represent the font files that caused the error, and were not successfully unregistered. Must be released by caller. Can be NULL.

    @result     Returns true if unregistration of all font URLs was successful. Otherwise false.
}
function CTFontManagerUnregisterFontsForURLs( fontURLs: CFArrayRef; scope: CTFontManagerScope; errors: CFArrayRefPtr {can be null} ): CBool; external name '_CTFontManagerUnregisterFontsForURLs';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerEnableFontDescriptors
    @abstract   Enables or disables the matching font descriptors for font descriptor matching.

    @param      descriptors
                Array of font descriptors.

    @param      enable
                Boolean value indicating whether the fonts matching descriptors should be enabled for font descriptor matching.
}
procedure CTFontManagerEnableFontDescriptors( descriptors: CFArrayRef; enable: CBool ); external name '_CTFontManagerEnableFontDescriptors';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerGetScopeForURL
    @abstract   Returns the registration scope of the specified URL.

    @param      fontURL
                Font URL.

    @result     Returns the registration scope of the specified URL, will return kCTFontManagerScopeNone if not currently registered.
}
function CTFontManagerGetScopeForURL( fontURL: CFURLRef ): CTFontManagerScope; external name '_CTFontManagerGetScopeForURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerIsSupportedFontFile
    @abstract   Determines whether the referenced font data (usually by file URL) is supported on the current platform.

    @param      fontURL
                A URL to font data.

    @result     This function returns true if the URL represents a valid font that can be used on the current platform.
}
function CTFontManagerIsSupportedFont( fontURL: CFURLRef ): CBool; external name '_CTFontManagerIsSupportedFont';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{! --------------------------------------------------------------------------
    @group Manager Auto-Activation
}//--------------------------------------------------------------------------


{!
    @const      kCTFontManagerBundleIdentifier
    @abstract   CTFontManage bundle identifier
    @discussion The CTFontManager bundle identifier to be used with get or set global auto-activation settings.
}
var kCTFontManagerBundleIdentifier: CFStringRef; external name '_kCTFontManagerBundleIdentifier'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @enum
    @abstract   Auto-activation settings.
    @constant   kCTFontManagerAutoActivationDefault
                Default auto-activation setting. When specified, the application will use the global setting.
    @constant   kCTFontManagerAutoActivationDisabled
                Disables auto-activation.
    @constant   kCTFontManagerAutoActivationEnabled
                Enables auto-activation.
    @constant   kCTFontManagerAutoActivationPromptUser
                Requires user input for auto-activation. A dialog will be presented to the user to confirm auto
                activation of the font.
}
const
	kCTFontManagerAutoActivationDefault = 0;
	kCTFontManagerAutoActivationDisabled = 1;
	kCTFontManagerAutoActivationEnabled = 2;
	kCTFontManagerAutoActivationPromptUser = 3;
type
	CTFontManagerAutoActivationSetting = UInt32;

{!
    @function   CTFontManagerSetAutoActivationSetting
    @abstract   Sets the auto-activation for the specified bundle identifier.
    @param      bundleIdentifier
                The bundle identifier. Used to specify a particular application bundle. If NULL,
                the current application bundle will be used. If kCTFontManagerBundleIdentifier is specified,
                will set the global auto-activation settings.
    @param      setting
                The new setting.
    @result     Function will apply the setting to the appropriate preferences location.
}
procedure CTFontManagerSetAutoActivationSetting( bundleIdentifier: CFStringRef; setting: CTFontManagerAutoActivationSetting ); external name '_CTFontManagerSetAutoActivationSetting';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{!
    @function   CTFontManagerGetAutoActivationSetting
    @abstract   Accessor for the auto-activation setting.
    @param      bundleIdentifier
                The bundle identifier. Used to specify a particular application bundle. If NULL,
                the current application bundle will be used. If kCTFontManagerBundleIdentifier is specified,
                will set the global auto-activation settings.
    @result     Will return the auto-activation setting for specified bundle identifier.
}
function CTFontManagerGetAutoActivationSetting( bundleIdentifier: CFStringRef ): CTFontManagerAutoActivationSetting; external name '_CTFontManagerGetAutoActivationSetting';
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{! --------------------------------------------------------------------------
    @group Manager Notifications
}//--------------------------------------------------------------------------

{!
    @constant   kCTFontManagerRegisteredFontsChangedNotification
    @abstract   Notification name for font registry changes.
    @discussion This is the string to use as the notification name when subscribing
                to CTFontManager notifications. This notification will be posted
                when fonts are added to the font registry. The client is responsible for
                registered with the distributed notification center to receive notifications
                for changes to the session or user scopes, and with a local notification
                for changes to the process scope.
}
var kCTFontManagerRegisteredFontsChangedNotification: CFStringRef; external name '_kCTFontManagerRegisteredFontsChangedNotification'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_6_AND_LATER *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
