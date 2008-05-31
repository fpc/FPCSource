{
     File:       LaunchServices.p
 
     Contains:   Public interfaces for LaunchServices.framework
 
     Version:    Technology: Mac OS X
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2001-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit LaunchServices;
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
uses MacTypes,CFBase,CFArray,CFString,Files,CFURL,AEDataModel;

{$ALIGN MAC68K}


{ ======================================================================================================== }
{ LaunchServices Structures and Enums                                                                      }
{ ======================================================================================================== }


const
	kLSAppInTrashErr            = -10660;						{ The app cannot be run when inside a Trash folder }
	kLSUnknownErr				= -10810;
	kLSNotAnApplicationErr		= -10811;
	kLSNotInitializedErr		= -10812;
	kLSDataUnavailableErr		= -10813;						{  e.g. no kind string }
	kLSApplicationNotFoundErr	= -10814;						{  e.g. no application claims the file }
	kLSUnknownTypeErr			= -10815;
	kLSDataTooOldErr			= -10816;
	kLSDataErr					= -10817;
	kLSLaunchInProgressErr		= -10818;						{  e.g. opening an alreay opening application }
	kLSNotRegisteredErr			= -10819;
	kLSAppDoesNotClaimTypeErr	= -10820;
	kLSAppDoesNotSupportSchemeWarning = -10821;					{  not an error, just a warning }
	kLSServerCommunicationErr	= -10822;						{  cannot set recent items }
	kLSCannotSetInfoErr			= -10823;						{  you may not set item info for this item }
	kLSNoRegistrationInfoErr    = -10824;						{ the item contains no registration info }
	kLSIncompatibleSystemVersionErr = -10825;					{ the app cannot run on the current OS version }
	kLSNoLaunchPermissionErr    = -10826;						{ user doesn't have permission to launch the app (managed networks) }
	kLSNoExecutableErr          = -10827;						{ the executable is missing or has an unusable format }
	kLSNoClassicEnvironmentErr  = -10828;						{ the Classic environment was required but is not available }
	kLSMultipleSessionsNotSupportedErr = -10829;				{ the app cannot run simultaneously in two different sessions }


type
	LSInitializeFlags					= OptionBits;

const
	kLSInitializeDefaults		= $00000001;

	kLSMinCatInfoBitmap			= $0000180A;					{ do not use }{  minimum info needed to avoid a FSGetCatalogInfo call when fetching item information  }

	kLSInvalidExtensionIndex	= $FFFFFFFF;					{  Index returned from LSGetExtensionInfo when name has no extension }


type
	LSRequestedInfo						= OptionBits;

const
	kLSRequestExtension			= $00000001;					{  safe to use from threads } { thread-safe in 10.0 }
	kLSRequestTypeCreator		= $00000002;					{  safe to use from threads } { thread-safe in 10.0 }
	kLSRequestBasicFlagsOnly	= $00000004;					{  all but type of application and extension flags - safe to use from threads } { thread-safe in 10.2 }
	kLSRequestAppTypeFlags		= $00000008;					{  NOT SAFE to use from threads } { thread-safe in 10.2 }
	kLSRequestAllFlags			= $00000010;					{  NOT SAFE to use from threads } { thread-safe in 10.2 }
	kLSRequestIconAndKind		= $00000020;					{  NOT SAFE to use from threads } { thread-safe in 10.2 }
	kLSRequestExtensionFlagsOnly = $00000040;					{  safe to use from threads } { thread-safe in 10.0 }
	kLSRequestAllInfo			= $FFFFFFFF;					{  NOT SAFE to use from threads } { thread-safe in 10.2 }


type
	LSItemInfoFlags						= OptionBits;

const
	kLSItemInfoIsPlainFile		= $00000001;					{  none of the following applies }
	kLSItemInfoIsPackage		= $00000002;					{  app, doc, or bundle package }
	kLSItemInfoIsApplication	= $00000004;					{  single-file or packaged }
	kLSItemInfoIsContainer		= $00000008;					{  folder or volume }
	kLSItemInfoIsAliasFile		= $00000010;					{  'real' alias }
	kLSItemInfoIsSymlink		= $00000020;					{  UNIX symbolic link only }
	kLSItemInfoIsInvisible		= $00000040;					{  does not include '.' files or '.hidden' entries }
	kLSItemInfoIsNativeApp		= $00000080;					{  Carbon or Cocoa native app }
	kLSItemInfoIsClassicApp		= $00000100;					{  CFM Classic app }
	kLSItemInfoAppPrefersNative	= $00000200;					{  Carbon app that prefers to be launched natively }
	kLSItemInfoAppPrefersClassic = $00000400;					{  Carbon app that prefers to be launched in Classic }
	kLSItemInfoAppIsScriptable	= $00000800;					{  App can be scripted }
	kLSItemInfoIsVolume			= $00001000;					{  item is a volume }
	kLSItemInfoExtensionIsHidden = $00100000;					{  item has a hidden extension }


type
	LSRolesMask							= OptionBits;

const
	kLSRolesNone				= $00000001;					{  no claim is made about support for this type/scheme }
	kLSRolesViewer				= $00000002;					{  claim to be able to view this type/scheme }
	kLSRolesEditor				= $00000004;					{  claim to be able to edit this type/scheme }
	kLSRolesAll					= $FFFFFFFF;					{  claim to do it all }


type
	LSKindID							= UInt32;

const
	kLSUnknownKindID			= 0;

	kLSUnknownType				= 0;
	kLSUnknownCreator			= 0;


type
	LSItemInfoRecordPtr = ^LSItemInfoRecord;
	LSItemInfoRecord = record
		flags:					LSItemInfoFlags;
		filetype:				OSType;
		creator:				OSType;									
		extension:				CFStringRef;							{  release when finished }
		iconFileName:			CFStringRef;							{  not for general use }
		kindID:					LSKindID;								{  not for general use }
	end;

	LSAcceptanceFlags					= OptionBits;

const
	kLSAcceptDefault			= $00000001;
	kLSAcceptAllowLoginUI		= $00000002;					{  show UI to log in if necessary }


type
	LSLaunchFlags						= OptionBits;

const
	kLSLaunchDefaults			= $00000001;					{  default = open, async, use Info.plist, start Classic }
	kLSLaunchAndPrint			= $00000002;					{  print items instead of open them }
	kLSLaunchReserved2			= $00000004;
	kLSLaunchReserved3			= $00000008;
	kLSLaunchReserved4			= $00000010;
	kLSLaunchReserved5			= $00000020;
	kLSLaunchReserved6			= $00000040;
	kLSLaunchInhibitBGOnly		= $00000080;					{  causes launch to fail if target is background-only. }
	kLSLaunchDontAddToRecents	= $00000100;					{  do not add app or documents to recents menus. }
	kLSLaunchDontSwitch			= $00000200;					{  don't bring new app to the foreground. }
	kLSLaunchNoParams			= $00000800;					{  Use Info.plist to determine launch parameters }
	kLSLaunchAsync				= $00010000;					{  launch async; obtain results from kCPSNotifyLaunch. }
	kLSLaunchStartClassic		= $00020000;					{  start up Classic environment if required for app. }
	kLSLaunchInClassic			= $00040000;					{  force app to launch in Classic environment. }
	kLSLaunchNewInstance		= $00080000;					{  Instantiate app even if it is already running. }
	kLSLaunchAndHide			= $00100000;					{  Send child a "hide" request as soon as it checks in. }
	kLSLaunchAndHideOthers		= $00200000;					{  Hide all other apps when child checks in. }


type
	LSLaunchFSRefSpecPtr = ^LSLaunchFSRefSpec;
	LSLaunchFSRefSpec = record
		appRef:					FSRefPtr;								{  app to use, can be NULL }
		numDocs:				UInt32;									{  items to open/print, can be NULL }
		itemRefs:				FSRefPtr;								{  array of FSRefs }
		passThruParams:			AEDescPtr;								{  passed untouched to application as optional parameter }
		launchFlags:			LSLaunchFlags;
		asyncRefCon:			Ptr;									{  used if you register for app birth/death notification }
	end;

	LSLaunchURLSpecPtr = ^LSLaunchURLSpec;
	LSLaunchURLSpec = record
		appURL:					CFURLRef;								{  app to use, can be NULL }
		itemURLs:				CFArrayRef;								{  items to open/print, can be NULL }
		passThruParams:			AEDescPtr;								{  passed untouched to application as optional parameter }
		launchFlags:			LSLaunchFlags;
		asyncRefCon:			Ptr;									{  used if you register for app birth/death notification }
	end;


	{	 ======================================================================================================== 	}
	{	 LaunchServices Public Entry Points                                                                       	}
	{	 ======================================================================================================== 	}

	{
	 *  LSInit()   *** DEPRECATED in 10.3 ***
	 *  
     *  Discussion:
     *    LSInit is deprecated. Do not use.
     *  
	 *  Pre 10.3 Summary:
	 *    Initialize LaunchServices for use.
	 *  
	 *  Pre 10.3 Discussion:
	 *    LSInit is optional but should be called by top level applications
	 *    to explicitly incur any startup costs at a known time. Frameworks
	 *    and libraries need never call LSInit.
	 *  
	 *  Pre 10.3 Parameters:
	 *    
	 *    inFlags:
	 *      Use kLSInitializeDefaults.
	 *  
	 *  Availability:
     *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.3
     *    CarbonLib:        not available in CarbonLib 1.x
     *    Non-Carbon CFM:   not available
	 	}

// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3
function LSInit(inFlags: LSInitializeFlags): OSStatus; external name '_LSInit';


{
 *  LSTerm()   *** DEPRECATED in 10.3 ***
 *  
 *  Discussion:
 *    LSTerm is deprecated. It does nothing.
 *  
 *  Pre 10.3 Summary:
 *    Terminate LaunchServices use.
 *  
 *  Pre 10.3 Discussion:
 *    LSTerm is optional but should be called by top level applications
 *    to explicitly terminate LaunchServices activity at a known time.
 *    Frameworks and libraries need never call LSTerm.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.3
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_3
function LSTerm: OSStatus; external name '_LSTerm';


{
 *  LSCopyItemInfoForRef()
 *  
 *  Summary:
 *    Return information about an item.
 *  
 *  Discussion:
 *    Returns as much or as little information as requested about
 *    inItemRef. Some information is available in a thread-safe manner,
 *    some is not. All CFStrings must be released after use.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inItemRef:
 *      The FSRef of the item about which information is requested.
 *    
 *    inWhichInfo:
 *      Flags indicating which information to return
 *    
 *    outItemInfo:
 *      Information is returned in this structure. Must not be NULL
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSCopyItemInfoForRef(const (*var*) inItemRef: FSRef; inWhichInfo: LSRequestedInfo; var outItemInfo: LSItemInfoRecord): OSStatus; external name '_LSCopyItemInfoForRef';


{
 *  LSCopyItemInfoForURL()
 *  
 *  Summary:
 *    Return information about an item.
 *  
 *  Discussion:
 *    Returns as much or as little information as requested about
 *    inURL. Some information is available in a thread-safe manner,
 *    some is not. All CFStrings must be released after use.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The CFURLRef of the item about which information is requested.
 *    
 *    inWhichInfo:
 *      Flags indicating which information to return
 *    
 *    outItemInfo:
 *      Information is returned in this structure. Must not be NULL
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSCopyItemInfoForURL(inURL: CFURLRef; inWhichInfo: LSRequestedInfo; var outItemInfo: LSItemInfoRecord): OSStatus; external name '_LSCopyItemInfoForURL';


{
 *  LSGetExtensionInfo()
 *  
 *  Summary:
 *    Get information about the extension for a file system name.
 *  
 *  Discussion:
 *    Returns the starting index of the extension (not including the
 *    period) or kLSInvalidExtensionIndex if the input name has no
 *    extension.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inNameLen:
 *      The number of the UniChars in inNameBuffer.
 *    
 *    inNameBuffer:
 *      The buffer containing the name's Unicode characters.
 *    
 *    outExtStartIndex:
 *      On success, the starting index of the extension if there is one
 *      (not including the period). Set to kLSInvalidExtensionIndex if
 *      inNameBuffer does not contain a valid extension.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 }
function LSGetExtensionInfo(inNameLen: UniCharCount; inNameBuffer: ConstUniCharPtr; var outExtStartIndex: UniCharCount): OSStatus; external name '_LSGetExtensionInfo';


{
 *  LSCopyDisplayNameForRef()
 *  
 *  Summary:
 *    Get the display name for an FSRef.
 *  
 *  Discussion:
 *    Return a copy of the display name for an FSRef. Takes into
 *    consideration whether this item has a hidden extension or not.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inRef:
 *      The FSRef for which the display name is desired.
 *    
 *    outDisplayName:
 *      Pointer to the CFString into which the display name should be
 *      copied. Callers must dispose of the resulting CFString.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 }
function LSCopyDisplayNameForRef(const (*var*) inRef: FSRef; var outDisplayName: CFStringRef): OSStatus; external name '_LSCopyDisplayNameForRef';


{
 *  LSCopyDisplayNameForURL()
 *  
 *  Summary:
 *    Get the display name for a CFURLRef.
 *  
 *  Discussion:
 *    Return a copy of the display name for a CFURLRef. Takes into
 *    consideration whether this item has a hidden extension or not.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The URL for which the display name is desired.
 *    
 *    outDisplayName:
 *      Pointer to the CFString into which the display name should be
 *      copied. Callers must dispose of the resulting CFString.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 }
function LSCopyDisplayNameForURL(inURL: CFURLRef; var outDisplayName: CFStringRef): OSStatus; external name '_LSCopyDisplayNameForURL';


{
 *  LSSetExtensionHiddenForRef()
 *  
 *  Summary:
 *    Sets whether the extension for an FSRef is hidden or not.
 *  
 *  Discussion:
 *    Sets the necessary file system state to indicate that the
 *    extension for inRef is hidden, as in the Finder. You can
 *    determine if an FSRef's extension is hidden using
 *    LSCopyItemInfoForRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inRef:
 *      The FSRef for which the extension is to be hidden or shown.
 *    
 *    inHide:
 *      True to hide inRef's extension, false to show it.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 }
function LSSetExtensionHiddenForRef(const (*var*) inRef: FSRef; inHide: boolean): OSStatus; external name '_LSSetExtensionHiddenForRef';


{
 *  LSSetExtensionHiddenForURL()
 *  
 *  Summary:
 *    Sets whether the extension for a CFURLRef is hidden or not.
 *  
 *  Discussion:
 *    Sets the necessary file system state to indicate that the
 *    extension for inURL is hidden, as in the Finder. You can
 *    determine if a CFURLRef's extension is hidden using
 *    LSCopyItemInfoForURL.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The CFURLRef for which the extension is to be hidden or shown.
 *    
 *    inHide:
 *      True to hide inURL's extension, false to show it.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.1 and later in ApplicationServices.framework
 }
function LSSetExtensionHiddenForURL(inURL: CFURLRef; inHide: boolean): OSStatus; external name '_LSSetExtensionHiddenForURL';


{
 *  LSCopyKindStringForRef()
 *  
 *  Summary:
 *    Get the kind string for an item.
 *  
 *  Discussion:
 *    Returns the kind string as used in the Finder and elsewhere for
 *    inFSRef. The CFStringRef must be released after use.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inFSRef:
 *      The item for which the kind string is requested.
 *    
 *    outKindString:
 *      A non-NULL CFStringRef* into which the kind string will be
 *      copied. This CFStringRef must be released after use.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSCopyKindStringForRef(const (*var*) inFSRef: FSRef; var outKindString: CFStringRef): OSStatus; external name '_LSCopyKindStringForRef';


{
 *  LSCopyKindStringForURL()
 *  
 *  Summary:
 *    Get the kind string for an item.
 *  
 *  Discussion:
 *    Returns the kind string as used in the Finder and elsewhere for
 *    inURL. The CFStringRef must be released after use.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The item for which the kind string is requested.
 *    
 *    outKindString:
 *      A non-NULL CFStringRef* into which the kind string will be
 *      copied. This CFStringRef must be released after use.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSCopyKindStringForURL(inURL: CFURLRef; var outKindString: CFStringRef): OSStatus; external name '_LSCopyKindStringForURL';

{
 *  LSCopyKindStringForTypeInfo()
 *  
 *  Summary:
 *    Return the kind string for items like the provided info
 *  
 *  Discussion:
 *    Returns the kind string as shown in the Finder for the those
 *    items whose type, creator, and/or extension match the provided
 *    information. The kind string returned will be the one that most
 *    closely describes all the information provided. The kind string
 *    is subject to the document binding preferences that have been
 *    specified by the user. For example, if a creator is specified but
 *    the user has asked for files with the given
 *    creator/type/extension combination to open in an application with
 *    a different creator, the kind string will be loaded from the
 *    user's preferred application.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inType:
 *      The OSType file type for which you want a kind string. Specify
 *      kLSUnknownType if no file type information is available.
 *    
 *    inCreator:
 *      The OSType creator for which you want a kind string. Specify
 *      kLSUnknownCreator if no creator information is available.
 *    
 *    inExtension:
 *      The extension for which you want a kind string. Specify NULL if
 *      no extension information is available.
 *    
 *    outKindString:
 *      A CFStringRef* to receive the copied kind string object. This
 *      CFStringRef must be released eventually.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function LSCopyKindStringForTypeInfo(
  inType: OSType;
  inCreator: OSType;
  inExtension: CFStringRef;         { can be NULL }
  VAR outKindString: CFStringRef): OSStatus; external name '_LSCopyKindStringForTypeInfo';


{
 *  LSCopyKindStringForMIMEType()
 *  
 *  Summary:
 *    Get the kind string for the specified MIME type.
 *  
 *  Discussion:
 *    Returns the localized kind string describing the specified MIME
 *    type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inMIMEType:
 *      The string specifying the MIME type.
 *    
 *    outKindString:
 *      A CFStringRef* to receive the copied kind string object. This
 *      CFStringRef must be released eventually.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function LSCopyKindStringForMIMEType(
  inMIMEType: CFStringRef;
  VAR outKindString: CFStringRef): OSStatus; external name '_LSCopyKindStringForMIMEType';

{
 *  LSGetApplicationForItem()
 *  
 *  Summary:
 *    Return the application used to open an item.
 *  
 *  Discussion:
 *    Consults the binding tables to return the application that would
 *    be used to open inItemRef if it were double-clicked in the
 *    Finder. This application will be the user-specified override if
 *    appropriate or the default otherwise. If no application is known
 *    to LaunchServices suitable for opening this item,
 *    kLSApplicationNotFoundErr will be returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inItemRef:
 *      The FSRef of the item for which the application is requested.
 *    
 *    inRoleMask:
 *      Whether to return the editor or viewer for inItemRef. If you
 *      don't care which, use kLSRolesAll.
 *    
 *    outAppRef:
 *      Filled in with the FSRef of the application if not NULL.
 *    
 *    outAppURL:
 *      Filled in with the CFURLRef of the application if not NULL.
 *      THIS FUNCTION, DESPITE ITS NAME, RETAINS THE URL REFERENCE ON
 *      BEHALF OF THE CALLER. THE CALLER MUST EVENTUALLY RELEASE THE
 *      RETURNED URL REFERENCE.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSGetApplicationForItem(const (*var*) inItemRef: FSRef; inRoleMask: LSRolesMask; outAppRef: FSRefPtr; outAppURL: CFURLRefPtr): OSStatus; external name '_LSGetApplicationForItem';


{
 *  LSGetApplicationForInfo()
 *  
 *  Summary:
 *    Return the application used to open items with particular data.
 *  
 *  Discussion:
 *    Consults the binding tables to return the application that would
 *    be used to open items with type, creator, and/or extension as
 *    provided if they were double-clicked in the Finder. This
 *    application will be the default for items like this if one has
 *    been set. If no application is known to LaunchServices suitable
 *    for opening such items, kLSApplicationNotFoundErr will be
 *    returned. Not all three input parameters can be NULL at the same
 *    time nor can both output parameters be NULL at the same time.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inType:
 *      The file type to consider. Can be kLSUnknownType.
 *    
 *    inCreator:
 *      The file creator to consider. Can be kLSUnknownCreator.
 *    
 *    inExtension:
 *      The file name extension to consider. Can be NULL.
 *    
 *    inRoleMask:
 *      Whether to return the editor or viewer for inItemRef. If you
 *      don't care which, use kLSRolesAll.
 *    
 *    outAppRef:
 *      Filled in with the FSRef of the application if not NULL.
 *    
 *    outAppURL:
 *      Filled in with the CFURLRef of the application if not NULL.
 *      THIS FUNCTION, DESPITE ITS NAME, RETAINS THE URL REFERENCE ON
 *      BEHALF OF THE CALLER. THE CALLER MUST EVENTUALLY RELEASE THE
 *      RETURNED URL REFERENCE.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSGetApplicationForInfo(inType: OSType; inCreator: OSType; inExtension: CFStringRef; inRoleMask: LSRolesMask; outAppRef: FSRefPtr; outAppURL: CFURLRefPtr): OSStatus; external name '_LSGetApplicationForInfo';

{
 *  LSCopyApplicationForMIMEType()
 *  
 *  Summary:
 *    Return the application used to handle data with the specified
 *    MIME type.
 *  
 *  Discussion:
 *    The returned application URL will be the user's preferred handler
 *    for the MIME type if one has been set. If no user preferred
 *    application has been set, Launch Services will select a default
 *    handler for the MIME type. If no application is known to handle
 *    the MIME type, kLSApplicationNotFoundErr will be returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inMIMEType:
 *      The string specifying the MIME type.
 *    
 *    inRoleMask:
 *      A role mask that the chosen application must satisfy. Use
 *      kLSRolesAll if the role is not important.
 *    
 *    outAppURL:
 *      Receives the copied CFURLRef, which must be released by the
 *      caller.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER
function LSCopyApplicationForMIMEType(
  inMIMEType: CFStringRef;
  inRoleMask: LSRolesMask;
  VAR outAppURL: CFURLRef): OSStatus; external name '_LSCopyApplicationForMIMEType';


{
 *  LSGetApplicationForURL()
 *  
 *  Summary:
 *    Return the application used to open an item.
 *  
 *  Discussion:
 *    Consults the binding tables to return the application that would
 *    be used to open inURL if it were double-clicked in the Finder.
 *    This application will be the user-specified override if
 *    appropriate or the default otherwise. If no application is known
 *    to LaunchServices suitable for opening this item,
 *    kLSApplicationNotFoundErr will be returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The CFURLRef of the item for which the application is requested.
 *    
 *    inRoleMask:
 *      Whether to return the editor or viewer for inItemRef. If you
 *      don't care which, use kLSRolesAll.
 *    
 *    outAppRef:
 *      Filled in with the FSRef of the application if not NULL.
 *    
 *    outAppURL:
 *      Filled in with the CFURLRef of the application if not NULL.
 *      THIS FUNCTION, DESPITE ITS NAME, RETAINS THE URL REFERENCE ON
 *      BEHALF OF THE CALLER. THE CALLER MUST EVENTUALLY RELEASE THE
 *      RETURNED URL REFERENCE.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSGetApplicationForURL(inURL: CFURLRef; inRoleMask: LSRolesMask; var outAppRef: FSRef; var outAppURL: CFURLRef): OSStatus; external name '_LSGetApplicationForURL';


{
 *  LSFindApplicationForInfo()
 *  
 *  Summary:
 *    Locate a specific application.
 *  
 *  Discussion:
 *    Returns the application with the corresponding input information.
 *    The registry of applications is consulted first in order of
 *    bundleID, then creator, then name. All comparisons are case
 *    insensitive and 'ties' are decided first by version, then by
 *    native vs. Classic.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inCreator:
 *      The file creator to consider. Can be kLSUnknownCreator.
 *    
 *    inBundleID:
 *      The bundle ID to consider. Can be NULL.
 *    
 *    inName:
 *      The name to consider. Can be NULL. Must include any extensions
 *      that are part of the file system name, e.g. '.app'.
 *    
 *    outAppRef:
 *      Filled in with the FSRef of the application if not NULL.
 *    
 *    outAppURL:
 *      Filled in with the CFURLRef of the application if not NULL.
 *      THIS FUNCTION, DESPITE ITS NAME, RETAINS THE URL REFERENCE ON
 *      BEHALF OF THE CALLER. THE CALLER MUST EVENTUALLY RELEASE THE
 *      RETURNED URL REFERENCE.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSFindApplicationForInfo(inCreator: OSType; inBundleID: CFStringRef; inName: CFStringRef; outAppRef: FSRefPtr; outAppURL: CFURLRefPtr): OSStatus; external name '_LSFindApplicationForInfo';


{
 *  LSCanRefAcceptItem()
 *  
 *  Summary:
 *    Determine whether an item can accept another item.
 *  
 *  Discussion:
 *    Returns in outAcceptsItem whether inTargetRef can accept
 *    inItemFSRef as in a drag and drop operation. If inRoleMask is
 *    other than kLSRolesAll then make sure inTargetRef claims to
 *    fulfill the requested role.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inItemFSRef:
 *      FSRef of the item about which acceptance is requested.
 *    
 *    inTargetRef:
 *      FSRef of the potential target.
 *    
 *    inRoleMask:
 *      The role(s) the target must claim in order to consider
 *      acceptance.
 *    
 *    inFlags:
 *      Use kLSAcceptDefault.
 *    
 *    outAcceptsItem:
 *      Filled in with result. Must not be NULL.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSCanRefAcceptItem(const (*var*) inItemFSRef: FSRef; const (*var*) inTargetRef: FSRef; inRoleMask: LSRolesMask; inFlags: LSAcceptanceFlags; var outAcceptsItem: boolean): OSStatus; external name '_LSCanRefAcceptItem';


{
 *  LSCanURLAcceptURL()
 *  
 *  Summary:
 *    Determine whether an item can accept another item.
 *  
 *  Discussion:
 *    Returns in outAcceptsItem whether inTargetURL can accept
 *    inItemURL as in a drag and drop operation. If inRoleMask is other
 *    than kLSRolesAll then make sure inTargetRef claims to fulfill the
 *    requested role.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inItemURL:
 *      CFURLRef of the item about which acceptance is requested.
 *    
 *    inTargetURL:
 *      CFURLRef of the potential target.
 *    
 *    inRoleMask:
 *      The role(s) the target must claim in order to consider
 *      acceptance.
 *    
 *    inFlags:
 *      Use kLSAcceptDefault.
 *    
 *    outAcceptsItem:
 *      Filled in with result. Must not be NULL.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSCanURLAcceptURL(inItemURL: CFURLRef; inTargetURL: CFURLRef; inRoleMask: LSRolesMask; inFlags: LSAcceptanceFlags; var outAcceptsItem: boolean): OSStatus; external name '_LSCanURLAcceptURL';


{
 *  LSRegisterURL()
 *  
 *  Discussion:
 *    If the specified URL refers to an application or other bundle
 *    claiming to handle documents or URLs, add the bundle's document
 *    and URL claims to the Launch Services database.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The CFURLRef of the item (a directory or file) to be registered.
 *    
 *    inUpdate:
 *      When false, LSRegisterURL does not register the item if it has
 *      already been registered and the current modification date of
 *      the item has not changed from when it was last registered. When
 *      true, the item's registered info is updated, even if the
 *      modification has not changed.
 *  
 *  Result:
 *    An OSStatus value: noErr - Success kLSNoRegistrationInfoErr - The
 *    item does not contain info requiring registration kLSDataErr -
 *    The item's property list info is malformed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER 
function LSRegisterURL(
  inURL: CFURLRef;
  inUpdate: Boolean): OSStatus; external name '_LSRegisterURL';


{
 *  LSRegisterFSRef()
 *  
 *  Discussion:
 *    If the specified FSRef refers to an application or other bundle
 *    claiming to handle documents or URLs, add the bundle's document
 *    and URL claims to the Launch Services database.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inRef:
 *      The FSRef of the item to be registered.
 *    
 *    inUpdate:
 *      When false, LSRegisterFSRef does not register the item if it
 *      has already been registered and the current modification date
 *      of the item has not changed from when it was last registered.
 *      When true, the item's registered info is updated, even if the
 *      modification has not changed.
 *  
 *  Result:
 *    An OSStatus value: noErr - Success kLSNoRegistrationInfoErr - The
 *    item does not contain info requiring registration kLSDataErr -
 *    The item's property list info is malformed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER 
function LSRegisterFSRef(
   const (*var*) inRef: FSRef;
  inUpdate: Boolean): OSStatus; external name '_LSRegisterFSRef';


{
 *  LSCopyApplicationURLsForURL()
 *  
 *  Discussion:
 *    Returns an array of URLs to applications that offer the requested
 *    role(s) for the input item.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The CFURLRef of the item for which all suitable applications
 *      are desired. If the URL is a file URL, it is treated as a
 *      document, and applications are selected based on the document's
 *      type information. Otherwise, applications are selected based on
 *      the URL's scheme.
 *    
 *    inRoleMask:
 *      The role(s) which must intersect with the role provided by an
 *      application for the specified item in order for the application
 *      to be included in the result. Pass kLSRolesAll if any role is
 *      acceptable.
 *  
 *  Result:
 *    An array of CFURLRefs, one for each application which can open
 *    inURL with at least one of the roles in inRoleMask, or NULL if no
 *    applications can open the item. When an array is returned, you
 *    must eventually release it.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function LSCopyApplicationURLsForURL(
  inURL: CFURLRef;
  inRoleMask: LSRolesMask): CFArrayRef; external name '_LSCopyApplicationURLsForURL';


{
 *  LSOpenFSRef()
 *  
 *  Summary:
 *    Open an application, document, or folder.
 *  
 *  Discussion:
 *    Opens applications, documents, and folders. Applications are
 *    opened via an 'oapp' or 'rapp' event. Documents are opened in
 *    their user-overridden or default applications as appropriate.
 *    Folders are opened in the Finder. Use the more specific
 *    LSOpenFromRefSpec for more control over launching.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inRef:
 *      The FSRef of the item to launch.
 *    
 *    outLaunchedRef:
 *      The FSRef of the item actually launched. For inRefs that are
 *      documents, outLaunchedRef will be the application used to
 *      launch the document. Can be NULL.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSOpenFSRef(const (*var*) inRef: FSRef; outLaunchedRef: FSRefPtr): OSStatus; external name '_LSOpenFSRef';


{
 *  LSOpenCFURLRef()
 *  
 *  Summary:
 *    Open an application, document, or folder.
 *  
 *  Discussion:
 *    Opens applications, documents, and folders. Applications are
 *    opened via an 'oapp' or 'rapp' event. Documents are opened in
 *    their user-overridden or default applications as appropriate.
 *    Folders are opened in the Finder. Use the more specific
 *    LSOpenFromURLSpec for more control over launching.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inURL:
 *      The CFURLRef of the item to launch.
 *    
 *    outLaunchedURL:
 *      The CFURLRef of the item actually launched. For inURLs that are
 *      documents, outLaunchedURL will be the application used to
 *      launch the document. Can be NULL. THIS FUNCTION, DESPITE ITS
 *      NAME, RETAINS THE URL REFERENCE ON BEHALF OF THE CALLER. THE
 *      CALLER MUST EVENTUALLY RELEASE THE RETURNED URL REFERENCE.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSOpenCFURLRef(inURL: CFURLRef; outLaunchedURL: CFURLRefPtr): OSStatus; external name '_LSOpenCFURLRef';


{
 *  LSOpenFromRefSpec()
 *  
 *  Summary:
 *    Opens an application or one or more documents or folders.
 *  
 *  Discussion:
 *    Opens applications, documents, and folders.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inLaunchSpec:
 *      The specification of what to launch and how to launch it.
 *    
 *    outLaunchedRef:
 *      The FSRef of the item actually launched. For inRefs that are
 *      documents, outLaunchedRef will be the application used to
 *      launch the document. Can be NULL.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 }
function LSOpenFromRefSpec(const (*var*) inLaunchSpec: LSLaunchFSRefSpec; outLaunchedRef: FSRefPtr): OSStatus; external name '_LSOpenFromRefSpec';


{
 *  LSOpenFromURLSpec()
 *  
 *  Summary:
 *    Opens an application or one or more documents or folders.
 *  
 *  Discussion:
 *    Opens applications, documents, and folders.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    inLaunchSpec:
 *      The specification of what to launch and how to launch it.
 *    
 *    outLaunchedURL:
 *      The CFURLRef of the item actually launched. For inURLs that are
 *      documents, outLaunchedURL will be the application used to
 *      launch the document. Can be NULL. THIS FUNCTION, DESPITE ITS
 *      NAME, RETAINS THE URL REFERENCE ON BEHALF OF THE CALLER. THE
 *      CALLER MUST EVENTUALLY RELEASE THE RETURNED URL REFERENCE.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Mac OS X:         in version 10.0 and later
 }
function LSOpenFromURLSpec(const (*var*) inLaunchSpec: LSLaunchURLSpec; outLaunchedURL: CFURLRefPtr): OSStatus; external name '_LSOpenFromURLSpec';


{$ALIGN MAC68K}


end.
