{
     File:       HIToolbox/Keyboards.h
 
     Contains:   Keyboard API.
 
     Version:    HIToolbox-624~3
 
     Copyright:  © 1997-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit Keyboards;
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
uses MacTypes, CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Keyboard API constants                                                           }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

{
 *  PhysicalKeyboardLayoutType
 *  
 *  Summary:
 *    Physical keyboard layout types indicate the physical keyboard
 *    layout. They are returned by the KBGetLayoutType API.
 }
type
	PhysicalKeyboardLayoutType = OSType;
const
{
   * A JIS keyboard layout type.
   }
	kKeyboardJIS = FourCharCode('JIS ');

  {
   * An ANSI keyboard layout type.
   }
	kKeyboardANSI = FourCharCode('ANSI');

  {
   * An ISO keyboard layout type.
   }
	kKeyboardISO = FourCharCode('ISO ');

  {
   * An unknown physical keyboard layout type.
   }
	kKeyboardUnknown = kUnknownType; { '????'}

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Keyboard API types                                                               }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}

{
 *  KeyboardLayoutRef
 *  
 *  Summary:
 *    The opaque keyboard layout contains information about a keyboard
 *    layout. It is used with the keyboard layout APIs.
 *  
 *  Discussion:
 *    KeyboardLayoutRef APIs follow CoreFoundation function naming
 *    convention. You mustn't release any references you get from APIs
 *    named "Get."
 }
type
	KeyboardLayoutRef = ^OpaqueKeyboardLayoutRef; { an opaque type }
	OpaqueKeyboardLayoutRef = record end;

{
 *  KeyboardLayoutPropertyTag
 *  
 *  Summary:
 *    Keyboard layout property tags specify the value you want to
 *    retrieve. They are used with the KLGetKeyboardLayoutProperty API.
 }
type
	KeyboardLayoutPropertyTag = UInt32;
const
{
   * The keyboard layout data (const void *).  It is used with the
   * KeyTranslate API.
   }
	kKLKCHRData = 0;

  {
   * The keyboard layout data (const void *).  It is used with the
   * UCKeyTranslate API.
   }
	kKLuchrData = 1;

  {
   * The keyboard layout identifier (KeyboardLayoutIdentifier).
   }
	kKLIdentifier = 2;

  {
   * The keyboard layout icon (IconRef).
   }
	kKLIcon = 3;

  {
   * The localized keyboard layout name (CFStringRef).
   }
	kKLLocalizedName = 4;

  {
   * The keyboard layout name (CFStringRef).
   }
	kKLName = 5;

  {
   * The keyboard layout group identifier (SInt32).
   }
	kKLGroupIdentifier = 6;

  {
   * The keyboard layout kind (KeyboardLayoutKind).
   }
	kKLKind = 7;

  {
   * The language/locale string associated with the keyboard, if any
   * (CFStringRef). This string uses ISO 639 and ISO 3166 codes
   * (examples: "fr", "en_US". Note: The CFStringRef may be NULL for
   * some keyboards.
   }
	kKLLanguageCode = 9;


{
 *  KeyboardLayoutKind
 *  
 *  Summary:
 *    Keyboard layout kinds indicate available keyboard layout formats.
 }
type
	KeyboardLayoutKind = SInt32;
const
{
   * Both KCHR and uchr formats are available.
   }
	kKLKCHRuchrKind = 0;

  {
   * Only KCHR format is avaiable.
   }
	kKLKCHRKind = 1;

  {
   * Only uchr format is available.
   }
	kKLuchrKind = 2;


{
 *  KeyboardLayoutIdentifier
 *  
 *  Summary:
 *    Keyboard layout identifiers specify particular keyboard layouts.
 }
type
	KeyboardLayoutIdentifier = SInt32;
const
	kKLUSKeyboard = 0;

{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ Keyboard API routines                                                            }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{
 *  KBGetLayoutType()
 *  
 *  Summary:
 *    Returns the physical keyboard layout type.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    iKeyboardType:
 *      The keyboard type ID.  LMGetKbdType().
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   in KeyboardsLib 1.0 and later
 }
function KBGetLayoutType( iKeyboardType: SInt16 ): PhysicalKeyboardLayoutType; external name '_KBGetLayoutType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ iterate keyboard layouts}

{$ifc not TARGET_CPU_64}
{
 *  KLGetKeyboardLayoutCount()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use TISCreateInputSourceList API to create a list of input
 *    sources that match specified properties.
 *  
 *  Summary:
 *    Returns the number of keyboard layouts.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    oCount:
 *      On exit, the number of keyboard layouts
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLGetKeyboardLayoutCount( var oCount: CFIndex ): OSStatus; external name '_KLGetKeyboardLayoutCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  KLGetKeyboardLayoutAtIndex()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use TISCreateInputSourceList API to create a list of input
 *    sources that match specified properties.
 *  
 *  Summary:
 *    Retrieves the keyboard layout at the given index.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    iIndex:
 *      The index of the keyboard layout to retrieve. If the index is
 *      outside the index space of the keyboard layouts (0 to N-1
 *      inclusive, where N is the count of the keyboard layouts), the
 *      behavior is undefined.
 *    
 *    oKeyboardLayout:
 *      On exit, the keyboard layout with the given index.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLGetKeyboardLayoutAtIndex( iIndex: CFIndex; var oKeyboardLayout: KeyboardLayoutRef ): OSStatus; external name '_KLGetKeyboardLayoutAtIndex';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
   *** deprecated. ***
   NOTE: "Indexed" is a wrong name, please use "AtIndex"...
   OSStatus KLGetIndexedKeyboardLayout(
                CFIndex                     iIndex,
                KeyboardLayoutRef           *oKeyboardLayout    );
}

{ get keyboard layout info}

{
 *  KLGetKeyboardLayoutProperty()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use TISGetInputSourceProperty API to query properties of a
 *    TISInputSourceRef.
 *  
 *  Summary:
 *    Retrives property value for the given keyboard layout and tag.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    iKeyboardLayout:
 *      The keyboard layout to be queried. If this parameter is not a
 *      valid KeyboardLayoutRef, the behavior is undefined.
 *    
 *    iPropertyTag:
 *      The property tag.
 *    
 *    oValue:
 *      On exit, the property value for the given keyboard layout and
 *      tag.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLGetKeyboardLayoutProperty( iKeyboardLayout: KeyboardLayoutRef; iPropertyTag: KeyboardLayoutPropertyTag; var oValue: UnivPtr ): OSStatus; external name '_KLGetKeyboardLayoutProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ get keyboard layout with identifier or name}

{
 *  KLGetKeyboardLayoutWithIdentifier()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use TISCreateInputSourceList API to create a list of input
 *    sources that match specified properties, such as the
 *    kTISPropertyInputSourceID property.
 *  
 *  Summary:
 *    Retrieves the keyboard layout with the given identifier.
 *  
 *  Discussion:
 *    For now, the identifier is in the range of SInt16 which is
 *    compatible with the Resource Manager resource ID. However, it
 *    will become an arbitrary SInt32 value at some point, so do not
 *    assume it is in SInt16 range or falls into the "script range" of
 *    the resource IDs.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    iIdentifier:
 *      The keyboard layout identifier.
 *    
 *    oKeyboardLayout:
 *      On exit, the keyboard layout with the given identifier.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLGetKeyboardLayoutWithIdentifier( iIdentifier: KeyboardLayoutIdentifier; var oKeyboardLayout: KeyboardLayoutRef ): OSStatus; external name '_KLGetKeyboardLayoutWithIdentifier';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  KLGetKeyboardLayoutWithName()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use TISCreateInputSourceList API to create a list of input
 *    sources that match specified properties, such as the
 *    kTISPropertyInputSourceID property.
 *  
 *  Summary:
 *    Retrieves the keyboard layout with the given name.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    iName:
 *      The keyboard layout name.
 *    
 *    oKeyboardLayout:
 *      On exit, the keyboard layout with the given name.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLGetKeyboardLayoutWithName( iName: CFStringRef; var oKeyboardLayout: KeyboardLayoutRef ): OSStatus; external name '_KLGetKeyboardLayoutWithName';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ get/set current keyboard layout of the current group identifier}

{
 *  KLGetCurrentKeyboardLayout()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    Use TISCopyCurrentKeyboardLayoutInputSource.
 *     TISCopyCurrentKeyboardLayoutInputSource will return the current
 *    input source if it is a keyboard layout.  If the current input
 *    source is an input method or one of its input modes, the TIS API
 *    will also return any keyboard override specified via
 *    TISSetInputMethodKeyboardLayoutOverride.
 *  
 *  Summary:
 *    Retrieves the current keyboard layout.
 *  
 *  Discussion:
 *    Retrieves the current keyboard layout for the current keyboard
 *    script.  To retrive the current keyboard script for Roman
 *    keyboard script, you need to call KeyScript( smRoman |
 *    smKeyForceKeyScriptMask ) then call KLGetCurrentKeyboardLayout().
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    oKeyboardLayout:
 *      On exit, the current keyboard layout.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLGetCurrentKeyboardLayout( var oKeyboardLayout: KeyboardLayoutRef ): OSStatus; external name '_KLGetCurrentKeyboardLayout';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  KLSetCurrentKeyboardLayout()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    To make a keyboard layout become the current input source, use
 *    TISSelectInputSource API.
 *    
 *    Input methods have used this API to override the keyboard layout
 *    used when the input method or one of its input modes is selected.
 *     TISSetInputMethodKeyboardLayoutOverride should be used for this
 *    purpose and TISCopyCurrentKeyboardLayoutInputSource will return
 *    the keyboard layout specified.
 *    An benefit of using the TIS API to specify an override is that
 *    when the input method is selected, the keyboard layout specified
 *    will be visible to users of the Keyboard Viewer and allow them to
 *    see the underlying keyboard used for event delivery to your input
 *    method.
 *  
 *  Summary:
 *    Sets the current keyboard layout.
 *  
 *  Discussion:
 *    Sets the current keyboard layout for the current keyboard script.
 *     Returns "paramErr" when the current keyboard layout is not
 *    Unicode and the specified keyboard layout belongs to Unicode
 *    group.  To set Roman keyboard script's current keyboard layout to
 *    "U.S." for example, you need to call KeyScript( smRoman |
 *    smKeyForceKeyScriptMask ) then call KLSetCurrentKeyboardLayout(
 *    theUSKeyboardLayoutRef ).
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    iKeyboardLayout:
 *      The keyboard layout.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function KLSetCurrentKeyboardLayout( iKeyboardLayout: KeyboardLayoutRef ): OSStatus; external name '_KLSetCurrentKeyboardLayout';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{  OBSOLETE                                                                        }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ These are obsolete.  Carbon does not support these. }
{ Keyboard API Trap Number. Should be moved to Traps.i }
{$endc} {not TARGET_CPU_64}

const
	_KeyboardDispatch = $AA7A;

{ Gestalt selector and values for the Keyboard API }
const
	gestaltKeyboardsAttr = FourCharCode('kbds');
	gestaltKBPS2Keyboards = 1;
	gestaltKBPS2SetIDToAny = 2;
	gestaltKBPS2SetTranslationTable = 4;

{ Keyboard API Error Codes }
{
   I stole the range blow from the empty space in the Allocation project but should
   be updated to the officially registered range.
}
const
	errKBPS2KeyboardNotAvailable = -30850;
	errKBIlligalParameters = -30851;
	errKBFailSettingID = -30852;
	errKBFailSettingTranslationTable = -30853;
	errKBFailWritePreference = -30854;

{$endc} {TARGET_OS_MAC}{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
