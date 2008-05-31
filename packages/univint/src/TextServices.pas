{
     File:       HIToolbox/TextServices.h
 
     Contains:   Text Services Manager Interfaces.
 
     Version:    HIToolbox-219.4.81~2
 
     Copyright:  © 1991-2005 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit TextServices;
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
uses MacTypes,Quickdraw,ConditionalMacros,CFBase,CarbonEventsCore,ATSTypes,CFArray,CFDictionary,Events,Menus,AEDataModel,AERegistry,AEInteraction,Components,CarbonEvents;


{$ALIGN MAC68K}

const
	kTextService = FourCharCode('tsvc'); { component type for the component description }

const
	kTSMVersion = $0150; { Version 1.5 of the Text Services Manager }
	kTSM15Version = kTSMVersion;
	kTSM20Version = $0200; { Version 2.0 as of MacOSX 10.0 }
	kTSM22Version = $0220; { Version 2.2 as of MacOSX 10.3 }
	kTSM23Version = $0230; { Version 2.3 as of MacOSX 10.4 }


{  Interface types for NewTSMDocument}
type
	InterfaceTypeList = array[0..$7F000000 div SizeOf(OSType)-1] of OSType;
	InterfaceTypeListPtr = ^InterfaceTypeList;
type
	TSMDocumentInterfaceType = OSType;

{
 *  Summary:
 *    InterfaceTypeList - array of OSTypes passed to NewTSMDocument to
 *    specify capabilities of the new TSMDocument.
 *  
 *  Discussion:
 *    TSM's Interface types, as of 10.3, are also stored as TSMDocument
 *    properties, so once a TSMDocument is created, you can easily find
 *    out what were its InterfaceTypes at document creation.
 }
const
{
   * This is the traditional TSMDocument type.  It does not support
   * Unicode. TSM will convert all Unicode produced by input methods to
   * the Mac encoding represented by the current keyboard script (or
   * the Mac encoding specified by the input method producing text.) 
   * Full Unicode input sources may not be selectable when this
   * TSMDocument is active.
   }
	kTextServiceDocumentInterfaceType = kTextService; { TSM Document type for traditional (non-Unicode) NewTSMDocument }

  {
   * TSMTE document type.  This requests automatic management of inline
   * input sessions by TextEdit (the text engine.)  See Technote TE27 -
   * Inline Input for TextEdit with TSMTE.
   }
	kTSMTEDocumentInterfaceType = FourCharCode('tmTE'); { TSM Document type for TSMTE document (see kTSMTEInterfaceType - TSMTE.h) }

  {
   * Unicode-savvy TSMDocument.  TSM will pass thru all Unicode text
   * unchanged. When this TSMDocument is active, the full range of
   * input sources is available to the user, such as Unicode keyboard
   * layouts.
   }
	kUnicodeDocumentInterfaceType = FourCharCode('udoc'); { TSM Document type for Unicode-savvy application }
                                        {    Older names, to be deprecated}
	kUnicodeDocument = kUnicodeDocumentInterfaceType; { TSM Document type for Unicode-savvy application }

{
    TextServiceClass constants supported by TSM
    Same as component subtype for the component description
}
type
	TextServiceClass = OSType;

{
 *  Summary:
 *    TextService classes
 *  
 *  Discussion:
 *    Text Service classes fall in two categories or behaviors.  Text
 *    services that belong to some classes are exclusive of oneanother
 *    within a given Mac script code, such input methods of the
 *    keyboard class.
 *    
 *    Input Methods of other classes are additive in nature, regardless
 *    of the current keyboard script.
 *    
 *    Within a given class and script, exclusive input methods can only
 *    be activated one at a time.  Input methods in additive classes
 *    are keyboard script agnostic and can be active in parallel with
 *    other text services in the same class, such as multiple character
 *    palettes.
 }
const
{
   * Text service class for keyboard input methods.  Behavior is
   * exclusive. Input Methods in this class are normally associated
   * with a Mac ScriptCode or Unicode, although they can be associated
   * with several scripts by adopting the Input Mode protocol. 
   * Keyboard input methods are always visible in the System UI.
   }
	kKeyboardInputMethodClass = FourCharCode('inpm');

  {
   * Text service class for Ink (Handwriting) input methods.  Behavior
   * is Additive. Text Services in the Ink class do not belong to any
   * given script in the sense that those of the Keyboard class do. 
   * Once selected, this kind of text service will remain active
   * regardless of the current keyboard script. Although text services
   * in this class are keyboard script agnostic, like input methods of
   * the keyboard class they can still profess to produce only those
   * Unicodes that are encoded in the mac encoding specified in their
   * component description record or their implementation of the
   * GetScriptLanguageSupport component call.
   * 
   * Unlike input methods in the keyboard class, multiple such text
   * services can be activate in parallel.
   * 
   * Dictionary Service input methods are visible in the system UI by
   * default.  Use the kComponentBundleInvisibleInSystemUIKey plist key
   * to make them invisible if a developer-provided UI is to be used
   * instead.  Mac OS X only provides System UI for Apple's Ink input
   * method.
   }
	kInkInputMethodClass = FourCharCode('ink ');

  {
   * Text service class for Character Palette input methods.  Behavior
   * is Additive. Text Services in the character palette class do not
   * belong to any given script in the same sense that do those of the
   * Keyboard class.  Once selected, this kind of text service will
   * remain active regardless of the current keyboard script. Although
   * text services in this class are keyboard script agnostic, like
   * input methods of the keyboard class they can still profess to
   * produce only those Unicodes that are encoded in the mac encoding
   * specified in their component description record or their
   * implementation of the GetScriptLanguageSupport component
   * call.
   * 
   * Unlike input methods in the keyboard class, multiple such text
   * services can be activate in parallel, and unlike input methods in
   * the Ink class, Mac OS X provides System UI to allow the user to
   * both enable and select multiple such input methods.
   * 
   * Use the kComponentBundleInvisibleInSystemUIKey plist key to make
   * Character Palette input methods invisible to the System UI.
   }
	kCharacterPaletteInputMethodClass = FourCharCode('cplt');

  {
   * Text Service class for Speech input methods.  Behavior is
   * Additive.
   * 
   * Similar to Character palette class.  System UI for these has not
   * yet been determined.
   }
	kSpeechInputMethodClass = FourCharCode('voic');

  {
   * Text Service class for Optical Character Recognition input
   * methods.  Behavior is Additive.
   * 
   * Similar to Character palette class.  System UI for these has not
   * yet been determined.
   }
	kOCRInputMethodClass = FourCharCode('ocr ');

{ New opaque definitions for types }
type
	TSMDocumentID = ^SInt32; { an opaque 32-bit type }
{ TSMDocumentID Properties}
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
{ TSMDocumentID Properties                                                             }
{ With the following property APIs, you can attach any piece of data you'd like to a   }
{ TSMDocument.}
{ to attach the data to the menu itself, and not to any specific menu item.            }
{ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ}
type
	TSMDocumentPropertyTag = OSType;

{
 *  Summary:
 *    TSMDocument Property Tags
 *  
 *  Discussion:
 *    With the following property APIs, you can attach any piece of
 *    data you'd like to a TSMDocument.  The property tags pre-defined
 *    below by TSM fall in several categories. Read-only properties: 
 *    these property tags are actually the interface type used to
 *    create a TSMDocument, such as kUnicodeDocumentInterfaceType.
 *    These properties are all property value-independent... see below.
 *    Value-independent properties:  these property tags are value
 *    independent. Only the fact that the tag is or is not a property
 *    of a TSMDocument is important.  These are used where the
 *    existence of the property is sufficient... Read-only TSMDocment
 *    properties are examples of value independent properties.
 *    SupportGlyphInfo is another. Pass a non-NULL actualSize parameter
 *    and a NULL propertyBuffer to check if the property exists (i.e.
 *    to obtain the size.) Value-dependent properties:  these are
 *    properties for which the value is meaningful.  Examples of this
 *    are the Refcon ProtocolVersion properties. NOTE: 
 *    Value-independent properties can read by other clients, in
 *    particular input methods.  For example, input methods may want to
 *    query the current TSMDocument to see if supports unrestricted
 *    Unicode input, or if it supports the GlyphInfo protocol.
 }
const
                                        {    Property values for value-independent property tags (i.e. only the existence of}
                                        {    the property is meaningful).  To remove these properties, use TSMRemoveDocumentProperty.}
                                        {    To test these properties, it is sufficient to obtain the property's size, i.e. buffer = NULL}
                                        {    and test for error.}
                                        {    Read-only (value-independent) properties}
                                        {    Identical to Interface types passed to NewTSMDocument}

  {
   * Property is read-only, value-independent.  The value is identical
   * to the TextServiceDocument interface type, but is not needed. 
   * Simply test for existence of the property. Property available in
   * TSM 2.2 and later
   }
	kTSMDocumentTextServicePropertyTag = kTextServiceDocumentInterfaceType;

  {
   * Property is read-only, value-independent.  The value is identical
   * to the UnicodeDocument interface type, but is not needed.  Simply
   * test for existence of the property. Property available in TSM 2.2
   * and later
   }
	kTSMDocumentUnicodePropertyTag = kUnicodeDocumentInterfaceType;

  {
   * Property is read-only, value-independent.  The value is identical
   * to the TSMTEDocument interface type, but is not needed.  Simply
   * test for existence of the property. Property available in TSM 2.2
   * and later
   }
	kTSMDocumentTSMTEPropertyTag = kTSMTEDocumentInterfaceType; {    }
                                        {    Settable value-independent properties}

  {
   * Property is value-independent.  The existence of this property in
   * a TSMDocument indicates that the event handlers associated with
   * the TSMDocument are aware of TSM's GlyhInfo data structure.  This
   * structure allows the input source producing text to apply Glyph
   * IDs, CIDs, or Fonts to subranges of text produced. This is useful
   * for characters in Unicode private use area, such as Windings. For
   * more information, see the Glyph Access protocol described in
   * TechNote TN20TT. By convention, this value can be a UInt32 with a
   * value of 0, but this is really arbitrary.  Simply test for
   * existence of the property. Property available in TSM 1.5 from
   * MacOSX 10.2 and later
   }
	kTSMDocumentSupportGlyphInfoPropertyTag = FourCharCode('dpgi'); {  property value is arbitrary}

  {
   * Property is value-independent.  The presence of this property tag
   * indicates that the TSMDocument should use TSM's floating input
   * window to handle input from input methods.  This form of input
   * does not support Unicode input by default, unless the
   * UnicodeInputWindow property is set. By convention, this value can
   * be a UInt32 with a value of 0, but this is really arbitrary. 
   * Simply test for existence of the property. Property available in
   * TSM 2.2 and later
   }
	kTSMDocumentUseFloatingWindowPropertyTag = FourCharCode('uswm'); {    use bottom-line input (floating TSM) window for text input}

  {
   * Property is value-independent.  The presence of this property tag
   * indicates that although the TSMDocument has been told to use TSM's
   * floating input window to handle input from input methods, the
   * floating window is to support Unicode input.  This is useful when
   * non input-related activity is to produce Unicode, such as keyboard
   * navigation . By convention, this value can be a UInt32 with a
   * value of 0, but this is really arbitrary.  Simply test for
   * existence of the property. Property available in TSM 2.2 and later
   }
	kTSMDocumentUnicodeInputWindowPropertyTag = FourCharCode('dpub'); {    Unicode support in bottom line input window}

  {
   * Property is value-independent.  The presence of this property tag
   * indicates that the event handlers associated with this TSMDocument
   * support TSM's DocumentAccess event suite (see CarbonEvents.h) This
   * property also indicates that the handler for TSM's
   * kEventTextInputUpdateActiveInputArea event supports the
   * 'replaceRange' parameter and that the handler is a Carbon event
   * handler, not an AppleEvent handler. By convention, this value can
   * be a UInt32 with a value of 0, but this is really arbitrary. 
   * Simply test for existence of the property. Property available in
   * TSM 2.2 and later
   }
	kTSMDocumentSupportDocumentAccessPropertyTag = FourCharCode('dapy'); {    support TSM Document Access protocol}
                                        {    Older names, to be deprecated}
	kTSMDocumentPropertySupportGlyphInfo = kTSMDocumentSupportGlyphInfoPropertyTag;
	kTSMDocumentPropertyUnicodeInputWindow = kTSMDocumentUnicodeInputWindowPropertyTag; {    }
                                        {    Settable value-dependent properties}
                                        {    Property values for the following property tags are meaningful}

  {
   * Property is value-dependent.  The property value initially
   * contains the Refcon value passed to NewTSMDocument. This property
   * is useful for changing the refcon on-the-fly after the TSMDocument
   * has been created. The refcon value is a long, the same as that
   * passed to NewTSMDocument. Property available in TSM 2.2 and later
   }
	kTSMDocumentRefconPropertyTag = FourCharCode('refc'); {    refcon passed to TSMDocument (UInt32)}

  {
   * Property is value-dependent.  The property value indicates which
   * input mode should be used by the current keyboard-class input
   * method.  It is useful for temporarily restricting text input to a
   * subset of characters normally produced by an input method in a
   * given script, such as Katakana for Japanese input.  See details in
   * TextServiceProperty API below. Also note that this property tag
   * and value are passed unchanged to the TextServiceProperty API, so
   * it also serves as a TextServicePropertyTag. See
   * kTextServiceInputModePropertyTag for discussion on the values
   * associated with this property. Usage Note:  Property value is a
   * CFStringRef. With TSMGetTextServiceProperty, the behavior is that
   * of a Copy function. The implementation of
   * TSMSetTextServiceProperty (in the component) retains or copies the
   * CFString... in either case the caller is responsible for releasing
   * its reference. Property available in TSM 2.2 and later
   }
	kTSMDocumentInputModePropertyTag = FourCharCode('imim'); {    Input mode property for input methods (CFStringRef - see Input Modes below)}

  {
   * Property is value-dependent.  The property value indicates the
   * level of the window that will host inline input sessions for a
   * given TSMDocument. Input Methods will query this property to
   * determine the level above which its "candidate" (floating)
   * window(s) should be positioned, typically by adding its window to
   * a window group (see Carbon's WindowGroup API) and incrementing the
   * level for the group.  If no level is available, the input method
   * is expected to use the default behavior, i.e. let the Window
   * Manager manage the level for floating windows. Window levels are
   * typed as CGWindowLevel. NSWindow levels are accessed through
   * NSWindow's (int)level method. Property available in TSM 2.3 and
   * later
   }
	kTSMDocumentWindowLevelPropertyTag = FourCharCode('twlp'); {    document window level (CGWindowLevel)}


{
 *  TSMSetDocumentProperty()
 *  
 *  Summary:
 *    Set a TSMDocument property
 *  
 *  Discussion:
 *    With the following property APIs, you can attach any piece of
 *    data you'd like to a TSMDocument.  Other uses include setting a
 *    (value-independent) property for input methods to query in order
 *    to determine an application's compliance with various TSM
 *    protocols. NOTE:  Property values are currently typed as SInt32,
 *    but should really be a void* since a variety of data types are
 *    used in practice. Note that the semantics for refcounted objects
 *    is ambiguous, so retain/release model is discussed for each
 *    TSM-defined property individually.  (See notes for
 *    TSMDocumentPropertyTag) Error Codes (MacErrors.h): 
 *    tsmComponentPropertyUnsupportedErr tsmInputModeChangeFailedErr
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function TSMSetDocumentProperty( docID: TSMDocumentID; propertyTag: TSMDocumentPropertyTag; propertySize: UInt32; propertyData: UnivPtr ): OSStatus; external name '_TSMSetDocumentProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  TSMGetDocumentProperty()
 *  
 *  Summary:
 *    Get a TSMDocument property
 *  
 *  Discussion:
 *    With the following property APIs, you can attach any piece of
 *    data you'd like to a TSMDocument.  Other uses include setting a
 *    (value-independent) property for input methods to query in order
 *    to determine an application's compliance with various TSM
 *    protocols. NOTE:  Property values are currently typed as SInt32,
 *    but should really be a void* since a variety of data types are
 *    used in practice. Note that the semantics for refcounted objects
 *    is ambiguous, so retain/release model is discussed for each
 *    TSM-defined property individually.  (See notes for
 *    TSMDocumentPropertyTag) Error Codes (MacErrors.h)
 *    tsmDocPropertyNotFoundErr tsmDocPropertyBufferTooSmallErr
 *    tsmComponentPropertyNotFoundErr tsmComponentPropertyUnsupportedErr
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function TSMGetDocumentProperty( docID: TSMDocumentID; propertyTag: TSMDocumentPropertyTag; bufferSize: UInt32; var actualSize: UInt32; propertyBuffer: UnivPtr { can be NULL } ): OSStatus; external name '_TSMGetDocumentProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  TSMRemoveDocumentProperty()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function TSMRemoveDocumentProperty( docID: TSMDocumentID; propertyTag: TSMDocumentPropertyTag ): OSStatus; external name '_TSMRemoveDocumentProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{ Language and Script constants}
const
	kUnknownLanguage = $FFFF;
	kUnknownScript = $FFFF;
	kNeutralScript = $FFFF;


const
{ Component Flags in ComponentDescription }
	bTakeActiveEvent = 15;   { bit set if the component takes active event }
	bHandleAERecording = 16;   { bit set if the component takes care of recording Apple Events <new in vers2.0> }
	bScriptMask = $00007F00; { bit 8 - 14 }
	bLanguageMask = $000000FF; { bit 0 - 7  }
	bScriptLanguageMask = bScriptMask + bLanguageMask; { bit 0 - 14  }


const
{ Low level routines which are dispatched directly to the Component Manager }
	kCMGetScriptLangSupport = $0001; { Component Manager call selector 1 }
	kCMInitiateTextService = $0002; { Component Manager call selector 2 }
	kCMTerminateTextService = $0003; { Component Manager call selector 3 }
	kCMActivateTextService = $0004; { Component Manager call selector 4 }
	kCMDeactivateTextService = $0005; { Component Manager call selector 5 }
	kCMTextServiceEvent = $0006; { Component Manager call selector 6 }
	kCMTextServiceEventRef = kCMTextServiceEvent;
	kCMGetTextServiceMenu = $0007; { Component Manager call selector 7 }
	kCMTextServiceMenuSelect = $0008; { Component Manager call selector 8 }
	kCMFixTextService = $0009; { Component Manager call selector 9 }
	kCMSetTextServiceCursor = $000A; { Component Manager call selector 10 }
	kCMHidePaletteWindows = $000B; { Component Manager call selector 11 }
	kCMGetTextServiceProperty = $000C; { Component Manager call selector 12 }
	kCMSetTextServiceProperty = $000D; { Component Manager call selector 13 }
	kCMUCTextServiceEvent = $000E; { Component Manager call selector 14 }
	kCMCopyTextServiceInputModeList = $000F; { Component Manager call selector 15 }
	kCMInputModePaletteItemHit = $0010; { Component Manager call selector 16 }
	kCMGetInputModePaletteMenu = $0011; { Component Manager call selector 17 }


{ extract Script/Language code from Component flag ... }
// #define     mGetScriptCode(cdRec)       ((ScriptCode)   ((cdRec.componentFlags & bScriptMask) >> 8))
// #define     mGetLanguageCode(cdRec)     ((LangCode)     cdRec.componentFlags & bLanguageMask)

{ Text Service Info List }
type
	TextServiceInfo = record
		fComponent: Component;
		fItemName: Str255;
	end;
	TextServiceInfoPtr = ^TextServiceInfo;
type
	TextServiceList = record
		fTextServiceCount: SInt16;      { number of entries in the 'fServices' array }
		fServices: array [0..0] of TextServiceInfo;		{  Note: array of 'TextServiceInfo' records follows  }
	end;
	TextServiceListPtr = ^TextServiceList;
type
	TextServiceListHandle = ^TextServiceListPtr;
	ScriptLanguageRecord = record
		fScript: ScriptCode;
		fLanguage: LangCode;
	end;
	ScriptLanguageRecordPtr = ^ScriptLanguageRecord;
type
	ScriptLanguageSupport = record
		fScriptLanguageCount: SInt16;   { number of entries in the 'fScriptLanguageArray' array }
		fScriptLanguageArray:	array [0..0] of ScriptLanguageRecord;	{  Note: array of 'ScriptLanguageRecord' records follows  }
	end;
	ScriptLanguageSupportPtr = ^ScriptLanguageSupport;
type
	ScriptLanguageSupportHandle = ^ScriptLanguageSupportPtr;
	TSMGlyphInfo = record
		range: CFRange;                  {    two SInt32s}
		fontRef: ATSFontRef;
		collection: UInt16;             {    kGlyphCollectionXXX enum}
		glyphID: UInt16;                {    GID (when collection==0) or CID}
	end;
type
	TSMGlyphInfoArray = record
		numGlyphInfo: ItemCount;           {    UInt32}
		glyphInfo: array[0..$7F000000 div SizeOf(TSMGlyphInfo)-1] of TSMGlyphInfo;
	end;

{ High level TSM Doucment routines }
{
 *  NewTSMDocument()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function NewTSMDocument( numOfInterface: SInt16; supportedInterfaceTypes: {variable-size-array} InterfaceTypeListPtr; var idocID: TSMDocumentID; refcon: SInt32 ): OSErr; external name '_NewTSMDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DeleteTSMDocument()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DeleteTSMDocument( idocID: TSMDocumentID ): OSErr; external name '_DeleteTSMDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ActivateTSMDocument()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ActivateTSMDocument( idocID: TSMDocumentID ): OSErr; external name '_ActivateTSMDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DeactivateTSMDocument()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DeactivateTSMDocument( idocID: TSMDocumentID ): OSErr; external name '_DeactivateTSMDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  FixTSMDocument()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FixTSMDocument( idocID: TSMDocumentID ): OSErr; external name '_FixTSMDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetServiceList()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetServiceList( numOfInterface: SInt16; {const} supportedInterfaceTypes: {variable-size-array} InterfaceTypeListPtr; var serviceInfo: TextServiceListHandle; var seedValue: SInt32 ): OSErr; external name '_GetServiceList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  OpenTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function OpenTextService( idocID: TSMDocumentID; aComponent: Component; var aComponentInstance: ComponentInstance ): OSErr; external name '_OpenTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CloseTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function CloseTextService( idocID: TSMDocumentID; aComponentInstance: ComponentInstance ): OSErr; external name '_CloseTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SendAEFromTSMComponent()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SendAEFromTSMComponent( const (*var*) theAppleEvent: AppleEvent; var reply: AppleEvent; sendMode: AESendMode; sendPriority: AESendPriority; timeOutInTicks: SInt32; idleProc: AEIdleUPP; filterProc: AEFilterUPP ): OSErr; external name '_SendAEFromTSMComponent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SendTextInputEvent()
 *  
 *  Discussion:
 *    This API replaces SendAEFromTSMComponent on Mac OS X only. Input
 *    Methods on Mac OS X are Carbon Event based instead of AppleEvent
 *    based.  The Carbon TextInput events which they generate are
 *    provided to TSM for dispatching via this API.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib N.e.v.e.r and later
 *    Non-Carbon CFM:   not available
 }
function SendTextInputEvent( inEvent: EventRef ): OSStatus; external name '_SendTextInputEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetDefaultInputMethod()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetDefaultInputMethod( ts: Component; var slRecordPtr: ScriptLanguageRecord ): OSErr; external name '_SetDefaultInputMethod';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetDefaultInputMethod()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetDefaultInputMethod( var ts: Component; var slRecordPtr: ScriptLanguageRecord ): OSErr; external name '_GetDefaultInputMethod';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetTextServiceLanguage()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetTextServiceLanguage( var slRecordPtr: ScriptLanguageRecord ): OSErr; external name '_SetTextServiceLanguage';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetTextServiceLanguage()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetTextServiceLanguage( var slRecordPtr: ScriptLanguageRecord ): OSErr; external name '_GetTextServiceLanguage';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UseInputWindow()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function UseInputWindow( idocID: TSMDocumentID; useWindow: Boolean ): OSErr; external name '_UseInputWindow';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  TSMSetInlineInputRegion()
 *  
 *  Summary:
 *    Tell TSM what region and which window make up the inline input
 *    session.
 *  
 *  Discussion:
 *    Tell TSM about the region occupied by an inline input session. If
 *    the location of certain mouse events (clicks, mouse moved) occur
 *    within the specified inline input region, TSM will forward these
 *    events to the current Input Method so that it can interact with
 *    the user. Note: If you do not specify this information, TSM will
 *    need to intercept mouse events in the entire content region as
 *    the default, when an input method is active, in order to ensure
 *    that input methods can manage user interaction properly.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inTSMDocument:
 *      The document.
 *    
 *    inWindow:
 *      The window that contains the inline input session. You can pass
 *      NULL for this parameter to indicate the user focus window.
 *    
 *    inRegion:
 *      The region occupied by the current inline input region. This
 *      should be in the coordinates of the port associated with the
 *      window you passed to inPort. It will need to be recomputed when
 *      the text content of the inline input session content changes
 *      (i.e. due to Update Active Input Area events) and when the
 *      region moves for other reasons, such as window resized,
 *      scrolling, etc. If you pass a NULL region for this parameter,
 *      TSM will default to intercept mouse events in the focus
 *      window's content region.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function TSMSetInlineInputRegion( inTSMDocument: TSMDocumentID; inWindow: WindowRef; inRegion: RgnHandle ): OSStatus; external name '_TSMSetInlineInputRegion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Following calls from Classic event loops not needed for Carbon clients. }
{
 *  TSMEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  TSMMenuSelect()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  SetTSMCursor()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{ Following ServiceWindow API replaced by Window Manager API in Carbon. }
{
 *  NewServiceWindow()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  CloseServiceWindow()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  GetFrontServiceWindow()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  FindServiceWindow()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  NewCServiceWindow()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }


{ Explicit initialization not needed for Carbon clients, since TSM is }
{ instanciated per-context. }
{
 *  InitTSMAwareApplication()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  CloseTSMAwareApplication()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{ Component Manager Interfaces to Input Methods }
{
 *  GetScriptLanguageSupport()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetScriptLanguageSupport( ts: ComponentInstance; var scriptHdl: ScriptLanguageSupportHandle ): ComponentResult; external name '_GetScriptLanguageSupport';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  InitiateTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function InitiateTextService( ts: ComponentInstance ): ComponentResult; external name '_InitiateTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  TerminateTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function TerminateTextService( ts: ComponentInstance ): ComponentResult; external name '_TerminateTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  ActivateTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ActivateTextService( ts: ComponentInstance ): ComponentResult; external name '_ActivateTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  DeactivateTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DeactivateTextService( ts: ComponentInstance ): ComponentResult; external name '_DeactivateTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetTextServiceMenu()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetTextServiceMenu( ts: ComponentInstance; var serviceMenu: MenuRef ): ComponentResult; external name '_GetTextServiceMenu';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ New Text Service call in Carbon. }
{ Note: Only Raw Key and Mouse-flavored events are passed to Text Services on MacOS X. }
{
 *  TextServiceEventRef()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function TextServiceEventRef( ts: ComponentInstance; event: EventRef ): ComponentResult; external name '_TextServiceEventRef';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  TextServiceEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  UCTextServiceEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }


{
 *  TextServiceMenuSelect()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  SetTextServiceCursor()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }


{
 *  FixTextService()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FixTextService( ts: ComponentInstance ): ComponentResult; external name '_FixTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  HidePaletteWindows()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HidePaletteWindows( ts: ComponentInstance ): ComponentResult; external name '_HidePaletteWindows';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
    Text Service Properties and standard values
    (used with GetTextServiceProperty/SetTextServiceProperty)
}

type
	TextServicePropertyTag = OSType;

{
 *  Summary:
 *    TextService Properties
 *  
 *  Discussion:
 *    Text Service properties are passed directly to a text service
 *    component via Component Mgr glue provided by TSM.  Applications
 *    that have special knowledge of specific input methods can
 *    communicate private properties between each other. The 2
 *    following errors have been introduced (as of Mac OS X 10.3) to
 *    better support these API: tsmComponentPropertyUnsupportedErr  // 
 *    SetTextServiceProperty failed tsmComponentPropertyNotFoundErr    
 *    //  GetTextServiceProperty failed
 }
const
{ Typing input mode property tag }

  {
   * The JaTypingMethod property is becoming obsolete.  It was
   * introduced as a way for the Setup Assistant to tell Apple's
   * Kotoeri (Japanese) input method to switch to either Roman or
   * Katakana input mode in certain text fields. The entire input mode
   * mechanism is being superceded by kTextServiceInputModePropertyTag.
   * Property available in TSM 1.5 on MacOSX 10.0 and later
   }
	kTextServiceJaTypingMethodPropertyTag = FourCharCode('jtyp'); { Japanese input method typing property}

  {
   * The InputMode property is a CFStringRef that uniquely identifies
   * which input mode should be made current by a keyboard class input
   * method, if possible. Input modes are either generic, i.e.
   * pre-defined by TSM, or specific to an input method.  An example of
   * a generic input mode is Katakana input (Japanese) where input in a
   * text field needs to be restricted to that character subset.
   * Another is Roman input mode.  This is useful to temporarily
   * provide Roman input from an input method that normally allows text
   * input in another script. The advantage to this over forcing the
   * keyboard script to Roman is that the same input method's UI
   * continues to be available to the user, even though the input
   * script has changed. An example of a special input mode (input
   * method specific) is Hanin input mode in Traditional Chinese input
   * methods. To temporarily change the current input mode from
   * whatever it is to a generic one, use GetTextServiceProperty to
   * obtain the current input mode, then SetTextServiceProperty to
   * switch to the generic mode, and when done restore the original
   * input mode. Input Methods themselves make known what input modes
   * they support via the CopyTextServiceInputModeList component call. 
   * If the input method does not support a specified inputMode,
   * GetTextServiceProperty and SetTextServiceProperty will return
   * tsmComponentPropertyUnsupportedErr.  GetTextServiceProperty will
   * return tsmComponentPropertyNotFoundErr.  RemoveTextServiceProperty
   * is not supported with this tag. NOTE: This property tag is
   * identical to the kTSMDocumentInputModePropertyTag passed to the
   * TSMDocumentProperty API.  This allows the tag and value to be
   * passed through without interpretation. NOTE: Input modes
   * CFStringRef's are cast to SInt32 in these API. Property available
   * in TSM 2.2 and later Error Codes (MacErrors.h)
   * tsmComponentPropertyUnsupportedErr - SetTextServiceProperty does
   * not support the specified inputMode. tsmInputModeChangeFailedErr -
   * can be returned if an input method failed to succeed in switching
   * to the requested input mode.
   }
	kTextServiceInputModePropertyTag = kTSMDocumentInputModePropertyTag; { input mode property for input methods}
                                        {    NOTE:  This property is being deprecated.  Use kTextServiceInputModePropertyTag instead. }
	kIMJaTypingMethodProperty = kTextServiceJaTypingMethodPropertyTag; { Typing method property for Japanese input methods}
                                        { Typing method property values for kTextServiceJaTypingMethodPropertyTag }
	kIMJaTypingMethodRoman = FourCharCode('roma'); { Roman typing}
	kIMJaTypingMethodKana = FourCharCode('kana'); { Kana typing}

type
	TextServicePropertyValue = SInt32;

{
    Generic, restricted, input modes
    Used as values for kTextServiceInputModePropertyTag TextServiceProperty API.
    These values require a cast from CFStringRef to SInt32.
}

{  Restrict output to Roman characters only}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeRoman CFSTRP('com.apple.inputmethod.Roman')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModePassword CFSTRP('com.apple.inputmethod.Password')}
{$endc}
{  Restrict output to Hiragana characters only (no conversion to Kanji, i.e. yomi)}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapaneseHiragana CFSTRP('com.apple.inputmethod.Japanese.Hiragana')}
{$endc}
{  Restrict output to Katakana characters only (no conversion to Kanji)}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapaneseKatakana CFSTRP('com.apple.inputmethod.Japanese.Katakana')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapaneseFullWidthRoman CFSTRP('com.apple.inputmethod.Japanese.FullWidthRoman')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapaneseHalfWidthKana CFSTRP('com.apple.inputmethod.Japanese.HalfWidthKana')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapanesePlaceName CFSTRP('com.apple.inputmethod.Japanese.PlaceName')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapaneseFirstName CFSTRP('com.apple.inputmethod.Japanese.FirstName')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapaneseLastName CFSTRP('com.apple.inputmethod.Japanese.LastName')}
{$endc}
{  Restrict output to Bopomofo characters only (no conversion to Han)}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeBopomofo CFSTRP('com.apple.inputmethod.TradChinese.Bopomofo')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeTradChinesePlaceName CFSTRP('com.apple.inputmethod.TradChinese.PlaceName')}
{$endc}
{  Restrict output to Hangul syllables only (no conversion to Hanja)}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeHangul CFSTRP('com.apple.inputmethod.Korean.Hangul')}
{$endc}
{
    Generic, unrestricted, Input Mode strings
    Used as values for kTextServiceInputModePropertyTag in TextServiceProperty API.
    Unrestricted Japanese output
}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeJapanese CFSTRP('com.apple.inputmethod.Japanese')}
{$endc}
{  Traditional Chinese generic (unrestricted) input mode}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeTradChinese CFSTRP('com.apple.inputmethod.TradChinese')}
{$endc}
{  Simplified Chinese generic (unrestricted) input mode}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeSimpChinese CFSTRP('com.apple.inputmethod.SimpChinese')}
{$endc}
{  Korean generic (unrestricted) output (i.e. Hanja possible)}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTextServiceInputModeKorean CFSTRP('com.apple.inputmethod.Korean')}
{$endc}


{
 *  GetTextServiceProperty()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function GetTextServiceProperty( ts: ComponentInstance; inPropertyTag: TextServicePropertyTag; var outPropertyValue: TextServicePropertyValue ): ComponentResult; external name '_GetTextServiceProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SetTextServiceProperty()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function SetTextServiceProperty( ts: ComponentInstance; inPropertyTag: TextServicePropertyTag; inPropertyValue: TextServicePropertyValue ): ComponentResult; external name '_SetTextServiceProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  CopyTextServiceInputModeList()
 *  
 *  Summary:
 *    Obtain a copy of the set of input modes supported by a
 *    keyboard-class input method.
 *  
 *  Discussion:
 *    This Component call is only intended to be supported by input
 *    methods that adopt the Input Mode protocol. If this component
 *    call is not supported by an input method, calls to the
 *    TextServiceProperty API with the kTextServiceInputModePropertyTag
 *    property will fail with tsmComponentPropertyUnsupportedErr. Below
 *    is the layout of the CFDictionary returned. <dict> <key>
 *    kTSInputModeListKey </key> <dict> <key> modeSignature : (internal
 *    ascii name) </key> <!-- This can be any of the generic input
 *    modes defined in this file, --> <!--    such as
 *    kTextServiceInputModeRoman, or can be a private input --> <!--   
 *    mode such as CFSTR(
 *    "com.apple.MyInputmethod.Japanese.CoolInputMode" ) --> <dict>
 *    <key>menuIconFile</key> <string> (path for menu icon image file)
 *    </string> <key>alternateMenuIconFile</key> <string> (path for
 *    alternate menu icon image file -- when item is hilited) </string>
 *    <key>paletteIconFile</key> <string> (path for palette icon image
 *    file) </string> <key>defaultState</key> <boolean> (default on/off
 *    state) </boolean> <key>script</key> <string> (scriptCode string
 *    for this mode, i.e. "smRoman", "smJapanese", ...) </string>
 *    <key>primaryInScript</key> <boolean> (true if this is primary
 *    mode in this script) </boolean> <key>isVisible</key> <boolean>
 *    (true if this input mode should appear in System UI) </boolean>
 *    <key>keyEquivalentModifiers</key> <integer> (modifiers)
 *    </integer> <key>keyEquivalent</key> <string> (key equivalent
 *    character) </string> <key>JISKeyboardShortcut</key> <integer>
 *    (optional: 0=none,1=hiragana,2=katakana,3=eisu) </integer>
 *    </dict> </dict> <key> kTSVisibleInputModeOrderedArrayKey </key>
 *    <!-- This array defines the ordering (for UI purposes) of input
 *    modes that are --> <!--    both visible and enabled (either by
 *    default, i.e. by the System, or by --> <!--    the user, i.e. via
 *    System provided UI) --> <array> <value> modeSignature </value>
 *    </array> </dict> Available in TSM 2.2 and later Important:  This
 *    dictionary must also be present in the component bundle's
 *    Info.plist, in addition to being available through this component
 *    call.  This will allow retrival of input modes by the System
 *    without opening the component.  The component call will be used
 *    whenever the System is notified of a change in the contents of
 *    the inputMode list, such as when the name or key-equivalents of
 *    individual input modes have changed. Note:       If, when the
 *    input method is first activated in a login session, the settings
 *    of the individual input modes (names or key-equivalents) differ
 *    from the default settings as found in the component bundle's
 *    Info.plist, the System will need to be notified of the change. 
 *    The input method does this by sending out a
 *    kEventTextInputInputMenuChanged Carbon event, just as it is when
 *    the change originally took place.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
function CopyTextServiceInputModeList( ts: ComponentInstance; var outInputModes: CFDictionaryRef ): ComponentResult; external name '_CopyTextServiceInputModeList';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
    Key for InputMode dictionary in Component bundle's Info.plist
    Value is a dictionary describing the set of input modes implemented by the component.
        The supported keys in this dictionary are:
            kTSInputModeListKey
            kTSVisibleInputModeOrderedArrayKey
    See CopyTextServiceInputModeList() for more details.
}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kComponentBundleInputModeDictKey CFSTRP('ComponentInputModeDict')}
{$endc}

{
    Key for dictionary of individual input modes
    NOTE:  This key is used both in the Component bundle's Info.plist dictionary and the dictionary
            returned by the CopyTextServiceInputModeList() component call.
}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeListKey CFSTRP('tsInputModeListKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeMenuIconFileKey CFSTRP('tsInputModeMenuIconFileKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeAlternateMenuIconFileKey CFSTRP('tsInputModeAlternateMenuIconFileKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteIconFileKey CFSTRP('tsInputModePaletteIconFileKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeDefaultStateKey CFSTRP('tsInputModeDefaultStateKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeScriptKey CFSTRP('tsInputModeScriptKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePrimaryInScriptKey CFSTRP('tsInputModePrimaryInScriptKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeIsVisibleKey CFSTRP('tsInputModeIsVisibleKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeKeyEquivalentModifiersKey CFSTRP('tsInputModeKeyEquivalentModifiersKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeKeyEquivalentKey CFSTRP('tsInputModeKeyEquivalentKey')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModeJISKeyboardShortcutKey CFSTRP('tsInputModeJISKeyboardShortcutKey')}
{$endc}
{
    Key for array of visible input modes to determine UI ordering
    NOTE:  This key is intended to be used both in the Component bundle's Info.plist dictionary and in
            in the dictionary returned by the CopyTextServiceInputModeList() component call.  In the latter
            case, the array should contain only the set of visible input modes that are also enabled.
}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSVisibleInputModeOrderedArrayKey CFSTRP('tsVisibleInputModeOrderedArrayKey')}
{$endc}

{
    Key for Component bundle's Info.plist, controls visibility in System UI.
    Value is a CFBoolean.
    NOTE:  Keyboard input methods are always visible in System UI.
}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kComponentBundleInvisibleInSystemUIKey CFSTRP('ComponentInvisibleInSystemUI')}
{$endc}

{
 *  TSMCopyInputMethodEnabledInputModes()
 *  
 *  Summary:
 *    Obtain the array of a component's enabled (and visible) input
 *    modes.
 *  
 *  Discussion:
 *    The intended use of this API is to allow an input method to query
 *    the System for the subset of its own input modes that are
 *    enabled, so that the component's own UI can ommit input modes
 *    disabled by the user or the System. This API is only meaningful
 *    for input methods that adopt the Input Mode protocol.  If the
 *    component passed is not inputMode-savvy, the returned array will
 *    be NULL. The enabled input modes returned in the array are always
 *    visible ones, i.e. those input modes for which
 *    kTSInputModeIsVisibleKey is true because non-visible input modes
 *    are not tracked by the System. It is the responsibility of the
 *    caller to release the returned array. <array> <value>
 *    modeSignature : (internal ascii name) </value> </array> Available
 *    in TSM 2.2 and later
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
function TSMCopyInputMethodEnabledInputModes( inComponent: Component; var outInputModeArray: CFArrayRef ): Boolean; external name '_TSMCopyInputMethodEnabledInputModes';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{
 *  TSMSelectInputMode()
 *  
 *  Summary:
 *    Make the specified input method input mode the current input
 *    source.
 *  
 *  Discussion:
 *    The intended use of this API is to allow an input method to
 *    select one of its own input modes as the current input source and
 *    update the TextInput menu icon in the menubar. This API is only
 *    meaningful for input methods that adopt TSM's Input Mode
 *    protocol. Available in TSM 2.2 and later
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
function TSMSelectInputMode( inComponent: Component; inInputMode: CFStringRef ): OSStatus; external name '_TSMSelectInputMode';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{ Get the active TSMDocument in the current application context.       }
{ If TSM has enabled bottom line input mode because no TSMDocument     }
{ is active, NULL will be returned.                                    }
{
 *  TSMGetActiveDocument()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Non-Carbon CFM:   not available
 }
function TSMGetActiveDocument: TSMDocumentID; external name '_TSMGetActiveDocument';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  GetDefaultInputMethodOfClass()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function GetDefaultInputMethodOfClass( var aComp: Component; var slRecPtr: ScriptLanguageRecord; tsClass: TextServiceClass ): OSStatus; external name '_GetDefaultInputMethodOfClass';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  SetDefaultInputMethodOfClass()
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function SetDefaultInputMethodOfClass( aComp: Component; var slRecPtr: ScriptLanguageRecord; tsClass: TextServiceClass ): OSStatus; external name '_SetDefaultInputMethodOfClass';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  SelectTextService()
 *  
 *  Summary:
 *    Select a text service component
 *  
 *  Discussion:
 *    This API is currently only intended for use by input methods in
 *    text service classes which are "additive" in nature, that is
 *    where the input method can operate in parallel to other input
 *    methods in the same class and other additive text service
 *    classes.  An example of such a class is the Character Palette
 *    class.  It is not for use by traditional input methods, such as
 *    those that belong to the keyboard input method class.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function SelectTextService( aComp: Component ): OSStatus; external name '_SelectTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  DeselectTextService()
 *  
 *  Summary:
 *    Deselect a text service component
 *  
 *  Discussion:
 *    See SelectTextService.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function DeselectTextService( aComp: Component ): OSStatus; external name '_DeselectTextService';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{
 *  IsTextServiceSelected()
 *  
 *  Summary:
 *    Checks if a text service component is selected
 *  
 *  Discussion:
 *    See SelectTextService.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   not available
 }
function IsTextServiceSelected( aComp: Component ): Boolean; external name '_IsTextServiceSelected';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


{$ifc OLDROUTINENAMES}
const
	kInputMethodService = kKeyboardInputMethodClass;

const
	kUnicodeTextService = FourCharCode('utsv'); { Component type for Unicode Text Service }

{$endc}  { OLDROUTINENAMES }


{-------------------------------------------}
{ Input Mode Palette configuration routines }
{-------------------------------------------}

{
 *  TSMInputModePaletteLoadButtons()
 *  
 *  Discussion:
 *    Notifies the Input Mode Palette of changes in an input method's
 *    controls.  Replaces the current controls with the new control
 *    array.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    paletteButtonsArray:
 *      CFArray containing descriptions of the controls.  Each control
 *      is described using a CFDictionary, using the keys described
 *      below.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
procedure TSMInputModePaletteLoadButtons( paletteButtonsArray: CFArrayRef ); external name '_TSMInputModePaletteLoadButtons';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  TSMInputModePaletteUpdateButtons()
 *  
 *  Discussion:
 *    Notifies the Input Mode Palette of updates in an input method's
 *    controls.  Will update controls based on the control tag ID. 
 *    Does not replace/remove existing controls.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    paletteButtonsArray:
 *      CFArray containing descriptions of the controls.  Each control
 *      is described using a CFDictionary, using the keys described
 *      below.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
procedure TSMInputModePaletteUpdateButtons( paletteButtonsArray: CFArrayRef ); external name '_TSMInputModePaletteUpdateButtons';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{----------------------------------------------------------------------------}
{ CFDictionary keys used to build the control description passed             }
{ to TSMInputModePaletteLoadButtons() and TSMInputModePaletteUpdateButtons() }
{----------------------------------------------------------------------------}


{******************************************************

Example of palette controls CFArray passed to TSMInputModePaletteLoadButtons():

    Buttons consist of a CFDictionary containing:
        - itemType
        - icon
        - alternate icon
        - ID
        - enabled
        - state
        
    Pulldown menus consist of a CFDictionary containing:
        - itemType
        - icon
        - alternate icon
        - ID

    <array>    
        <dict>
        <!-- item 1 - a push button -->
            <key>tsInputModePaletteItemTypeKey</key>
            <integer>0</integer>
            <key>tsInputModePaletteItemIconKey</key>
            <string>modeButton1.tif</string>
            <key>tsInputModePaletteItemAltIconKey</key>
            <string>modeButton1Alt.tif</string>
            <key>tsInputModePaletteItemIDKey</key>
            <integer>1</integer>
            <key>tsInputModePaletteItemEnabledKey</key>
            <true/>
            <key>tsInputModePaletteItemStateKey</key>
            <integer>0</integer>
        </dict>
        <dict>
        <!-- item 2 - a pulldown menu -->
            <key>tsInputModePaletteItemTypeKey</key>
            <integer>2</integer>
            <key>tsInputModePaletteItemIconKey</key>
            <string>modeMenu1.tif</string>
            <key>tsInputModePaletteItemAltIconKey</key>
            <string>modeMenu1Alt.tif</string>
            <key>tsInputModePaletteItemIDKey</key>
            <integer>2</integer>
        </dict>
    </array>         

******************************************************}


{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemTypeKey CFSTRP('tsInputModePaletteItemTypeKey')}
{$endc}
{ CFNumber - the type of control (0: push button, 1: toggle button, 2: pulldown menu) }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemIconKey CFSTRP('tsInputModePaletteItemIconKey')}
{$endc}
{ CFString - icon file name (File located in IM bundle's resource directory, so this is just the file name, not full path) }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemAltIconKey CFSTRP('tsInputModePaletteItemAltIconKey')}
{$endc}
{ CFString - alternate icon file name (File located in IM bundle's resource directory, so this is just the file name, not full path) }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemStateKey CFSTRP('tsInputModePaletteItemStateKey')}
{$endc}
{ CFNumber - state of the control (0: clear/unpressed, 1: checked/pressed, 2: mixed) }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemEnabledKey CFSTRP('tsInputModePaletteItemEnabledKey')}
{$endc}
{ CFBoolean - enabled state of the control }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemIDKey CFSTRP('tsInputModePaletteItemIDKey')}
{$endc}
{ CFNumber - UInt32 tag ID for control }


{
 *  InputModePaletteItemHit()
 *  
 *  Discussion:
 *    Component Manager call to tell an Input Method that a function
 *    button on the Input Mode Palette was pressed.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inInstance:
 *      The component instance.
 *    
 *    inItemID:
 *      The item ID of the function button pressed on the palette.
 *    
 *    inItemState:
 *      The new state of the button.
 *  
 *  Result:
 *    Return non-null on successful handling of call.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function InputModePaletteItemHit( inInstance: ComponentInstance; inItemID: UInt32; inItemState: UInt32 ): ComponentResult; external name '_InputModePaletteItemHit';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  GetInputModePaletteMenu()
 *  
 *  Discussion:
 *    Component Manager call to ask an Input Method for the menu to
 *    display for a pull-down menu on the Input Mode Palette.
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inInstance:
 *      The component instance.
 *    
 *    inItemID:
 *      The item ID of the pull-down menu button.
 *    
 *    outMenuItemsArray:
 *      The menu array to return to the Input Mode Palette.
 *  
 *  Result:
 *    Return non-null on successful handling of call.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in Carbon.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function GetInputModePaletteMenu( inInstance: ComponentInstance; inItemID: UInt32; var outMenuItemsArray: CFArrayRef ): ComponentResult; external name '_GetInputModePaletteMenu';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{----------------------------------------------------------}
{ CFDictionary keys for menu definition returned           }
{ for kCMGetInputModePaletteMenu/GetInputModePaletteMenu() }
{----------------------------------------------------------}
{******************************************************

Example of menu CFArray returned to GetInputModePaletteMenu():

    Pull-down menus consist of an array of CFDictionary's containing:
        - title
        - ID
        - state
        - enabled
        - key equivalent (optional)
        - key equivalent modifiers (optional)

    <array>    
        <dict>
        <!-- menu item w/ modifiers -->
            <key>tsInputModePaletteItemTitleKey</key>
            <string>menu item 1</string>
            <key>tsInputModePaletteItemIDKey</key>
            <integer>10</integer>
            <key>tsInputModePaletteItemEnabledKey</key>
            <true/>
            <key>tsInputModePaletteItemStateKey</key>
            <integer>0</integer>
            <key>tsInputModePaletteItemKeyEquivalentKey</key>
            <string>j</string>
            <key>tsInputModePaletteItemKeyEquivalentModifiersKey</key>
            <integer>2048</integer>
        </dict>
        <dict>
        <!-- divider -->
            <key>tsInputModePaletteItemTypeKey</key>
            <string>-</string>
        </dict>
        <!-- menu item w/o modifiers -->
            <key>tsInputModePaletteItemTitleKey</key>
            <string>menu item 2</string>
            <key>tsInputModePaletteItemIDKey</key>
            <integer>11</integer>
            <key>tsInputModePaletteItemEnabledKey</key>
            <true/>
            <key>tsInputModePaletteItemStateKey</key>
            <integer>0</integer>
        </dict>
    </array>         

******************************************************}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemTitleKey CFSTRP('tsInputModePaletteItemTitleKey')}
{$endc}
{ CFString - menu item title (use '-' for a separator) }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemKeyEquivalentKey CFSTRP('tsInputModePaletteItemKeyEquivalentKey')}
{$endc}
{ CFString - menu item keyboard shortcut (example: 'j') }

{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kTSInputModePaletteItemKeyEquivalentModifiersKey CFSTRP('tsInputModePaletteItemKeyEquivalentModifiersKey')}
{$endc}
{ CFNumber - menu item keyboard shortcut modifiers (from Events.h) }




end.
