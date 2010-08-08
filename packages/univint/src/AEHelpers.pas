{
     File:       AE/AEHelpers.h
 
     Contains:   AEPrint, AEBuild and AEStream for Carbon
 
     Version:    AppleEvents-496~1
 
     Copyright:  © 1999-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit AEHelpers;
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
	{$setc TARGET_CPU_PPC := FALSE}
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
uses MacTypes,AppleEvents,AEDataModel;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{
 * Originally from AEGIzmos by Jens Alfke, circa 1992.
 }


{$ALIGN MAC68K}

{
 * AEBuild:
 *
 * AEBuild provides a very high level abstraction for building
 * complete AppleEvents and complex ObjectSpeciers.  Using AEBuild it
 * is easy to produce a textual representation of an AEDesc.  The
 * format is similar to the stdio printf call, where meta data is
 * extracted from a format string and used to build the final
 * representation.
 * 
 * For more information on AEBuild and other APIs in AEHelpers, see:
 *     <http://developer.apple.com/technotes/tn/tn2045.html>
 }
{ Syntax Error Codes: }
type
	AEBuildErrorCode = UInt32;
const
	aeBuildSyntaxNoErr = 0;    { (No error) }
	aeBuildSyntaxBadToken = 1;    { Illegal character }
	aeBuildSyntaxBadEOF = 2;    { Unexpected end of format string }
	aeBuildSyntaxNoEOF = 3;    { Unexpected extra stuff past end }
	aeBuildSyntaxBadNegative = 4;    { "-" not followed by digits }
	aeBuildSyntaxMissingQuote = 5;    { Missing close "'" }
	aeBuildSyntaxBadHex = 6;    { Non-digit in hex string }
	aeBuildSyntaxOddHex = 7;    { Odd # of hex digits }
	aeBuildSyntaxNoCloseHex = 8;    { Missing hex quote close "È" }
	aeBuildSyntaxUncoercedHex = 9;    { Hex string must be coerced to a type }
	aeBuildSyntaxNoCloseString = 10;   { Missing close quote }
	aeBuildSyntaxBadDesc = 11;   { Illegal descriptor }
	aeBuildSyntaxBadData = 12;   { Bad data value inside (Ç È) }
	aeBuildSyntaxNoCloseParen = 13;   { Missing ")" after data value }
	aeBuildSyntaxNoCloseBracket = 14;   { Expected "," or "]" }
	aeBuildSyntaxNoCloseBrace = 15;   { Expected "," or ")" }
	aeBuildSyntaxNoKey = 16;   { Missing keyword in record }
	aeBuildSyntaxNoColon = 17;   { Missing ":" after keyword in record }
	aeBuildSyntaxCoercedList = 18;   { Cannot coerce a list }
	aeBuildSyntaxUncoercedDoubleAt = 19;   { "@@" substitution must be coerced }

{ A structure containing error state.}

type
	AEBuildErrorPtr = ^AEBuildError;
	AEBuildError = record
		fError: AEBuildErrorCode;
		fErrorPos: UInt32;
	end;
{
   Create an AEDesc from the format string.  AEBuildError can be NULL, in which case
   no explicit error information will be returned.
}
{
 *  AEBuildDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEBuildDesc( var dst: AEDesc; error: AEBuildErrorPtr { can be NULL }; src: ConstCStringPtr; ... ): OSStatus; external name '_AEBuildDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ varargs version of AEBuildDesc}
{
 *  vAEBuildDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
// function vAEBuildDesc( var dst: AEDesc; error: AEBuildErrorPtr { can be NULL }; src: ConstCStringPtr; args: va_list ): OSStatus;
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER;


{ Append parameters to an existing AppleEvent}
{
 *  AEBuildParameters()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEBuildParameters( var event: AppleEvent; error: AEBuildErrorPtr { can be NULL }; format: ConstCStringPtr; ... ): OSStatus; external name '_AEBuildParameters';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ varargs version of AEBuildParameters}
{
 *  vAEBuildParameters()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
// function vAEBuildParameters( var event: AppleEvent; error: AEBuildErrorPtr { can be NULL }; format: ConstCStringPtr; args: va_list ): OSStatus;
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER;


{ Building an entire Apple event:}
{
 *  AEBuildAppleEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEBuildAppleEvent( theClass: AEEventClass; theID: AEEventID; addressType: DescType; addressData: {const} UnivPtr; addressLength: Size; returnID: SInt16; transactionID: SInt32; var result: AppleEvent; error: AEBuildErrorPtr { can be NULL }; paramsFmt: ConstCStringPtr; ... ): OSStatus; external name '_AEBuildAppleEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ varargs version of AEBuildAppleEvent}
{
 *  vAEBuildAppleEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
// function vAEBuildAppleEvent( theClass: AEEventClass; theID: AEEventID; addressType: DescType; addressData: {const} UnivPtr; addressLength: Size; returnID: SInt16; transactionID: SInt32; var resultEvt: AppleEvent; error: AEBuildErrorPtr { can be NULL }; paramsFmt: ConstCStringPtr; args: va_list ): OSStatus;
// AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER;


{
 * AEPrintDescToHandle
 *
 * AEPrintDescToHandle provides a way to turn an AEDesc into a textual
 * representation.  This is most useful for debugging calls to
 * AEBuildDesc and friends.  The Handle returned should be disposed by
 * the caller.  The size of the handle is the actual number of
 * characters in the string.
 }
{
 *  AEPrintDescToHandle()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEPrintDescToHandle( const (*var*) desc: AEDesc; var result: Handle ): OSStatus; external name '_AEPrintDescToHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 * AEStream:
 *
 * The AEStream interface allows you to build AppleEvents by appending
 * to an opaque structure (an AEStreamRef) and then turning this
 * structure into an AppleEvent.  The basic idea is to open the
 * stream, write data, and then close it - closing it produces an
 * AEDesc, which may be partially complete, or may be a complete
 * AppleEvent.
 }
type
	AEStreamRef = ^SInt32; { an opaque type }
{
   Create and return an AEStreamRef
   Returns NULL on memory allocation failure
}
{
 *  AEStreamOpen()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOpen: AEStreamRef; external name '_AEStreamOpen';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Closes and disposes of an AEStreamRef, producing
   results in the desc.  You must dispose of the desc yourself.
   If you just want to dispose of the AEStreamRef, you can pass NULL for desc.
}
{
 *  AEStreamClose()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamClose( ref: AEStreamRef; var desc: AEDesc ): OSStatus; external name '_AEStreamClose';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Prepares an AEStreamRef for appending data to a newly created desc.
   You append data with AEStreamWriteData
}
{
 *  AEStreamOpenDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOpenDesc( ref: AEStreamRef; newType: DescType ): OSStatus; external name '_AEStreamOpenDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Append data to the previously opened desc.}
{
 *  AEStreamWriteData()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamWriteData( ref: AEStreamRef; data: {const} UnivPtr; length: Size ): OSStatus; external name '_AEStreamWriteData';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Finish a desc.  After this, you can close the stream, or adding new
   descs, if you're assembling a list.
}
{
 *  AEStreamCloseDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamCloseDesc( ref: AEStreamRef ): OSStatus; external name '_AEStreamCloseDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Write data as a desc to the stream}
{
 *  AEStreamWriteDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamWriteDesc( ref: AEStreamRef; newType: DescType; data: {const} UnivPtr; length: Size ): OSStatus; external name '_AEStreamWriteDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Write an entire desc to the stream}
{
 *  AEStreamWriteAEDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamWriteAEDesc( ref: AEStreamRef; const (*var*) desc: AEDesc ): OSStatus; external name '_AEStreamWriteAEDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Begin a list.  You can then append to the list by doing
   AEStreamOpenDesc, or AEStreamWriteDesc.
}
{
 *  AEStreamOpenList()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOpenList( ref: AEStreamRef ): OSStatus; external name '_AEStreamOpenList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Finish a list.}
{
 *  AEStreamCloseList()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamCloseList( ref: AEStreamRef ): OSStatus; external name '_AEStreamCloseList';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Begin a record.  A record usually has type 'reco', however, this is
   rather generic, and frequently a different type is used.
}
{
 *  AEStreamOpenRecord()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOpenRecord( ref: AEStreamRef; newType: DescType ): OSStatus; external name '_AEStreamOpenRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Change the type of a record.}
{
 *  AEStreamSetRecordType()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamSetRecordType( ref: AEStreamRef; newType: DescType ): OSStatus; external name '_AEStreamSetRecordType';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Finish a record}
{
 *  AEStreamCloseRecord()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamCloseRecord( ref: AEStreamRef ): OSStatus; external name '_AEStreamCloseRecord';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Add a keyed descriptor to a record.  This is analogous to AEPutParamDesc.
   it can only be used when writing to a record.
}
{
 *  AEStreamWriteKeyDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamWriteKeyDesc( ref: AEStreamRef; key: AEKeyword; newType: DescType; data: {const} UnivPtr; length: Size ): OSStatus; external name '_AEStreamWriteKeyDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   OpenDesc for a keyed record entry.  You can use AEStreamWriteData
   after opening a keyed desc.
}
{
 *  AEStreamOpenKeyDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOpenKeyDesc( ref: AEStreamRef; key: AEKeyword; newType: DescType ): OSStatus; external name '_AEStreamOpenKeyDesc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Write a key to the stream - you can follow this with an AEWriteDesc.}
{
 *  AEStreamWriteKey()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamWriteKey( ref: AEStreamRef; key: AEKeyword ): OSStatus; external name '_AEStreamWriteKey';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   Create a complete AppleEvent.  This creates and returns a new stream.
   Use this call to populate the meta fields in an AppleEvent record.
   After this, you can add your records, lists and other parameters.
}
{
 *  AEStreamCreateEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamCreateEvent( clazz: AEEventClass; id: AEEventID; targetType: DescType; targetData: {const} UnivPtr; targetLength: Size; returnID: SInt16; transactionID: SInt32 ): AEStreamRef; external name '_AEStreamCreateEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
   This call lets you augment an existing AppleEvent using the stream
   APIs.  This would be useful, for example, in constructing the reply
   record in an AppleEvent handler.  Note that AEStreamOpenEvent will
   consume the AppleEvent passed in - you can't access it again until the
   stream is closed.  When you're done building the event, AEStreamCloseStream
    will reconstitute it.
}
{
 *  AEStreamOpenEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOpenEvent( var event: AppleEvent ): AEStreamRef; external name '_AEStreamOpenEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ Mark a keyword as being an optional parameter.}
{
 *  AEStreamOptionalParam()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function AEStreamOptionalParam( ref: AEStreamRef; key: AEKeyword ): OSStatus; external name '_AEStreamOptionalParam';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
