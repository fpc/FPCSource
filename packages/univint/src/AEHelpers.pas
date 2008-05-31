{
     File:       AEHelpers.p
 
     Contains:   AEPrint, AEBuild and AEStream for Carbon
 
     Version:    Technology: Mac OS X, CarbonLib
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1999-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{
 * Originally from AEGIzmos by Jens Alfke, circa 1992.
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

unit AEHelpers;
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
uses MacTypes,AppleEvents,AEDataModel;

{$ALIGN MAC68K}

{
 * AEBuild is only available for C programmers.
 }
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
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPrintDescToHandle(const (*var*) desc: AEDesc; var result: Handle): OSStatus; external name '_AEPrintDescToHandle';

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
	AEStreamRef    = ^SInt32; { an opaque 32-bit type }
	AEStreamRefPtr = ^AEStreamRef;  { when a var xx:AEStreamRef parameter can be nil, it is changed to xx: AEStreamRefPtr }
	{
	   Create and return an AEStreamRef
	   Returns NULL on memory allocation failure
	}
	{
	 *  AEStreamOpen()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function AEStreamOpen: AEStreamRef; external name '_AEStreamOpen';

{
   Closes and disposes of an AEStreamRef, producing
   results in the desc.  You must dispose of the desc yourself.
   If you just want to dispose of the AEStreamRef, you can pass NULL for desc.
}
{
 *  AEStreamClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamClose(ref: AEStreamRef; var desc: AEDesc): OSStatus; external name '_AEStreamClose';

{
   Prepares an AEStreamRef for appending data to a newly created desc.
   You append data with AEStreamWriteData
}
{
 *  AEStreamOpenDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamOpenDesc(ref: AEStreamRef; newType: DescType): OSStatus; external name '_AEStreamOpenDesc';

{  Append data to the previously opened desc. }
{
 *  AEStreamWriteData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamWriteData(ref: AEStreamRef; data: UnivPtr; length: Size): OSStatus; external name '_AEStreamWriteData';

{
   Finish a desc.  After this, you can close the stream, or adding new
   descs, if you're assembling a list.
}
{
 *  AEStreamCloseDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamCloseDesc(ref: AEStreamRef): OSStatus; external name '_AEStreamCloseDesc';

{  Write data as a desc to the stream }
{
 *  AEStreamWriteDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamWriteDesc(ref: AEStreamRef; newType: DescType; data: UnivPtr; length: Size): OSStatus; external name '_AEStreamWriteDesc';

{  Write an entire desc to the stream }
{
 *  AEStreamWriteAEDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamWriteAEDesc(ref: AEStreamRef; const (*var*) desc: AEDesc): OSStatus; external name '_AEStreamWriteAEDesc';

{
   Begin a list.  You can then append to the list by doing
   AEStreamOpenDesc, or AEStreamWriteDesc.
}
{
 *  AEStreamOpenList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamOpenList(ref: AEStreamRef): OSStatus; external name '_AEStreamOpenList';

{  Finish a list. }
{
 *  AEStreamCloseList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamCloseList(ref: AEStreamRef): OSStatus; external name '_AEStreamCloseList';

{
   Begin a record.  A record usually has type 'reco', however, this is
   rather generic, and frequently a different type is used.
}
{
 *  AEStreamOpenRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamOpenRecord(ref: AEStreamRef; newType: DescType): OSStatus; external name '_AEStreamOpenRecord';

{  Change the type of a record. }
{
 *  AEStreamSetRecordType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamSetRecordType(ref: AEStreamRef; newType: DescType): OSStatus; external name '_AEStreamSetRecordType';

{  Finish a record }
{
 *  AEStreamCloseRecord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamCloseRecord(ref: AEStreamRef): OSStatus; external name '_AEStreamCloseRecord';

{
   Add a keyed descriptor to a record.  This is analogous to AEPutParamDesc.
   it can only be used when writing to a record.
}
{
 *  AEStreamWriteKeyDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamWriteKeyDesc(ref: AEStreamRef; key: AEKeyword; newType: DescType; data: UnivPtr; length: Size): OSStatus; external name '_AEStreamWriteKeyDesc';

{
   OpenDesc for a keyed record entry.  You can youse AEStreamWriteData
   after opening a keyed desc.
}
{
 *  AEStreamOpenKeyDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamOpenKeyDesc(ref: AEStreamRef; key: AEKeyword; newType: DescType): OSStatus; external name '_AEStreamOpenKeyDesc';

{  Write a key to the stream - you can follow this with an AEWriteDesc. }
{
 *  AEStreamWriteKey()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamWriteKey(ref: AEStreamRef; key: AEKeyword): OSStatus; external name '_AEStreamWriteKey';

{
   Create a complete AppleEvent.  This creates and returns a new stream.
   Use this call to populate the meta fields in an AppleEvent record.
   After this, you can add your records, lists and other parameters.
}
{
 *  AEStreamCreateEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamCreateEvent(clazz: AEEventClass; id: AEEventID; targetType: DescType; targetData: UnivPtr; targetLength: SInt32; returnID: SInt16; transactionID: SInt32): AEStreamRef; external name '_AEStreamCreateEvent';

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
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamOpenEvent(var event: AppleEvent): AEStreamRef; external name '_AEStreamOpenEvent';

{  Mark a keyword as being an optional parameter. }
{
 *  AEStreamOptionalParam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEStreamOptionalParam(ref: AEStreamRef; key: AEKeyword): OSStatus; external name '_AEStreamOptionalParam';

{$ALIGN MAC68K}


end.
