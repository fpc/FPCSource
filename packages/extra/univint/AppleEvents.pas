{
     File:       AppleEvents.p
 
     Contains:   AppleEvent Package Interfaces.
 
     Version:    Technology: System 7.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1989-2002 by Apple Computer, Inc., all rights reserved
 
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

unit AppleEvents;
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
uses MacTypes,MixedMode,AEDataModel,AEInteraction;
{
    Note:   The functions and types for the building and parsing AppleEvent  
            messages has moved to AEDataModel.h
}

{
    Note:   The functions for interacting with events has moved to AEInteraction.h
}

{$ALIGN MAC68K}


const
																{  Keywords for Apple event parameters  }
	keyDirectObject				= $2D2D2D2D (* '----' *);
	keyErrorNumber				= $6572726E (* 'errn' *);
	keyErrorString				= $65727273 (* 'errs' *);
	keyProcessSerialNumber		= $70736E20 (* 'psn ' *);						{  Keywords for special handlers  }
	keyPreDispatch				= $70686163 (* 'phac' *);						{  preHandler accessor call  }
	keySelectProc				= $73656C68 (* 'selh' *);						{  more selector call  }
																{  Keyword for recording  }
	keyAERecorderCount			= $72656372 (* 'recr' *);						{  available only in vers 1.0.1 and greater  }
																{  Keyword for version information  }
	keyAEVersion				= $76657273 (* 'vers' *);						{  available only in vers 1.0.1 and greater  }

	{	 Event Class 	}
	kCoreEventClass				= $61657674 (* 'aevt' *);

	{	 Event IDÕs 	}
	kAEOpenApplication			= $6F617070 (* 'oapp' *);
	kAEOpenDocuments			= $6F646F63 (* 'odoc' *);
	kAEPrintDocuments			= $70646F63 (* 'pdoc' *);
	kAEQuitApplication			= $71756974 (* 'quit' *);
	kAEAnswer					= $616E7372 (* 'ansr' *);
	kAEApplicationDied			= $6F626974 (* 'obit' *);
	kAEShowPreferences			= $70726566 (* 'pref' *);						{  sent by Mac OS X when the user chooses the Preferences item  }

	{	 Constants for recording 	}
	kAEStartRecording			= $72656361 (* 'reca' *);						{  available only in vers 1.0.1 and greater  }
	kAEStopRecording			= $72656363 (* 'recc' *);						{  available only in vers 1.0.1 and greater  }
	kAENotifyStartRecording		= $72656331 (* 'rec1' *);						{  available only in vers 1.0.1 and greater  }
	kAENotifyStopRecording		= $72656330 (* 'rec0' *);						{  available only in vers 1.0.1 and greater  }
	kAENotifyRecording			= $72656372 (* 'recr' *);						{  available only in vers 1.0.1 and greater  }


	{	
	 * AEEventSource is defined as an SInt8 for compatability with pascal.
	 * Important note: keyEventSourceAttr is returned by AttributePtr as a typeShortInteger.
	 * Be sure to pass at least two bytes of storage to AEGetAttributePtr - the result can be
	 * compared directly against the following enums.
	 	}

type
	AEEventSource 				= SInt8;
const
	kAEUnknownSource			= 0;
	kAEDirectCall				= 1;
	kAESameProcess				= 2;
	kAELocalProcess				= 3;
	kAERemoteProcess			= 4;

	{	*************************************************************************
	  These calls are used to set up and modify the event dispatch table.
	*************************************************************************	}
	{
	 *  AEInstallEventHandler()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function AEInstallEventHandler(theAEEventClass: AEEventClass; theAEEventID: AEEventID; handler: AEEventHandlerUPP; handlerRefcon: SInt32; isSysHandler: boolean): OSErr; external name '_AEInstallEventHandler';
{
 *  AERemoveEventHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AERemoveEventHandler(theAEEventClass: AEEventClass; theAEEventID: AEEventID; handler: AEEventHandlerUPP; isSysHandler: boolean): OSErr; external name '_AERemoveEventHandler';
{
 *  AEGetEventHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetEventHandler(theAEEventClass: AEEventClass; theAEEventID: AEEventID; var handler: AEEventHandlerUPP; var handlerRefcon: SInt32; isSysHandler: boolean): OSErr; external name '_AEGetEventHandler';
{*************************************************************************
  These calls are used to set up and modify special hooks into the
  AppleEvent manager.
*************************************************************************}
{
 *  AEInstallSpecialHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEInstallSpecialHandler(functionClass: AEKeyword; handler: AEEventHandlerUPP; isSysHandler: boolean): OSErr; external name '_AEInstallSpecialHandler';
{
 *  AERemoveSpecialHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AERemoveSpecialHandler(functionClass: AEKeyword; handler: AEEventHandlerUPP; isSysHandler: boolean): OSErr; external name '_AERemoveSpecialHandler';
{
 *  AEGetSpecialHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetSpecialHandler(functionClass: AEKeyword; var handler: AEEventHandlerUPP; isSysHandler: boolean): OSErr; external name '_AEGetSpecialHandler';
{*************************************************************************
  This call was added in version 1.0.1. If called with the keyword
  keyAERecorderCount ('recr'), the number of recorders that are
  currently active is returned in 'result'
  (available only in vers 1.0.1 and greater).
*************************************************************************}
{
 *  AEManagerInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEManagerInfo(keyWord: AEKeyword; var result: SInt32): OSErr; external name '_AEManagerInfo';
{$ALIGN MAC68K}


end.
