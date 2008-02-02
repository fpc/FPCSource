{
     File:       PMApplication.p
 
     Contains:   Carbon Printing Manager Interfaces.
 
     Version:    Technology: Mac OS X
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved
 
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

unit PMApplication;
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
uses MacTypes,Quickdraw,PMDefinitions,PMCore,Dialogs;


{$ALIGN MAC68K}

{ Callbacks }

type
{$ifc TYPED_FUNCTION_POINTERS}
	PMItemProcPtr = procedure(theDialog: DialogRef; item: SInt16);
{$elsec}
	PMItemProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PMPrintDialogInitProcPtr = procedure(printSettings: PMPrintSettings; var theDialog: PMDialog);
{$elsec}
	PMPrintDialogInitProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PMPageSetupDialogInitProcPtr = procedure(pageFormat: PMPageFormat; var theDialog: PMDialog);
{$elsec}
	PMPageSetupDialogInitProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PMSheetDoneProcPtr = procedure(printSession: PMPrintSession; documentWindow: WindowRef; accepted: boolean);
{$elsec}
	PMSheetDoneProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	PMItemUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PMItemUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PMPrintDialogInitUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PMPrintDialogInitUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PMPageSetupDialogInitUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PMPageSetupDialogInitUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PMSheetDoneUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PMSheetDoneUPP = UniversalProcPtr;
{$endc}	

const
	uppPMItemProcInfo = $000002C0;
	uppPMPrintDialogInitProcInfo = $000003C0;
	uppPMPageSetupDialogInitProcInfo = $000003C0;
	uppPMSheetDoneProcInfo = $000007C0;
	{
	 *  NewPMItemUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewPMItemUPP(userRoutine: PMItemProcPtr): PMItemUPP; external name '_NewPMItemUPP';
{
 *  NewPMPrintDialogInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPMPrintDialogInitUPP(userRoutine: PMPrintDialogInitProcPtr): PMPrintDialogInitUPP; external name '_NewPMPrintDialogInitUPP';
{
 *  NewPMPageSetupDialogInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPMPageSetupDialogInitUPP(userRoutine: PMPageSetupDialogInitProcPtr): PMPageSetupDialogInitUPP; external name '_NewPMPageSetupDialogInitUPP';
{
 *  NewPMSheetDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPMSheetDoneUPP(userRoutine: PMSheetDoneProcPtr): PMSheetDoneUPP; external name '_NewPMSheetDoneUPP';
{
 *  DisposePMItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePMItemUPP(userUPP: PMItemUPP); external name '_DisposePMItemUPP';
{
 *  DisposePMPrintDialogInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePMPrintDialogInitUPP(userUPP: PMPrintDialogInitUPP); external name '_DisposePMPrintDialogInitUPP';
{
 *  DisposePMPageSetupDialogInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePMPageSetupDialogInitUPP(userUPP: PMPageSetupDialogInitUPP); external name '_DisposePMPageSetupDialogInitUPP';
{
 *  DisposePMSheetDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePMSheetDoneUPP(userUPP: PMSheetDoneUPP); external name '_DisposePMSheetDoneUPP';
{
 *  InvokePMItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePMItemUPP(theDialog: DialogRef; item: SInt16; userRoutine: PMItemUPP); external name '_InvokePMItemUPP';
{
 *  InvokePMPrintDialogInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePMPrintDialogInitUPP(printSettings: PMPrintSettings; var theDialog: PMDialog; userRoutine: PMPrintDialogInitUPP); external name '_InvokePMPrintDialogInitUPP';
{
 *  InvokePMPageSetupDialogInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePMPageSetupDialogInitUPP(pageFormat: PMPageFormat; var theDialog: PMDialog; userRoutine: PMPageSetupDialogInitUPP); external name '_InvokePMPageSetupDialogInitUPP';
{
 *  InvokePMSheetDoneUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePMSheetDoneUPP(printSession: PMPrintSession; documentWindow: WindowRef; accepted: boolean; userRoutine: PMSheetDoneUPP); external name '_InvokePMSheetDoneUPP';
{$ifc PM_USE_SESSION_APIS}
{ Print loop }
{
 *  PMSessionBeginDocument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionBeginDocument(printSession: PMPrintSession; printSettings: PMPrintSettings; pageFormat: PMPageFormat): OSStatus; external name '_PMSessionBeginDocument';

{
 *  PMSessionEndDocument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionEndDocument(printSession: PMPrintSession): OSStatus; external name '_PMSessionEndDocument';

{
 *  PMSessionBeginPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionBeginPage(printSession: PMPrintSession; pageFormat: PMPageFormat; pageFrame: PMRectPtr): OSStatus; external name '_PMSessionBeginPage';

{
 *  PMSessionEndPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionEndPage(printSession: PMPrintSession): OSStatus; external name '_PMSessionEndPage';

{ Session Printing Dialogs }
{
 *  PMSessionPageSetupDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPageSetupDialog(printSession: PMPrintSession; pageFormat: PMPageFormat; var accepted: boolean): OSStatus; external name '_PMSessionPageSetupDialog';

{
 *  PMSessionPrintDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPrintDialog(printSession: PMPrintSession; printSettings: PMPrintSettings; constPageFormat: PMPageFormat; var accepted: boolean): OSStatus; external name '_PMSessionPrintDialog';

{
 *  PMSessionPageSetupDialogInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPageSetupDialogInit(printSession: PMPrintSession; pageFormat: PMPageFormat; var newDialog: PMDialog): OSStatus; external name '_PMSessionPageSetupDialogInit';

{
 *  PMSessionPrintDialogInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPrintDialogInit(printSession: PMPrintSession; printSettings: PMPrintSettings; constPageFormat: PMPageFormat; var newDialog: PMDialog): OSStatus; external name '_PMSessionPrintDialogInit';

{
 *  PMSessionPrintDialogMain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPrintDialogMain(printSession: PMPrintSession; printSettings: PMPrintSettings; constPageFormat: PMPageFormat; var accepted: boolean; myInitProc: PMPrintDialogInitUPP): OSStatus; external name '_PMSessionPrintDialogMain';

{
 *  PMSessionPageSetupDialogMain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionPageSetupDialogMain(printSession: PMPrintSession; pageFormat: PMPageFormat; var accepted: boolean; myInitProc: PMPageSetupDialogInitUPP): OSStatus; external name '_PMSessionPageSetupDialogMain';

{**********************}
{  Sheets are not available on classic. }
{**********************}
{
 *  PMSessionUseSheets()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSessionUseSheets(printSession: PMPrintSession; documentWindow: WindowRef; sheetDoneProc: PMSheetDoneUPP): OSStatus; external name '_PMSessionUseSheets';

{$elsec}
{ Print loop }
{
 *  PMBeginDocument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMBeginDocument(printSettings: PMPrintSettings; pageFormat: PMPageFormat; var printContext: PMPrintContext): OSStatus; external name '_PMBeginDocument';

{
 *  PMEndDocument()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMEndDocument(printContext: PMPrintContext): OSStatus; external name '_PMEndDocument';

{
 *  PMBeginPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMBeginPage(printContext: PMPrintContext; const (*var*) pageFrame: PMRect): OSStatus; external name '_PMBeginPage';

{
 *  PMEndPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMEndPage(printContext: PMPrintContext): OSStatus; external name '_PMEndPage';

{ Printing Dialogs }
{
 *  PMPageSetupDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPageSetupDialog(pageFormat: PMPageFormat; var accepted: boolean): OSStatus; external name '_PMPageSetupDialog';

{
 *  PMPrintDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrintDialog(printSettings: PMPrintSettings; constPageFormat: PMPageFormat; var accepted: boolean): OSStatus; external name '_PMPrintDialog';

{
 *  PMPageSetupDialogInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPageSetupDialogInit(pageFormat: PMPageFormat; var newDialog: PMDialog): OSStatus; external name '_PMPageSetupDialogInit';

{**********************}
{  PMPrintDialogInit is not recommended. You should instead use }
{  PMPrintDialogInitWithPageFormat or PMSessionPrintDialogInit }
{**********************}
{
 *  PMPrintDialogInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrintDialogInit(printSettings: PMPrintSettings; var newDialog: PMDialog): OSStatus; external name '_PMPrintDialogInit';

{
 *  PMPrintDialogInitWithPageFormat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrintDialogInitWithPageFormat(printSettings: PMPrintSettings; constPageFormat: PMPageFormat; var newDialog: PMDialog): OSStatus; external name '_PMPrintDialogInitWithPageFormat';

{
 *  PMPrintDialogMain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPrintDialogMain(printSettings: PMPrintSettings; constPageFormat: PMPageFormat; var accepted: boolean; myInitProc: PMPrintDialogInitUPP): OSStatus; external name '_PMPrintDialogMain';

{
 *  PMPageSetupDialogMain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMPageSetupDialogMain(pageFormat: PMPageFormat; var accepted: boolean; myInitProc: PMPageSetupDialogInitUPP): OSStatus; external name '_PMPageSetupDialogMain';

{$endc}  {PM_USE_SESSION_APIS}

{ Printing Dialog accessors }
{
 *  PMGetDialogPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetDialogPtr(pmDialog_: PMDialog; var theDialog: DialogRef): OSStatus; external name '_PMGetDialogPtr';

{
 *  PMGetModalFilterProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetModalFilterProc(pmDialog_: PMDialog; var filterProc: ModalFilterUPP): OSStatus; external name '_PMGetModalFilterProc';

{
 *  PMSetModalFilterProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetModalFilterProc(pmDialog_: PMDialog; filterProc: ModalFilterUPP): OSStatus; external name '_PMSetModalFilterProc';

{
 *  PMGetItemProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetItemProc(pmDialog_: PMDialog; var itemProc: PMItemUPP): OSStatus; external name '_PMGetItemProc';

{
 *  PMSetItemProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetItemProc(pmDialog_: PMDialog; itemProc: PMItemUPP): OSStatus; external name '_PMSetItemProc';

{
 *  PMGetDialogAccepted()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetDialogAccepted(pmDialog_: PMDialog; var process: boolean): OSStatus; external name '_PMGetDialogAccepted';

{
 *  PMSetDialogAccepted()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetDialogAccepted(pmDialog_: PMDialog; process: boolean): OSStatus; external name '_PMSetDialogAccepted';

{
 *  PMGetDialogDone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMGetDialogDone(pmDialog_: PMDialog; var done: boolean): OSStatus; external name '_PMGetDialogDone';

{
 *  PMSetDialogDone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSetDialogDone(pmDialog_: PMDialog; done: boolean): OSStatus; external name '_PMSetDialogDone';

{$ALIGN MAC68K}


end.
