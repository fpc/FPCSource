{
     File:       CMCalibrator.p
 
     Contains:   ColorSync Calibration API
 
     Version:    Technology: ColorSync 2.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit CMCalibrator;
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
uses MacTypes,CMTypes,CMApplication,Events;


{$ALIGN MAC68K}


type
{$ifc TYPED_FUNCTION_POINTERS}
	CalibrateEventProcPtr = procedure(var event: EventRecord);
{$elsec}
	CalibrateEventProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	CalibrateEventUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CalibrateEventUPP = UniversalProcPtr;
{$endc}	

	{  Interface for new ColorSync monitor calibrators (ColorSync 2.6 and greater)  }


const
	kCalibratorNamePrefix		= $63616C69 (* 'cali' *);


type
	CalibratorInfoPtr = ^CalibratorInfo;
	CalibratorInfo = record
		dataSize:				UInt32;									{  Size of this structure - compatibility  }
		displayID:				CMDisplayIDType;						{  Contains an hDC on Win32  }
		profileLocationSize:	UInt32;									{  Max size for returned profile location  }
		profileLocationPtr:		CMProfileLocationPtr;					{  For returning the profile  }
		eventProc:				CalibrateEventUPP;						{  Ignored on Win32  }
		isGood:					boolean;								{  true or false  }
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	CanCalibrateProcPtr = function(displayID: CMDisplayIDType; var errMessage: Str255): boolean;
{$elsec}
	CanCalibrateProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CalibrateProcPtr = function(var theInfo: CalibratorInfo): OSErr;
{$elsec}
	CalibrateProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	CanCalibrateUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CanCalibrateUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CalibrateUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CalibrateUPP = UniversalProcPtr;
{$endc}	

const
	uppCalibrateEventProcInfo = $000000C0;
	uppCanCalibrateProcInfo = $000003D0;
	uppCalibrateProcInfo = $000000E0;
	{
	 *  NewCalibrateEventUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewCalibrateEventUPP(userRoutine: CalibrateEventProcPtr): CalibrateEventUPP; external name '_NewCalibrateEventUPP'; { old name was NewCalibrateEventProc }
{
 *  NewCanCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NewCanCalibrateUPP(userRoutine: CanCalibrateProcPtr): CanCalibrateUPP; external name '_NewCanCalibrateUPP'; { old name was NewCanCalibrateProc }
{
 *  NewCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NewCalibrateUPP(userRoutine: CalibrateProcPtr): CalibrateUPP; external name '_NewCalibrateUPP'; { old name was NewCalibrateProc }
{
 *  DisposeCalibrateEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCalibrateEventUPP(userUPP: CalibrateEventUPP); external name '_DisposeCalibrateEventUPP';
{
 *  DisposeCanCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure DisposeCanCalibrateUPP(userUPP: CanCalibrateUPP); external name '_DisposeCanCalibrateUPP';
{
 *  DisposeCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
procedure DisposeCalibrateUPP(userUPP: CalibrateUPP); external name '_DisposeCalibrateUPP';
{
 *  InvokeCalibrateEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeCalibrateEventUPP(var event: EventRecord; userRoutine: CalibrateEventUPP); external name '_InvokeCalibrateEventUPP'; { old name was CallCalibrateEventProc }
{
 *  InvokeCanCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function InvokeCanCalibrateUPP(displayID: CMDisplayIDType; var errMessage: Str255; userRoutine: CanCalibrateUPP): boolean; external name '_InvokeCanCalibrateUPP'; { old name was CallCanCalibrateProc }
{
 *  InvokeCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function InvokeCalibrateUPP(var theInfo: CalibratorInfo; userRoutine: CalibrateUPP): OSErr; external name '_InvokeCalibrateUPP'; { old name was CallCalibrateProc }
{
 *  CMCalibrateDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function CMCalibrateDisplay(var theInfo: CalibratorInfo): OSErr; external name '_CMCalibrateDisplay';


{$ifc OLDROUTINENAMES}
{  Interface for original ColorSync monitor calibrators (ColorSync 2.5.x)  }

const
	kOldCalibratorNamePrefix	= $43616C69 (* 'Cali' *);


type
	OldCalibratorInfoPtr = ^OldCalibratorInfo;
	OldCalibratorInfo = record
		displayID:				CMDisplayIDType;						{  Contains an hDC on Win32  }
		profileLocation:		CMProfileLocation;
		eventProc:				CalibrateEventUPP;						{  Ignored on Win32  }
		reserved:				UInt32;									{  Unused  }
		flags:					UInt32;									{  Unused  }
		isGood:					boolean;								{  true or false  }
		byteFiller:				SInt8;									{  Unused  }
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	OldCanCalibrateProcPtr = function(displayID: CMDisplayIDType): boolean;
{$elsec}
	OldCanCalibrateProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OldCalibrateProcPtr = function(var theInfo: OldCalibratorInfo): OSErr;
{$elsec}
	OldCalibrateProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	OldCanCalibrateUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OldCanCalibrateUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OldCalibrateUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OldCalibrateUPP = UniversalProcPtr;
{$endc}	

const
	uppOldCanCalibrateProcInfo = $000000D0;
	uppOldCalibrateProcInfo = $000000E0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewOldCanCalibrateUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewOldCanCalibrateUPP(userRoutine: OldCanCalibrateProcPtr): OldCanCalibrateUPP; external name '_NewOldCanCalibrateUPP'; { old name was NewOldCanCalibrateProc }
{
 *  NewOldCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewOldCalibrateUPP(userRoutine: OldCalibrateProcPtr): OldCalibrateUPP; external name '_NewOldCalibrateUPP'; { old name was NewOldCalibrateProc }
{
 *  DisposeOldCanCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeOldCanCalibrateUPP(userUPP: OldCanCalibrateUPP); external name '_DisposeOldCanCalibrateUPP';
{
 *  DisposeOldCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposeOldCalibrateUPP(userUPP: OldCalibrateUPP); external name '_DisposeOldCalibrateUPP';
{
 *  InvokeOldCanCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokeOldCanCalibrateUPP(displayID: CMDisplayIDType; userRoutine: OldCanCalibrateUPP): boolean; external name '_InvokeOldCanCalibrateUPP'; { old name was CallOldCanCalibrateProc }
{
 *  InvokeOldCalibrateUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokeOldCalibrateUPP(var theInfo: OldCalibratorInfo; userRoutine: OldCalibrateUPP): OSErr; external name '_InvokeOldCalibrateUPP'; { old name was CallOldCalibrateProc }
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}

{$ALIGN MAC68K}


end.
