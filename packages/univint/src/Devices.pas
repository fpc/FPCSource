{
     File:       Devices.p
 
     Contains:   Device Manager Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
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

unit Devices;
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
uses MacTypes,OSUtils,Files,Quickdraw,NameRegistry,CodeFragments,Multiprocessing,DriverFamilyMatching;

{$ALIGN MAC68K}

{ Values of the 'message' parameter to a Chooser device package }

const
	chooserInitMsg				= 11;							{  the user selected this device package  }
	newSelMsg					= 12;							{  the user made new device selections  }
	fillListMsg					= 13;							{  fill the device list with choices  }
	getSelMsg					= 14;							{  mark one or more choices as selected  }
	selectMsg					= 15;							{  the user made a selection  }
	deselectMsg					= 16;							{  the user canceled a selection  }
	terminateMsg				= 17;							{  allows device package to clean up  }
	buttonMsg					= 19;							{  the user selected a button  }


	{	 Values of the 'caller' parameter to a Chooser device package 	}
	chooserID					= 1;


	{	 Values of the 'message' parameter to a Monitor 'mntr' 	}
	initMsg						= 1;							{ initialization }
	okMsg						= 2;							{ user clicked OK button }
	cancelMsg					= 3;							{ user clicked Cancel button }
	hitMsg						= 4;							{ user clicked control in Options dialog }
	nulMsg						= 5;							{ periodic event }
	updateMsg					= 6;							{ update event }
	activateMsg					= 7;							{ not used }
	deactivateMsg				= 8;							{ not used }
	keyEvtMsg					= 9;							{ keyboard event }
	superMsg					= 10;							{ show superuser controls }
	normalMsg					= 11;							{ show only normal controls }
	startupMsg					= 12;							{ code has been loaded }


	{	 control codes for DeskAccessories 	}
	goodbye						= -1;							{  heap being reinitialized  }
	killCode					= 1;							{  KillIO requested  }
	accEvent					= 64;							{  handle an event  }
	accRun						= 65;							{  time for periodic action  }
	accCursor					= 66;							{  change cursor shape  }
	accMenu						= 67;							{  handle menu item  }
	accUndo						= 68;							{  handle undo command  }
	accCut						= 70;							{  handle cut command  }
	accCopy						= 71;							{  handle copy command  }
	accPaste					= 72;							{  handle paste command  }
	accClear					= 73;							{  handle clear command  }

	{	 Control/Status Call Codes 	}
	{	 drvStsCode, ejectCode and tgBuffCode are now defined in Disks.h/p/a 	}

	{	 miscellaneous Device Manager constants 	}
	ioInProgress				= 1;							{  predefined value of ioResult while I/O is pending  }
	aRdCmd						= 2;							{  low byte of ioTrap for Read calls  }
	aWrCmd						= 3;							{  low byte of ioTrap for Write calls  }
	asyncTrpBit					= 10;							{  trap word modifier  }
	noQueueBit					= 9;							{  trap word modifier  }

	{	 flags used in the driver header and device control entry 	}
	dReadEnable					= 0;							{  set if driver responds to read requests  }
	dWritEnable					= 1;							{  set if driver responds to write requests  }
	dCtlEnable					= 2;							{  set if driver responds to control requests  }
	dStatEnable					= 3;							{  set if driver responds to status requests  }
	dNeedGoodBye				= 4;							{  set if driver needs time for performing periodic tasks  }
	dNeedTime					= 5;							{  set if driver needs time for performing periodic tasks  }
	dNeedLock					= 6;							{  set if driver must be locked in memory as soon as it is opened  }

	dNeedLockMask				= $4000;						{  set if driver must be locked in memory as soon as it is opened  }
	dNeedTimeMask				= $2000;						{  set if driver needs time for performing periodic tasks  }
	dNeedGoodByeMask			= $1000;						{  set if driver needs to be called before the application heap is initialized  }
	dStatEnableMask				= $0800;						{  set if driver responds to status requests  }
	dCtlEnableMask				= $0400;						{  set if driver responds to control requests  }
	dWritEnableMask				= $0200;						{  set if driver responds to write requests  }
	dReadEnableMask				= $0100;						{  set if driver responds to read requests  }


	{	 run-time flags used in the device control entry 	}
	dVMImmuneBit				= 0;							{  driver does not need VM protection  }
	dOpened						= 5;							{  driver is open  }
	dRAMBased					= 6;							{  dCtlDriver is a handle (1) or pointer (0)  }
	drvrActive					= 7;							{  driver is currently processing a request  }

	dVMImmuneMask				= $0001;						{  driver does not need VM protection  }
	dOpenedMask					= $0020;						{  driver is open  }
	dRAMBasedMask				= $0040;						{  dCtlDriver is a handle (1) or pointer (0)  }
	drvrActiveMask				= $0080;						{  driver is currently processing a request  }


type
	DRVRHeaderPtr = ^DRVRHeader;
	DRVRHeader = record
		drvrFlags:				SInt16;
		drvrDelay:				SInt16;
		drvrEMask:				SInt16;
		drvrMenu:				SInt16;
		drvrOpen:				SInt16;
		drvrPrime:				SInt16;
		drvrCtl:				SInt16;
		drvrStatus:				SInt16;
		drvrClose:				SInt16;
		drvrName:				SInt8;
	end;

	DRVRHeaderHandle					= ^DRVRHeaderPtr;
	DCtlEntryPtr = ^DCtlEntry;
	DCtlEntry = record
		dCtlDriver:				Ptr;
		dCtlFlags:				SInt16;
		dCtlQHdr:				QHdr;
		dCtlPosition:			SInt32;
		dCtlStorage:			Handle;
		dCtlRefNum:				SInt16;
		dCtlCurTicks:			SInt32;
		dCtlWindow:				GrafPtr;
		dCtlDelay:				SInt16;
		dCtlEMask:				SInt16;
		dCtlMenu:				SInt16;
	end;

	DCtlPtr								= ^DCtlEntry;
	DCtlHandle							= ^DCtlPtr;
	AuxDCEPtr = ^AuxDCE;
	AuxDCE = packed record
		dCtlDriver:				Ptr;
		dCtlFlags:				SInt16;
		dCtlQHdr:				QHdr;
		dCtlPosition:			SInt32;
		dCtlStorage:			Handle;
		dCtlRefNum:				SInt16;
		dCtlCurTicks:			SInt32;
		dCtlWindow:				GrafPtr;
		dCtlDelay:				SInt16;
		dCtlEMask:				SInt16;
		dCtlMenu:				SInt16;
		dCtlSlot:				SInt8;
		dCtlSlotId:				SInt8;
		dCtlDevBase:			SInt32;
		dCtlOwner:				Ptr;
		dCtlExtDev:				SInt8;
		fillByte:				SInt8;
		dCtlNodeID:				UInt32;
	end;

	AuxDCEHandle						= ^AuxDCEPtr;
	{	  The NDRV Driver IO Entry Point and Commands 	}
	UnitNumber							= UInt16;
	DriverOpenCount						= UInt32;
	DriverRefNum						= SInt16;
	DriverFlags							= SInt16;
	IOCommandCode						= UInt32;

const
	kOpenCommand				= 0;
	kCloseCommand				= 1;
	kReadCommand				= 2;
	kWriteCommand				= 3;
	kControlCommand				= 4;
	kStatusCommand				= 5;
	kKillIOCommand				= 6;
	kInitializeCommand			= 7;							{  init driver and device }
	kFinalizeCommand			= 8;							{  shutdown driver and device }
	kReplaceCommand				= 9;							{  replace an old driver }
	kSupersededCommand			= 10;							{  prepare to be replaced by a new driver }
	kSuspendCommand				= 11;							{  prepare driver to go to sleep }
	kResumeCommand				= 12;							{  wake up sleeping driver }

																{  one more IOCommandCode }
	kPowerManagementCommand		= 13;							{  power management command, supercedes kSuspendCommand and kResumeCommand }


type
	AddressSpaceID						= MPAddressSpaceID;
	IOCommandID    = ^SInt32; { an opaque 32-bit type }
	IOCommandIDPtr = ^IOCommandID;  { when a var xx:IOCommandID parameter can be nil, it is changed to xx: IOCommandIDPtr }
	IOCommandKind						= UInt32;

const
	kSynchronousIOCommandKind	= $00000001;
	kAsynchronousIOCommandKind	= $00000002;
	kImmediateIOCommandKind		= $00000004;


type
	DriverInitInfoPtr = ^DriverInitInfo;
	DriverInitInfo = record
		refNum:					DriverRefNum;
		deviceEntry:			RegEntryID;
	end;

	DriverReplaceInfo					= DriverInitInfo;
	DriverReplaceInfoPtr 				= ^DriverReplaceInfo;
	DriverFinalInfoPtr = ^DriverFinalInfo;
	DriverFinalInfo = record
		refNum:					DriverRefNum;
		deviceEntry:			RegEntryID;
	end;

	DriverSupersededInfo				= DriverFinalInfo;
	DriverSupersededInfoPtr 			= ^DriverSupersededInfo;

	{  Contents are command specific }

	IOCommandContentsPtr = ^IOCommandContents;
	IOCommandContents = record
		case SInt16 of
		0: (
			pb:					ParmBlkPtr;
			);
		1: (
			initialInfo:		DriverInitInfoPtr;
			);
		2: (
			finalInfo:			DriverFinalInfoPtr;
			);
		3: (
			replaceInfo:		DriverReplaceInfoPtr;
			);
		4: (
			supersededInfo:		DriverSupersededInfoPtr;
			);
	end;

{$ifc TYPED_FUNCTION_POINTERS}
	DriverEntryPointPtr = function(SpaceID: AddressSpaceID; CommandID: IOCommandID; Contents: IOCommandContents; Code: IOCommandCode; Kind: IOCommandKind): OSErr;
{$elsec}
	DriverEntryPointPtr = ProcPtr;
{$endc}

	{	 Record to describe a file-based driver candidate 	}
	FileBasedDriverRecordPtr = ^FileBasedDriverRecord;
	FileBasedDriverRecord = record
		theSpec:				FSSpec;									{  file specification }
		theType:				MacDriverType;							{  nameInfoStr + version number }
		compatibleProp:			boolean;								{  true if matched using a compatible name }
		pad1,pad2,pad3:			SInt8;							{  alignment }
	end;

	{	 Detailed Record to describe a file-based driver candidate. Includes fragment name 	}
	FileBasedDriverDetailedPtr = ^FileBasedDriverDetailed;
	FileBasedDriverDetailed = record
		fileBasedDriver:		FileBasedDriverRecord;
		fragName:				Str63;
	end;

	{	 Driver Loader API 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  HigherDriverVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function HigherDriverVersion(const (*var*) driverVersion1: NumVersion; const (*var*) driverVersion2: NumVersion): SInt16; external name '_HigherDriverVersion';

{
 *  VerifyFragmentAsDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function VerifyFragmentAsDriver(fragmentConnID: CFragConnectionID; var fragmentMain: DriverEntryPointPtr; var driverDesc: DriverDescriptionPtr): OSErr; external name '_VerifyFragmentAsDriver';

{
 *  GetDriverMemoryFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDriverMemoryFragment(memAddr: Ptr; length: SInt32; const (*var*) fragName: Str63; var fragmentConnID: CFragConnectionID; var fragmentMain: DriverEntryPointPtr; var driverDesc: DriverDescriptionPtr): OSErr; external name '_GetDriverMemoryFragment';

{
 *  GetDriverDiskFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDriverDiskFragment(fragmentSpec: FSSpecPtr; var fragmentConnID: CFragConnectionID; var fragmentMain: DriverEntryPointPtr; var driverDesc: DriverDescriptionPtr): OSErr; external name '_GetDriverDiskFragment';

{
 *  GetNamedDriverDiskFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetNamedDriverDiskFragment(fragmentSpec: FSSpecPtr; const (*var*) fragName: Str63; var fragmentConnID: CFragConnectionID; var fragmentMain: DriverEntryPointPtr; var driverDesc: DriverDescriptionPtr): OSErr; external name '_GetNamedDriverDiskFragment';

{
 *  InstallDriverFromFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InstallDriverFromFragment(fragmentConnID: CFragConnectionID; var device: RegEntryID; beginningUnit: UnitNumber; endingUnit: UnitNumber; var refNum: DriverRefNum): OSErr; external name '_InstallDriverFromFragment';

{
 *  InstallDriverFromFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InstallDriverFromFile(fragmentSpec: FSSpecPtr; var device: RegEntryID; beginningUnit: UnitNumber; endingUnit: UnitNumber; var refNum: DriverRefNum): OSErr; external name '_InstallDriverFromFile';

{
 *  InstallDriverFromMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InstallDriverFromMemory(memory: Ptr; length: SInt32; const (*var*) fragName: Str63; var device: RegEntryID; beginningUnit: UnitNumber; endingUnit: UnitNumber; var refNum: DriverRefNum): OSErr; external name '_InstallDriverFromMemory';

{
 *  InstallDriverFromResource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InstallDriverFromResource(theRsrcID: SInt16; const (*var*) theRsrcName: Str255; theDevice: RegEntryIDPtr; theBeginningUnit: UnitNumber; theEndingUnit: UnitNumber; var theRefNum: DriverRefNum): OSErr; external name '_InstallDriverFromResource';

{
 *  InstallDriverFromDisk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InstallDriverFromDisk(theDriverName: Ptr; var theDevice: RegEntryID; theBeginningUnit: UnitNumber; theEndingUnit: UnitNumber; var theRefNum: DriverRefNum): OSErr; external name '_InstallDriverFromDisk';

{
 *  FindDriversForDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindDriversForDevice(var device: RegEntryID; var fragmentSpec: FSSpec; var fileDriverDesc: DriverDescription; var memAddr: Ptr; var length: SInt32; fragName: StringPtr; var memDriverDesc: DriverDescription): OSErr; external name '_FindDriversForDevice';

{
 *  FindDriverForDeviceFromFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindDriverForDeviceFromFile(var device: RegEntryID; var fragmentSpec: FSSpec; var driverDesc: DriverDescription; fragName: StringPtr): OSErr; external name '_FindDriverForDeviceFromFile';

{
 *  FindDriverCandidates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindDriverCandidates(var deviceID: RegEntryID; var propBasedDriver: Ptr; var propBasedDriverSize: RegPropertyValueSize; deviceName: StringPtr; var propBasedDriverType: MacDriverType; var gotPropBasedDriver: boolean; fileBasedDrivers: FileBasedDriverRecordPtr; var nFileBasedDrivers: ItemCount): OSErr; external name '_FindDriverCandidates';

{
 *  FindDriverCandidatesDetailed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindDriverCandidatesDetailed(deviceID: RegEntryIDPtr; var propBasedDriver: Ptr; var propBasedDriverSize: RegPropertyValueSize; deviceName: StringPtr; var propBasedDriverType: MacDriverType; var gotPropBasedDriver: boolean; fileBasedDrivers: FileBasedDriverDetailedPtr; var nFileBasedDrivers: ItemCount): OSErr; external name '_FindDriverCandidatesDetailed';

{
 *  ScanDriverCandidates()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ScanDriverCandidates(var deviceID: RegEntryID; fileBasedDrivers: FileBasedDriverRecordPtr; nFileBasedDrivers: ItemCount; matchingDrivers: FileBasedDriverRecordPtr; var nMatchingDrivers: ItemCount): OSErr; external name '_ScanDriverCandidates';

{
 *  ScanDriverCandidatesDetailed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ScanDriverCandidatesDetailed(var deviceID: RegEntryID; fileBasedDrivers: FileBasedDriverDetailedPtr; nFileBasedDrivers: ItemCount; matchingDrivers: FileBasedDriverDetailedPtr; var nMatchingDrivers: ItemCount): OSErr; external name '_ScanDriverCandidatesDetailed';

{
 *  CompareFileCandToPropCand()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CompareFileCandToPropCand(var device: RegEntryID; deviceName: StringPtr; propBasedCandidate: DriverTypePtr; fileBasedCandidate: FileBasedDriverRecordPtr): SInt16; external name '_CompareFileCandToPropCand';

{
 *  GetCompatibleProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure GetCompatibleProperty(var device: RegEntryID; var compatibleNames: StringPtr; var nCompatibleNames: ItemCount); external name '_GetCompatibleProperty';

{
 *  CompatibleDriverNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CompatibleDriverNames(nameInfoStr: StringPtr; compatibleNames: StringPtr; nCompatibleNames: ItemCount; var nameCount: SInt32): boolean; external name '_CompatibleDriverNames';

{
 *  GetDriverForDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDriverForDevice(var device: RegEntryID; var fragmentConnID: CFragConnectionID; var fragmentMain: DriverEntryPointPtr; var driverDesc: DriverDescriptionPtr): OSErr; external name '_GetDriverForDevice';

{
 *  InstallDriverForDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InstallDriverForDevice(var device: RegEntryID; beginningUnit: UnitNumber; endingUnit: UnitNumber; var refNum: DriverRefNum): OSErr; external name '_InstallDriverForDevice';

{
 *  GetDriverInformation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDriverInformation(refNum: DriverRefNum; var unitNum: UnitNumber; var flags: DriverFlags; var count: DriverOpenCount; name: StringPtr; var device: RegEntryID; var driverLoadLocation: CFragSystem7Locator; var fragmentConnID: CFragConnectionID; var fragmentMain: DriverEntryPointPtr; var driverDesc: DriverDescription): OSErr; external name '_GetDriverInformation';

{
 *  GetDriverDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDriverDescription(fragmentPtr: LogicalAddress; var theDriverDesc: DriverDescriptionPtr): OSErr; external name '_GetDriverDescription';

{
 *  GetNamedDriverDescFromFSSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetNamedDriverDescFromFSSpec(fragmentSpec: FSSpecPtr; fragName: StringPtr; var driverDesc: DriverDescriptionPtr): OSStatus; external name '_GetNamedDriverDescFromFSSpec';

{
 *  SetDriverClosureMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetDriverClosureMemory(fragmentConnID: CFragConnectionID; holdDriverMemory: boolean): OSErr; external name '_SetDriverClosureMemory';

{
 *  ReplaceDriverWithFragment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ReplaceDriverWithFragment(theRefNum: DriverRefNum; fragmentConnID: CFragConnectionID): OSErr; external name '_ReplaceDriverWithFragment';

{
 *  OpenInstalledDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenInstalledDriver(refNum: DriverRefNum; ioPermission: SInt8): OSErr; external name '_OpenInstalledDriver';

{
 *  RenameDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RenameDriver(refNum: DriverRefNum; newDriverName: StringPtr): OSErr; external name '_RenameDriver';

{
 *  RemoveDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RemoveDriver(refNum: DriverRefNum; immediate: boolean): OSErr; external name '_RemoveDriver';

{
 *  LookupDrivers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LookupDrivers(beginningUnit: UnitNumber; endingUnit: UnitNumber; emptyUnits: boolean; var returnedRefNums: ItemCount; var refNums: DriverRefNum): OSErr; external name '_LookupDrivers';

{
 *  HighestUnitNumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function HighestUnitNumber: UnitNumber; external name '_HighestUnitNumber';

{
 *  DriverGestaltOn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DriverGestaltOn(refNum: DriverRefNum): OSErr; external name '_DriverGestaltOn';

{
 *  DriverGestaltOff()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DriverGestaltOff(refNum: DriverRefNum): OSErr; external name '_DriverGestaltOff';

{
 *  DriverGestaltIsOn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverLoaderLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DriverGestaltIsOn(flags: DriverFlags): boolean; external name '_DriverGestaltIsOn';

{
 *  PBOpenSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenSync';
{
 *  PBOpenAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenAsync';
{
 *  PBOpenImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenImmed';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBCloseSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCloseSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBCloseSync';
{
 *  PBCloseAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCloseAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBCloseAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBCloseImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCloseImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBCloseImmed';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBReadSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBReadSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBReadSync';
{
 *  PBReadAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBReadAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBReadAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBReadImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBReadImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBReadImmed';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBWriteSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBWriteSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBWriteSync';
{
 *  PBWriteAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBWriteAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBWriteAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBWriteImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBWriteImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBWriteImmed';
{
    PBWaitIOComplete is a friendly way for applications to monitor
    a pending asynchronous I/O operation in power-managed and
    preemptive multitasking systems.
 }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBWaitIOComplete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBWaitIOComplete(paramBlock: ParmBlkPtr; timeout: Duration): OSErr; external name '_PBWaitIOComplete';

{ AddDrive and GetDrvQHdr are now defined in Disks.h/p/a }

{$ifc CALL_NOT_IN_CARBON}
{
 *  GetDCtlEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDCtlEntry(refNum: SInt16): DCtlHandle; external name '_GetDCtlEntry';

{
    SetChooserAlert used to simply set a bit in a low-mem global
    to tell the Chooser not to display its warning message when
    the printer is changed. However, under MultiFinder and System 7,
    this low-mem is swapped out when a layer change occurs, and the
    Chooser never sees the change. It is obsolete, and completely
    unsupported on the PowerPC. 68K apps can still call it if they
    wish.
    
    pascal Boolean SetChooserAlert(Boolean f);

}
{
 *  DriverInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DriverInstall(drvrPtr: DRVRHeaderPtr; refNum: SInt16): OSErr; external name '_DriverInstall';
{
 *  DriverInstallReserveMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DriverInstallReserveMem(drvrPtr: DRVRHeaderPtr; refNum: SInt16): OSErr; external name '_DriverInstallReserveMem';
{
  Note: DrvrInstall() is no longer supported, becuase it never really worked anyways.
        There will soon be a DriverInstall() which does the right thing.

        DrvrRemove has been renamed to DriverRemove.  But, InterfaceLib for PowerPC
        still exports DrvrRemove, so a macro is used to map the new name to old.

}
{
 *  DrvrRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DrvrRemove(refNum: SInt16): OSErr; external name '_DrvrRemove';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc TARGET_CPU_68K}
{$ifc CALL_NOT_IN_CARBON}
{
 *  DriverRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DriverRemove(refNum: SInt16): OSErr; external name '_DriverRemove';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_CPU_68K}


{$ifc CALL_NOT_IN_CARBON}
{
 *  [Mac]OpenDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenDriver(const (*var*) name: Str255; var drvrRefNum: SInt16): OSErr; external name '_OpenDriver';

{
 *  [Mac]CloseDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CloseDriver(refNum: SInt16): OSErr; external name '_CloseDriver';

{
 *  Control()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Control(refNum: SInt16; csCode: SInt16; csParamPtr: UnivPtr): OSErr; external name '_Control';

{
 *  Status()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Status(refNum: SInt16; csCode: SInt16; csParamPtr: UnivPtr): OSErr; external name '_Status';

{
 *  KillIO()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function KillIO(refNum: SInt16): OSErr; external name '_KillIO';

{
 *  Fetch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Fetch(dce: DCtlPtr): SInt32; external name '_Fetch';
{
 *  Stash()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Stash(dce: DCtlPtr; data: ByteParameter): SInt32; external name '_Stash';
{
 *  IODone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure IODone(dce: DCtlPtr; ioResult: OSErr); external name '_IODone';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  PBControlSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBControlSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBControlSync';
{
 *  PBControlAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBControlAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBControlAsync';
{
 *  PBControlImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBControlImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBControlImmed';
{
 *  PBStatusSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBStatusSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBStatusSync';
{
 *  PBStatusAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBStatusAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBStatusAsync';
{
 *  PBStatusImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBStatusImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBStatusImmed';
{
 *  PBKillIOSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBKillIOSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBKillIOSync';
{
 *  PBKillIOAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBKillIOAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBKillIOAsync';
{
 *  PBKillIOImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBKillIOImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBKillIOImmed';
{
 *  OpenDeskAcc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenDeskAcc(const (*var*) deskAccName: Str255): SInt16; external name '_OpenDeskAcc';
{
 *  CloseDeskAcc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure CloseDeskAcc(refNum: SInt16); external name '_CloseDeskAcc';
{$endc}  {CALL_NOT_IN_CARBON}

{
    The PBxxx() routines are obsolete.  
    
    Use the PBxxxSync(), PBxxxAsync(), or PBxxxImmed version instead.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBControl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBControl(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBControl';

{
 *  PBStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBStatus(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBStatus';

{
 *  PBKillIO()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBKillIO(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBKillIO';

{$endc}  {CALL_NOT_IN_CARBON}


{$ifc CALL_NOT_IN_CARBON}
{
 *  PBOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpen(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBOpen';

{
 *  PBClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBClose(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBClose';

{
 *  PBRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRead(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBRead';

{
 *  PBWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBWrite(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBWrite';

{$endc}  {CALL_NOT_IN_CARBON}


{$ALIGN MAC68K}


end.
