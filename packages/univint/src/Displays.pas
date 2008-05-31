{
     File:       Displays.p
 
     Contains:   Display Manager Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit Displays;
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
uses MacTypes,Quickdraw,CMTypes,AEDataModel,ConditionalMacros,Components,Video,CMApplication,AppleEvents,Events,Processes,Dialogs;


{$ALIGN MAC68K}


type
	DMProcessInfoPtr					= ProcessSerialNumberPtr;
	DMModalFilterUPP					= ModalFilterUPP;


const
																{  AppleEvents Core Suite  }
	kAESystemConfigNotice		= FourCharCode('cnfg');						{  Core Suite types  }
	kAEDisplayNotice			= FourCharCode('dspl');
	kAEDisplaySummary			= FourCharCode('dsum');
	keyDMConfigVersion			= FourCharCode('dmcv');
	keyDMConfigFlags			= FourCharCode('dmcf');
	keyDMConfigReserved			= FourCharCode('dmcr');
	keyDisplayID				= FourCharCode('dmid');
	keyDisplayComponent			= FourCharCode('dmdc');
	keyDisplayDevice			= FourCharCode('dmdd');
	keyDisplayFlags				= FourCharCode('dmdf');
	keyDisplayMode				= FourCharCode('dmdm');
	keyDisplayModeReserved		= FourCharCode('dmmr');
	keyDisplayReserved			= FourCharCode('dmdr');
	keyDisplayMirroredId		= FourCharCode('dmmi');
	keyDeviceFlags				= FourCharCode('dddf');
	keyDeviceDepthMode			= FourCharCode('dddm');
	keyDeviceRect				= FourCharCode('dddr');
	keyPixMapRect				= FourCharCode('dpdr');
	keyPixMapHResolution		= FourCharCode('dphr');
	keyPixMapVResolution		= FourCharCode('dpvr');
	keyPixMapPixelType			= FourCharCode('dppt');
	keyPixMapPixelSize			= FourCharCode('dpps');
	keyPixMapCmpCount			= FourCharCode('dpcc');
	keyPixMapCmpSize			= FourCharCode('dpcs');
	keyPixMapAlignment			= FourCharCode('dppa');
	keyPixMapResReserved		= FourCharCode('dprr');
	keyPixMapReserved			= FourCharCode('dppr');
	keyPixMapColorTableSeed		= FourCharCode('dpct');
	keySummaryMenubar			= FourCharCode('dsmb');
	keySummaryChanges			= FourCharCode('dsch');
	keyDisplayOldConfig			= FourCharCode('dold');
	keyDisplayNewConfig			= FourCharCode('dnew');

	dmOnlyActiveDisplays		= true;
	dmAllDisplays				= false;


																{  DMSendDependentNotification notifyClass  }
	kDependentNotifyClassShowCursor = FourCharCode('shcr');					{  When display mgr shows a hidden cursor during an unmirror  }
	kDependentNotifyClassDriverOverride = FourCharCode('ndrv');				{  When a driver is overridden  }
	kDependentNotifyClassDisplayMgrOverride = FourCharCode('dmgr');			{  When display manager is upgraded  }
	kDependentNotifyClassProfileChanged = FourCharCode('prof');				{  When DMSetProfileByAVID is called  }


																{  Switch Flags  }
	kNoSwitchConfirmBit			= 0;							{  Flag indicating that there is no need to confirm a switch to this mode  }
	kDepthNotAvailableBit		= 1;							{  Current depth not available in new mode  }
	kShowModeBit				= 3;							{  Show this mode even though it requires a confirm.  }
	kModeNotResizeBit			= 4;							{  Do not use this mode to resize display (for cards that mode drives a different connector).  }
	kNeverShowModeBit			= 5;							{  This mode should not be shown in the user interface.  }

	{	    Summary Change Flags (sticky bits indicating an operation was performed)
	    For example, moving a display then moving it back will still set the kMovedDisplayBit.
		}
	kBeginEndConfigureBit		= 0;
	kMovedDisplayBit			= 1;
	kSetMainDisplayBit			= 2;
	kSetDisplayModeBit			= 3;
	kAddDisplayBit				= 4;
	kRemoveDisplayBit			= 5;
	kNewDisplayBit				= 6;
	kDisposeDisplayBit			= 7;
	kEnabledDisplayBit			= 8;
	kDisabledDisplayBit			= 9;
	kMirrorDisplayBit			= 10;
	kUnMirrorDisplayBit			= 11;


																{  Notification Messages for extended call back routines  }
	kDMNotifyRequestConnectionProbe = 0;						{  Like kDMNotifyRequestDisplayProbe only not for smart displays (used in wake before all busses are awake)  }
	kDMNotifyInstalled			= 1;							{  At install time  }
	kDMNotifyEvent				= 2;							{  Post change time  }
	kDMNotifyRemoved			= 3;							{  At remove time  }
	kDMNotifyPrep				= 4;							{  Pre change time  }
	kDMNotifyExtendEvent		= 5;							{  Allow registrees to extend apple event before it is sent  }
	kDMNotifyDependents			= 6;							{  Minor notification check without full update  }
	kDMNotifySuspendConfigure	= 7;							{  Temporary end of configuration  }
	kDMNotifyResumeConfigure	= 8;							{  Resume configuration  }
	kDMNotifyRequestDisplayProbe = 9;							{  Request smart displays re-probe (used in sleep and hot plugging)  }
																{  Notification Flags  }
	kExtendedNotificationProc	= $00010000;


	{	 types for notifyType 	}
	kFullNotify					= 0;							{  This is the appleevent whole nine yards notify  }
	kFullDependencyNotify		= 1;							{  Only sends to those who want to know about interrelated functionality (used for updating UI)  }

	{	 DisplayID/DeviceID constants 	}
	kDummyDeviceID				= $00FF;						{  This is the ID of the dummy display, used when the last ÒrealÓ display is disabled. }
	kInvalidDisplayID			= $0000;						{  This is the invalid ID }
	kFirstDisplayID				= $0100;

																{  bits for panelListFlags  }
	kAllowDuplicatesBit			= 0;

																{  bits for nameFlags  }
	kSuppressNumberBit			= 0;
	kSuppressNumberMask			= 1;
	kForceNumberBit				= 1;
	kForceNumberMask			= 2;
	kSuppressNameBit			= 2;
	kSuppressNameMask			= 4;

	{  DMGetNameByAVID masks }
	kDMSupressNumbersMask		= $01;							{  Supress the numbers and return only names }
	kDMForceNumbersMask			= $02;							{  Force numbers to always be shown (even on single display configs) }
	kDMSupressNameMask			= $04;							{  Supress the names and return only numbers. }


	{	 Constants for fidelity checks 	}
	kNoFidelity					= 0;
	kMinimumFidelity			= 1;
	kDefaultFidelity			= 500;							{  I'm just picking a number for Apple default panels and engines }
	kDefaultManufacturerFidelity = 1000;						{  I'm just picking a number for Manufacturer's panels and engines (overrides apple defaults) }

	kAnyPanelType				= 0;							{  Pass to DMNewEngineList for list of all panels (as opposed to specific types) }
	kAnyEngineType				= 0;							{  Pass to DMNewEngineList for list of all engines }
	kAnyDeviceType				= 0;							{  Pass to DMNewDeviceList for list of all devices }
	kAnyPortType				= 0;							{  Pass to DMNewDevicePortList for list of all devices }

	{	 portListFlags for DM_NewDevicePortList 	}
																{  Should offline devices be put into the port list (such as dummy display)  }
	kPLIncludeOfflineDevicesBit	= 0;


	{	 confirmFlags for DMConfirmConfiguration 	}
	kForceConfirmBit			= 0;							{  Force a confirm dialog  }
	kForceConfirmMask			= $01;


	{	 Flags for displayModeFlags 	}
	kDisplayModeListNotPreferredBit = 0;
	kDisplayModeListNotPreferredMask = $01;


	{	 Flags for itemFlags 	}
	kComponentListNotPreferredBit = 0;
	kComponentListNotPreferredMask = $01;

	kDisplayTimingInfoVersionZero = 1;
	kDisplayTimingInfoReservedCountVersionZero = 16;
	kDisplayModeEntryVersionZero = 0;							{  displayModeVersion - original version }
	kDisplayModeEntryVersionOne	= 1;							{  displayModeVersion - added displayModeOverrideInfo }


	kMakeAndModelReservedCount	= 4;							{  Number of reserved fields }


	{  Display Gestalt for DMDisplayGestalt }
	kDisplayGestaltDisplayCommunicationAttr = FourCharCode('comm');
	kDisplayGestaltForbidI2CMask = $01;							{  Some displays have firmware problems if they get I2C communication.  If this bit is set, then I2C communication is forbidden }
	kDisplayGestaltUseI2CPowerMask = $02;						{  Some displays require I2C power settings (most use DPMS). }
	kDisplayGestaltCalibratorAttr = FourCharCode('cali');
	kDisplayGestaltBrightnessAffectsGammaMask = $01;			{  Used by default calibrator (should we show brightness panel)  }
	kDisplayGestaltViewAngleAffectsGammaMask = $02;				{  Currently not used by color sync }


type
	DMFidelityType						= UInt32;
	{
	   AVID is an ID for ports and devices the old DisplayID type
	    is carried on for compatibility
	}


	DMListType							= Ptr;
	DMListIndexType						= UInt32;
	AVPowerStateRec						= VDPowerStateRec;
	AVPowerStateRecPtr 					= ^AVPowerStateRec;
	AVPowerStatePtr						= ^VDPowerStateRec;
	DMDisplayTimingInfoRecPtr = ^DMDisplayTimingInfoRec;
	DMDisplayTimingInfoRec = record
		timingInfoVersion:		UInt32;
		timingInfoAttributes:	UInt32;									{  Flags  }
		timingInfoRelativeQuality: SInt32;								{  quality of the timing  }
		timingInfoRelativeDefault: SInt32;								{  relative default of the timing  }
		timingInfoReserved:		array [0..15] of UInt32;				{  Reserved  }
	end;

	DMDisplayTimingInfoPtr				= ^DMDisplayTimingInfoRec;

	DMComponentListEntryRecPtr = ^DMComponentListEntryRec;
	DMComponentListEntryRec = record
		itemID:					DisplayIDType;							{  DisplayID Manager }
		itemComponent:			Component;								{  Component Manager }
		itemDescription:		ComponentDescription;					{  We can always construct this if we use something beyond the compontent mgr. }
		itemClass:				ResType;								{  Class of group to put this panel (eg geometry/color/etc for panels, brightness/contrast for engines, video out/sound/etc for devices) }
		itemFidelity:			DMFidelityType;							{  How good is this item for the specified search? }
		itemSubClass:			ResType;								{  Subclass of group to put this panel.  Can use to do sub-grouping (eg volume for volume panel and mute panel) }
		itemSort:				Point;									{  Set to 0 - future to sort the items in a sub group. }
		itemFlags:				UInt32;									{  Set to 0 (future expansion) }
		itemReserved:			ResType;								{  What kind of code does the itemReference point to  (right now - kPanelEntryTypeComponentMgr only) }
		itemFuture1:			UInt32;									{  Set to 0 (future expansion - probably an alternate code style) }
		itemFuture2:			UInt32;									{  Set to 0 (future expansion - probably an alternate code style) }
		itemFuture3:			UInt32;									{  Set to 0 (future expansion - probably an alternate code style) }
		itemFuture4:			UInt32;									{  Set to 0 (future expansion - probably an alternate code style) }
	end;

	DMComponentListEntryPtr				= ^DMComponentListEntryRec;
	{  ¥¥¥ Move AVLocationRec to AVComponents.i AFTER AVComponents.i is created }
	AVLocationRecPtr = ^AVLocationRec;
	AVLocationRec = record
		locationConstant:		UInt32;									{  Set to 0 (future expansion - probably an alternate code style) }
	end;

	AVLocationPtr						= ^AVLocationRec;
	DMDepthInfoRecPtr = ^DMDepthInfoRec;
	DMDepthInfoRec = record
		depthSwitchInfo:		VDSwitchInfoPtr;						{  This is the switch mode to choose this timing/depth  }
		depthVPBlock:			VPBlockPtr;								{  VPBlock (including size, depth and format)  }
		depthFlags:				UInt32;									{  VDVideoParametersInfoRec.csDepthFlags   }
		depthReserved1:			UInt32;									{  Reserved  }
		depthReserved2:			UInt32;									{  Reserved  }
	end;

	DMDepthInfoPtr						= ^DMDepthInfoRec;
	DMDepthInfoBlockRecPtr = ^DMDepthInfoBlockRec;
	DMDepthInfoBlockRec = record
		depthBlockCount:		UInt32;									{  How many depths are there?  }
		depthVPBlock:			DMDepthInfoPtr;							{  Array of DMDepthInfoRec  }
		depthBlockFlags:		UInt32;									{  Reserved  }
		depthBlockReserved1:	UInt32;									{  Reserved  }
		depthBlockReserved2:	UInt32;									{  Reserved  }
	end;

	DMDepthInfoBlockPtr					= ^DMDepthInfoBlockRec;
	DMDisplayModeListEntryRecPtr = ^DMDisplayModeListEntryRec;
	DMDisplayModeListEntryRec = record
		displayModeFlags:		UInt32;
		displayModeSwitchInfo:	VDSwitchInfoPtr;
		displayModeResolutionInfo: VDResolutionInfoPtr;
		displayModeTimingInfo:	VDTimingInfoPtr;
		displayModeDepthBlockInfo: DMDepthInfoBlockPtr;					{  Information about all the depths }
		displayModeVersion:		UInt32;									{  What version is this record (now kDisplayModeEntryVersionOne) }
		displayModeName:		StringPtr;								{  Name of the timing mode }
		displayModeDisplayInfo:	DMDisplayTimingInfoPtr;					{  Information from the display. }
	end;

	DMDisplayModeListEntryPtr			= ^DMDisplayModeListEntryRec;

	DependentNotifyRecPtr = ^DependentNotifyRec;
	DependentNotifyRec = record
		notifyType:				ResType;								{  What type was the engine that made the change (may be zero) }
		notifyClass:			ResType;								{  What class was the change (eg geometry, color etc) }
		notifyPortID:			DisplayIDType;							{  Which device was touched (kInvalidDisplayID -> all or none) }
		notifyComponent:		ComponentInstance;						{  What engine did it (may be 0)? }
		notifyVersion:			UInt32;									{  Set to 0 (future expansion) }
		notifyFlags:			UInt32;									{  Set to 0 (future expansion) }
		notifyReserved:			UInt32;									{  Set to 0 (future expansion) }
		notifyFuture:			UInt32;									{  Set to 0 (future expansion) }
	end;

	DependentNotifyPtr					= ^DependentNotifyRec;

	DMMakeAndModelRecPtr = ^DMMakeAndModelRec;
	DMMakeAndModelRec = record
		manufacturer:			ResType;
		model:					UInt32;
		serialNumber:			UInt32;
		manufactureDate:		UInt32;
		makeReserved:			array [0..3] of UInt32;
	end;

	DMMakeAndModelPtr					= ^DMMakeAndModelRec;
	{  DMNewDisplayList displayListIncludeFlags }

const
	kIncludeOnlineActiveDisplaysMask = $01;
	kIncludeOnlineDisabledDisplaysMask = $02;
	kIncludeOfflineDisplaysMask	= $04;
	kIncludeOfflineDummyDisplaysMask = $08;
	kIncludeHardwareMirroredDisplaysMask = $10;


																{  modeListFlags for DMNewDisplayModeList  }
	kDMModeListIncludeAllModesMask = $01;						{  Include all timing modes not _explicitly_ excluded (see other bits) }
	kDMModeListIncludeOfflineModesMask = $02;
	kDMModeListExcludeDriverModesMask = $04;					{  Exclude old-style timing modes (cscGetNextResolution/kDisplayModeIDFindFirstResolution modes) }
	kDMModeListExcludeDisplayModesMask = $08;					{  Exclude timing modes that come from the display (always arbritrary timing modes) }
	kDMModeListExcludeCustomModesMask = $10;					{  Exclude custom modes that came neither from the driver or display (need a better name) }
	kDMModeListPreferStretchedModesMask = $20;					{  Prefer modes that are stretched over modes that are letterboxed when setting kDisplayModeListNotPreferredBit }
	kDMModeListPreferSafeModesMask = $40;						{  Prefer modes that are safe over modes that are not when setting kDisplayModeListNotPreferredBit }
	kDMModeListExcludeDriverScaledModesMask = $80;				{  Exclude modes that are scaled by the driver (usually on fixed resolution displays) }


	{  DMNewDisplayList displayListFlags }

type
	DisplayListEntryRecPtr = ^DisplayListEntryRec;
	DisplayListEntryRec = record
		displayListEntryGDevice: GDHandle;
		displayListEntryDisplayID: DisplayIDType;
		displayListEntryIncludeFlags: UInt32;							{  Reason this entry was included }
		displayListEntryReserved1: UInt32;
		displayListEntryReserved2: UInt32;								{  Zero }
		displayListEntryReserved3: UInt32;								{  Zero }
		displayListEntryReserved4: UInt32;								{  Zero }
		displayListEntryReserved5: UInt32;								{  Zero }
	end;

	DisplayListEntryPtr					= ^DisplayListEntryRec;
	DMProfileListEntryRecPtr = ^DMProfileListEntryRec;
	DMProfileListEntryRec = record
		profileRef:				CMProfileRef;
		profileReserved1:		Ptr;									{  Reserved }
		profileReserved2:		Ptr;									{  Reserved }
		profileReserved3:		Ptr;									{  Reserved }
	end;

	DMProfileListEntryPtr				= ^DMProfileListEntryRec;
{$ifc TYPED_FUNCTION_POINTERS}
	DMNotificationProcPtr = procedure(var theEvent: AppleEvent);
{$elsec}
	DMNotificationProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DMExtendedNotificationProcPtr = procedure(userData: UnivPtr; theMessage: SInt16; notifyData: UnivPtr);
{$elsec}
	DMExtendedNotificationProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DMComponentListIteratorProcPtr = procedure(userData: UnivPtr; itemIndex: DMListIndexType; componentInfo: DMComponentListEntryPtr);
{$elsec}
	DMComponentListIteratorProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DMDisplayModeListIteratorProcPtr = procedure(userData: UnivPtr; itemIndex: DMListIndexType; displaymodeInfo: DMDisplayModeListEntryPtr);
{$elsec}
	DMDisplayModeListIteratorProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DMProfileListIteratorProcPtr = procedure(userData: UnivPtr; itemIndex: DMListIndexType; profileInfo: DMProfileListEntryPtr);
{$elsec}
	DMProfileListIteratorProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	DMDisplayListIteratorProcPtr = procedure(userData: UnivPtr; itemIndex: DMListIndexType; displaymodeInfo: DisplayListEntryPtr);
{$elsec}
	DMDisplayListIteratorProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	DMNotificationUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DMNotificationUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DMExtendedNotificationUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DMExtendedNotificationUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DMComponentListIteratorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DMComponentListIteratorUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DMDisplayModeListIteratorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DMDisplayModeListIteratorUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DMProfileListIteratorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DMProfileListIteratorUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	DMDisplayListIteratorUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DMDisplayListIteratorUPP = UniversalProcPtr;
{$endc}	

const
	uppDMNotificationProcInfo = $000000C0;
	uppDMExtendedNotificationProcInfo = $00000EC0;
	uppDMComponentListIteratorProcInfo = $00000FC0;
	uppDMDisplayModeListIteratorProcInfo = $00000FC0;
	uppDMProfileListIteratorProcInfo = $00000FC0;
	uppDMDisplayListIteratorProcInfo = $00000FC0;
	{
	 *  NewDMNotificationUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewDMNotificationUPP(userRoutine: DMNotificationProcPtr): DMNotificationUPP; external name '_NewDMNotificationUPP'; { old name was NewDMNotificationProc }
{
 *  NewDMExtendedNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDMExtendedNotificationUPP(userRoutine: DMExtendedNotificationProcPtr): DMExtendedNotificationUPP; external name '_NewDMExtendedNotificationUPP'; { old name was NewDMExtendedNotificationProc }
{
 *  NewDMComponentListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDMComponentListIteratorUPP(userRoutine: DMComponentListIteratorProcPtr): DMComponentListIteratorUPP; external name '_NewDMComponentListIteratorUPP'; { old name was NewDMComponentListIteratorProc }
{
 *  NewDMDisplayModeListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDMDisplayModeListIteratorUPP(userRoutine: DMDisplayModeListIteratorProcPtr): DMDisplayModeListIteratorUPP; external name '_NewDMDisplayModeListIteratorUPP'; { old name was NewDMDisplayModeListIteratorProc }
{
 *  NewDMProfileListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDMProfileListIteratorUPP(userRoutine: DMProfileListIteratorProcPtr): DMProfileListIteratorUPP; external name '_NewDMProfileListIteratorUPP'; { old name was NewDMProfileListIteratorProc }
{
 *  NewDMDisplayListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDMDisplayListIteratorUPP(userRoutine: DMDisplayListIteratorProcPtr): DMDisplayListIteratorUPP; external name '_NewDMDisplayListIteratorUPP'; { old name was NewDMDisplayListIteratorProc }
{
 *  DisposeDMNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDMNotificationUPP(userUPP: DMNotificationUPP); external name '_DisposeDMNotificationUPP';
{
 *  DisposeDMExtendedNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDMExtendedNotificationUPP(userUPP: DMExtendedNotificationUPP); external name '_DisposeDMExtendedNotificationUPP';
{
 *  DisposeDMComponentListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDMComponentListIteratorUPP(userUPP: DMComponentListIteratorUPP); external name '_DisposeDMComponentListIteratorUPP';
{
 *  DisposeDMDisplayModeListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDMDisplayModeListIteratorUPP(userUPP: DMDisplayModeListIteratorUPP); external name '_DisposeDMDisplayModeListIteratorUPP';
{
 *  DisposeDMProfileListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDMProfileListIteratorUPP(userUPP: DMProfileListIteratorUPP); external name '_DisposeDMProfileListIteratorUPP';
{
 *  DisposeDMDisplayListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDMDisplayListIteratorUPP(userUPP: DMDisplayListIteratorUPP); external name '_DisposeDMDisplayListIteratorUPP';
{
 *  InvokeDMNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDMNotificationUPP(var theEvent: AppleEvent; userRoutine: DMNotificationUPP); external name '_InvokeDMNotificationUPP'; { old name was CallDMNotificationProc }
{
 *  InvokeDMExtendedNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDMExtendedNotificationUPP(userData: UnivPtr; theMessage: SInt16; notifyData: UnivPtr; userRoutine: DMExtendedNotificationUPP); external name '_InvokeDMExtendedNotificationUPP'; { old name was CallDMExtendedNotificationProc }
{
 *  InvokeDMComponentListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDMComponentListIteratorUPP(userData: UnivPtr; itemIndex: DMListIndexType; componentInfo: DMComponentListEntryPtr; userRoutine: DMComponentListIteratorUPP); external name '_InvokeDMComponentListIteratorUPP'; { old name was CallDMComponentListIteratorProc }
{
 *  InvokeDMDisplayModeListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDMDisplayModeListIteratorUPP(userData: UnivPtr; itemIndex: DMListIndexType; displaymodeInfo: DMDisplayModeListEntryPtr; userRoutine: DMDisplayModeListIteratorUPP); external name '_InvokeDMDisplayModeListIteratorUPP'; { old name was CallDMDisplayModeListIteratorProc }
{
 *  InvokeDMProfileListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDMProfileListIteratorUPP(userData: UnivPtr; itemIndex: DMListIndexType; profileInfo: DMProfileListEntryPtr; userRoutine: DMProfileListIteratorUPP); external name '_InvokeDMProfileListIteratorUPP'; { old name was CallDMProfileListIteratorProc }
{
 *  InvokeDMDisplayListIteratorUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDMDisplayListIteratorUPP(userData: UnivPtr; itemIndex: DMListIndexType; displaymodeInfo: DisplayListEntryPtr; userRoutine: DMDisplayListIteratorUPP); external name '_InvokeDMDisplayListIteratorUPP'; { old name was CallDMDisplayListIteratorProc }
{$ifc CALL_NOT_IN_CARBON}
{
 *  DMDisplayGestalt()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMDisplayGestalt(theDisplayID: DisplayIDType; displayGestaltSelector: ResType; var displayGestaltResponse: UInt32): OSErr; external name '_DMDisplayGestalt';
{
 *  DMUseScreenPrefs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMUseScreenPrefs(usePrefs: boolean; displayState: Handle): OSErr; external name '_DMUseScreenPrefs';
{
 *  DMSuspendConfigure()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMSuspendConfigure(displayState: Handle; reserved1: UInt32): OSErr; external name '_DMSuspendConfigure';
{
 *  DMResumeConfigure()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMResumeConfigure(displayState: Handle; reserved1: UInt32): OSErr; external name '_DMResumeConfigure';
{
 *  DMSetGammaByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMSetGammaByAVID(gammaAVID: AVIDType; setGammaFlags: UInt32; theGamma: GammaTblHandle): OSErr; external name '_DMSetGammaByAVID';
{
 *  DMGetGammaByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMGetGammaByAVID(gammaAVID: AVIDType; getGammaFlags: UInt32; var theGamma: GammaTblHandle): OSErr; external name '_DMGetGammaByAVID';
{
 *  DMGetMakeAndModelByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMGetMakeAndModelByAVID(theAVID: AVIDType; theMakeAndModel: DMMakeAndModelPtr): OSErr; external name '_DMGetMakeAndModelByAVID';
{
 *  DMNewDisplayList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMNewDisplayList(displayListIncludeFlags: UInt32; reserved1: UInt32; reserved2: UInt32; var theCount: DMListIndexType; var theDisplayList: DMListType): OSErr; external name '_DMNewDisplayList';
{
 *  DMGetIndexedDisplayFromList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMGetIndexedDisplayFromList(theDisplayList: DMListType; itemIndex: DMListIndexType; reserved: UInt32; listIterator: DMDisplayListIteratorUPP; userData: UnivPtr): OSErr; external name '_DMGetIndexedDisplayFromList';
{
 *  DMNewProfileListByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMNewProfileListByAVID(theAVID: AVIDType; reserved: UInt32; var profileCount: DMListIndexType; var profileList: DMListType): OSErr; external name '_DMNewProfileListByAVID';
{
 *  DMGetIndexedProfileFromList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMGetIndexedProfileFromList(profileList: DMListType; itemIndex: DMListIndexType; reserved: UInt32; listIterator: DMProfileListIteratorUPP; userData: UnivPtr): OSErr; external name '_DMGetIndexedProfileFromList';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DMGetFirstScreenDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetFirstScreenDevice(activeOnly: boolean): GDHandle; external name '_DMGetFirstScreenDevice';
{
 *  DMGetNextScreenDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetNextScreenDevice(theDevice: GDHandle; activeOnly: boolean): GDHandle; external name '_DMGetNextScreenDevice';
{
 *  DMDrawDesktopRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DMDrawDesktopRect(var globalRect: Rect); external name '_DMDrawDesktopRect';
{
 *  DMDrawDesktopRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DMDrawDesktopRegion(globalRgn: RgnHandle); external name '_DMDrawDesktopRegion';
{
 *  DMBeginConfigureDisplays()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMBeginConfigureDisplays(var displayState: Handle): OSErr; external name '_DMBeginConfigureDisplays';
{
 *  DMEndConfigureDisplays()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMEndConfigureDisplays(displayState: Handle): OSErr; external name '_DMEndConfigureDisplays';
{
 *  DMAddDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMAddDisplay(newDevice: GDHandle; driver: SInt16; mode: UInt32; reserved: UInt32; displayID: UInt32; displayComponent: Component; displayState: Handle): OSErr; external name '_DMAddDisplay';
{
 *  DMMoveDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMMoveDisplay(moveDevice: GDHandle; x: SInt16; y: SInt16; displayState: Handle): OSErr; external name '_DMMoveDisplay';
{
 *  DMDisableDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMDisableDisplay(disableDevice: GDHandle; displayState: Handle): OSErr; external name '_DMDisableDisplay';
{
 *  DMEnableDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMEnableDisplay(enableDevice: GDHandle; displayState: Handle): OSErr; external name '_DMEnableDisplay';
{
 *  DMRemoveDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMRemoveDisplay(removeDevice: GDHandle; displayState: Handle): OSErr; external name '_DMRemoveDisplay';
{
 *  DMSetMainDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSetMainDisplay(newMainDevice: GDHandle; displayState: Handle): OSErr; external name '_DMSetMainDisplay';
{
 *  DMSetDisplayMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSetDisplayMode(theDevice: GDHandle; mode: UInt32; var depthMode: UInt32; reserved: UInt32; displayState: Handle): OSErr; external name '_DMSetDisplayMode';
{
 *  DMCheckDisplayMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMCheckDisplayMode(theDevice: GDHandle; mode: UInt32; depthMode: UInt32; var switchFlags: UInt32; reserved: UInt32; var modeOk: boolean): OSErr; external name '_DMCheckDisplayMode';
{
 *  DMGetDeskRegion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetDeskRegion(var desktopRegion: RgnHandle): OSErr; external name '_DMGetDeskRegion';
{
 *  DMRegisterNotifyProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMRegisterNotifyProc(notificationProc: DMNotificationUPP; whichPSN: DMProcessInfoPtr): OSErr; external name '_DMRegisterNotifyProc';
{
 *  DMRemoveNotifyProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMRemoveNotifyProc(notificationProc: DMNotificationUPP; whichPSN: DMProcessInfoPtr): OSErr; external name '_DMRemoveNotifyProc';
{
 *  DMQDIsMirroringCapable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMQDIsMirroringCapable(var qdIsMirroringCapable: boolean): OSErr; external name '_DMQDIsMirroringCapable';
{
 *  DMCanMirrorNow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMCanMirrorNow(var canMirrorNow: boolean): OSErr; external name '_DMCanMirrorNow';
{
 *  DMIsMirroringOn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMIsMirroringOn(var isMirroringOn: boolean): OSErr; external name '_DMIsMirroringOn';
{
 *  DMMirrorDevices()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMMirrorDevices(gD1: GDHandle; gD2: GDHandle; displayState: Handle): OSErr; external name '_DMMirrorDevices';
{
 *  DMUnmirrorDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMUnmirrorDevice(gDevice: GDHandle; displayState: Handle): OSErr; external name '_DMUnmirrorDevice';
{
 *  DMGetNextMirroredDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetNextMirroredDevice(gDevice: GDHandle; var mirroredDevice: GDHandle): OSErr; external name '_DMGetNextMirroredDevice';
{
 *  DMBlockMirroring()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMBlockMirroring: OSErr; external name '_DMBlockMirroring';
{
 *  DMUnblockMirroring()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMUnblockMirroring: OSErr; external name '_DMUnblockMirroring';
{$ifc CALL_NOT_IN_CARBON}
{
 *  DMGetDisplayMgrA5World()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function DMGetDisplayMgrA5World(var dmA5: Ptr): OSErr; external name '_DMGetDisplayMgrA5World';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  DMGetDisplayIDByGDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetDisplayIDByGDevice(displayDevice: GDHandle; var displayID: DisplayIDType; failToMain: boolean): OSErr; external name '_DMGetDisplayIDByGDevice';
{
 *  DMGetGDeviceByDisplayID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetGDeviceByDisplayID(displayID: DisplayIDType; var displayDevice: GDHandle; failToMain: boolean): OSErr; external name '_DMGetGDeviceByDisplayID';
{
 *  DMSetDisplayComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSetDisplayComponent(theDevice: GDHandle; displayComponent: Component): OSErr; external name '_DMSetDisplayComponent';
{
 *  DMGetDisplayComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetDisplayComponent(theDevice: GDHandle; var displayComponent: Component): OSErr; external name '_DMGetDisplayComponent';
{
 *  DMNewDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewDisplay(var newDevice: GDHandle; driverRefNum: SInt16; mode: UInt32; reserved: UInt32; displayID: DisplayIDType; displayComponent: Component; displayState: Handle): OSErr; external name '_DMNewDisplay';
{
 *  DMDisposeDisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMDisposeDisplay(disposeDevice: GDHandle; displayState: Handle): OSErr; external name '_DMDisposeDisplay';
{
 *  DMResolveDisplayComponents()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMResolveDisplayComponents: OSErr; external name '_DMResolveDisplayComponents';
{
 *  DMRegisterExtendedNotifyProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMRegisterExtendedNotifyProc(notifyProc: DMExtendedNotificationUPP; notifyUserData: UnivPtr; nofifyOnFlags: UInt16; whichPSN: DMProcessInfoPtr): OSErr; external name '_DMRegisterExtendedNotifyProc';
{
 *  DMRemoveExtendedNotifyProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMRemoveExtendedNotifyProc(notifyProc: DMExtendedNotificationUPP; notifyUserData: UnivPtr; whichPSN: DMProcessInfoPtr; removeFlags: UInt16): OSErr; external name '_DMRemoveExtendedNotifyProc';
{
 *  DMNewAVPanelList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVPanelList(displayID: DisplayIDType; panelType: ResType; minimumFidelity: DMFidelityType; panelListFlags: UInt32; reserved: UInt32; var thePanelCount: DMListIndexType; var thePanelList: DMListType): OSErr; external name '_DMNewAVPanelList';
{
 *  DMNewAVEngineList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVEngineList(displayID: DisplayIDType; engineType: ResType; minimumFidelity: DMFidelityType; engineListFlags: UInt32; reserved: UInt32; var engineCount: DMListIndexType; var engineList: DMListType): OSErr; external name '_DMNewAVEngineList';
{
 *  DMNewAVDeviceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVDeviceList(deviceType: ResType; deviceListFlags: UInt32; reserved: UInt32; var deviceCount: DMListIndexType; var deviceList: DMListType): OSErr; external name '_DMNewAVDeviceList';
{
 *  DMNewAVPortListByPortType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVPortListByPortType(subType: ResType; portListFlags: UInt32; reserved: UInt32; var devicePortCount: DMListIndexType; var theDevicePortList: DMListType): OSErr; external name '_DMNewAVPortListByPortType';
{
 *  DMGetIndexedComponentFromList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetIndexedComponentFromList(panelList: DMListType; itemIndex: DMListIndexType; reserved: UInt32; listIterator: DMComponentListIteratorUPP; userData: UnivPtr): OSErr; external name '_DMGetIndexedComponentFromList';
{
 *  DMDisposeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMDisposeList(panelList: DMListType): OSErr; external name '_DMDisposeList';
{
 *  DMGetNameByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetNameByAVID(theID: AVIDType; nameFlags: UInt32; var name: Str255): OSErr; external name '_DMGetNameByAVID';
{
 *  DMNewAVIDByPortComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVIDByPortComponent(thePortComponent: Component; portKind: ResType; reserved: UInt32; var newID: AVIDType): OSErr; external name '_DMNewAVIDByPortComponent';
{
 *  DMGetPortComponentByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetPortComponentByAVID(thePortID: DisplayIDType; var thePortComponent: Component; var theDesciption: ComponentDescription; var thePortKind: ResType): OSErr; external name '_DMGetPortComponentByAVID';
{
 *  DMSendDependentNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSendDependentNotification(notifyType: ResType; notifyClass: ResType; displayID: AVIDType; notifyComponent: ComponentInstance): OSErr; external name '_DMSendDependentNotification';
{
 *  DMDisposeAVComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMDisposeAVComponent(theAVComponent: Component): OSErr; external name '_DMDisposeAVComponent';
{
 *  DMSaveScreenPrefs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSaveScreenPrefs(reserved1: UInt32; saveFlags: UInt32; reserved2: UInt32): OSErr; external name '_DMSaveScreenPrefs';
{
 *  DMNewAVIDByDeviceComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVIDByDeviceComponent(theDeviceComponent: Component; portKind: ResType; reserved: UInt32; var newID: DisplayIDType): OSErr; external name '_DMNewAVIDByDeviceComponent';
{
 *  DMNewAVPortListByDeviceAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewAVPortListByDeviceAVID(theID: AVIDType; minimumFidelity: DMFidelityType; portListFlags: UInt32; reserved: UInt32; var devicePortCount: DMListIndexType; var theDevicePortList: DMListType): OSErr; external name '_DMNewAVPortListByDeviceAVID';
{
 *  DMGetDeviceComponentByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetDeviceComponentByAVID(theDeviceID: AVIDType; var theDeviceComponent: Component; var theDesciption: ComponentDescription; var theDeviceKind: ResType): OSErr; external name '_DMGetDeviceComponentByAVID';
{
 *  DMNewDisplayModeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMNewDisplayModeList(displayID: DisplayIDType; modeListFlags: UInt32; reserved: UInt32; var thePanelCount: DMListIndexType; var thePanelList: DMListType): OSErr; external name '_DMNewDisplayModeList';
{
 *  DMGetIndexedDisplayModeFromList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetIndexedDisplayModeFromList(panelList: DMListType; itemIndex: DMListIndexType; reserved: UInt32; listIterator: DMDisplayModeListIteratorUPP; userData: UnivPtr): OSErr; external name '_DMGetIndexedDisplayModeFromList';
{
 *  DMGetGraphicInfoByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetGraphicInfoByAVID(theID: AVIDType; var theAVPcit: PicHandle; var theAVIconSuite: Handle; var theAVLocation: AVLocationRec): OSErr; external name '_DMGetGraphicInfoByAVID';
{
 *  DMGetAVPowerState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetAVPowerState(theID: AVIDType; getPowerState: AVPowerStatePtr; reserved1: UInt32): OSErr; external name '_DMGetAVPowerState';
{
 *  DMSetAVPowerState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSetAVPowerState(theID: AVIDType; setPowerState: AVPowerStatePtr; powerFlags: UInt32; displayState: Handle): OSErr; external name '_DMSetAVPowerState';
{
 *  DMGetDeviceAVIDByPortAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetDeviceAVIDByPortAVID(portAVID: AVIDType; var deviceAVID: AVIDType): OSErr; external name '_DMGetDeviceAVIDByPortAVID';
{
 *  DMGetEnableByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetEnableByAVID(theAVID: AVIDType; var isAVIDEnabledNow: boolean; var canChangeEnableNow: boolean): OSErr; external name '_DMGetEnableByAVID';
{
 *  DMSetEnableByAVID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMSetEnableByAVID(theAVID: AVIDType; doEnable: boolean; displayState: Handle): OSErr; external name '_DMSetEnableByAVID';
{
 *  DMGetDisplayMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib68k 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMGetDisplayMode(theDevice: GDHandle; switchInfo: VDSwitchInfoPtr): OSErr; external name '_DMGetDisplayMode';
{
 *  DMConfirmConfiguration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DisplayLib 2.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DMConfirmConfiguration(filterProc: DMModalFilterUPP; confirmFlags: UInt32; reserved: UInt32; displayState: Handle): OSErr; external name '_DMConfirmConfiguration';
{$ALIGN MAC68K}


end.
