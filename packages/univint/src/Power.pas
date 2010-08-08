{
     File:       OSServices/Power.h
 
     Contains:   Power Manager Interfaces.
 
     Version:    OSServices-352~2
 
     Copyright:  © 1990-2008 by Apple Computer, Inc.  All rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{      Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit Power;
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
uses MacTypes,Multiprocessing,MacErrors;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN MAC68K}

const
{ commands to SleepQRec sleepQProc }
	kSleepRequest = 1;
	kSleepDemand = 2;
	kSleepWakeUp = 3;
	kSleepRevoke = 4;
	kSleepUnlock = 4;
	kSleepDeny = 5;    { A non-zero value clients can use to deny requests}
	kSleepNow = 6;
	kDozeDemand = 7;
	kDozeWakeUp = 8;
	kDozeRequest = 9;    { additional messages for Power Mgr 2.0}
	kEnterStandby = 10;   { Idle Queue Only}
	kEnterRun = 11;   { Idle Queue Only}
	kSuspendRequest = 12;
	kSuspendDemand = 13;
	kSuspendRevoke = 14;
	kSuspendWakeUp = 15;
	kGetPowerLevel = 16;
	kSetPowerLevel = 17;
	kDeviceInitiatedWake = 18;
	kWakeToDoze = 19;
	kDozeToFullWakeUp = 20;
	kGetPowerInfo = 21;
	kGetWakeOnNetInfo = 22;
	kSuspendWakeToDoze = 23;
	kEnterIdle = 24;   { Idle Queue Only}
	kStillIdle = 25;   { Idle Queue Only}
	kExitIdle = 26;    { Idle Queue Only}

const
{ SleepQRec.sleepQFlags }
	noCalls = 1;
	noRequest = 2;
	slpQType = 16;
	sleepQType = 16;

{ System Activity Selectors }
{ Notes:  The IdleActivity selector is not available unless the hasAggressiveIdling PMFeatures bit is set. }
{         Use IdleActivity where you used to use OverallAct if necessary.  OverallAct will only            }
{         delay power cycling if it's enabled, and will delay sleep by a small amount when                 }
{         hasAggressiveIdling is set.  Don't use IdleActivity unless hasAggressiveIdling is set; when      }
{         hasAggressiveIdling is not set, the use of IdleActivity is undefined, and well do different      }
{         things depending on which Power Manager is currently running.                                    }
const
	OverallAct = 0;    { Delays idle sleep by small amount                 }
	UsrActivity = 1;    { Delays idle sleep and dimming by timeout time          }
	NetActivity = 2;    { Delays idle sleep and power cycling by small amount         }
	HDActivity = 3;    { Delays hard drive spindown and idle sleep by small amount  }
	IdleActivity = 4;     { Delays idle sleep by timeout time                 }

type
	SleepQRecPtr = ^SleepQRec;
	SleepQProcPtr = function( message: SIGNEDLONG; qRecPtr: SleepQRecPtr ): SIGNEDLONG;
	SleepQUPP = SleepQProcPtr;
	SleepQRec = record
		sleepQLink: SleepQRecPtr;             { pointer to next queue element          }
		sleepQType: SInt16;             { queue element type (must be SleepQType)       }
		sleepQProc: SleepQUPP;             { pointer to sleep universal proc ptr         }
		sleepQFlags: SInt16;            { flags                       }
	end;
{
 *  NewSleepQUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewSleepQUPP( userRoutine: SleepQProcPtr ): SleepQUPP; external name '_NewSleepQUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeSleepQUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeSleepQUPP( userUPP: SleepQUPP ); external name '_DisposeSleepQUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeSleepQUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeSleepQUPP( message: SIGNEDLONG; qRecPtr: SleepQRecPtr; userUPP: SleepQUPP ): SIGNEDLONG; external name '_InvokeSleepQUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  GetCPUSpeed()
 *  
 *  Discussion:
 *    GetCPUSpeed() returns the current effective clock speed of the
 *    CPU in megahertz.
 *  
 *  Result:
 *    the current effective clock speed of the CPU in megahertz.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetCPUSpeed: SIGNEDLONG; external name '_GetCPUSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SleepQInstall()
 *  
 *  Discussion:
 *    Adds an entry to the sleep queue.
 *  
 *  Parameters:
 *    
 *    qRecPtr:
 *      A pointer to a sleep queue record to be installed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SleepQInstall( qRecPtr: SleepQRecPtr ); external name '_SleepQInstall';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  SleepQRemove()
 *  
 *  Discussion:
 *    Remove an entry from the sleep queue.
 *  
 *  Parameters:
 *    
 *    qRecPtr:
 *      A pointer to a sleep queue record to be removed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure SleepQRemove( qRecPtr: SleepQRecPtr ); external name '_SleepQRemove';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MaximumProcessorSpeed()
 *  
 *  Discussion:
 *    MaximumProcessorSpeed() returns the maximum effective clock speed
 *    of the CPU in megahertz.
 *  
 *  Result:
 *    the maximum effective clock speed of the CPU in megahertz.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function MaximumProcessorSpeed: SInt16; external name '_MaximumProcessorSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  MinimumProcessorSpeed()
 *  
 *  Discussion:
 *    MinimumProcessorSpeed() returns the minimum effective clock speed
 *    of the CPU in megahertz. Before Mac OS X 10.4, this function
 *    always returns the maximum cpu speed, not the minimum as expected.
 *  
 *  Result:
 *    the minimum effective clock speed of the CPU in megahertz.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function MinimumProcessorSpeed: SInt16; external name '_MinimumProcessorSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)


{
 *  CurrentProcessorSpeed()
 *  
 *  Discussion:
 *    CurrentProcessorSpeed() returns the current effective clock speed
 *    of the CPU in megahertz. Before Mac OS X 10.4, this function
 *    always returns the maximum cpu speed, not the actual current
 *    speed the processor is running at.  One MHz represents one
 *    million cycles per second.
 *  
 *  Result:
 *    the current effective clock speed of the CPU in megahertz.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function CurrentProcessorSpeed: SInt16; external name '_CurrentProcessorSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  BatteryCount()
 *  
 *  Summary:
 *    Return the count of batteries installed on this computer.
 *  
 *  Result:
 *    the count of batteries installed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function BatteryCount: SInt16; external name '_BatteryCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  UpdateSystemActivity()
 *  
 *  Summary:
 *    You can use the UpdateSystemActivity function to notify the Power
 *    Manager that activity has taken place .
 *  
 *  Discussion:
 *    The UpdateSystemActivity function is used to notify the Power
 *    Manager that activity has taken place and the timers used to
 *    measure idle time should be updated to the time of this call.
 *    This function can be used by device drivers to prevent the
 *    computer from entering a low-power mode while critical activity
 *    is taking place on a particular device. The function is passed a
 *    parameter indicating the type of activity that has
 *    occurred.
 *    
 *    This function is slightly different from DelaySystemIdle, which
 *    should be used to prevent sleep or idle during a critical
 *    section. UpdateSystemActivity simply updates the tick count for
 *    the activity type selected. Conversely, DelaySystemIdle actually
 *    moves the counter to some number of ticks into the future, which
 *    allows the caller to go off and do somethingwithout fear of
 *    idling.
 *    
 *    The valid types of activity are:
 *    Value Name       Value        Description
 *    OverallAct       0            general type of activity
 *     UsrActivity      1            user activity (i.e.keyboard or
 *    mouse)
 *    NetActivity      2            interaction with network(s)
 *     HDActivity       3            hard disk or storage device in use
 *  
 *  Parameters:
 *    
 *    activity:
 *      The type of activity which has occurred.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function UpdateSystemActivity( activity: UInt8 ): OSErr; external name '_UpdateSystemActivity';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{*********************************************************************************************
 *
 *  Everything below this point in this file is deprecated and should not be used
 *  in new code on Mac OS X.  Existing clients should move to non-deprecated
 *  where possible.
 *
 *********************************************************************************************}
{$ifc not TARGET_CPU_64}
{ Storage Media sleep mode defines }
const
	kMediaModeOn = 0;    { Media active (Drive spinning and at full power)    }
	kMediaModeStandBy = 1;    { Media standby (not implemented)    }
	kMediaModeSuspend = 2;    { Media Idle (not implemented)   }
	kMediaModeOff = 3;     { Media Sleep (Drive not spinning and at min power, max recovery time)   }

const
	kMediaPowerCSCode = 70;

{ definitions for HDQueueElement.hdFlags   }
const
	kHDQueuePostBit = 0;    { 1 = call this routine on the second pass     }
	kHDQueuePostMask = 1 shl kHDQueuePostBit;

type
	ActivityInfoPtr = ^ActivityInfo;
	ActivityInfo = record
		ActivityType: SInt16;           { Type of activity to be fetched.  Same as UpdateSystemActivity Selectors }
		ActivityTime: UNSIGNEDLONG;           { Time of last activity (in ticks) of specified type. }
	end;
{ information returned by GetScaledBatteryInfo }
type
	BatteryInfoPtr = ^BatteryInfo;
	BatteryInfo = record
		flags: UInt8;                  { misc flags (see below)                  }
		warningLevel: UInt8;           { scaled warning level (0-255)               }
		reserved: UInt8;               { reserved for internal use             }
		batteryLevel: UInt8;           { scaled battery level (0-255)               }
	end;

type
	ModemByte = SInt8;
	BatteryByte = SInt8;
	SoundMixerByte = SInt8;
	PMResultCode = SIGNEDLONG;
const
{ depreciated commands to SleepQRec sleepQProc }
	sleepRequest = kSleepRequest;
	sleepDemand = kSleepDemand;
	sleepWakeUp = kSleepWakeUp;
	sleepRevoke = kSleepRevoke;
	sleepUnlock = kSleepUnlock;
	sleepDeny = kSleepDeny;
	sleepNow = kSleepNow;
	dozeDemand = kDozeDemand;
	dozeWakeUp = kDozeWakeUp;
	dozeRequest = kDozeRequest;
	enterStandby = kEnterStandby;
	enterRun = kEnterRun;
	suspendRequestMsg = kSuspendRequest;
	suspendDemandMsg = kSuspendDemand;
	suspendRevokeMsg = kSuspendRevoke;
	suspendWakeUpMsg = kSuspendWakeUp;
	getPowerLevel = kGetPowerLevel;
	setPowerLevel = kSetPowerLevel;

{ Power Handler func messages }
type
	PowerLevel = UInt32;
{ Power levels corresponding to PCI Bus Power Management Interface Spec (PMIS) }
const
	kPMDevicePowerLevel_On = 0;    { fully-powered 'On' state (D0 state)    }
	kPMDevicePowerLevel_D1 = 1;    { not used by Apple system SW         }
	kPMDevicePowerLevel_D2 = 2;    { not used by Apple system SW         }
	kPMDevicePowerLevel_Off = 3;     { main PCI bus power 'Off', but PCI standby power available (D3cold state) }

{ PowerHandlerProc definition }
{$endc} {not TARGET_CPU_64}
type
	RegEntryID = UNSIGNEDLONG;
	PowerHandlerProcPtr = function( message: UInt32; param: UnivPtr; refCon: UInt32; var regEntryID_: RegEntryID ): OSStatus;
	PowerHandlerUPP = PowerHandlerProcPtr;
{
 *  NewPowerHandlerUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  DisposePowerHandlerUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{
 *  InvokePowerHandlerUPP()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   available as macro/inline
 }

{$ifc not TARGET_CPU_64}
{ Power Mgt Apple Event types and errors }
const
{ Bit positions for ModemByte }
	modemOnBit = 0;
	ringWakeUpBit = 2;
	modemInstalledBit = 3;
	ringDetectBit = 4;
	modemOnHookBit = 5;

const
{ masks for ModemByte }
	modemOnMask = $01;
	ringWakeUpMask = $04;
	modemInstalledMask = $08;
	ringDetectMask = $10;
	modemOnHookMask = $20;

const
{ bit positions for BatteryByte }
	chargerConnBit = 0;
	hiChargeBit = 1;
	chargeOverFlowBit = 2;
	batteryDeadBit = 3;
	batteryLowBit = 4;
	connChangedBit = 5;

const
{ masks for BatteryByte }
	chargerConnMask = $01;
	hiChargeMask = $02;
	chargeOverFlowMask = $04;
	batteryDeadMask = $08;
	batteryLowMask = $10;
	connChangedMask = $20;

const
{ bit positions for SoundMixerByte }
	MediaBaySndEnBit = 0;
	PCISndEnBit = 1;
	ZVSndEnBit = 2;
	PCCardSndEnBit = 3;

const
{ masks for SoundMixerByte }
	MediaBaySndEnMask = $01;
	PCISndEnMask = $02;
	ZVSndEnMask = $04;
	PCCardSndEnMask = $08;

const
{ power mgt class}
	kAEMacPowerMgtEvt = FourCharCode('pmgt'); { event ids}
	kAEMacToWake = FourCharCode('wake');
	kAEMacLowPowerSaveData = FourCharCode('pmsd');
	kAEMacEmergencySleep = FourCharCode('emsl');
	kAEMacEmergencyShutdown = FourCharCode('emsd');


{
   These are result values returned by a Power Handler when queries
   by the Power Mgr if the device which that Power Handler represents
   woke the machine.
}
const
	kDeviceDidNotWakeMachine = 0;    { device did NOT wake machine}
	kDeviceRequestsFullWake = 1;    { device did wake machine and requests full wakeup}
	kDeviceRequestsWakeToDoze = 2;     { device did wake machine and requests partial wakeup}

{ bits in bitfield returned by PMFeatures }
const
	hasWakeupTimer = 0;    { 1=wakeup timer is supported                    }
	hasSharedModemPort = 1;    { 1=modem port shared by SCC and internal modem       }
	hasProcessorCycling = 2;    { 1=processor cycling is supported                }
	mustProcessorCycle = 3;    { 1=processor cycling should not be turned off          }
	hasReducedSpeed = 4;    { 1=processor can be started up at reduced speed        }
	dynamicSpeedChange = 5;    { 1=processor speed can be switched dynamically       }
	hasSCSIDiskMode = 6;    { 1=SCSI Disk Mode is supported                 }
	canGetBatteryTime = 7;    { 1=battery time can be calculated                }
	canWakeupOnRing = 8;    { 1=can wakeup when the modem detects a ring          }
	hasDimmingSupport = 9;    { 1=has dimming support built in (DPMS standby by default)   }
	hasStartupTimer = 10;   { 1=startup timer is supported                    }
	hasChargeNotification = 11;   { 1=client can determine of charge connect status change notifications available }
	hasDimSuspendSupport = 12;   { 1=supports dimming LCD and CRT to DPMS suspend state     }
	hasWakeOnNetActivity = 13;   { 1=hardware supports wake on network activity          }
	hasWakeOnLid = 14;   { 1=hardware can wake when opened                   }
	canPowerOffPCIBus = 15;   { 1=hardware can power off PCI bus during sleep if cards allow }
	hasDeepSleep = 16;   { 1=hardware supports deep sleep (hibernation) mode   }
	hasSleep = 17;   { 1=hardware supports normal (PowerBook-like) sleep   }
	supportsServerModeAPIs = 18;   { 1=hardware supports server mode API routines          }
	supportsUPSIntegration = 19;   { 1=hardware support UPS integration and reporting      }
	hasAggressiveIdling = 20;   { 1=Power Manager only resets OverallAct on UsrActvity     }
	supportsIdleQueue = 21;    { 1=Power Manager supports the idle queue              }

{ bits in bitfield returned by GetIntModemInfo and set by SetIntModemState }
const
	hasInternalModem = 0;    { 1=internal modem installed               }
	intModemRingDetect = 1;    { 1=internal modem has detected a ring          }
	intModemOffHook = 2;    { 1=internal modem is off hook               }
	intModemRingWakeEnb = 3;    { 1=wakeup on ring is enabled                 }
	extModemSelected = 4;    { 1=external modem selected             }
	modemSetBit = 15;    { 1=set bit, 0=clear bit (SetIntModemState)   }

{ bits in BatteryInfo.flags                                    }
{ ("chargerConnected" doesn't mean the charger is plugged in)  }
const
	batteryInstalled = 7;    { 1=battery is currently connected             }
	batteryCharging = 6;    { 1=battery is being charged               }
	chargerConnected = 5;    { 1=charger is connected to the PowerBook         }
	upsConnected = 4;    { 1=there is a UPS connected               }
	upsIsPowerSource = 3;     { 1=UPS is source of power                }

const
	HDPwrQType = $4844; { 'HD' hard disk spindown queue element type     }
	PMgrStateQType = $504D; { 'PM' Power Manager state queue element type       }

{ client notification bits in PMgrQueueElement.pmNotifyBits }
const
	pmSleepTimeoutChanged = 0;
	pmSleepEnableChanged = 1;
	pmHardDiskTimeoutChanged = 2;
	pmHardDiskSpindownChanged = 3;
	pmDimmingTimeoutChanged = 4;
	pmDimmingEnableChanged = 5;
	pmDiskModeAddressChanged = 6;
	pmProcessorCyclingChanged = 7;
	pmProcessorSpeedChanged = 8;
	pmWakeupTimerChanged = 9;
	pmStartupTimerChanged = 10;
	pmHardDiskPowerRemovedbyUser = 11;
	pmChargeStatusChanged = 12;
	pmPowerLevelChanged = 13;
	pmWakeOnNetActivityChanged = 14;

const
	pmSleepTimeoutChangedMask = 1 shl pmSleepTimeoutChanged;
	pmSleepEnableChangedMask = 1 shl pmSleepEnableChanged;
	pmHardDiskTimeoutChangedMask = 1 shl pmHardDiskTimeoutChanged;
	pmHardDiskSpindownChangedMask = 1 shl pmHardDiskSpindownChanged;
	pmDimmingTimeoutChangedMask = 1 shl pmDimmingTimeoutChanged;
	pmDimmingEnableChangedMask = 1 shl pmDimmingEnableChanged;
	pmDiskModeAddressChangedMask = 1 shl pmDiskModeAddressChanged;
	pmProcessorCyclingChangedMask = 1 shl pmProcessorCyclingChanged;
	pmProcessorSpeedChangedMask = 1 shl pmProcessorSpeedChanged;
	pmWakeupTimerChangedMask = 1 shl pmWakeupTimerChanged;
	pmStartupTimerChangedMask = 1 shl pmStartupTimerChanged;
	pmHardDiskPowerRemovedbyUserMask = 1 shl pmHardDiskPowerRemovedbyUser;
	pmChargeStatusChangedMask = 1 shl pmChargeStatusChanged;
	pmPowerLevelChangedMask = 1 shl pmPowerLevelChanged;
	pmWakeOnNetActivityChangedMask = 1 shl pmWakeOnNetActivityChanged;

{
   Use kIdleQueueDeviceType as the deviceType argument to AddDevicePowerHandler() to get the
   handler into the idle queue instead of the device sleep queue.
}

const
	kIdleQueueDeviceType = 'idle-queue';
{ PCI power management support}

const
	kUseDefaultMinimumWakeTime = 0;    { Defaults to 5 minutes}
	kPowerSummaryVersion = 1;    { Version of PowerSummary structure.}
	kDevicePowerInfoVersion = 1;     { Version of DevicePowerInfo structure.}

const
{ PowerSummary flags}
	kPCIPowerOffAllowed = 1 shl 0; { PCI power off is allowed.}

const
{ DevicePowerInfo flags}
	kDevicePCIPowerOffAllowed = 1 shl 0; { PCI power off is allowed for device.}
	kDeviceSupportsPMIS = 1 shl 1; { Device supports Power Mgt Interface Spec.}
	kDeviceCanAssertPMEDuringSleep = 1 shl 2; { Device can assert PME# during sleep.}
	kDeviceUsesCommonLogicPower = 1 shl 3; { Device uses common-logic power}
	kDeviceDriverPresent = 1 shl 4; { Driver present for device.}
	kDeviceDriverSupportsPowerMgt = 1 shl 5; { Driver installed a power handler.}

type
	DevicePowerInfoPtr = ^DevicePowerInfo;
	DevicePowerInfo = record
		version: UInt32;                { Version of this structure.}
		regID: RegEntryID;                  { RegEntryID for device.}
		flags: OptionBits;                  { Flags}
		minimumWakeTime: UInt32;        { Minimum seconds before sleeping again.}
		sleepPowerNeeded: UInt32;       { Milliwatts needed in the sleep state.}
	end;
type
	PowerSummaryPtr = ^PowerSummary;
	PowerSummary = record
		version: UInt32;                { Version of this structure.}
		flags: OptionBits;                  { Flags}
		sleepPowerAvailable: UInt32;    { Milliwatts available during sleep.}
		sleepPowerNeeded: UInt32;       { Milliwatts needed during sleep.}
		minimumWakeTime: UInt32;        { Minimum seconds before sleeping again.}
		deviceCount: ItemCount;            { Number of device power info records.}
		devices: array [0..0] of DevicePowerInfo;             { Array of device power info records.}
	end;
{$endc} {not TARGET_CPU_64}
type
	HDQueueElementPtr = ^HDQueueElement;
	PMgrQueueElementPtr = ^PMgrQueueElement;
	HDSpindownProcPtr = procedure( theElement: HDQueueElementPtr );
	PMgrStateChangeProcPtr = procedure( theElement: PMgrQueueElementPtr; stateBits: SIGNEDLONG );
	HDSpindownUPP = HDSpindownProcPtr;
	PMgrStateChangeUPP = PMgrStateChangeProcPtr;
{$ifc TARGET_CPU_64}
	HDQueueElement = record end;
	PMgrQueueElement = record end;
{$elsec} {TARGET_CPU_64}
	HDQueueElement = record
		hdQLink: HDQueueElementPtr;            { pointer to next queue element          }
		hdQType: SInt16;                { queue element type (must be HDPwrQType)       }
		hdFlags: SInt16;                { miscellaneous flags                   }
		hdProc: HDSpindownUPP;                 { pointer to routine to call           }
		hdUser: SIGNEDLONG;                 { user-defined (variable storage, etc.)   }
	end;

	PMgrQueueElement = record
		pmQLink: PMgrQueueElementPtr;          {  pointer to next queue element          }
		pmQType: SInt16;                { queue element type (must be PMgrStateQType)    }
		pmFlags: SInt16;                { miscellaneous flags                   }
		pmNotifyBits: SIGNEDLONG;           { bitmap of which changes to be notified for }
		pmProc: PMgrStateChangeUPP;                 { pointer to routine to call           }
		pmUser: SIGNEDLONG;                 { user-defined (variable storage, etc.)   }
	end;


	BatteryTimeRecPtr = ^BatteryTimeRec;
	BatteryTimeRec = record
		expectedBatteryTime: UNSIGNEDLONG;    { estimated battery time remaining (seconds) }
		minimumBatteryTime: UNSIGNEDLONG;     { minimum battery time remaining (seconds)     }
		maximumBatteryTime: UNSIGNEDLONG;     { maximum battery time remaining (seconds)     }
		timeUntilCharged: UNSIGNEDLONG;       { time until battery is fully charged (seconds)}
	end;
type
	WakeupTimePtr = ^WakeupTime;
	WakeupTime = record
		wakeTime: UNSIGNEDLONG;               { wakeup time (same format as current time)   }
		wakeEnabled: Boolean;            { 1=enable wakeup timer, 0=disable wakeup timer  }
		filler: SInt8;
	end;
type
	StartupTimePtr = ^StartupTime;
	StartupTime = record
		startTime: UNSIGNEDLONG;              { startup time (same format as current time)     }
		startEnabled: Boolean;           { 1=enable startup timer, 0=disable startup timer    }
		filler: SInt8;
	end;
{$ifc not TARGET_CPU_64}
{
 *  SetSpindownDisable()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetSpindownDisable( setDisable: Boolean ); external name '_SetSpindownDisable';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PMSelectorCount()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function PMSelectorCount: SInt16; external name '_PMSelectorCount';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  PMFeatures()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function PMFeatures: UInt32; external name '_PMFeatures';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  SetProcessorSpeed()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function SetProcessorSpeed( fullSpeed: Boolean ): Boolean; external name '_SetProcessorSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{
 *  FullProcessorSpeed()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function FullProcessorSpeed: Boolean; external name '_FullProcessorSpeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_5 *)


{  The following constants, structures, and functions have all been deprecated on Mac OS X and are not recommended for use.}
{
 *  DisableWUTime()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DisableWUTime: OSErr; external name '_DisableWUTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetWUTime()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetWUTime( wuTime: SIGNEDLONG ): OSErr; external name '_SetWUTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
{
 *  GetWUTime()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetWUTime( var wuTime: SIGNEDLONG; var wuFlag: SignedByte ): OSErr; external name '_GetWUTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  BatteryStatus()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function BatteryStatus( var status: SignedByte; var power: SignedByte ): OSErr; external name '_BatteryStatus';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  ModemStatus()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function ModemStatus( var status: SignedByte ): OSErr; external name '_ModemStatus';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
{
 *  IdleUpdate()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Power Manager is deprecated in Mac OS X.  Some of this
 *    functionality is provided in similar form in IOKit; some is
 *    provided in the Carbon and Cocoa frameworks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function IdleUpdate: SIGNEDLONG; external name '_IdleUpdate';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  EnableIdle()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Power Manager is deprecated in Mac OS X.  Some of this
 *    functionality is provided in similar form in IOKit; some is
 *    provided in the Carbon and Cocoa frameworks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure EnableIdle; external name '_EnableIdle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  DisableIdle()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    The Power Manager is deprecated in Mac OS X.  Some of this
 *    functionality is provided in similar form in IOKit; some is
 *    provided in the Carbon and Cocoa frameworks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure DisableIdle; external name '_DisableIdle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  AOn()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated on Mac OS X.  IOKit may provide
 *    replacement functionality depending on what this was being used
 *    for.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure AOn; external name '_AOn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  AOnIgnoreModem()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated on Mac OS X.  IOKit may provide
 *    replacement functionality depending on what this was being used
 *    for.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure AOnIgnoreModem; external name '_AOnIgnoreModem';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  BOn()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated on Mac OS X.  IOKit may provide
 *    replacement functionality depending on what this was being used
 *    for.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure BOn; external name '_BOn';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  AOff()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated on Mac OS X.  IOKit may provide
 *    replacement functionality depending on what this was being used
 *    for.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure AOff; external name '_AOff';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  BOff()   *** DEPRECATED ***
 *  
 *  Deprecated:
 *    This function is deprecated on Mac OS X.  IOKit may provide
 *    replacement functionality depending on what this was being used
 *    for.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
procedure BOff; external name '_BOff';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{ Public Power Management API  }
{
 *  GetSleepTimeout()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetSleepTimeout: UInt8; external name '_GetSleepTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetSleepTimeout()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetSleepTimeout( timeout: ByteParameter ); external name '_SetSleepTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetHardDiskTimeout()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetHardDiskTimeout: UInt8; external name '_GetHardDiskTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetHardDiskTimeout()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetHardDiskTimeout( timeout: ByteParameter ); external name '_SetHardDiskTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  HardDiskPowered()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function HardDiskPowered: Boolean; external name '_HardDiskPowered';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SpinDownHardDisk()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SpinDownHardDisk; external name '_SpinDownHardDisk';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  IsSpindownDisabled()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function IsSpindownDisabled: Boolean; external name '_IsSpindownDisabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  HardDiskQInstall()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function HardDiskQInstall( var theElement: HDQueueElement ): OSErr; external name '_HardDiskQInstall';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  HardDiskQRemove()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function HardDiskQRemove( var theElement: HDQueueElement ): OSErr; external name '_HardDiskQRemove';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetScaledBatteryInfo()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure GetScaledBatteryInfo( whichBattery: SInt16; var theInfo: BatteryInfo ); external name '_GetScaledBatteryInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  AutoSleepControl()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure AutoSleepControl( enableSleep: Boolean ); external name '_AutoSleepControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetIntModemInfo()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetIntModemInfo: UInt32; external name '_GetIntModemInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetIntModemState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetIntModemState( theState: SInt16 ); external name '_SetIntModemState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetSCSIDiskModeAddress()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetSCSIDiskModeAddress: SInt16; external name '_GetSCSIDiskModeAddress';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetSCSIDiskModeAddress()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetSCSIDiskModeAddress( scsiAddress: SInt16 ); external name '_SetSCSIDiskModeAddress';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetWakeupTimer()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure GetWakeupTimer( var theTime: WakeupTime ); external name '_GetWakeupTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetWakeupTimer()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetWakeupTimer( var theTime: WakeupTime ); external name '_SetWakeupTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  IsProcessorCyclingEnabled()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function IsProcessorCyclingEnabled: Boolean; external name '_IsProcessorCyclingEnabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  EnableProcessorCycling()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure EnableProcessorCycling( enable: Boolean ); external name '_EnableProcessorCycling';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetBatteryVoltage()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetBatteryVoltage( whichBattery: SInt16 ): Fixed; external name '_GetBatteryVoltage';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetBatteryTimes()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure GetBatteryTimes( whichBattery: SInt16; var theTimes: BatteryTimeRec ); external name '_GetBatteryTimes';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetDimmingTimeout()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetDimmingTimeout: UInt8; external name '_GetDimmingTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetDimmingTimeout()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure SetDimmingTimeout( timeout: ByteParameter ); external name '_SetDimmingTimeout';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  DimmingControl()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
procedure DimmingControl( enableSleep: Boolean ); external name '_DimmingControl';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  IsDimmingControlDisabled()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function IsDimmingControlDisabled: Boolean; external name '_IsDimmingControlDisabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  IsAutoSlpControlDisabled()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function IsAutoSlpControlDisabled: Boolean; external name '_IsAutoSlpControlDisabled';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  PMgrStateQInstall()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function PMgrStateQInstall( var theElement: PMgrQueueElement ): OSErr; external name '_PMgrStateQInstall';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  PMgrStateQRemove()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function PMgrStateQRemove( var theElement: PMgrQueueElement ): OSErr; external name '_PMgrStateQRemove';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  DelaySystemIdle()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function DelaySystemIdle: OSErr; external name '_DelaySystemIdle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetStartupTimer()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetStartupTimer( var theTime: StartupTime ): OSErr; external name '_GetStartupTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetStartupTimer()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function SetStartupTimer( var theTime: StartupTime ): OSErr; external name '_SetStartupTimer';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetLastActivity()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 }
function GetLastActivity( var theActivity: ActivityInfo ): OSErr; external name '_GetLastActivity';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetSoundMixerState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 }
function GetSoundMixerState( var theSoundMixerByte: SoundMixerByte ): OSErr; external name '_GetSoundMixerState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetSoundMixerState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 }
function SetSoundMixerState( var theSoundMixerByte: SoundMixerByte ): OSErr; external name '_SetSoundMixerState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  GetDimSuspendState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 }
function GetDimSuspendState: Boolean; external name '_GetDimSuspendState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{
 *  SetDimSuspendState()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.0
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 }
procedure SetDimSuspendState( dimSuspendState: Boolean ); external name '_SetDimSuspendState';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED *)


{$endc} {not TARGET_CPU_64}
{$endc} {TARGET_CPU_64}

{
 *  NewHDSpindownUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewHDSpindownUPP( userRoutine: HDSpindownProcPtr ): HDSpindownUPP; external name '_NewHDSpindownUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  NewPMgrStateChangeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewPMgrStateChangeUPP( userRoutine: PMgrStateChangeProcPtr ): PMgrStateChangeUPP; external name '_NewPMgrStateChangeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposeHDSpindownUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeHDSpindownUPP( userUPP: HDSpindownUPP ); external name '_DisposeHDSpindownUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  DisposePMgrStateChangeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposePMgrStateChangeUPP( userUPP: PMgrStateChangeUPP ); external name '_DisposePMgrStateChangeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokeHDSpindownUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeHDSpindownUPP( theElement: HDQueueElementPtr; userUPP: HDSpindownUPP ); external name '_InvokeHDSpindownUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{
 *  InvokePMgrStateChangeUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokePMgrStateChangeUPP( var theElement: PMgrQueueElement; stateBits: SIGNEDLONG; userUPP: PMgrStateChangeUPP ); external name '_InvokePMgrStateChangeUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
