{
     File:       Power.p
 
     Contains:   Power Manager Interfaces.
 
     Version:    Technology: Mac OS 9
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1990-2002 by Apple Computer, Inc.  All rights reserved
 
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

unit Power;
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
uses MacTypes,MixedMode,Multiprocessing,NameRegistry,MacErrors;


{$ALIGN MAC68K}


const
																{  Bit positions for ModemByte  }
	modemOnBit					= 0;
	ringWakeUpBit				= 2;
	modemInstalledBit			= 3;
	ringDetectBit				= 4;
	modemOnHookBit				= 5;

																{  masks for ModemByte  }
	modemOnMask					= $01;
	ringWakeUpMask				= $04;
	modemInstalledMask			= $08;
	ringDetectMask				= $10;
	modemOnHookMask				= $20;

																{  bit positions for BatteryByte  }
	chargerConnBit				= 0;
	hiChargeBit					= 1;
	chargeOverFlowBit			= 2;
	batteryDeadBit				= 3;
	batteryLowBit				= 4;
	connChangedBit				= 5;

																{  masks for BatteryByte  }
	chargerConnMask				= $01;
	hiChargeMask				= $02;
	chargeOverFlowMask			= $04;
	batteryDeadMask				= $08;
	batteryLowMask				= $10;
	connChangedMask				= $20;

																{  bit positions for SoundMixerByte  }
	MediaBaySndEnBit			= 0;
	PCISndEnBit					= 1;
	ZVSndEnBit					= 2;
	PCCardSndEnBit				= 3;

																{  masks for SoundMixerByte  }
	MediaBaySndEnMask			= $01;
	PCISndEnMask				= $02;
	ZVSndEnMask					= $04;
	PCCardSndEnMask				= $08;

																{  commands to SleepQRec sleepQProc  }
	kSleepRequest				= 1;
	kSleepDemand				= 2;
	kSleepWakeUp				= 3;
	kSleepRevoke				= 4;
	kSleepUnlock				= 4;
	kSleepDeny					= 5;							{  A non-zero value clients can use to deny requests }
	kSleepNow					= 6;
	kDozeDemand					= 7;
	kDozeWakeUp					= 8;
	kDozeRequest				= 9;							{  additional messages for Power Mgr 2.0 }
	kEnterStandby				= 10;							{  Idle Queue Only }
	kEnterRun					= 11;							{  Idle Queue Only }
	kSuspendRequest				= 12;
	kSuspendDemand				= 13;
	kSuspendRevoke				= 14;
	kSuspendWakeUp				= 15;
	kGetPowerLevel				= 16;
	kSetPowerLevel				= 17;
	kDeviceInitiatedWake		= 18;
	kWakeToDoze					= 19;
	kDozeToFullWakeUp			= 20;
	kGetPowerInfo				= 21;
	kGetWakeOnNetInfo			= 22;
	kSuspendWakeToDoze			= 23;
	kEnterIdle					= 24;							{  Idle Queue Only }
	kStillIdle					= 25;							{  Idle Queue Only }
	kExitIdle					= 26;							{  Idle Queue Only }

																{  depreciated commands to SleepQRec sleepQProc  }
	sleepRequest				= 1;
	sleepDemand					= 2;
	sleepWakeUp					= 3;
	sleepRevoke					= 4;
	sleepUnlock					= 4;
	sleepDeny					= 5;
	sleepNow					= 6;
	dozeDemand					= 7;
	dozeWakeUp					= 8;
	dozeRequest					= 9;
	enterStandby				= 10;
	enterRun					= 11;
	suspendRequestMsg			= 12;
	suspendDemandMsg			= 13;
	suspendRevokeMsg			= 14;
	suspendWakeUpMsg			= 15;
	getPowerLevel				= 16;
	setPowerLevel				= 17;

	{	 Power Handler func messages 	}

type
	PowerLevel							= UInt32;
	{	 Power levels corresponding to PCI Bus Power Management Interface Spec (PMIS) 	}

const
	kPMDevicePowerLevel_On		= 0;							{  fully-powered 'On' state (D0 state)     }
	kPMDevicePowerLevel_D1		= 1;							{  not used by Apple system SW          }
	kPMDevicePowerLevel_D2		= 2;							{  not used by Apple system SW          }
	kPMDevicePowerLevel_Off		= 3;							{  main PCI bus power 'Off', but PCI standby power available (D3cold state)  }

	{	 PowerHandlerProc definition 	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	PowerHandlerProcPtr = function(message: UInt32; param: UnivPtr; refCon: UInt32; var regEntryID_: RegEntryID): OSStatus;
{$elsec}
	PowerHandlerProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	PowerHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PowerHandlerUPP = UniversalProcPtr;
{$endc}	

const
	uppPowerHandlerProcInfo = $00003FF0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewPowerHandlerUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewPowerHandlerUPP(userRoutine: PowerHandlerProcPtr): PowerHandlerUPP; external name '_NewPowerHandlerUPP'; { old name was NewPowerHandlerProc }
{
 *  DisposePowerHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposePowerHandlerUPP(userUPP: PowerHandlerUPP); external name '_DisposePowerHandlerUPP';
{
 *  InvokePowerHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokePowerHandlerUPP(message: UInt32; param: UnivPtr; refCon: UInt32; var regEntryID_: RegEntryID; userRoutine: PowerHandlerUPP): OSStatus; external name '_InvokePowerHandlerUPP'; { old name was CallPowerHandlerProc }
{$endc}  {CALL_NOT_IN_CARBON}

{
   Use kIdleQueueDeviceType as the deviceType argument to AddDevicePowerHandler() to get the
   handler into the idle queue instead of the device sleep queue.
}
{  PCI power management support }


const
	kUseDefaultMinimumWakeTime	= 0;							{  Defaults to 5 minutes }
	kPowerSummaryVersion		= 1;							{  Version of PowerSummary structure. }
	kDevicePowerInfoVersion		= 1;							{  Version of DevicePowerInfo structure. }

																{  PowerSummary flags }
	kPCIPowerOffAllowed			= $00000001;					{  PCI power off is allowed. }

																{  DevicePowerInfo flags }
	kDevicePCIPowerOffAllowed	= $00000001;					{  PCI power off is allowed for device. }
	kDeviceSupportsPMIS			= $00000002;					{  Device supports Power Mgt Interface Spec. }
	kDeviceCanAssertPMEDuringSleep = $00000004;					{  Device can assert PME# during sleep. }
	kDeviceUsesCommonLogicPower	= $00000008;					{  Device uses common-logic power }
	kDeviceDriverPresent		= $00000010;					{  Driver present for device. }
	kDeviceDriverSupportsPowerMgt = $00000020;					{  Driver installed a power handler. }


type
	DevicePowerInfoPtr = ^DevicePowerInfo;
	DevicePowerInfo = record
		version:				UInt32;									{  Version of this structure. }
		regID:					RegEntryID;								{  RegEntryID for device. }
		flags:					OptionBits;								{  Flags }
		minimumWakeTime:		UInt32;									{  Minimum seconds before sleeping again. }
		sleepPowerNeeded:		UInt32;									{  Milliwatts needed in the sleep state. }
	end;

	PowerSummaryPtr = ^PowerSummary;
	PowerSummary = record
		version:				UInt32;									{  Version of this structure. }
		flags:					OptionBits;								{  Flags }
		sleepPowerAvailable:	UInt32;									{  Milliwatts available during sleep. }
		sleepPowerNeeded:		UInt32;									{  Milliwatts needed during sleep. }
		minimumWakeTime:		UInt32;									{  Minimum seconds before sleeping again. }
		deviceCount:			ItemCount;								{  Number of device power info records. }
		devices:				array [0..0] of DevicePowerInfo;		{  Array of device power info records. }
	end;


const
																{  SleepQRec.sleepQFlags  }
	noCalls						= 1;
	noRequest					= 2;
	slpQType					= 16;
	sleepQType					= 16;

	{	 Power Mgt Apple Event types and errors 	}
																{  power mgt class }
	kAEMacPowerMgtEvt			= FourCharCode('pmgt');						{  event ids }
	kAEMacToWake				= FourCharCode('wake');
	kAEMacLowPowerSaveData		= FourCharCode('pmsd');
	kAEMacEmergencySleep		= FourCharCode('emsl');
	kAEMacEmergencyShutdown		= FourCharCode('emsd');


	{
	   These are result values returned by a Power Handler when queries
	   by the Power Mgr if the device which that Power Handler represents
	   woke the machine.
	}
	kDeviceDidNotWakeMachine	= 0;							{  device did NOT wake machine }
	kDeviceRequestsFullWake		= 1;							{  device did wake machine and requests full wakeup }
	kDeviceRequestsWakeToDoze	= 2;							{  device did wake machine and requests partial wakeup }

	{	 bits in bitfield returned by PMFeatures 	}
	hasWakeupTimer				= 0;							{  1=wakeup timer is supported                     }
	hasSharedModemPort			= 1;							{  1=modem port shared by SCC and internal modem        }
	hasProcessorCycling			= 2;							{  1=processor cycling is supported                 }
	mustProcessorCycle			= 3;							{  1=processor cycling should not be turned off           }
	hasReducedSpeed				= 4;							{  1=processor can be started up at reduced speed         }
	dynamicSpeedChange			= 5;							{  1=processor speed can be switched dynamically        }
	hasSCSIDiskMode				= 6;							{  1=SCSI Disk Mode is supported                  }
	canGetBatteryTime			= 7;							{  1=battery time can be calculated                 }
	canWakeupOnRing				= 8;							{  1=can wakeup when the modem detects a ring           }
	hasDimmingSupport			= 9;							{  1=has dimming support built in (DPMS standby by default)    }
	hasStartupTimer				= 10;							{  1=startup timer is supported                     }
	hasChargeNotification		= 11;							{  1=client can determine of charge connect status change notifications available  }
	hasDimSuspendSupport		= 12;							{  1=supports dimming LCD and CRT to DPMS suspend state      }
	hasWakeOnNetActivity		= 13;							{  1=hardware supports wake on network activity           }
	hasWakeOnLid				= 14;							{  1=hardware can wake when opened                    }
	canPowerOffPCIBus			= 15;							{  1=hardware can power off PCI bus during sleep if cards allow  }
	hasDeepSleep				= 16;							{  1=hardware supports deep sleep (hibernation) mode    }
	hasSleep					= 17;							{  1=hardware supports normal (PowerBook-like) sleep    }
	supportsServerModeAPIs		= 18;							{  1=hardware supports server mode API routines           }
	supportsUPSIntegration		= 19;							{  1=hardware support UPS integration and reporting       }
	hasAggressiveIdling			= 20;							{  1=Power Manager only resets OverallAct on UsrActvity      }
	supportsIdleQueue			= 21;							{  1=Power Manager supports the idle queue               }

	{	 bits in bitfield returned by GetIntModemInfo and set by SetIntModemState 	}
	hasInternalModem			= 0;							{  1=internal modem installed                }
	intModemRingDetect			= 1;							{  1=internal modem has detected a ring           }
	intModemOffHook				= 2;							{  1=internal modem is off hook                }
	intModemRingWakeEnb			= 3;							{  1=wakeup on ring is enabled                  }
	extModemSelected			= 4;							{  1=external modem selected              }
	modemSetBit					= 15;							{  1=set bit, 0=clear bit (SetIntModemState)    }

	{	 bits in BatteryInfo.flags                                    	}
	{	 ("chargerConnected" doesn't mean the charger is plugged in)  	}
	batteryInstalled			= 7;							{  1=battery is currently connected              }
	batteryCharging				= 6;							{  1=battery is being charged                }
	chargerConnected			= 5;							{  1=charger is connected to the PowerBook          }
	upsConnected				= 4;							{  1=there is a UPS connected                }
	upsIsPowerSource			= 3;							{  1=UPS is source of power                 }

	HDPwrQType					= $4844;						{  'HD' hard disk spindown queue element type      }
	PMgrStateQType				= $504D;						{  'PM' Power Manager state queue element type        }

	{	 client notification bits in PMgrQueueElement.pmNotifyBits 	}
	pmSleepTimeoutChanged		= 0;
	pmSleepEnableChanged		= 1;
	pmHardDiskTimeoutChanged	= 2;
	pmHardDiskSpindownChanged	= 3;
	pmDimmingTimeoutChanged		= 4;
	pmDimmingEnableChanged		= 5;
	pmDiskModeAddressChanged	= 6;
	pmProcessorCyclingChanged	= 7;
	pmProcessorSpeedChanged		= 8;
	pmWakeupTimerChanged		= 9;
	pmStartupTimerChanged		= 10;
	pmHardDiskPowerRemovedbyUser = 11;
	pmChargeStatusChanged		= 12;
	pmPowerLevelChanged			= 13;
	pmWakeOnNetActivityChanged	= 14;

	pmSleepTimeoutChangedMask	= $01;
	pmSleepEnableChangedMask	= $02;
	pmHardDiskTimeoutChangedMask = $04;
	pmHardDiskSpindownChangedMask = $08;
	pmDimmingTimeoutChangedMask	= $10;
	pmDimmingEnableChangedMask	= $20;
	pmDiskModeAddressChangedMask = $40;
	pmProcessorCyclingChangedMask = $80;
	pmProcessorSpeedChangedMask	= $0100;
	pmWakeupTimerChangedMask	= $0200;
	pmStartupTimerChangedMask	= $0400;
	pmHardDiskPowerRemovedbyUserMask = $0800;
	pmChargeStatusChangedMask	= $1000;
	pmPowerLevelChangedMask		= $2000;
	pmWakeOnNetActivityChangedMask = $4000;

	{	 System Activity Selectors 	}
	{	 Notes:  The IdleActivity selector is not available unless the hasAggressiveIdling PMFeatures bit is set. 	}
	{	         Use IdleActivity where you used to use OverallAct if necessary.  OverallAct will only            	}
	{	         delay power cycling if it's enabled, and will delay sleep by a small amount when                 	}
	{	         hasAggressiveIdling is set.  Don't use IdleActivity unless hasAggressiveIdling is set; when      	}
	{	         hasAggressiveIdling is not set, the use of IdleActivity is undefined, and well do different      	}
	{	         things depending on which Power Manager is currently running.                                    	}
	OverallAct					= 0;							{  Delays idle sleep by small amount                  }
	UsrActivity					= 1;							{  Delays idle sleep and dimming by timeout time           }
	NetActivity					= 2;							{  Delays idle sleep and power cycling by small amount          }
	HDActivity					= 3;							{  Delays hard drive spindown and idle sleep by small amount   }
	IdleActivity				= 4;							{  Delays idle sleep by timeout time                  }

	{	 Storage Media sleep mode defines 	}
	kMediaModeOn				= 0;							{  Media active (Drive spinning and at full power)     }
	kMediaModeStandBy			= 1;							{  Media standby (not implemented)     }
	kMediaModeSuspend			= 2;							{  Media Idle (not implemented)    }
	kMediaModeOff				= 3;							{  Media Sleep (Drive not spinning and at min power, max recovery time)    }

	kMediaPowerCSCode			= 70;


	{	 definitions for HDQueueElement.hdFlags   	}
	kHDQueuePostBit				= 0;							{  1 = call this routine on the second pass      }
	kHDQueuePostMask			= $01;


type
	ActivityInfoPtr = ^ActivityInfo;
	ActivityInfo = record
		ActivityType:			SInt16;								{  Type of activity to be fetched.  Same as UpdateSystemActivity Selectors  }
		ActivityTime:			UInt32;									{  Time of last activity (in ticks) of specified type.  }
	end;

	{	 information returned by GetScaledBatteryInfo 	}
	BatteryInfoPtr = ^BatteryInfo;
	BatteryInfo = packed record
		flags:					UInt8;									{  misc flags (see below)                   }
		warningLevel:			UInt8;									{  scaled warning level (0-255)                }
		reserved:				UInt8;									{  reserved for internal use              }
		batteryLevel:			UInt8;									{  scaled battery level (0-255)                }
	end;

	ModemByte							= SInt8;
	BatteryByte							= SInt8;
	SoundMixerByte						= SInt8;
	PMResultCode						= SInt32;
	SleepQRecPtr = ^SleepQRec;
	HDQueueElementPtr = ^HDQueueElement;
	PMgrQueueElementPtr = ^PMgrQueueElement;
{$ifc TYPED_FUNCTION_POINTERS}
	SleepQProcPtr = function(message: SInt32; qRecPtr: SleepQRecPtr): SInt32;
{$elsec}
	SleepQProcPtr = Register68kProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	HDSpindownProcPtr = procedure(theElement: HDQueueElementPtr);
{$elsec}
	HDSpindownProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PMgrStateChangeProcPtr = procedure(theElement: PMgrQueueElementPtr; stateBits: SInt32);
{$elsec}
	PMgrStateChangeProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	SleepQUPP = ^SInt32; { an opaque UPP }
{$elsec}
	SleepQUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	HDSpindownUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HDSpindownUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PMgrStateChangeUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PMgrStateChangeUPP = UniversalProcPtr;
{$endc}	
	SleepQRec = record
		sleepQLink:				SleepQRecPtr;							{  pointer to next queue element           }
		sleepQType:				SInt16;								{  queue element type (must be SleepQType)        }
		sleepQProc:				SleepQUPP;								{  pointer to sleep universal proc ptr          }
		sleepQFlags:			SInt16;								{  flags                        }
	end;

	HDQueueElement = record
		hdQLink:				HDQueueElementPtr;						{  pointer to next queue element           }
		hdQType:				SInt16;								{  queue element type (must be HDPwrQType)        }
		hdFlags:				SInt16;								{  miscellaneous flags                    }
		hdProc:					HDSpindownUPP;							{  pointer to routine to call            }
		hdUser:					SInt32;								{  user-defined (variable storage, etc.)    }
	end;

	PMgrQueueElement = record
		pmQLink:				PMgrQueueElementPtr;					{  pointer to next queue element           }
		pmQType:				SInt16;								{  queue element type (must be PMgrStateQType)     }
		pmFlags:				SInt16;								{  miscellaneous flags                    }
		pmNotifyBits:			SInt32;								{  bitmap of which changes to be notified for  }
		pmProc:					PMgrStateChangeUPP;						{  pointer to routine to call            }
		pmUser:					SInt32;								{  user-defined (variable storage, etc.)    }
	end;


	BatteryTimeRecPtr = ^BatteryTimeRec;
	BatteryTimeRec = record
		expectedBatteryTime:	UInt32;									{  estimated battery time remaining (seconds)  }
		minimumBatteryTime:		UInt32;									{  minimum battery time remaining (seconds)      }
		maximumBatteryTime:		UInt32;									{  maximum battery time remaining (seconds)      }
		timeUntilCharged:		UInt32;									{  time until battery is fully charged (seconds) }
	end;


	WakeupTimePtr = ^WakeupTime;
	WakeupTime = record
		wakeTime:				UInt32;									{  wakeup time (same format as current time)    }
		wakeEnabled:			boolean;								{  1=enable wakeup timer, 0=disable wakeup timer   }
		filler:					SInt8;
	end;


	StartupTimePtr = ^StartupTime;
	StartupTime = record
		startTime:				UInt32;									{  startup time (same format as current time)      }
		startEnabled:			boolean;								{  1=enable startup timer, 0=disable startup timer     }
		filler:					SInt8;
	end;

	{  PowerSource version }

const
	kVersionOnePowerSource		= 1;
	kVersionTwoPowerSource		= 2;
	kCurrentPowerSourceVersion	= 2;

	{  PowerSourceAttrs bits }

	bSourceIsBattery			= 0;							{  power source is battery }
	bSourceIsAC					= 1;							{  power source is AC }
	bSourceCanBeCharged			= 2;							{  power source can be charged }
	bSourceIsUPS				= 3;							{  power source is UPS. NOTE: software should set bSourceIsBattery and bSourceIsAC also, as appropriate }
	bSourceProvidesWarnLevels	= 4;							{  power source provides low power and dead battery warning levels }
	kSourceIsBatteryMask		= $01;
	kSourceIsACMask				= $02;
	kSourceCanBeChargedMask		= $04;
	kSourceIsUPSMask			= $08;
	kSourceProvidesWarnLevelsMask = $10;

	{  PowerSourceFlags bits }

	bSourceIsAvailable			= 0;							{  power source is installed }
	bSourceIsCharging			= 1;							{  power source being charged }
	bChargerIsAttached			= 2;							{  a charger is connected }
	kSourceIsAvailableMask		= $01;
	kSourceIsChargingMask		= $02;
	kChargerIsAttachedMask		= $04;

	{  Power Capacity Types }

	kCapacityIsActual			= 0;							{  current capacity is expessed as actual capacity in same units as max }
	kCapacityIsPercentOfMax		= 1;							{  current capacity is expressed as a percentage of maximumCapacity }

	{  Net Activity Wake Options }
	kConfigSupportsWakeOnNetBit	= 0;
	kWakeOnNetAdminAccessesBit	= 1;
	kWakeOnAllNetAccessesBit	= 2;
	kUnmountServersBeforeSleepingBit = 3;
	kConfigSupportsWakeOnNetMask = $01;
	kWakeOnNetAdminAccessesMask	= $02;
	kWakeOnAllNetAccessesMask	= $04;
	kUnmountServersBeforeSleepingMask = $08;

	{  Power Source capacity usage types }
	kCurrentCapacityIsActualValue = 0;							{  currentCapacity is a real value in same units as maxCapacity }
	kCurrentCapacityIsPercentOfMax = 1;							{  currentCapacity is expressed as a percentage of maxCapacity. }


type
	PowerSourceID						= SInt16;
	PowerSourceParamBlockPtr = ^PowerSourceParamBlock;
	PowerSourceParamBlock = record
		sourceID:				PowerSourceID;							{  unique id assigned by Power Mgr }
		sourceCapacityUsage:	UInt16;									{  how currentCapacity is used }
		sourceVersion:			UInt32;									{  version of this record }
		sourceAttr:				OptionBits;								{  attribute flags (see below) }
		sourceState:			OptionBits;								{  state flags (see below) }
		currentCapacity:		UInt32;									{  current capacity, in }
																		{    milliwatts or % }
		maxCapacity:			UInt32;									{  full capacity, in milliwatts }
		timeRemaining:			UInt32;									{  time left to deplete,  }
																		{    in milliwatt-hours }
		timeToFullCharge:		UInt32;									{  time to charge,  }
																		{    in milliwatt-hours }
		voltage:				UInt32;									{  voltage in millivolts }
		current:				SInt32;									{  current in milliamperes  }
																		{   (negative if consuming,  }
																		{    positive if charging) }
		lowWarnLevel:			UInt32;									{  low warning level in milliwatts (or % if sourceCapacityUsage is %) }
		deadWarnLevel:			UInt32;									{  dead warning level in milliwatts (or % if sourceCapacityUsage is %) }
		reserved:				array [0..15] of UInt32;				{  for future expansion }
	end;

	{
	 *  DisableWUTime()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function DisableWUTime: OSErr; external name '_DisableWUTime';

{
 *  SetWUTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetWUTime(wuTime: SInt32): OSErr; external name '_SetWUTime';

{$ifc CALL_NOT_IN_CARBON}
{
 *  GetWUTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetWUTime(var wuTime: SInt32; var wuFlag: SignedByte): OSErr; external name '_GetWUTime';

{
 *  BatteryStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function BatteryStatus(var status: SignedByte; var power: SignedByte): OSErr; external name '_BatteryStatus';

{
 *  ModemStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ModemStatus(var status: SignedByte): OSErr; external name '_ModemStatus';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  IdleUpdate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IdleUpdate: SInt32; external name '_IdleUpdate';
{
 *  GetCPUSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCPUSpeed: SInt32; external name '_GetCPUSpeed';
{
 *  EnableIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure EnableIdle; external name '_EnableIdle';
{
 *  DisableIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisableIdle; external name '_DisableIdle';
{
 *  SleepQInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SleepQInstall(qRecPtr: SleepQRecPtr); external name '_SleepQInstall';
{
 *  SleepQRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SleepQRemove(qRecPtr: SleepQRecPtr); external name '_SleepQRemove';
{
 *  AOn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AOn; external name '_AOn';
{
 *  AOnIgnoreModem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AOnIgnoreModem; external name '_AOnIgnoreModem';
{
 *  BOn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BOn; external name '_BOn';
{
 *  AOff()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AOff; external name '_AOff';
{
 *  BOff()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure BOff; external name '_BOff';
{ Public Power Management API  }
{
 *  PMSelectorCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMSelectorCount: SInt16; external name '_PMSelectorCount';
{
 *  PMFeatures()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMFeatures: UInt32; external name '_PMFeatures';
{
 *  GetSleepTimeout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSleepTimeout: ByteParameter; external name '_GetSleepTimeout';
{
 *  SetSleepTimeout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetSleepTimeout(timeout: ByteParameter); external name '_SetSleepTimeout';
{
 *  GetHardDiskTimeout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetHardDiskTimeout: ByteParameter; external name '_GetHardDiskTimeout';
{
 *  SetHardDiskTimeout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetHardDiskTimeout(timeout: ByteParameter); external name '_SetHardDiskTimeout';
{
 *  HardDiskPowered()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HardDiskPowered: boolean; external name '_HardDiskPowered';
{
 *  SpinDownHardDisk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SpinDownHardDisk; external name '_SpinDownHardDisk';
{
 *  IsSpindownDisabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsSpindownDisabled: boolean; external name '_IsSpindownDisabled';
{
 *  SetSpindownDisable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetSpindownDisable(setDisable: boolean); external name '_SetSpindownDisable';
{
 *  HardDiskQInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HardDiskQInstall(var theElement: HDQueueElement): OSErr; external name '_HardDiskQInstall';
{
 *  HardDiskQRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HardDiskQRemove(var theElement: HDQueueElement): OSErr; external name '_HardDiskQRemove';
{
 *  GetScaledBatteryInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetScaledBatteryInfo(whichBattery: SInt16; var theInfo: BatteryInfo); external name '_GetScaledBatteryInfo';
{
 *  AutoSleepControl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AutoSleepControl(enableSleep: boolean); external name '_AutoSleepControl';
{
 *  GetIntModemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIntModemInfo: UInt32; external name '_GetIntModemInfo';
{
 *  SetIntModemState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetIntModemState(theState: SInt16); external name '_SetIntModemState';
{
 *  MaximumProcessorSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MaximumProcessorSpeed: SInt16; external name '_MaximumProcessorSpeed';
{
 *  MinimumProcessorSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MinimumProcessorSpeed: SInt16; external name '_MinimumProcessorSpeed';
{
 *  CurrentProcessorSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CurrentProcessorSpeed: SInt16; external name '_CurrentProcessorSpeed';
{
 *  FullProcessorSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FullProcessorSpeed: boolean; external name '_FullProcessorSpeed';
{
 *  SetProcessorSpeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetProcessorSpeed(fullSpeed: boolean): boolean; external name '_SetProcessorSpeed';
{
 *  GetSCSIDiskModeAddress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSCSIDiskModeAddress: SInt16; external name '_GetSCSIDiskModeAddress';
{
 *  SetSCSIDiskModeAddress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetSCSIDiskModeAddress(scsiAddress: SInt16); external name '_SetSCSIDiskModeAddress';
{
 *  GetWakeupTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetWakeupTimer(var theTime: WakeupTime); external name '_GetWakeupTimer';
{
 *  SetWakeupTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetWakeupTimer(var theTime: WakeupTime); external name '_SetWakeupTimer';
{
 *  IsProcessorCyclingEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsProcessorCyclingEnabled: boolean; external name '_IsProcessorCyclingEnabled';
{
 *  EnableProcessorCycling()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure EnableProcessorCycling(enable: boolean); external name '_EnableProcessorCycling';
{
 *  BatteryCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function BatteryCount: SInt16; external name '_BatteryCount';
{
 *  GetBatteryVoltage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetBatteryVoltage(whichBattery: SInt16): Fixed; external name '_GetBatteryVoltage';
{
 *  GetBatteryTimes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetBatteryTimes(whichBattery: SInt16; var theTimes: BatteryTimeRec); external name '_GetBatteryTimes';
{
 *  GetDimmingTimeout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDimmingTimeout: ByteParameter; external name '_GetDimmingTimeout';
{
 *  SetDimmingTimeout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetDimmingTimeout(timeout: ByteParameter); external name '_SetDimmingTimeout';
{
 *  DimmingControl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DimmingControl(enableSleep: boolean); external name '_DimmingControl';
{
 *  IsDimmingControlDisabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsDimmingControlDisabled: boolean; external name '_IsDimmingControlDisabled';
{
 *  IsAutoSlpControlDisabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsAutoSlpControlDisabled: boolean; external name '_IsAutoSlpControlDisabled';
{
 *  PMgrStateQInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMgrStateQInstall(var theElement: PMgrQueueElement): OSErr; external name '_PMgrStateQInstall';
{
 *  PMgrStateQRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PMgrStateQRemove(var theElement: PMgrQueueElement): OSErr; external name '_PMgrStateQRemove';
{
 *  UpdateSystemActivity()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UpdateSystemActivity(activity: ByteParameter): OSErr; external name '_UpdateSystemActivity';
{
 *  DelaySystemIdle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DelaySystemIdle: OSErr; external name '_DelaySystemIdle';
{
 *  GetStartupTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetStartupTimer(var theTime: StartupTime): OSErr; external name '_GetStartupTimer';
{
 *  SetStartupTimer()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetStartupTimer(var theTime: StartupTime): OSErr; external name '_SetStartupTimer';
{
 *  GetLastActivity()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetLastActivity(var theActivity: ActivityInfo): OSErr; external name '_GetLastActivity';
{
 *  GetSoundMixerState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetSoundMixerState(var theSoundMixerByte: SoundMixerByte): OSErr; external name '_GetSoundMixerState';
{
 *  SetSoundMixerState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetSoundMixerState(var theSoundMixerByte: SoundMixerByte): OSErr; external name '_SetSoundMixerState';
{
 *  GetDimSuspendState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDimSuspendState: boolean; external name '_GetDimSuspendState';
{
 *  SetDimSuspendState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetDimSuspendState(dimSuspendState: boolean); external name '_SetDimSuspendState';
{$ifc CALL_NOT_IN_CARBON}
{
 *  GetCoreProcessorTemperature()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetCoreProcessorTemperature(inCpuID: MPCpuID): SInt32; external name '_GetCoreProcessorTemperature';
{
 *  GetWakeOnNetworkOptions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetWakeOnNetworkOptions: OptionBits; external name '_GetWakeOnNetworkOptions';
{
 *  SetWakeOnNetworkOptions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure SetWakeOnNetworkOptions(inOptions: OptionBits); external name '_SetWakeOnNetworkOptions';
{
 *  AddPowerSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AddPowerSource(var ioPowerSource: PowerSourceParamBlock): OSStatus; external name '_AddPowerSource';
{
 *  RemovePowerSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RemovePowerSource(inSourceID: PowerSourceID): OSStatus; external name '_RemovePowerSource';
{
 *  UpdatePowerSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function UpdatePowerSource(var ioSource: PowerSourceParamBlock): OSStatus; external name '_UpdatePowerSource';
{
 *  IsServerModeEnabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IsServerModeEnabled: boolean; external name '_IsServerModeEnabled';
{
 *  EnableServerMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure EnableServerMode(inEnable: boolean); external name '_EnableServerMode';
{ 
   NumBatteriesInstalled is different from BatteryCount in that it
   indicates how many batteries are actually available at the time
   it is called (including UPS batteries). BatteryCount shows a 
   static number of batteries a machine is capable of holding which does NOT
   include UPS batteries. So, while a desktop might show a BatteryCount
   of zero, its NumBatteriesInstalled value might be 1 or more if a UPS
   is attached. 
}
{
 *  NumBatteriesInstalled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in PowerMgrLib 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NumBatteriesInstalled: UInt32; external name '_NumBatteriesInstalled';
{ Power Handler Management }
{
 *  IsPCIPowerOffDisabled()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IsPCIPowerOffDisabled: boolean; external name '_IsPCIPowerOffDisabled';

{
 *  EnablePCIPowerOff()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure EnablePCIPowerOff(inEnable: boolean); external name '_EnablePCIPowerOff';

{
 *  AddDevicePowerHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AddDevicePowerHandler(regEntryID: RegEntryIDPtr; handler: PowerHandlerProcPtr; refCon: UInt32; deviceType: CStringPtr): OSStatus; external name '_AddDevicePowerHandler';

{
 *  RemoveDevicePowerHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RemoveDevicePowerHandler(regEntryID: RegEntryIDPtr): OSStatus; external name '_RemoveDevicePowerHandler';

{
 *  RemoveDevicePowerHandlerForProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RemoveDevicePowerHandlerForProc(proc: PowerHandlerProcPtr): OSStatus; external name '_RemoveDevicePowerHandlerForProc';

{
 *  GetDevicePowerLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetDevicePowerLevel(regEntryID: RegEntryIDPtr; var devicePowerLevel: PowerLevel): OSStatus; external name '_GetDevicePowerLevel';

{
 *  SetDevicePowerLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DriverServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetDevicePowerLevel(regEntryID: RegEntryIDPtr; devicePowerLevel: PowerLevel): OSStatus; external name '_SetDevicePowerLevel';


{$endc}  {CALL_NOT_IN_CARBON}


const
	uppSleepQProcInfo = $00131832;
	uppHDSpindownProcInfo = $000000C0;
	uppPMgrStateChangeProcInfo = $000003C0;
	{
	 *  NewSleepQUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewSleepQUPP(userRoutine: SleepQProcPtr): SleepQUPP; external name '_NewSleepQUPP'; { old name was NewSleepQProc }
{
 *  NewHDSpindownUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHDSpindownUPP(userRoutine: HDSpindownProcPtr): HDSpindownUPP; external name '_NewHDSpindownUPP'; { old name was NewHDSpindownProc }
{
 *  NewPMgrStateChangeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewPMgrStateChangeUPP(userRoutine: PMgrStateChangeProcPtr): PMgrStateChangeUPP; external name '_NewPMgrStateChangeUPP'; { old name was NewPMgrStateChangeProc }
{
 *  DisposeSleepQUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeSleepQUPP(userUPP: SleepQUPP); external name '_DisposeSleepQUPP';
{
 *  DisposeHDSpindownUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHDSpindownUPP(userUPP: HDSpindownUPP); external name '_DisposeHDSpindownUPP';
{
 *  DisposePMgrStateChangeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposePMgrStateChangeUPP(userUPP: PMgrStateChangeUPP); external name '_DisposePMgrStateChangeUPP';
{
 *  InvokeSleepQUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeSleepQUPP(message: SInt32; qRecPtr: SleepQRecPtr; userRoutine: SleepQUPP): SInt32; external name '_InvokeSleepQUPP'; { old name was CallSleepQProc }
{
 *  InvokeHDSpindownUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeHDSpindownUPP(theElement: HDQueueElementPtr; userRoutine: HDSpindownUPP); external name '_InvokeHDSpindownUPP'; { old name was CallHDSpindownProc }
{
 *  InvokePMgrStateChangeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokePMgrStateChangeUPP(theElement: PMgrQueueElementPtr; stateBits: SInt32; userRoutine: PMgrStateChangeUPP); external name '_InvokePMgrStateChangeUPP'; { old name was CallPMgrStateChangeProc }
{$ALIGN MAC68K}


end.
