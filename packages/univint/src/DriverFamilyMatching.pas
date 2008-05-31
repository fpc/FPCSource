{
     File:       DriverFamilyMatching.p
 
     Contains:   Interfaces for create native drivers NDRV
 
     Version:    Universal Interfaces 3.4.2
 
     Copyright:  © 1995-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit DriverFamilyMatching;
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
uses MacTypes,NameRegistry,CodeFragments;


{$ALIGN MAC68K}

{
  ##############################################
   Well known properties in the Name Registry
  ##############################################
}
{ CPassThru }
{
  #########################################################
   Descriptor for Drivers and NDRVs
  #########################################################
}
{ 
    QuickTime 3.0: "DriverType" has a name collision with cross-platform code.
    Use Mac prefix to avoid collision 
}
{ Driver Typing Information Used to Match Drivers With Devices }

type
	MacDriverTypePtr = ^MacDriverType;
	MacDriverType = record
		nameInfoStr:			Str31;									{  Driver Name/Info String }
		version:				NumVersion;								{  Driver Version Number }
	end;

{$ifc TARGET_OS_MAC}
	DriverType							= MacDriverType;
	DriverTypePtr 						= ^DriverType;
{$endc}  {TARGET_OS_MAC}

	{	 OS Runtime Information Used to Setup and Maintain a Driver's Runtime Environment 	}
	RuntimeOptions						= OptionBits;

const
	kDriverIsLoadedUponDiscovery = $00000001;					{  auto-load driver when discovered }
	kDriverIsOpenedUponLoad		= $00000002;					{  auto-open driver when loaded }
	kDriverIsUnderExpertControl	= $00000004;					{  I/O expert handles loads/opens }
	kDriverIsConcurrent			= $00000008;					{  supports concurrent requests }
	kDriverQueuesIOPB			= $00000010;					{  device manager doesn't queue IOPB }
	kDriverIsLoadedAtBoot		= $00000020;					{  Driver is loaded at the boot time  }
	kDriverIsForVirtualDevice	= $00000040;					{  Driver is for a virtual Device  }
	kDriverSupportDMSuspendAndResume = $00000080;				{  Driver supports Device Manager Suspend and Resume command  }


type
	DriverOSRuntimePtr = ^DriverOSRuntime;
	DriverOSRuntime = record
		driverRuntime:			RuntimeOptions;							{  Options for OS Runtime }
		driverName:				Str31;									{  Driver's name to the OS }
		driverDescReserved:		array [0..7] of UInt32;					{  Reserved area }
	end;

	{	 OS Service Information Used To Declare What APIs a Driver Supports 	}
	ServiceCount						= UInt32;
	DriverServiceInfoPtr = ^DriverServiceInfo;
	DriverServiceInfo = record
		serviceCategory:		OSType;									{  Service Category Name }
		serviceType:			OSType;									{  Type within Category }
		serviceVersion:			NumVersion;								{  Version of service }
	end;

	DriverOSServicePtr = ^DriverOSService;
	DriverOSService = record
		nServices:				ServiceCount;							{  Number of Services Supported }
		service:				array [0..0] of DriverServiceInfo;		{  The List of Services (at least one) }
	end;

	{	 Categories 	}

const
	kServiceCategoryDisplay		= FourCharCode('disp');						{  Display Manager }
	kServiceCategoryOpenTransport = FourCharCode('otan');						{  Open Transport }
	kServiceCategoryBlockStorage = FourCharCode('blok');						{  Block Storage }
	kServiceCategoryNdrvDriver	= FourCharCode('ndrv');						{  Generic Native Driver }
	kServiceCategoryScsiSIM		= FourCharCode('scsi');						{  SCSI  }
	kServiceCategoryFileManager	= FourCharCode('file');						{  File Manager  }
	kServiceCategoryIDE			= FourCharCode('ide-');						{  ide  }
	kServiceCategoryADB			= FourCharCode('adb-');						{  adb  }
	kServiceCategoryPCI			= FourCharCode('pci-');						{  pci bus  }
																{  Nu Bus  }
	kServiceCategoryDFM			= FourCharCode('dfm-');						{  DFM  }
	kServiceCategoryMotherBoard	= FourCharCode('mrbd');						{  mother Board  }
	kServiceCategoryKeyboard	= FourCharCode('kybd');						{  Keyboard  }
	kServiceCategoryPointing	= FourCharCode('poit');						{  Pointing  }
	kServiceCategoryRTC			= FourCharCode('rtc-');						{  RTC  }
	kServiceCategoryNVRAM		= FourCharCode('nram');						{  NVRAM  }
	kServiceCategorySound		= FourCharCode('sond');						{  Sound (1/3/96 MCS)  }
	kServiceCategoryPowerMgt	= FourCharCode('pgmt');						{  Power Management  }
	kServiceCategoryGeneric		= FourCharCode('genr');						{  Generic Service Category to receive general Events  }

	{	 Ndrv ServiceCategory Types 	}
	kNdrvTypeIsGeneric			= FourCharCode('genr');						{  generic }
	kNdrvTypeIsVideo			= FourCharCode('vido');						{  video }
	kNdrvTypeIsBlockStorage		= FourCharCode('blok');						{  block storage }
	kNdrvTypeIsNetworking		= FourCharCode('netw');						{  networking }
	kNdrvTypeIsSerial			= FourCharCode('serl');						{  serial }
	kNdrvTypeIsParallel			= FourCharCode('parl');						{  parallel  }
	kNdrvTypeIsSound			= FourCharCode('sond');						{  sound }
	kNdrvTypeIsBusBridge		= FourCharCode('brdg');
	kNdrvTypeIsFWConference		= FourCharCode('crsh');						{  FireWire conference camera  }
	kNdrvTypeIsAVC				= FourCharCode('avc ');						{  FireWire AVC devices (DV cameras)  }


type
	DriverDescVersion					= UInt32;
	DriverDescVersion_fix               = DriverDescVersion; { used as field type when a record declaration contains a DriverDescVersion field identifier }
	{	  The Driver Description 	}

const
	kTheDescriptionSignature	= FourCharCode('mtej');
	kDriverDescriptionSignature	= FourCharCode('pdes');

	kInitialDriverDescriptor	= 0;
	kVersionOneDriverDescriptor	= 1;


type
	DriverDescriptionPtr = ^DriverDescription;
	DriverDescription = record
		driverDescSignature:	OSType;									{  Signature field of this structure }
		driverDescVersion:		DriverDescVersion_fix;                  {  Version of this data structure }
		driverType:				MacDriverType;							{  Type of Driver }
		driverOSRuntimeInfo:	DriverOSRuntime;						{  OS Runtime Requirements of Driver }
		driverServices:			DriverOSService;						{  Apple Service API Membership }
	end;


{$ALIGN MAC68K}


end.
