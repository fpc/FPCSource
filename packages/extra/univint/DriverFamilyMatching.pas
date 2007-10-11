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
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit DriverFamilyMatching;
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
	kServiceCategoryDisplay		= $64697370 (* 'disp' *);						{  Display Manager }
	kServiceCategoryOpenTransport = $6F74616E (* 'otan' *);						{  Open Transport }
	kServiceCategoryBlockStorage = $626C6F6B (* 'blok' *);						{  Block Storage }
	kServiceCategoryNdrvDriver	= $6E647276 (* 'ndrv' *);						{  Generic Native Driver }
	kServiceCategoryScsiSIM		= $73637369 (* 'scsi' *);						{  SCSI  }
	kServiceCategoryFileManager	= $66696C65 (* 'file' *);						{  File Manager  }
	kServiceCategoryIDE			= $6964652D (* 'ide-' *);						{  ide  }
	kServiceCategoryADB			= $6164622D (* 'adb-' *);						{  adb  }
	kServiceCategoryPCI			= $7063692D (* 'pci-' *);						{  pci bus  }
																{  Nu Bus  }
	kServiceCategoryDFM			= $64666D2D (* 'dfm-' *);						{  DFM  }
	kServiceCategoryMotherBoard	= $6D726264 (* 'mrbd' *);						{  mother Board  }
	kServiceCategoryKeyboard	= $6B796264 (* 'kybd' *);						{  Keyboard  }
	kServiceCategoryPointing	= $706F6974 (* 'poit' *);						{  Pointing  }
	kServiceCategoryRTC			= $7274632D (* 'rtc-' *);						{  RTC  }
	kServiceCategoryNVRAM		= $6E72616D (* 'nram' *);						{  NVRAM  }
	kServiceCategorySound		= $736F6E64 (* 'sond' *);						{  Sound (1/3/96 MCS)  }
	kServiceCategoryPowerMgt	= $70676D74 (* 'pgmt' *);						{  Power Management  }
	kServiceCategoryGeneric		= $67656E72 (* 'genr' *);						{  Generic Service Category to receive general Events  }

	{	 Ndrv ServiceCategory Types 	}
	kNdrvTypeIsGeneric			= $67656E72 (* 'genr' *);						{  generic }
	kNdrvTypeIsVideo			= $7669646F (* 'vido' *);						{  video }
	kNdrvTypeIsBlockStorage		= $626C6F6B (* 'blok' *);						{  block storage }
	kNdrvTypeIsNetworking		= $6E657477 (* 'netw' *);						{  networking }
	kNdrvTypeIsSerial			= $7365726C (* 'serl' *);						{  serial }
	kNdrvTypeIsParallel			= $7061726C (* 'parl' *);						{  parallel  }
	kNdrvTypeIsSound			= $736F6E64 (* 'sond' *);						{  sound }
	kNdrvTypeIsBusBridge		= $62726467 (* 'brdg' *);
	kNdrvTypeIsFWConference		= $63727368 (* 'crsh' *);						{  FireWire conference camera  }
	kNdrvTypeIsAVC				= $61766320 (* 'avc ' *);						{  FireWire AVC devices (DV cameras)  }


type
	DriverDescVersion					= UInt32;
	DriverDescVersion_fix               = DriverDescVersion; { used as field type when a record declaration contains a DriverDescVersion field identifier }
	{	  The Driver Description 	}

const
	kTheDescriptionSignature	= $6D74656A (* 'mtej' *);
	kDriverDescriptionSignature	= $70646573 (* 'pdes' *);

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
