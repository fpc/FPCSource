{
     File:       DriverGestalt.p
 
     Contains:   Driver Gestalt interfaces
 
     Version:    Technology: Mac OS 9
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1995-2002 by Apple Computer, Inc., all rights reserved
 
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

unit DriverGestalt;
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
uses MacTypes,OSUtils,NameRegistry,SCSI,USB;


{$ALIGN MAC68K}

{__________________________________________________________________________________}
{ The Driver Gestalt bit in the dCtlFlags }

const
	kbDriverGestaltEnable		= 2;
	kmDriverGestaltEnableMask	= $04;

	{	__________________________________________________________________________________	}
	{	 Driver Gestalt related csCodes 	}
	kDriverGestaltCode			= 43;							{  various uses  }
	kDriverConfigureCode		= 43;							{  various uses  }
	kcsSetBootPartitionCode		= 44;							{  Set Startup Partition Control Call  }
	kcsGetBootPartitionStatus	= 44;							{  Get Startup Partition Status Call  }
	kdgLowPowerMode				= 70;							{  Sets/Returns the current energy consumption level  }
	kdgReturnDeviceID			= 120;							{  returns SCSI DevID in csParam[0]  }
	kdgGetCDDeviceInfo			= 121;							{  returns CDDeviceCharacteristics in csParam[0]  }

	{	__________________________________________________________________________________	}
	{	 Driver Gestalt selectors 	}
	kdgVersion					= $76657273 (* 'vers' *);						{  Version number of the driver in standard Apple format  }
	kdgDeviceType				= $64657674 (* 'devt' *);						{  The type of device the driver is driving.  }
	kdgInterface				= $696E7466 (* 'intf' *);						{  The underlying interface that the driver is using (if any)  }
	kdgSync						= $73796E63 (* 'sync' *);						{  True if driver only behaves synchronously.  }
	kdgBoot						= $626F6F74 (* 'boot' *);						{  value to place in PRAM for this drive (long)  }
	kdgWide						= $77696465 (* 'wide' *);						{  True if driver supports ioWPosOffset  }
	kdgPurge					= $70757267 (* 'purg' *);						{  Driver purge permission (True = purge; False = no purge)  }
	kdgSupportsSwitching		= $6C707772 (* 'lpwr' *);						{  True if driver supports power switching  }
	kdgMin3VPower				= $706D6E33 (* 'pmn3' *);						{  Minimum 3.3V power consumption in microWatts  }
	kdgMin5VPower				= $706D6E35 (* 'pmn5' *);						{  Minimum 5V power consumption in microWatts  }
	kdgMax3VPower				= $706D7833 (* 'pmx3' *);						{  Maximum 3.3V power consumption in microWatts  }
	kdgMax5VPower				= $706D7835 (* 'pmx5' *);						{  Maximum 5V power consumption in microWatts  }
	kdgInHighPower				= $70737461 (* 'psta' *);						{  True if device is currently in high power mode  }
	kdgSupportsPowerCtl			= $70737570 (* 'psup' *);						{  True if driver supports following five calls  }
	kdgAPI						= $64415049 (* 'dAPI' *);						{  API support for PC Exchange  }
	kdgEject					= $656A6563 (* 'ejec' *);						{  Eject options for shutdown/restart (Shutdown Mgr)  }
	kdgFlush					= $666C7573 (* 'flus' *);						{  Determine if disk driver supports flush and if it needs a flush  }
	kdgVMOptions				= $766D6F70 (* 'vmop' *);						{  Disk drive's Virtual Memory options  }
	kdgMediaInfo				= $6D696E66 (* 'minf' *);						{  return media specific information  }
	kdgPhysDriveIconSuite		= $64696373 (* 'dics' *);						{  Return a pointer to a IconFamily ('icns') data structure for  }
																{  Disk Driver physical drive (formerly in csCode 21) in driverGestaltResponse.  }
	kdgMediaIconSuite			= $6D696373 (* 'mics' *);						{  Return a pointer to a IconFamily ('icns') data structure for  }
																{  Disk Driver media (formerly in csCode 22) in driverGestaltResponse.  }
																{  See IconServices.r for information detailing the 'icns' resource data format  }
	kdgMediaName				= $6D6E616D (* 'mnam' *);						{  Return a pointer to a pascal string describing the Disk Driver (formerly in csCode 21) in driverGestaltResponse.  }
	kdgGetDriveAddInfo			= $64696774 (* 'digt' *);						{  Get a disk driver's add-drive information record  }
	kdcAddDriveWithInfo			= $64696164 (* 'diad' *);						{  Tell disk driver to add the drive specified with the drive information record  }
																{  DriverGestalt selector for ATA drivers to signify that they are device 0/1 compliant.  }
																{  see http://developer.apple.com/techpubs/hardware/Developer_Notes/System_Software/ATA_Device_Zero_One.pdf  }
	kdgATADev1					= $64657631 (* 'dev1' *);
	kdgDeviceReference			= $64767266 (* 'dvrf' *);						{  Returns a 32-bit reference number for the device, format is interface specific  }
	kdgNameRegistryEntry		= $6E6D7267 (* 'nmrg' *);						{  Returns a pointer to the Name Registry ID for the device  }
	kdgDeviceModelInfo			= $696E666F (* 'info' *);						{  Returns a pointer to a Device Model Info structure  }
	kdgSupportedMediaTypes		= $6D647479 (* 'mdty' *);						{  Returns a count and a pointer to list of all media types supported by the device  }
	kdgOpenFirmwareBootSupport	= $6F667074 (* 'ofpt' *);						{  Returns information that Open Firmware needs to support booting from the device  }
	kdgOpenFirmwareBootingSupport = $6F666274 (* 'ofbt' *);						{  Returns same information as kdgOpenFirmwareBootSupport, but is only used during booting }

	{	__________________________________________________________________________________	}
	{	 Driver Configure selectors 	}
	kdcFlush					= $666C7573 (* 'flus' *);						{  Tell a disk driver to flush its cache and any hardware caches  }
	kdcVMOptions				= $766D6F70 (* 'vmop' *);						{  Change the disk driver's Virtual Memory options  }

	{	__________________________________________________________________________________	}
	{	 control parameter block for Driver Configure calls 	}

type
	DriverConfigParamPtr = ^DriverConfigParam;
	DriverConfigParam = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			ProcPtr;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		ioCRefNum:				SInt16;								{  refNum for I/O operation  }
		csCode:					SInt16;								{  == kDriverConfigureCode  }
		driverConfigureSelector: OSType;
		driverConfigureParameter: UInt32;
	end;

	{	__________________________________________________________________________________	}
	{	 status parameter block for Driver Gestalt calls 	}
	DriverGestaltParamPtr = ^DriverGestaltParam;
	DriverGestaltParam = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			ProcPtr;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		ioCRefNum:				SInt16;								{  refNum for I/O operation  }
		csCode:					SInt16;								{     == kDriverGestaltCode  }
		driverGestaltSelector:	OSType;									{  'sync', 'vers', etc.  }
		driverGestaltResponse:	UInt32;									{  Could be a pointer, bit field or other format  }
		driverGestaltResponse1:	UInt32;									{  Could be a pointer, bit field or other format  }
		driverGestaltResponse2:	UInt32;									{  Could be a pointer, bit field or other format  }
		driverGestaltResponse3:	UInt32;									{  Could be a pointer, bit field or other format  }
		driverGestaltfiller:	UInt16;									{  To pad out to the size of a controlPB  }
	end;

	{	__________________________________________________________________________________	}
	{	 Device Types response 	}
	DriverGestaltDevTResponsePtr = ^DriverGestaltDevTResponse;
	DriverGestaltDevTResponse = record
		deviceType:				OSType;
	end;


const
	kdgDiskType					= $6469736B (* 'disk' *);						{  standard r/w disk drive  }
	kdgTapeType					= $74617065 (* 'tape' *);						{  tape drive  }
	kdgPrinterType				= $70726E74 (* 'prnt' *);						{  printer  }
	kdgProcessorType			= $70726F63 (* 'proc' *);						{  processor  }
	kdgWormType					= $776F726D (* 'worm' *);						{  write-once  }
	kdgCDType					= $6364726D (* 'cdrm' *);						{  cd-rom drive  }
	kdgFloppyType				= $666C6F70 (* 'flop' *);						{  floppy disk drive  }
	kdgScannerType				= $7363616E (* 'scan' *);						{  scanner  }
	kdgFileType					= $66696C65 (* 'file' *);						{  Logical Partition type based on a file (Drive Container)  }
	kdgRemovableType			= $7264736B (* 'rdsk' *);						{  A removable media hard disk drive ie. Syquest, Bernioulli  }

	{	__________________________________________________________________________________	}
	{	 Device Interfaces response 	}

type
	DriverGestaltIntfResponsePtr = ^DriverGestaltIntfResponse;
	DriverGestaltIntfResponse = record
		interfaceType:			OSType;
	end;


const
	kdgScsiIntf					= $73637369 (* 'scsi' *);
	kdgPcmciaIntf				= $70636D63 (* 'pcmc' *);
	kdgATAIntf					= $61746120 (* 'ata ' *);
	kdgUSBIntf					= $75736220 (* 'usb ' *);
	kdgFireWireIntf				= $66697265 (* 'fire' *);
	kdgExtBus					= $63617264 (* 'card' *);
	kdgNetworkIntf				= $6E657420 (* 'net ' *);

	{	__________________________________________________________________________________	}
	{	 Power Saving 	}

type
	DriverGestaltPowerResponsePtr = ^DriverGestaltPowerResponse;
	DriverGestaltPowerResponse = record
		powerValue:				UInt32;									{  Power consumed in µWatts  }
	end;

	{	__________________________________________________________________________________	}
	{	 Disk Specific 	}
	DriverGestaltSyncResponsePtr = ^DriverGestaltSyncResponse;
	DriverGestaltSyncResponse = record
		behavesSynchronously:	boolean;
		pad1,pad2,pad3: SInt8;
	end;

	DriverGestaltBootResponsePtr = ^DriverGestaltBootResponse;
	DriverGestaltBootResponse = record
		extDev:					SInt8;									{   Packed target (upper 5 bits) LUN (lower 3 bits)  }
		partition:				SInt8;									{   Unused  }
		SIMSlot:				SInt8;									{   Slot  }
		SIMsRSRC:				SInt8;									{   sRsrcID  }
	end;

	DriverGestaltAPIResponsePtr = ^DriverGestaltAPIResponse;
	DriverGestaltAPIResponse = record
		partitionCmds:			SInt16;								{  if bit 0 is nonzero, supports partition control and status calls  }
																		{        prohibitMounting (control, kProhibitMounting)  }
																		{       partitionToVRef (status, kGetPartitionStatus)  }
																		{       getPartitionInfo (status, kGetPartInfo)  }
		unused1:				SInt16;								{  all the unused fields should be zero  }
		unused2:				SInt16;
		unused3:				SInt16;
		unused4:				SInt16;
		unused5:				SInt16;
		unused6:				SInt16;
		unused7:				SInt16;
		unused8:				SInt16;
		unused9:				SInt16;
		unused10:				SInt16;
	end;

	DriverGestaltFlushResponsePtr = ^DriverGestaltFlushResponse;
	DriverGestaltFlushResponse = record
		canFlush:				boolean;								{  Return true if driver supports the  }
																		{  kdcFlush Driver Configure _Control call  }
		needsFlush:				boolean;								{  Return true if driver/device has data cached  }
																		{  and needs to be flushed when the disk volume  }
																		{  is flushed by the File Manager  }
		pad:					packed array [0..1] of UInt8;
	end;

	{	 Flags for purge permissions 	}

const
	kbCloseOk					= 0;							{  Ok to call Close  }
	kbRemoveOk					= 1;							{  Ok to call RemoveDrvr  }
	kbPurgeOk					= 2;							{  Ok to call DisposePtr  }
	kmNoCloseNoPurge			= 0;
	kmOkCloseNoPurge			= $03;
	kmOkCloseOkPurge			= $07;

	{	 Driver purge permission structure 	}

type
	DriverGestaltPurgeResponsePtr = ^DriverGestaltPurgeResponse;
	DriverGestaltPurgeResponse = record
		purgePermission:		UInt16;									{  0 = Do not change the state of the driver  }
																		{  3 = Do Close() and DrvrRemove() this driver  }
																		{  but don't deallocate driver code  }
																		{  7 = Do Close(), DrvrRemove(), and DisposePtr()  }
		purgeReserved:			UInt16;
		purgeDriverPointer:		Ptr;									{  pointer to the start of the driver block (valid  }
																		{  only of DisposePtr permission is given  }
	end;

	DriverGestaltEjectResponsePtr = ^DriverGestaltEjectResponse;
	DriverGestaltEjectResponse = record
		ejectFeatures:			UInt32;									{    }
	end;

	{	 Flags for Ejection Features field 	}

const
	kRestartDontEject			= 0;							{  Dont Want eject during Restart  }
	kShutDownDontEject			= 1;							{  Dont Want eject during Shutdown  }
	kRestartDontEject_Mask		= $01;
	kShutDownDontEject_Mask		= $02;

	{	
	    The DriverGestaltVMOptionsResponse is returned by a disk driver in response to a
	    kdgVMOptions Driver Gestalt request. This allows a disk driver to tell VM a few
	    things about a disk drive. For example:
	    
	    ¥ A drive that should never be in the page fault path should return kAllowVMNoneMask.
	      Examples of this are drives that have manual eject buttons that are not disabled by
	      software, drives with very slow throughput, or drives that depend on
	      a network connection.
	    ¥ A drive that should never be written to but is safe for read-only file mapping
	      should return kAllowVMReadOnlyMask. Examples of this are WORM drives where each write
	      eats write-once space on the disk and CD-ROM drives which are read-only media.
	    ¥ A drive that should allow VM to create its main backing store file should return
	      kAllowVMReadWriteMask. Examples of this are fast read/write drives that don't allow
	      manual eject and don't use a network connection.
	    
	    A disk driver must look at the ioVRefNum field of the DriverGestaltParam to determine
	    what disk drive this call is for. This is a per-drive call, not a per-driver call.
	    
	    The only three valid responses to kdgVMOptions at this time are kAllowVMNoneMask,
	    kAllowVMReadOnlyMask, and kAllowVMReadWriteMask (i.e., setting only kAllowVMWriteBit
	    is not valid).
	    
	    Important: All bits not defined here are reserved and should be set to zero until
	    they are defined for a specific purpose.
	    
	    The kdcVMOptions Driver Configure _Control call provides the ability to change a driver's
	    response to kdgVMOptions Driver Gestalt requests. A driver should return controlErr if
	    it doesn't want to provide the ability to change the kdgVMOptions response. If a driver
	    supports the kdcVMOptions Driver Configure _Control call, but is asked to set an option bit
	    that it doesn't support (for example, if a read-only device is asked to set the kAllowVMWriteBit),
	    it should return paramErr.
		}

type
	DriverGestaltVMOptionsResponsePtr = ^DriverGestaltVMOptionsResponse;
	DriverGestaltVMOptionsResponse = record
		vmOptions:				UInt32;
	end;

	{	 Bits and masks for DriverGestaltVMOptionsResponse.vmOptions field 	}

const
	kAllowVMReadBit				= 0;							{  Allow VM to use this drive for read access  }
	kAllowVMWriteBit			= 1;							{  Allow VM to use this drive for write access  }
	kAllowVMNoneMask			= 0;
	kAllowVMReadOnlyMask		= $01;
	kAllowVMReadWriteMask		= $03;

	{	 
	kdgGetDriveAddInfo/kdcAddDriveWithInfo is used by the Alias Manager to
	assist in the  remounting of container files. eg DiskCopy.  The driver
	can use it to store whatever information it needs to remount a drive
	For example the Volume name and FileID..  What is in here is entirely 
	dependant on the driver, we never interpret it.
	
	This extension to the Alias Manager works as such:
	   
	When creating an alias 
	----------------------
	    
	If driver supports Driver Gestalt, the Alias Mgr calls the driver with a 
	 kdgGetDriveAddInfo DriverGestalt request. 
	        
	Inputs to DriverGestalt:
	    ioVRefNum   = drive number
	    ioCRefNum   = driver refNum
	    csCode      = kDriverGestaltCode
	    driverGestaltSelector = kdgGetDriveAddInfo
	 
	If the driver supports kdgGetDriveAddInfo, it returns a pointer to the drive's
	drive  information record (DriverGestaltDriveAddInfoResponse*) in the 
	driverGestaltResponse field and returns noErr. 
	
	  The memory used by the DriverGestaltDriveAddInfoResponse and it's pointed 
	to data, must be previously held by the disk driver with HoldMemory. 
	        
	If the driver doesn't support kdgGetDriveAddInfo,  it returns statusErr.
	
	If noErr, the Alias Mgr stores the drive information record in the alias record 
	(the length of the drive information record data is in the length field).
	
	
	When resolving an alias:
	------------------------
	
	If driver supports Driver Gestalt, the Alias Mgr holds the drive information record 
	in the alias with HoldMemory and then calls the driver with a kdcAddDriveWithInfo
	DriverConfigure request. 
	        
	Inputs to DriverConfigure:
	    ioCRefNum   = driver refNum
	    csCode      = kDriverConfigureCode
	    driverGestaltSelector = pointer to DriverGestaltDriveAddInfoResponse
	            
	
	If the driver supports kdcAddDriveWithInfo, it uses the information in the drive 
	information record to add a drive to the drive queue. If the drive is added,
	the driver returns noErr and returns the drive number in the ioVRefNum field of 
	the parameter block. 
	
	If the drive cannot be added  (but kdcAddDriveWithInfo is supported), the driver 
	returns nsDrvErr. 
	
	If the driver doesn't  support kdcAddDriveWithInfo, it returns controlErr. 
	
	If noErr is returned, but the data in the  drive information record is not 
	up-to-date, the driver should set driveInfoChangedBit in the options field of the 
	drive information record. 
	
	If driveInfoInteractBit in the options field  is set at input, the driver 
	may perform user interaction to mount the drive.
	
	If noErr, the Alias Mgr tries to resolve the alias the rest of the way.
	 	}
	{	 drive info option flags 	}
	driveInfoInteractBit		= 31;							{  Input to kdcAddDriveWithInfo: If set, it's OK for the driver  }
	driveInfoInteractMask		= $80000000;					{   to perform user interaction to add the drive  }
	driveInfoChangedBit			= 30;							{  Output from kdgGetDriveInfo: If set, the drive was mounted, but  }
	driveInfoChangedMask		= $40000000;					{   the drive information record needs to be updated.  }
	driveInfoDriverReservedMask	= $0000FFFF;					{  bits 0-15 are defined by each driver for its own use  }
	driveInfoSystemReservedMask	= $FFFF0000;					{  bits 16-31 are reserved for Apple system use  }


type
	DriverGestaltDriveAddInfoResponsePtr = ^DriverGestaltDriveAddInfoResponse;
	DriverGestaltDriveAddInfoResponse = record
		options:				OptionBits;								{  option flags.  }
		length:					ByteCount;								{  length of data  }
		data:					Ptr;									{  this data must be held with HoldMemory  }
	end;

	{	
	    The DriverGestaltMediaInfoResponse is returned by a disk driver in response to a
	    kdgMediaInfo DriverGestalt request. This allows a disk driver to tell callers the
	    physical block size, the number of blocks that are of that size, and the media type
	    for a given device.
	    
	    A disk driver must look at the ioVRefNum field of the DriverGestaltParam to determine
	    what disk drive this call is for. This is a per-drive call, not a per-driver call.
	    
	    On drives that support ejectable media, the response can change depending on what
	    media is currently in the drive.
		}
	DriverGestaltMediaInfoResponsePtr = ^DriverGestaltMediaInfoResponse;
	DriverGestaltMediaInfoResponse = record
		numberBlocks:			UInt32;									{  number of blocks  }
		blockSize:				UInt32;									{  physical size of blocks  }
		mediaType:				SInt16;									{  media type identifier  }
	end;

	{	 DriverGestaltMediaInfoResponse.mediaType constants 	}

const
	kMediaTypeUnknown			= 128;							{  media type is unknown  }
	kMediaTypeCDROM				= 129;							{  media type is a CD-ROM  }
	kMediaTypeDVDROM			= 130;							{  media type is a DVD-ROM  }
	kMediaTypeDVDRAM			= 131;							{  media type is a DVD-RAM  }
	kMediaTypeDVDR				= 132;							{  media type is a DVD-RW  }
	kMediaTypeReadOnly			= 133;							{  basic read only type  }
	kMediaTypeWriteOnce			= 134;							{  basic write once type  }
	kMediaTypeRewritable		= 135;							{  rewritable media, i.e CD-RW  }
	kMediaTypeOverwritable		= 136;							{  random access read write media  }
	kMediaTypeNoMedia			= -1;							{  no media is present  }


type
	DriverGestaltATADev1ResponsePtr = ^DriverGestaltATADev1Response;
	DriverGestaltATADev1Response = record
		dev1Support:			UInt32;									{  1 = supports devices 0 and 1 }
	end;

	{	__________________________________________________________________________________	}
	{	 Device Reference 	}
	{	
	    The driver will return a 32-bit device reference number in the driverGestaltResponse
	    field of the DriverGestaltParam structure. The device refernce number will be interpreted 
	    differently depending on the interface retrieved by using the 'intf' Driver Gestalt call.
		}
	{	The union for the kdgDeviceReference Gestalt 	}
	DriverGestaltDeviceReferenceResponsePtr = ^DriverGestaltDeviceReferenceResponse;
	DriverGestaltDeviceReferenceResponse = record
		case SInt16 of
		0: (
			devRef:				UInt32;									{  Generic reference number for interfaces not specified  }
			);
		1: (
			scsiID:				DeviceIdent;							{  kdgScsiIntf devices will return a DeviceIdent  }
			);
		2: (
			ataID:				DeviceIdent;							{  kdgATAIntf devices will return a DeviceIdent  }
			);
		3: (
			usbRef:				USBDeviceRef;							{  kdgUSBIntf devices will return a USBDeviceRef }
			);
	end;

	{	__________________________________________________________________________________	}
	{	 Name Registry ID 	}
	{	
	    The driver will pass back in the driverGestaltResponse field of the DriverGestaltParam
	    a pointer to the Name Registry Entry ID for the device that it controls
		}
	{	 The structure for the kdgNameRegistryEntry Gestalt 	}
	DriverGestaltNameRegistryResponsePtr = ^DriverGestaltNameRegistryResponse;
	DriverGestaltNameRegistryResponse = record
		entryID:				RegEntryIDPtr;
	end;

	{	__________________________________________________________________________________	}
	{	 Device Model Information 	}
	{	
	    The driver will pass a pointer to the DriverGestaltDeviceModelInfoResponse in the
	    driverGestaltResponse field of the DriverGestaltParam structure
	
	    The structure for the kdgDeviceModelInfo Gestalt
	    If any of theses pointers are nil, that means that the device doesn't provide that information
		}
	DriverGestaltDeviceModelInfoResponsePtr = ^DriverGestaltDeviceModelInfoResponse;
	DriverGestaltDeviceModelInfoResponse = record
		infoStructVersion:		UInt32;
		vendorName:				StringPtr;
		productName:			StringPtr;
		revisionNumber:			StringPtr;
		subRevisionNumber:		StringPtr;
		serialNumber:			StringPtr;
	end;

	{	 infoStructVersion field values 	}
	{	 NOTE: May need/want to add a UniCode version of the InfoStruct at some point 	}

const
	kInfoStructStringPtrsVers1	= 1;


	{	__________________________________________________________________________________	}
	{	 Supported Media Types 	}
	{	
	    The driver will return the number of media types supported by the device in the 
	    driverGestaltResponse field of the DriverGestaltParam structure and an array of 
	    'devt' values for the supported media types will be returned in the driverGestaltResponse1
	    field of the DriverGestaltParam.
	
	    This call will return the number of media types supported by the device, and an array
	    of 'devt' values for the media types (i.e. if a device supports floppy disks
	    and large capacity removables, the driver would return that 2 media types are supported,
	    and the array would contain kdgFloppyType ('flop') and kdgRemovableType ('rdsk')).
		}
	{	 The structure for the kdgSupportedMediaTypes Gestalt 	}

type
	DriverGestaltSupportedMediaTypesResponsePtr = ^DriverGestaltSupportedMediaTypesResponse;
	DriverGestaltSupportedMediaTypesResponse = record
		supportTypesCount:		UInt32;									{  The number of Media Types in the array  }
		supportedTypesArray:	array [0..0] of OSType;					{  Array of supported media types   }
	end;

	{	__________________________________________________________________________________	}
	{	 Open Firmware Boot support  and Open Firmware Booting support 	}
	{	
	    The kdgOpenFirmwareBootSupport selector is used to get information about boot support 
	    for the driver/device.  The kdgOpenFirmwareBootingSupport is used during the boot
	    process to allow booting from partitions that Open Firmware may not be able to read
	    directly (i.e. Disk Array partitions, encrypted partitions, compressed partitions, etc..)
	    
	    The driver will return the bootPartitionQualifier value in the driverGestaltResponse 
	    field of the DriverGestaltParam and will return the exact partition map entry for
	    the boot partition in the driverGestaltResponse1 field of the DriverGestaltParam if
	    kOFBootSpecifiedPartition is returned in the driverGestaltResponse.
	    The number that is returned for the bootPartitionMapEntry field if the kOFBootSpecifiedPartition 
	    is returned is the partition map entry number for the partition. (i.e. If the partition map entry
	    for the boot partition is the 2nd partition entry in the partition map, the bootPartitionMapEntry
	    field would be set to 2.  If the partition map entry for the boot partition is the 3rd partition 
	    map, the bootPartitionMapEntry field would be set to 3 and etc.)
		}
	DriverGestaltOFBootSupportResponsePtr = ^DriverGestaltOFBootSupportResponse;
	DriverGestaltOFBootSupportResponse = record
		bootPartitionQualifier:	UInt32;									{     The exact level of booting that the driver and device supports  }
		bootPartitionMapEntry:	UInt32;									{     The Partition Map entry for the boot partition if applicable  }
	end;

	{	 Levels of boot support that the driver/device supports 	}
	{	 These values are used in the bootPartitionQualifier field of the DriverGestaltOFBootSupportResponse 	}

const
	kOFBootAnyPartition			= 1;
	kOFBootSpecifiedPartition	= 2;
	kOFBootNotBootable			= 3;
	kOFBootNotPartitioned		= 4;

	{	__________________________________________________________________________________	}
	{	 CD-ROM Specific 	}
	{	 The CDDeviceCharacteristics result is returned in csParam[0..2] of a 
	   standard CntrlParam parameter block called with csCode kdgGetCDDeviceInfo.
		}

type
	CDDeviceCharacteristicsPtr = ^CDDeviceCharacteristics;
	CDDeviceCharacteristics = record
		speedMajor:				SInt8;									{  High byte of fixed point number containing drive speed  }
		speedMinor:				SInt8;									{  Low byte of "" CD 300 == 2.2, CD_SC == 1.0 etc.  }
		cdFeatures:				UInt16;									{  Flags field for features and transport type of this CD-ROM  }
		extendedCdFeatures:		UInt16;									{  extended flags to support new DVD-ROM/DVD-R etc.  }
	end;


const
	cdFeatureFlagsMask			= $FFFC;						{  The Flags are in the first 14 bits of the cdFeatures field  }
	cdTransportMask				= $0003;						{  The transport type is in the last 2 bits of the cdFeatures field  }


	{	 Flags for cdFeatures field 	}
	cdPowerInject				= 0;							{  device supports power inject of media }
	cdNotPowerEject				= 1;							{  device does not support power eject of media }
	cdMute						= 2;							{  device supports audio channels muting }
	cdLeftToChannel				= 3;							{  device supports left channel only mono audio }
	cdRightToChannel			= 4;							{  device supports right channel only mono audio }
	cdLeftPlusRight				= 5;							{  device supports left + right channels mono audio }
	cdSCSI_2					= 10;							{  device supports SCSI2 command set (SCSI only) }
	cdStereoVolume				= 11;							{  device supports independent volume per channel }
	cdDisconnect				= 12;							{  device supports disconnect / reconnect (SCSI only) }
	cdWriteOnce					= 13;							{  device is a write-once type of drive }
	cdLockableButton			= 14;							{  device drawer/tray can be locked }
	cdExtendedFeatures			= 15;							{  extendedCdFeatures field is used }
	cdPowerInject_Mask			= $01;
	cdNotPowerEject_Mask		= $02;
	cdMute_Mask					= $04;
	cdLeftToChannel_Mask		= $08;
	cdRightToChannel_Mask		= $10;
	cdLeftPlusRight_Mask		= $20;
	cdSCSI_2_Mask				= $0400;
	cdStereoVolume_Mask			= $0800;
	cdDisconnect_Mask			= $1000;
	cdWriteOnce_Mask			= $2000;
	cdLockableButton_Mask		= $4000;
	cdExtendedFeatures_Mask		= $8000;

	{	 Transport types 	}
	cdCaddy						= 0;							{  CD_SC,CD_SC_PLUS,CD-300 etc. - power eject only }
	cdTray						= $01;							{  CD_300_PLUS etc. - power inject only }
	cdLid						= $02;							{  Power CD - no power inject/no power eject }

	{
	    Flags for extendedCdFeatures field
	    Only valid when cdExtendedFeatures_Mask bit of cdFeatures field is set
	}
	cdDVDROM					= 0;							{  device reads DVD-ROM media }
	cdDVDR						= 1;							{  device writes DVD-R media }
	cdDVDRAM					= 2;							{  device writes DVD-RAM media }
	cdDVDAudio					= 3;							{  device reads DVD-Audio media }
	cdDVDRW						= 4;							{  device writes DVD-RW media }
	cdCDRom						= 5;							{  device reads CD-ROM media }
	cdCDR						= 6;							{  device writes CD-R media }
	cdCDRW						= 7;							{  device writes CD-RW media }
	cdDVDROM_Mask				= $01;
	cdDVDR_Mask					= $02;
	cdDVDRAM_Mask				= $04;
	cdDVDAudio_Mask				= $08;
	cdDVDRW_Mask				= $10;
	cdCDRom_Mask				= $20;
	cdCDR_Mask					= $40;
	cdCDRW_Mask					= $80;


	{  the following are used by PC Exchange (and Apple DOS/PC Compatibility Card) }

	{  Control Codes }
	kRegisterPartition			= 50;							{  PCX needs a new Drive (for a non-macintosh partition found on the disk) }
	OLD_REGISTER_PARTITION		= 301;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card }
	THE_DRIVE					= 0;							{  DrvQElPtr for the partition to register }
	THE_PHYS_START				= 1;							{  The start of the partition in logical blocks }
	THE_PHYS_SIZE				= 2;							{  The size of the partition in logical blocks }
	kGetADrive					= 51;							{  control call to ask the driver to create a drive }
	OLD_GET_A_DRIVE				= 302;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card }
	THE_VAR_QUEL				= 0;							{  a var parameter for the returned DrvQElPtr }
	kProhibitMounting			= 52;							{  Dont allow mounting of the following drives }
	kOldProhibitMounting		= 2100;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card }
	kProhibitDevice				= 0;							{  CS Param 0 and 1 (partInfoRecPtr) }
	kIsContainerMounted			= 53;
	kOldIsContainerMounted		= 2201;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card          }
	kContainerVRef				= 0;							{  CS Param 0 and 1 (VRefNum) }
	kContainerParID				= 1;							{  CS Param 2 and 3 (Parent ID) }
	kContainerName				= 2;							{  CS Param 4 and 5 (File Name) }
	kContainerResponse			= 3;							{  CS Param 6 and 7 (var pointer to short result) }
	kMountVolumeImg				= 54;
	OLD_MOUNT_VOLUME_IMG		= 2000;
	MV_HOST_VREFNUM				= 0;
	MV_HOST_PAR_ID				= 1;
	MV_HOST_NAME				= 2;
	MV_REQ_PERM					= 3;

	{  Status Codes }

	kGetPartitionStatus			= 50;							{  what is the status of this partition? }
	kOldGetPartitionStatus		= 2200;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card }
	kDeviceToQuery				= 0;							{  CS Param 0 and 1 (partInfoRecPtr) }
	kDeviceResponse				= 1;							{  CS Param 2 and 3 (var pointer to short result) }
	kGetPartInfo				= 51;							{  Get a partition info record based on the provided vrefnum }
	kOldGetPartInfo				= 2300;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card }
	kPartInfoResponse			= 0;							{  var parameter (pointer to partInfoRec) CSParam [0-1] }
	kGetContainerAlias			= 52;							{  Get the alias that describes the file this drive was mounted from. }
	kOldGetContainerAlias		= 2400;							{  left in for compatibility with shipping Apple DOS/PC Compatibility Card }
	kGetAliasResponse			= 0;							{     var parameter (pointer to a Handle) CSParam [0-1] }

	{  the result codes to come from the driver interface  }

	DRIVER_NOT_INSTALLED		= -1;
	DRIVER_BUSY					= -2;
	CANT_MOUNT_WITHIN_THIS_FS	= -3;							{  can only mount container within residing on HFS volume }
	VOLUME_ALREADY_MOUNTED		= -4;							{  Already Mounted }

	{  requisite structures for PCX control and status calls }

	kMaxProhibted				= 2;							{  the max number of volumes the PC can possibly have mounted }

	{  GestaltSelector for Finding Driver information }

	kGetDriverInfo				= $76647263 (* 'vdrc' *);

	{	 VerifyCmd, FormatCmd and EjectCmd are now defined in Disks.h/p/a 	}
	{  Partition information passed back and forth between PCX and the driver }

type
	partInfoRecPtr = ^partInfoRec;
	partInfoRec = record
		SCSIID:					DeviceIdent;							{  DeviceIdent for the device }
		physPartitionLoc:		UInt32;									{  physical block number of beginning of partition }
		partitionNumber:		UInt32;									{  the partition number of this partition }
	end;

	vPartInfoRecPtr = ^vPartInfoRec;
	vPartInfoRec = record
		VPRTVers:				SInt8;									{  Virtual partition version number }
		VPRTType:				SInt8;									{  virtual partition type (DOS, HFS, etc) }
		drvrRefNum:				SInt16;									{  Driver Reference number of partition driver }
	end;

	{  Information related to DOS partitions }

const
	kDOSSigLow					= $01FE;						{  offset into boot block for DOS signature }
	kDOSSigHi					= $01FF;						{  offset into boot block for DOS signature }
	kDOSSigValLo				= $55;							{  DOS signature value in low byte }
	kDOSSigValHi				= $AA;							{  DOS signature value in high byte }


{$ALIGN MAC68K}


end.
