{
     File:       USB.p
 
     Contains:   Public API for USB Services Library (and associated components)
 
     Version:    Technology: USB 1.4
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit USB;
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
uses MacTypes,Files,NameRegistry,CodeFragments,Devices,MacErrors;


{$ALIGN MAC68K}

{ ************* Constants ************* }


const
																{  Flags  }
	kUSBTaskTimeFlag			= 1;
	kUSBHubPower				= 2;
	kUSBPowerReset				= 4;
	kUSBHubReaddress			= 8;
	kUSBAddressRequest			= 16;
	kUSBReturnOnException		= 32;
	kUSBNo5SecTimeout			= 64;
	kUSBTimeout					= 128;
	kUSBNoDataTimeout			= 256;
	kUSBDebugAwareFlag			= 512;
	kUSBResetDescriptorCache	= 1024;
	kUSBHighSpeedFlag			= 2048;
	kUSBDevZeroDetatch			= 4096;
	kUSBLowSpeedFlag			= 8192;
	kUSBTaskTimeSetAddressFlag	= 16384;

																{  Hub messages  }
	kUSBHubPortResetRequest		= 1;
	kUSBHubPortSuspendRequest	= 2;
	kUSBHubPortStatusRequest	= 3;

	kVendorID_AppleComputer		= $05AC;

	{	 ************* types ************* 	}


type
	USBReference						= SInt32;
	USBReference_fix	                = USBReference; { used as field type when a record declaration contains a USBReference field identifier }
	USBDeviceRef						= USBReference;
	USBDeviceRef_fix	                = USBDeviceRef; { used as field type when a record declaration contains a USBDeviceRef field identifier }
	USBDeviceRefPtr						= ^USBDeviceRef;
	USBInterfaceRef						= USBReference;
	USBPipeRef							= USBReference;
	USBBusRef							= USBReference;
	USBPipeState						= UInt32;
	USBCount							= UInt32;
	USBFlags							= UInt32;
	USBFlags_fix	                    = USBFlags; { used as field type when a record declaration contains a USBFlags field identifier }
	USBRequest							= UInt8;
	USBDirection						= UInt8;
	USBRqRecipient						= UInt8;
	USBRqType							= UInt8;
	USBRqIndex							= UInt16;
	USBRqValue							= UInt16;


	usbControlBitsPtr = ^usbControlBits;
	usbControlBits = record
		BMRequestType:			SInt8;
		BRequest:				SInt8;
		WValue:					USBRqValue;
		WIndex:					USBRqIndex;
		reserved4:				UInt16;
	end;

	USBIsocFramePtr = ^USBIsocFrame;
	USBIsocFrame = record
		frStatus:				OSStatus;
		frReqCount:				UInt16;
		frActCount:				UInt16;
	end;


const
	kUSBMaxIsocFrameReqCount	= 1023;							{  maximum size (bytes) of any one Isoc frame }


type
	usbIsocBitsPtr = ^usbIsocBits;
	usbIsocBits = record
		FrameList:				USBIsocFramePtr;
		NumFrames:				UInt32;
	end;

	usbHubBitsPtr = ^usbHubBits;
	usbHubBits = record
		Request:				UInt32;
		Spare:					UInt32;
	end;

	USBPBPtr = ^USBPB;
{$ifc TYPED_FUNCTION_POINTERS}
	USBCompletion = procedure(pb: USBPBPtr);
	USBCompletion_fix = USBCompletion; { used as field type when a record declaration contains a USBCompletion field identifier }
{$elsec}
	USBCompletion = ProcPtr;
	USBCompletion_fix = USBCompletion; { used as field type when a record declaration contains a USBCompletion field identifier }
{$endc}

	USBVariantBitsPtr = ^USBVariantBits;
	USBVariantBits = record
		case SInt16 of
		0: (
			cntl:				usbControlBits;
			);
		1: (
			isoc:				usbIsocBits;
			);
		2: (
			hub:				usbHubBits;
			);
	end;

	USBPB = packed record
		qlink:					Ptr;
		qType:					UInt16;
		pbLength:				UInt16;
		pbVersion:				UInt16;
		reserved1:				UInt16;
		reserved2:				UInt32;
		usbStatus:				OSStatus;
		usbCompletion:			USBCompletion_fix;
		usbRefcon:				UInt32;
		usbReference:			USBReference_fix;
		usbBuffer:				Ptr;
		usbReqCount:			USBCount;
		usbActCount:			USBCount;
		usbFlags:				USBFlags_fix;
		usb:					USBVariantBits;
		usbFrame:				UInt32;
		usbClassType:			UInt8;
		usbSubclass:			UInt8;
		usbProtocol:			UInt8;
		usbOther:				UInt8;
		reserved6:				UInt32;
		reserved7:				UInt16;
		reserved8:				UInt16;
	end;

	uslReqPtr = ^uslReq;
	uslReq = record
		usbDirection:			SInt8;
		usbType:				SInt8;
		usbRecipient:			SInt8;
		usbRequest:				SInt8;
	end;


const
																{  BT 19Aug98, bump up to v1.10 for Isoc }
	kUSBCurrentPBVersion		= $0100;						{  v1.00 }
	kUSBIsocPBVersion			= $0109;						{  v1.10 }
	kUSBCurrentHubPB			= $0109;


	kUSBNoCallBack				= -1;


type
	bcdUSB								= UInt8;

const
	kUSBControl					= 0;
	kUSBIsoc					= 1;
	kUSBBulk					= 2;
	kUSBInterrupt				= 3;
	kUSBAnyType					= $FF;

	{	 endpoint type 	}
	kUSBOut						= 0;
	kUSBIn						= 1;
	kUSBNone					= 2;
	kUSBAnyDirn					= 3;

	{	USBDirection	}
	kUSBStandard				= 0;
	kUSBClass					= 1;
	kUSBVendor					= 2;

	{	USBRqType	}
	kUSBDevice					= 0;
	kUSBInterface				= 1;
	kUSBEndpoint				= 2;
	kUSBOther					= 3;

	{	USBRqRecipient	}
	kUSBRqGetStatus				= 0;
	kUSBRqClearFeature			= 1;
	kUSBRqReserved1				= 2;
	kUSBRqSetFeature			= 3;
	kUSBRqReserved2				= 4;
	kUSBRqSetAddress			= 5;
	kUSBRqGetDescriptor			= 6;
	kUSBRqSetDescriptor			= 7;
	kUSBRqGetConfig				= 8;
	kUSBRqSetConfig				= 9;
	kUSBRqGetInterface			= 10;
	kUSBRqSetInterface			= 11;
	kUSBRqSyncFrame				= 12;

	{	USBRequest	}

	kUSBDeviceDesc				= 1;
	kUSBConfDesc				= 2;
	kUSBStringDesc				= 3;
	kUSBInterfaceDesc			= 4;
	kUSBEndpointDesc			= 5;
	kUSBHIDDesc					= $21;
	kUSBReportDesc				= $22;
	kUSBPhysicalDesc			= $23;
	kUSBHUBDesc					= $29;

	{	 descriptorType 	}

	kUSBFeatureDeviceRemoteWakeup = 1;
	kUSBFeatureEndpointStall	= 0;

	{	 Feature selectors 	}
	kUSBActive					= 0;							{  Pipe can accept new transactions }
	kUSBIdle					= 1;							{  Pipe will not accept new transactions }
	kUSBStalled					= 2;							{  An error occured on the pipe }
	kUSBSuspended				= 4;							{  Device is suspended }
	kUSBNoBandwidth				= 8;							{  (Isoc or Int) Pipe could not be initialised due to bandwidth constraint }

	kUSB100mAAvailable			= 50;
	kUSB500mAAvailable			= 250;
	kUSB100mA					= 50;
	kUSBAtrBusPowered			= $80;
	kUSBAtrSelfPowered			= $40;
	kUSBAtrRemoteWakeup			= $20;

	kUSBRel10					= $0100;
	kUSBRel11					= $0110;
	kUSBRel20					= $0200;

	kUSBDeviceDescriptorLength	= $12;
	kUSBInterfaceDescriptorLength = $09;
	kUSBConfigDescriptorLength	= $09;


type
	USBDeviceDescriptorPtr = ^USBDeviceDescriptor;
	USBDeviceDescriptor = record
		length:					SInt8;
		descType:				SInt8;
		usbRel:					UInt16;
		deviceClass:			SInt8;
		deviceSubClass:			SInt8;
		protocol:				SInt8;
		maxPacketSize:			SInt8;
		vendor:					UInt16;
		product:				UInt16;
		devRel:					UInt16;
		manuIdx:				SInt8;
		prodIdx:				SInt8;
		serialIdx:				SInt8;
		numConf:				SInt8;
	end;

	USBDescriptorHeaderPtr = ^USBDescriptorHeader;
	USBDescriptorHeader = record
		length:					SInt8;
		descriptorType:			SInt8;
	end;

	USBConfigurationDescriptorPtr = ^USBConfigurationDescriptor;
	USBConfigurationDescriptor = packed record
		length:					UInt8;
		descriptorType:			UInt8;
		totalLength:			UInt16;
		numInterfaces:			UInt8;
		configValue:			UInt8;
		configStrIndex:			UInt8;
		attributes:				UInt8;
		maxPower:				UInt8;
	end;

	USBInterfaceDescriptorPtr = ^USBInterfaceDescriptor;
	USBInterfaceDescriptor = packed record
		length:					UInt8;
		descriptorType:			UInt8;
		interfaceNumber:		UInt8;
		alternateSetting:		UInt8;
		numEndpoints:			UInt8;
		interfaceClass:			UInt8;
		interfaceSubClass:		UInt8;
		interfaceProtocol:		UInt8;
		interfaceStrIndex:		UInt8;
	end;

	USBEndPointDescriptorPtr = ^USBEndPointDescriptor;
	USBEndPointDescriptor = packed record
		length:					UInt8;
		descriptorType:			UInt8;
		endpointAddress:		UInt8;
		attributes:				UInt8;
		maxPacketSize:			UInt16;
		interval:				UInt8;
	end;

	USBHIDDescriptorPtr = ^USBHIDDescriptor;
	USBHIDDescriptor = packed record
		descLen:				UInt8;
		descType:				UInt8;
		descVersNum:			UInt16;
		hidCountryCode:			UInt8;
		hidNumDescriptors:		UInt8;
		hidDescriptorType:		UInt8;
		hidDescriptorLengthLo:	UInt8;									{  can't make this a single 16bit value or the compiler will add a filler byte }
		hidDescriptorLengthHi:	UInt8;
	end;

	USBHIDReportDescPtr = ^USBHIDReportDesc;
	USBHIDReportDesc = packed record
		hidDescriptorType:		UInt8;
		hidDescriptorLengthLo:	UInt8;									{  can't make this a single 16bit value or the compiler will add a filler byte }
		hidDescriptorLengthHi:	UInt8;
	end;

	USBHubPortStatusPtr = ^USBHubPortStatus;
	USBHubPortStatus = record
		portFlags:				UInt16;									{  Port status flags  }
		portChangeFlags:		UInt16;									{  Port changed flags  }
	end;

	{	 ********* ProtoTypes *************** 	}
	{	 For dealing with endianisms 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  HostToUSBWord()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function HostToUSBWord(value: UInt16): UInt16; external name '_HostToUSBWord';

{
 *  USBToHostWord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBToHostWord(value: UInt16): UInt16; external name '_USBToHostWord';

{
 *  HostToUSBLong()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function HostToUSBLong(value: UInt32): UInt32; external name '_HostToUSBLong';

{
 *  USBToHostLong()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBToHostLong(value: UInt32): UInt32; external name '_USBToHostLong';

{ Main prototypes }
{ Transfer commands }
{
 *  USBDeviceRequest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBDeviceRequest(var pb: USBPB): OSStatus; external name '_USBDeviceRequest';

{
 *  USBBulkWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBBulkWrite(var pb: USBPB): OSStatus; external name '_USBBulkWrite';

{
 *  USBBulkRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBBulkRead(var pb: USBPB): OSStatus; external name '_USBBulkRead';

{
 *  USBIntRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBIntRead(var pb: USBPB): OSStatus; external name '_USBIntRead';

{
 *  USBIntWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.2 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBIntWrite(var pb: USBPB): OSStatus; external name '_USBIntWrite';

{
 *  USBIsocRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBIsocRead(var pb: USBPB): OSStatus; external name '_USBIsocRead';

{
 *  USBIsocWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBIsocWrite(var pb: USBPB): OSStatus; external name '_USBIsocWrite';

{ Pipe state control }
{
 *  USBClearPipeStallByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBClearPipeStallByReference(ref: USBPipeRef): OSStatus; external name '_USBClearPipeStallByReference';

{
 *  USBAbortPipeByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBAbortPipeByReference(ref: USBReference): OSStatus; external name '_USBAbortPipeByReference';

{
 *  USBResetPipeByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBResetPipeByReference(ref: USBReference): OSStatus; external name '_USBResetPipeByReference';

{
 *  USBSetPipeIdleByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBSetPipeIdleByReference(ref: USBPipeRef): OSStatus; external name '_USBSetPipeIdleByReference';

{
 *  USBSetPipeActiveByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBSetPipeActiveByReference(ref: USBPipeRef): OSStatus; external name '_USBSetPipeActiveByReference';

{
 *  USBClosePipeByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBClosePipeByReference(ref: USBPipeRef): OSStatus; external name '_USBClosePipeByReference';

{
 *  USBGetPipeStatusByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetPipeStatusByReference(ref: USBReference; var state: USBPipeState): OSStatus; external name '_USBGetPipeStatusByReference';


{ Configuration services }
{
 *  USBFindNextInterface()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBFindNextInterface(var pb: USBPB): OSStatus; external name '_USBFindNextInterface';

{
 *  USBOpenDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBOpenDevice(var pb: USBPB): OSStatus; external name '_USBOpenDevice';

{
 *  USBSetConfiguration()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBSetConfiguration(var pb: USBPB): OSStatus; external name '_USBSetConfiguration';

{
 *  USBNewInterfaceRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBNewInterfaceRef(var pb: USBPB): OSStatus; external name '_USBNewInterfaceRef';

{
 *  USBDisposeInterfaceRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBDisposeInterfaceRef(var pb: USBPB): OSStatus; external name '_USBDisposeInterfaceRef';

{
 *  USBConfigureInterface()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBConfigureInterface(var pb: USBPB): OSStatus; external name '_USBConfigureInterface';

{
 *  USBFindNextPipe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBFindNextPipe(var pb: USBPB): OSStatus; external name '_USBFindNextPipe';

{
 *  USBSetPipePolicy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.4 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBSetPipePolicy(var pb: USBPB): OSStatus; external name '_USBSetPipePolicy';

{ Dealing with descriptors. }
{ Note most of this is temprorary }
{
 *  USBGetConfigurationDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetConfigurationDescriptor(var pb: USBPB): OSStatus; external name '_USBGetConfigurationDescriptor';

{
 *  USBGetFullConfigurationDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetFullConfigurationDescriptor(var pb: USBPB): OSStatus; external name '_USBGetFullConfigurationDescriptor';

{
 *  USBGetStringDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetStringDescriptor(var pb: USBPB): OSStatus; external name '_USBGetStringDescriptor';

{
 *  USBFindNextEndpointDescriptorImmediate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBFindNextEndpointDescriptorImmediate(var pb: USBPB): OSStatus; external name '_USBFindNextEndpointDescriptorImmediate';

{
 *  USBFindNextInterfaceDescriptorImmediate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBFindNextInterfaceDescriptorImmediate(var pb: USBPB): OSStatus; external name '_USBFindNextInterfaceDescriptorImmediate';

{
 *  USBFindNextAssociatedDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBFindNextAssociatedDescriptor(var pb: USBPB): OSStatus; external name '_USBFindNextAssociatedDescriptor';


{ Utility functions }
{
 *  USBResetDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBResetDevice(var pb: USBPB): OSStatus; external name '_USBResetDevice';

{
 *  USBPortStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.4 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBPortStatus(var pb: USBPB): OSStatus; external name '_USBPortStatus';

{
 *  USBSuspendDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.3 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBSuspendDevice(var pb: USBPB): OSStatus; external name '_USBSuspendDevice';

{
 *  USBResumeDeviceByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.3 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBResumeDeviceByReference(refIn: USBReference): OSStatus; external name '_USBResumeDeviceByReference';

{
 *  USBGetBandwidthAvailableByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.4 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetBandwidthAvailableByReference(ref: USBReference; var avail: UInt32): OSStatus; external name '_USBGetBandwidthAvailableByReference';

{
 *  USBGetFrameNumberImmediate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetFrameNumberImmediate(var pb: USBPB): OSStatus; external name '_USBGetFrameNumberImmediate';

{
 *  USBDelay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBDelay(var pb: USBPB): OSStatus; external name '_USBDelay';

{
 *  USBSAbortQueuesByReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.3 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBSAbortQueuesByReference(ref: USBReference): OSStatus; external name '_USBSAbortQueuesByReference';

{
 *  USBAllocMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBAllocMem(var pb: USBPB): OSStatus; external name '_USBAllocMem';

{
 *  USBDeallocMem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBDeallocMem(var pb: USBPB): OSStatus; external name '_USBDeallocMem';

{ Expert interface functions }
{
 *  USBExpertInstallInterfaceDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertInstallInterfaceDriver(ref: USBDeviceRef; desc: USBDeviceDescriptorPtr; interfacePtr: USBInterfaceDescriptorPtr; hubRef: USBReference; busPowerAvailable: UInt32): OSStatus; external name '_USBExpertInstallInterfaceDriver';

{
 *  USBExpertRemoveInterfaceDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertRemoveInterfaceDriver(ref: USBDeviceRef): OSStatus; external name '_USBExpertRemoveInterfaceDriver';

{
 *  USBExpertInstallDeviceDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertInstallDeviceDriver(ref: USBDeviceRef; desc: USBDeviceDescriptorPtr; hubRef: USBReference; port: UInt32; busPowerAvailable: UInt32): OSStatus; external name '_USBExpertInstallDeviceDriver';

{
 *  USBExpertRemoveDeviceDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertRemoveDeviceDriver(ref: USBDeviceRef): OSStatus; external name '_USBExpertRemoveDeviceDriver';

{
 *  USBExpertStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertStatus(ref: USBDeviceRef; pointer: UnivPtr; value: UInt32): OSStatus; external name '_USBExpertStatus';

{
 *  USBExpertFatalError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertFatalError(ref: USBDeviceRef; status: OSStatus; pointer: UnivPtr; value: UInt32): OSStatus; external name '_USBExpertFatalError';

{
 *  USBExpertNotify()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBFamilyExpertLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertNotify(note: UnivPtr): OSStatus; external name '_USBExpertNotify';

{
 *  USBExpertStatusLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.2 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertStatusLevel(level: UInt32; ref: USBDeviceRef; status: StringPtr; value: UInt32): OSStatus; external name '_USBExpertStatusLevel';

{
 *  USBExpertGetStatusLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.3 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertGetStatusLevel: UInt32; external name '_USBExpertGetStatusLevel';

{
 *  USBExpertSetStatusLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.3 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure USBExpertSetStatusLevel(level: UInt32); external name '_USBExpertSetStatusLevel';


{
 *  USBExpertSetDevicePowerStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertSetDevicePowerStatus(ref: USBDeviceRef; reserved1: UInt32; reserved2: UInt32; powerStatus: UInt32; busPowerAvailable: UInt32; busPowerNeeded: UInt32): OSStatus; external name '_USBExpertSetDevicePowerStatus';

{$endc}  {CALL_NOT_IN_CARBON}


const
	kUSBDevicePower_PowerOK		= 0;
	kUSBDevicePower_BusPowerInsufficient = 1;
	kUSBDevicePower_BusPowerNotAllFeatures = 2;
	kUSBDevicePower_SelfPowerInsufficient = 3;
	kUSBDevicePower_SelfPowerNotAllFeatures = 4;
	kUSBDevicePower_HubPortOk	= 5;
	kUSBDevicePower_HubPortOverCurrent = 6;
	kUSBDevicePower_BusPoweredHubOnLowPowerPort = 7;
	kUSBDevicePower_BusPoweredHubToBusPoweredHub = 8;
	kUSBDevicePower_Reserved3	= 9;
	kUSBDevicePower_Reserved4	= 10;


	{	 For hubs only 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  USBHubAddDevice()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function USBHubAddDevice(var pb: USBPB): OSStatus; external name '_USBHubAddDevice';

{
 *  USBHubConfigurePipeZero()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHubConfigurePipeZero(var pb: USBPB): OSStatus; external name '_USBHubConfigurePipeZero';

{
 *  USBHubSetAddress()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHubSetAddress(var pb: USBPB): OSStatus; external name '_USBHubSetAddress';

{
 *  USBHubDeviceRemoved()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHubDeviceRemoved(var pb: USBPB): OSStatus; external name '_USBHubDeviceRemoved';

{
 *  USBMakeBMRequestType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBMakeBMRequestType(direction: ByteParameter; reqtype: ByteParameter; recipient: ByteParameter): ByteParameter; external name '_USBMakeBMRequestType';

{
 *  USBControlRequest()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBServicesLib 1.2 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBControlRequest(var pb: USBPB): OSStatus; external name '_USBControlRequest';

{$endc}  {CALL_NOT_IN_CARBON}


type
	USBLocationID						= UInt32;

const
	kUSBLocationNibbleFormat	= 0;							{  Other values are reserved for future types (like when we have more than 16 ports per hub) }


	kNoDeviceRef				= -1;

	{  Status Level constants }
	{	
	Level 1: Fatal errors
	Level 2: General errors that may or may not effect operation
	Level 3: General driver messages.  The "AddStatus" call that drivers use comes through as a level 3.  This is also the default level at boot time.
	Level 4: Important status messages from the Expert and USL.
	Level 5: General status messages from the Expert and USL.
		}
	kUSBStatusLevelFatal		= 1;
	kUSBStatusLevelError		= 2;
	kUSBStatusLevelClient		= 3;
	kUSBStatusLevelGeneral		= 4;
	kUSBStatusLevelVerbose		= 5;

	{  Expert Notification Types }

type
	USBNotificationType 		= UInt8;
const
	kNotifyAddDevice			= $00;
	kNotifyRemoveDevice			= $01;
	kNotifyAddInterface			= $02;
	kNotifyRemoveInterface		= $03;
	kNotifyGetDeviceDescriptor	= $04;
	kNotifyGetInterfaceDescriptor = $05;
	kNotifyGetNextDeviceByClass	= $06;
	kNotifyGetDriverConnectionID = $07;
	kNotifyInstallDeviceNotification = $08;
	kNotifyRemoveDeviceNotification = $09;
	kNotifyDeviceRefToBusRef	= $0A;
	kNotifyDriverNotify			= $0C;
	kNotifyParentNotify			= $0D;
	kNotifyAnyEvent				= $FF;
	kNotifyPowerState			= $17;
	kNotifyStatus				= $18;
	kNotifyFatalError			= $19;
	kNotifyStatusLevel			= $20;


type
	USBDriverMessage					= USBNotificationType;
	{
	   USB Manager wildcard constants for USBGetNextDeviceByClass
	   and USBInstallDeviceNotification.
	}
	USBManagerWildcard 			= UInt16;
const
	kUSBAnyClass				= $FFFF;
	kUSBAnySubClass				= $FFFF;
	kUSBAnyProtocol				= $FFFF;
	kUSBAnyVendor				= $FFFF;
	kUSBAnyProduct				= $FFFF;


type
	ExpertNotificationDataPtr = ^ExpertNotificationData;
	ExpertNotificationData = record
		notification:			SInt8;
		filler:					SInt8;									{  unused due to 2-byte 68k alignment }
		deviceRef:				USBDeviceRefPtr;
		busPowerAvailable:		UInt32;
		data:					Ptr;
		info1:					UInt32;
		info2:					UInt32;
	end;

	{  Definition of function pointer passed in ExpertEntryProc }
{$ifc TYPED_FUNCTION_POINTERS}
	ExpertNotificationProcPtr = function(pNotificationData: ExpertNotificationDataPtr): OSStatus;
{$elsec}
	ExpertNotificationProcPtr = ProcPtr;
{$endc}

	{  Definition of expert's callback installation function }
{$ifc TYPED_FUNCTION_POINTERS}
	ExpertEntryProcPtr = function(pExpertNotify: ExpertNotificationProcPtr): OSStatus;
{$elsec}
	ExpertEntryProcPtr = ProcPtr;
{$endc}

	{  Device Notification Callback Routine }
{$ifc TYPED_FUNCTION_POINTERS}
	USBDeviceNotificationCallbackProcPtr = procedure(pb: UnivPtr);
{$elsec}
	USBDeviceNotificationCallbackProcPtr = ProcPtr;
{$endc}

	{  Device Notification Parameter Block }
	USBDeviceNotificationParameterBlockPtr = ^USBDeviceNotificationParameterBlock;
	USBDeviceNotificationParameterBlock = record
		pbLength:				UInt16;
		pbVersion:				UInt16;
		usbDeviceNotification:	SInt8;
		reserved1:				SInt8;									{  needed because of 2-byte 68k alignment }
		usbDeviceRef:			USBDeviceRef_fix;
		usbClass:				UInt16;
		usbSubClass:			UInt16;
		usbProtocol:			UInt16;
		usbVendor:				UInt16;
		usbProduct:				UInt16;
		result:					OSStatus;
		token:					UInt32;
		callback:				USBDeviceNotificationCallbackProcPtr;
		refcon:					UInt32;
	end;

	{  Definition of USBDriverNotificationCallback Routine }
{$ifc TYPED_FUNCTION_POINTERS}
	USBDriverNotificationCallbackPtr = procedure(status: OSStatus; refcon: UInt32);
{$elsec}
	USBDriverNotificationCallbackPtr = ProcPtr;
{$endc}

	{  Public Functions }
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  USBGetVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in USBServicesLib 1.3 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function USBGetVersion: UInt32; external name '_USBGetVersion';

{
 *  USBGetNextDeviceByClass()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetNextDeviceByClass(var deviceRef: USBDeviceRef; var connID: CFragConnectionID; theClass: UInt16; theSubClass: UInt16; theProtocol: UInt16): OSStatus; external name '_USBGetNextDeviceByClass';

{
 *  USBGetDeviceDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetDeviceDescriptor(var deviceRef: USBDeviceRef; var deviceDescriptor: USBDeviceDescriptor; size: UInt32): OSStatus; external name '_USBGetDeviceDescriptor';

{
 *  USBGetInterfaceDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetInterfaceDescriptor(var interfaceRef: USBInterfaceRef; var interfaceDescriptor: USBInterfaceDescriptor; size: UInt32): OSStatus; external name '_USBGetInterfaceDescriptor';

{
 *  USBGetDriverConnectionID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBGetDriverConnectionID(var deviceRef: USBDeviceRef; var connID: CFragConnectionID): OSStatus; external name '_USBGetDriverConnectionID';

{
 *  USBInstallDeviceNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure USBInstallDeviceNotification(var pb: USBDeviceNotificationParameterBlock); external name '_USBInstallDeviceNotification';

{
 *  USBRemoveDeviceNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBRemoveDeviceNotification(token: UInt32): OSStatus; external name '_USBRemoveDeviceNotification';

{
 *  USBDeviceRefToBusRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBDeviceRefToBusRef(var deviceRef: USBDeviceRef; var busRef: USBBusRef): OSStatus; external name '_USBDeviceRefToBusRef';

{
 *  USBDriverNotify()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBDriverNotify(reference: USBReference; mesg: ByteParameter; refcon: UInt32; callback: USBDriverNotificationCallbackPtr): OSStatus; external name '_USBDriverNotify';

{
 *  USBExpertNotifyParent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBExpertNotifyParent(reference: USBReference; pointer: UnivPtr): OSStatus; external name '_USBExpertNotifyParent';

{
 *  USBAddDriverForFSSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.3 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBAddDriverForFSSpec(reference: USBReference; var fileSpec: FSSpec): OSStatus; external name '_USBAddDriverForFSSpec';

{
 *  USBAddShimFromDisk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.4 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBAddShimFromDisk(var shimFilePtr: FSSpec): OSStatus; external name '_USBAddShimFromDisk';

{
 *  USBReferenceToRegEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.4 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBReferenceToRegEntry(var parentEntry: RegEntryID; parentDeviceRef: USBDeviceRef): OSStatus; external name '_USBReferenceToRegEntry';

{
 *  USBConfigureADBShim()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in USBManagerLib 1.4 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBConfigureADBShim(inCommandID: UInt32; arg1: UnivPtr; arg2: UnivPtr): OSStatus; external name '_USBConfigureADBShim';


{$endc}  {CALL_NOT_IN_CARBON}


type
{$ifc TYPED_FUNCTION_POINTERS}
	HIDInterruptProcPtr = procedure(refcon: UInt32; theData: UnivPtr);
{$elsec}
	HIDInterruptProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	HIDNotificationProcPtr = procedure(refcon: UInt32; reportSize: UInt32; theReport: UnivPtr; theInterfaceRef: USBReference);
{$elsec}
	HIDNotificationProcPtr = ProcPtr;
{$endc}

	{  HID Install Interrupt prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDInstallInterruptProcPtr = function(pInterruptProc: HIDInterruptProcPtr; refcon: UInt32): OSStatus;
{$elsec}
	USBHIDInstallInterruptProcPtr = ProcPtr;
{$endc}

	{  HID Poll Device prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDPollDeviceProcPtr = function: OSStatus;
{$elsec}
	USBHIDPollDeviceProcPtr = ProcPtr;
{$endc}

	{  HID Control Device prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDControlDeviceProcPtr = function(theControlSelector: UInt32; theControlData: UnivPtr): OSStatus;
{$elsec}
	USBHIDControlDeviceProcPtr = ProcPtr;
{$endc}

	{  HID Get Device Info prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDGetDeviceInfoProcPtr = function(theInfoSelector: UInt32; theInfo: UnivPtr): OSStatus;
{$elsec}
	USBHIDGetDeviceInfoProcPtr = ProcPtr;
{$endc}

	{  HID Enter Polled Mode prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDEnterPolledModeProcPtr = function: OSStatus;
{$elsec}
	USBHIDEnterPolledModeProcPtr = ProcPtr;
{$endc}

	{  HID Exit Polled Mode prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDExitPolledModeProcPtr = function: OSStatus;
{$elsec}
	USBHIDExitPolledModeProcPtr = ProcPtr;
{$endc}

	{  HID Install Notification prototype }
{$ifc TYPED_FUNCTION_POINTERS}
	USBHIDInstallNotificationProcPtr = function(pNotificationProc: HIDNotificationProcPtr; refcon: UInt32): OSStatus;
{$elsec}
	USBHIDInstallNotificationProcPtr = ProcPtr;
{$endc}


const
	kHIDStandardDispatchVersion	= 0;
	kHIDReservedDispatchVersion	= 1;
	kHIDNotificationDispatchVersion = 2;
	kHIDCurrentDispatchVersion	= 2;


type
	USBHIDRev2DispatchTablePtr = ^USBHIDRev2DispatchTable;
	USBHIDRev2DispatchTable = record
		hidDispatchVersion:		UInt32;
		pUSBHIDInstallInterrupt: USBHIDInstallInterruptProcPtr;
		pUSBHIDPollDevice:		USBHIDPollDeviceProcPtr;
		pUSBHIDControlDevice:	USBHIDControlDeviceProcPtr;
		pUSBHIDGetDeviceInfo:	USBHIDGetDeviceInfoProcPtr;
		pUSBHIDEnterPolledMode:	USBHIDEnterPolledModeProcPtr;
		pUSBHIDExitPolledMode:	USBHIDExitPolledModeProcPtr;
		pUSBHIDInstallNotification: USBHIDInstallNotificationProcPtr;
	end;

	USBHIDModuleDispatchTablePtr = ^USBHIDModuleDispatchTable;
	USBHIDModuleDispatchTable = record
		hidDispatchVersion:		UInt32;
		pUSBHIDInstallInterrupt: USBHIDInstallInterruptProcPtr;
		pUSBHIDPollDevice:		USBHIDPollDeviceProcPtr;
		pUSBHIDControlDevice:	USBHIDControlDeviceProcPtr;
		pUSBHIDGetDeviceInfo:	USBHIDGetDeviceInfoProcPtr;
		pUSBHIDEnterPolledMode:	USBHIDEnterPolledModeProcPtr;
		pUSBHIDExitPolledMode:	USBHIDExitPolledModeProcPtr;
	end;

	{	  Prototypes Tue, Mar 17, 1998 4:54:30 PM 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  USBHIDInstallInterrupt()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function USBHIDInstallInterrupt(HIDInterruptFunction: HIDInterruptProcPtr; refcon: UInt32): OSStatus; external name '_USBHIDInstallInterrupt';

{
 *  USBHIDPollDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHIDPollDevice: OSStatus; external name '_USBHIDPollDevice';

{
 *  USBHIDControlDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHIDControlDevice(theControlSelector: UInt32; theControlData: UnivPtr): OSStatus; external name '_USBHIDControlDevice';

{
 *  USBHIDGetDeviceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHIDGetDeviceInfo(theInfoSelector: UInt32; theInfo: UnivPtr): OSStatus; external name '_USBHIDGetDeviceInfo';

{
 *  USBHIDEnterPolledMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHIDEnterPolledMode: OSStatus; external name '_USBHIDEnterPolledMode';

{
 *  USBHIDExitPolledMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHIDExitPolledMode: OSStatus; external name '_USBHIDExitPolledMode';

{
 *  USBHIDInstallNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function USBHIDInstallNotification(HIDNotificationFunction: HIDNotificationProcPtr; refcon: UInt32): OSStatus; external name '_USBHIDInstallNotification';

{
 *  HIDNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure HIDNotification(devicetype: UInt32; var NewHIDData: UInt8; var OldHIDData: UInt8); external name '_HIDNotification';

{$endc}  {CALL_NOT_IN_CARBON}


const
	kHIDRqGetReport				= 1;
	kHIDRqGetIdle				= 2;
	kHIDRqGetProtocol			= 3;
	kHIDRqSetReport				= 9;
	kHIDRqSetIdle				= 10;
	kHIDRqSetProtocol			= 11;

	kHIDRtInputReport			= 1;
	kHIDRtOutputReport			= 2;
	kHIDRtFeatureReport			= 3;

	kHIDBootProtocolValue		= 0;
	kHIDReportProtocolValue		= 1;

	kHIDKeyboardInterfaceProtocol = 1;
	kHIDMouseInterfaceProtocol	= 2;

	kHIDSetLEDStateByBits		= 1;
	kHIDSetLEDStateByBitMask	= 1;
	kHIDSetLEDStateByIDNumber	= 2;
	kHIDRemoveInterruptHandler	= 3;
	kHIDEnableDemoMode			= 4;
	kHIDDisableDemoMode			= 5;
	kHIDRemoveNotification		= $1000;

	kHIDGetLEDStateByBits		= 1;							{  not supported in 1.0 of keyboard module }
	kHIDGetLEDStateByBitMask	= 1;							{  not supported in 1.0 of keyboard module }
	kHIDGetLEDStateByIDNumber	= 2;
	kHIDGetDeviceCountryCode	= 3;							{  not supported in 1.0 HID modules }
	kHIDGetDeviceUnitsPerInch	= 4;							{  only supported in mouse HID module }
	kHIDGetInterruptHandler		= 5;
	kHIDGetCurrentKeys			= 6;							{  only supported in keyboard HID module }
	kHIDGetInterruptRefcon		= 7;
	kHIDGetVendorID				= 8;
	kHIDGetProductID			= 9;


	kNumLockLED					= 0;
	kCapsLockLED				= 1;
	kScrollLockLED				= 2;
	kComposeLED					= 3;
	kKanaLED					= 4;

	kNumLockLEDMask				= $01;
	kCapsLockLEDMask			= $02;
	kScrollLockLEDMask			= $04;
	kComposeLEDMask				= $08;
	kKanaLEDMask				= $10;

	kUSBCapsLockKey				= $39;
	kUSBNumLockKey				= $53;
	kUSBScrollLockKey			= $47;


type
	USBMouseDataPtr = ^USBMouseData;
	USBMouseData = record
		buttons:				UInt16;
		XDelta:					SInt16;
		YDelta:					SInt16;
	end;

	USBKeyboardDataPtr = ^USBKeyboardData;
	USBKeyboardData = record
		keycount:				UInt16;
		usbkeycode:				array [0..31] of UInt16;
	end;

	USBHIDDataPtr = ^USBHIDData;
	USBHIDData = record
		case SInt16 of
		0: (
			kbd:				USBKeyboardData;
			);
		1: (
			mouse:				USBMouseData;
			);
	end;

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  StartCompoundClassDriver()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
procedure StartCompoundClassDriver(device: USBDeviceRef; classID: UInt16; subClass: UInt16); external name '_StartCompoundClassDriver';

{$endc}  {CALL_NOT_IN_CARBON}


const
	kUSBCompositeClass			= 0;
	kUSBAudioClass				= 1;
	kUSBCommClass				= 2;
	kUSBHIDClass				= 3;
	kUSBDisplayClass			= 4;
	kUSBPrintingClass			= 7;
	kUSBMassStorageClass		= 8;
	kUSBHubClass				= 9;
	kUSBDataClass				= 10;
	kUSBVendorSpecificClass		= $FF;

	kUSBCompositeSubClass		= 0;
	kUSBHubSubClass				= 1;
	kUSBPrinterSubclass			= 1;
	kUSBVendorSpecificSubClass	= $FF;

	kUSBHIDInterfaceClass		= $03;

	kUSBNoInterfaceSubClass		= $00;
	kUSBBootInterfaceSubClass	= $01;

	kUSBNoInterfaceProtocol		= $00;
	kUSBKeyboardInterfaceProtocol = $01;
	kUSBMouseInterfaceProtocol	= $02;
	kUSBVendorSpecificProtocol	= $FF;

	kUSBPrinterUnidirectionalProtocol = $01;
	kUSBPrinterBidirectionalProtocol = $02;


	kServiceCategoryUSB			= FourCharCode('usb ');						{  USB }

	kUSBDriverFileType			= FourCharCode('ndrv');
	kUSBDriverRsrcType			= FourCharCode('usbd');
	kUSBShimRsrcType			= FourCharCode('usbs');

	kTheUSBDriverDescriptionSignature = FourCharCode('usbd');

	kInitialUSBDriverDescriptor	= 0;


type
	USBDriverDescVersion				= UInt32;
	USBDriverDescVersion_fix	= USBDriverDescVersion;    { used as field type when a record declaration contains a USBDriverDescVersion field identifier }
	{   Driver Loading Options }
	USBDriverLoadingOptions 	= UInt32;
	USBDriverLoadingOptions_fix = USBDriverLoadingOptions; { used as field type when a record declaration contains a USBDriverLoadingOptions field identifier }
const
	kUSBDoNotMatchGenericDevice	= $00000001;					{  Driver's VendorID must match Device's VendorID }
	kUSBDoNotMatchInterface		= $00000002;					{  Do not load this driver as an interface driver. }
	kUSBProtocolMustMatch		= $00000004;					{  Do not load this driver if protocol field doesn't match. }
	kUSBInterfaceMatchOnly		= $00000008;					{  Only load this driver as an interface driver. }

	kClassDriverPluginVersion	= $00001100;


type
	USBDeviceInfoPtr = ^USBDeviceInfo;
	USBDeviceInfo = record
		usbVendorID:			UInt16;									{  USB Vendor ID }
		usbProductID:			UInt16;									{  USB Product ID. }
		usbDeviceReleaseNumber:	UInt16;									{  Release Number of Device }
		usbDeviceProtocol:		UInt16;									{  Protocol Info. }
	end;
	USBDeviceInfo_fix = USBDeviceInfo; { used as field type when a record declaration contains a USBDeviceInfo field identifier }

	USBInterfaceInfoPtr = ^USBInterfaceInfo;
	USBInterfaceInfo = record
		usbConfigValue:			SInt8;									{  Configuration Value }
		usbInterfaceNum:		SInt8;									{  Interface Number }
		usbInterfaceClass:		SInt8;									{  Interface Class }
		usbInterfaceSubClass:	SInt8;									{  Interface SubClass }
		usbInterfaceProtocol:	SInt8;									{  Interface Protocol }
		pad:					SInt8
	end;
	USBInterfaceInfo_fix = USBInterfaceInfo; { used as field type when a record declaration contains a USBInterfaceInfo field identifier }

	USBDriverTypePtr = ^USBDriverType;
	USBDriverType = record
		nameInfoStr:			Str31;									{  Driver's name when loading into the Name Registry. }
		usbDriverClass:			SInt8;									{  USB Class this driver belongs to. }
		usbDriverSubClass:		SInt8;									{  Module type }
		usbDriverVersion:		NumVersion;								{  Class driver version number. }
	end;
	USBDriverType_fix = USBDriverType; { used as field type when a record declaration contains a USBDriverType field identifier }

	USBDriverDescriptionPtr = ^USBDriverDescription;
	USBDriverDescription = record
		usbDriverDescSignature:	OSType;									{  Signature field of this structure. }
		usbDriverDescVersion:	USBDriverDescVersion_fix;               {  Version of this data structure. }
		usbDeviceInfo:			USBDeviceInfo_fix;                      {  Product & Vendor Info }
		usbInterfaceInfo:		USBInterfaceInfo_fix;                   {  Interface info }
		usbDriverType:			USBDriverType_fix;                      {  Driver Info. }
		usbDriverLoadingOptions: USBDriverLoadingOptions_fix;           {  Options for class driver loading. }
	end;

	{
	   Dispatch Table
	   Definition of class driver's HW Validation proc.
	}
{$ifc TYPED_FUNCTION_POINTERS}
	USBDValidateHWProcPtr = function(device: USBDeviceRef; pDesc: USBDeviceDescriptorPtr): OSStatus;
{$elsec}
	USBDValidateHWProcPtr = ProcPtr;
{$endc}

	{
	   Definition of class driver's device initialization proc.
	   Called if the driver is being loaded for a device
	}
{$ifc TYPED_FUNCTION_POINTERS}
	USBDInitializeDeviceProcPtr = function(device: USBDeviceRef; pDesc: USBDeviceDescriptorPtr; busPowerAvailable: UInt32): OSStatus;
{$elsec}
	USBDInitializeDeviceProcPtr = ProcPtr;
{$endc}

	{  Definition of class driver's interface initialization proc. }
{$ifc TYPED_FUNCTION_POINTERS}
	USBDInitializeInterfaceProcPtr = function(interfaceNum: UInt32; pInterface: USBInterfaceDescriptorPtr; pDevice: USBDeviceDescriptorPtr; interfaceRef: USBInterfaceRef): OSStatus;
{$elsec}
	USBDInitializeInterfaceProcPtr = ProcPtr;
{$endc}

	{  Definition of class driver's finalization proc. }
{$ifc TYPED_FUNCTION_POINTERS}
	USBDFinalizeProcPtr = function(device: USBDeviceRef; pDesc: USBDeviceDescriptorPtr): OSStatus;
{$elsec}
	USBDFinalizeProcPtr = ProcPtr;
{$endc}

	USBDriverNotification 		= UInt32;
const
	kNotifySystemSleepRequest	= $00000001;
	kNotifySystemSleepDemand	= $00000002;
	kNotifySystemSleepWakeUp	= $00000003;
	kNotifySystemSleepRevoke	= $00000004;
	kNotifyHubEnumQuery			= $00000006;
	kNotifyChildMessage			= $00000007;
	kNotifyExpertTerminating	= $00000008;
	kNotifyDriverBeingRemoved	= $0000000B;
	kNotifyAllowROMDriverRemoval = $0000000E;

	{
	   Definition of driver's notification proc.      
	   Added refcon for 1.1 version of dispatch table
	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	USBDDriverNotifyProcPtr = function(notification: USBDriverNotification; pointer: UnivPtr; refcon: UInt32): OSStatus;
{$elsec}
	USBDDriverNotifyProcPtr = ProcPtr;
{$endc}

	USBClassDriverPluginDispatchTablePtr = ^USBClassDriverPluginDispatchTable;
	USBClassDriverPluginDispatchTable = record
		pluginVersion:			UInt32;
		validateHWProc:			USBDValidateHWProcPtr;					{  Proc for driver to verify proper HW }
		initializeDeviceProc:	USBDInitializeDeviceProcPtr;			{  Proc that initializes the class driver. }
		initializeInterfaceProc: USBDInitializeInterfaceProcPtr;		{  Proc that initializes a particular interface in the class driver. }
		finalizeProc:			USBDFinalizeProcPtr;					{  Proc that finalizes the class driver. }
		notificationProc:		USBDDriverNotifyProcPtr;				{  Proc to pass notifications to the driver. }
	end;

	{  Shim Defines }

const
	kTheUSBShimDescriptionSignature = FourCharCode('usbs');


type
	USBShimDescVersion 			= UInt32;
	USBShimDescVersion_fix      = USBShimDescVersion; { used as field type when a record declaration contains a USBShimDescVersion field identifier }
const
	kCurrentUSBShimDescVers		= $0100;

	{   Shim Loading Options }

type
	USBShimLoadingOptions 		= UInt32;
const
	kUSBRegisterShimAsSharedLibrary = $00000001;				{  Driver's VendorID must match Device's VendorID }


type
	USBShimDescriptionPtr = ^USBShimDescription;
	USBShimDescription = record
		usbShimDescSignature:	OSType;									{  Signature field of this structure. }
		usbShimDescVersion:		USBShimDescVersion_fix;                 {  Version of this data structure. }
		usbDriverLoadingOptions: USBShimLoadingOptions;					{  Options for shim loading. }
		libraryName:			Str63;									{  For optional shared library registration }
	end;

	{  Hub defines }


const
	kUSBHubDescriptorType		= $29;

																{  Hub features  }
	kUSBHubLocalPowerChangeFeature = 0;
	kUSBHubOverCurrentChangeFeature = 1;						{  port features  }
	kUSBHubPortConnectionFeature = 0;
	kUSBHubPortEnableFeature	= 1;
	kUSBHubPortSuspendFeature	= 2;
	kUSBHubPortOverCurrentFeature = 3;
	kUSBHubPortResetFeature		= 4;
	kUSBHubPortPowerFeature		= 8;
	kUSBHubPortLowSpeedFeature	= 9;
	kUSBHubPortConnectionChangeFeature = 16;
	kUSBHubPortEnableChangeFeature = 17;
	kUSBHubPortSuspendChangeFeature = 18;
	kUSBHubPortOverCurrentChangeFeature = 19;
	kUSBHubPortResetChangeFeature = 20;


	kHubPortConnection			= 1;
	kHubPortEnabled				= 2;
	kHubPortSuspend				= 4;
	kHubPortOverCurrent			= 8;
	kHubPortBeingReset			= 16;
	kHubPortPower				= $0100;
	kHubPortLowSpeed			= $0200;
	kHubPortHighSpeed			= $0400;
	kHubPortTestMode			= $0800;
	kHubPortPortIndicator		= $1000;

																{  Originally this was a Boolean, (low speed)? }
	kUSBFullSpeed				= 0;
	kUSBLowSpeed				= 1;
	kUSBHighSpeed				= 2;

	kHubLocalPowerStatus		= 1;
	kHubOverCurrentIndicator	= 2;
	kHubLocalPowerStatusChange	= 1;
	kHubOverCurrentIndicatorChange = 2;

	off							= false;
	on							= true;


type
	hubDescriptorPtr = ^hubDescriptor;
	hubDescriptor = packed record
																		{  See usbDoc pg 250??  }
		dummy:					UInt8;									{  to align charcteristics  }
		length:					UInt8;
		hubType:				UInt8;
		numPorts:				UInt8;
		characteristics:		UInt16;
		powerOnToGood:			UInt8;									{  Port settling time, in 2ms  }
		hubCurrent:				UInt8;
																		{  These are received packed, will have to be unpacked  }
		removablePortFlags:		packed array [0..7] of UInt8;
		pwrCtlPortFlags:		packed array [0..7] of UInt8;
	end;


{$ALIGN MAC68K}


end.
