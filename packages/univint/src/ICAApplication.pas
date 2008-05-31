{
     File:       ICAApplication.p
 
     Contains:   General purpose Image Capture definitions
 
     Version:    Technology: 1.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit ICAApplication;
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
uses MacTypes,AEDataModel,Files,CFDictionary;


{$ALIGN MAC68K}


type
	ICAObject    = ^SInt32; { an opaque 32-bit type }
	ICAObjectPtr = ^ICAObject;  { when a var xx:ICAObject parameter can be nil, it is changed to xx: ICAObjectPtr }
	ICAProperty    = ^SInt32; { an opaque 32-bit type }
	ICAPropertyPtr = ^ICAProperty;  { when a var xx:ICAProperty parameter can be nil, it is changed to xx: ICAPropertyPtr }
	ICAConnectionID    = ^SInt32; { an opaque 32-bit type }
	ICAConnectionIDPtr = ^ICAConnectionID;  { when a var xx:ICAConnectionID parameter can be nil, it is changed to xx: ICAConnectionIDPtr }
	{	
	--------------- Defines --------------- 
		}
	{  Error codes (Image Capture range = -9900...-9949) }

const
	kICACommunicationErr		= -9900;
	kICADeviceNotFoundErr		= -9901;
	kICADeviceNotOpenErr		= -9902;
	kICAFileCorruptedErr		= -9903;
	kICAIOPendingErr			= -9904;
	kICAInvalidObjectErr		= -9905;
	kICAInvalidPropertyErr		= -9906;
	kICAIndexOutOfRangeErr		= -9907;
	kICAPropertyTypeNotFoundErr	= -9908;

	{	
	NOTE: vendor specific constants are UPPERCASE  (exception: 'TEXT')
		}
	{  ICAObject types and subtypes  }
	kICADevice					= FourCharCode('icdv');						{  Also creator of device library files  }
	kICADeviceCamera			= FourCharCode('cmra');						{  Also file type of device library files  }
	kICADeviceScanner			= FourCharCode('scan');						{  Also file type of device library files  }
	kICADeviceMFP				= FourCharCode('mfp ');						{  Also file type of device library files  }
	kICAList					= FourCharCode('objl');
	kICADirectory				= FourCharCode('dire');
	kICAFile					= FourCharCode('file');
	kICAFileImage				= FourCharCode('imag');
	kICAFileMovie				= FourCharCode('moov');
	kICAFileAudio				= FourCharCode('audo');
	kICAFileFirmware			= FourCharCode('firm');
	kICAFileOther				= FourCharCode('othe');

	{  ICAProperties  }
	kICAProperty				= FourCharCode('prop');						{   }
																{     file properties  }
																{   }
																{  for images, refer to 'Digital Still Camera Image File Format Standard' Exif Version 2.1 section 2.6.4. and 2.6.5. }
	kICAPropertyImageWidth		= FourCharCode('0100');						{  UInt32  }
	kICAPropertyImageHeight		= FourCharCode('0101');						{  UInt32  }
	kICAPropertyImageBitDepth	= FourCharCode('0102');						{  UInt32  }
	kICAPropertyImageDPI		= FourCharCode('011A');						{  UInt32  }
	kICAPropertyImageExposureTime = FourCharCode('829A');
	kICAPropertyImageFNumber	= FourCharCode('829D');
	kICAPropertyImageDateOriginal = FourCharCode('9003');						{  null terminated string (YYYYMMDDThhmmss.s)  }
	kICAPropertyImageDateDigitized = FourCharCode('9004');					{  null terminated string (YYYYMMDDThhmmss.s)  }
	kICAPropertyImageShutterSpeed = FourCharCode('9201');						{   }
	kICAPropertyImageAperture	= FourCharCode('9202');						{   }
	kICAPropertyImageFlash		= FourCharCode('9209');						{  UInt16 }
	kICAPropertyColorSpace		= FourCharCode('A001');						{  UInt16 }
	kICAPropertyImageFilename	= FourCharCode('ifil');						{  null terminated string  }
	kICAPropertyImageSize		= FourCharCode('isiz');						{  UInt32  }
	kICAPropertyImageData		= FourCharCode('idat');						{  void *  }
	kICAPropertyImageThumbnail	= FourCharCode('thum');						{  void *  }
	kICAPropertyColorSyncProfile = FourCharCode('prof');

	{  Messages  }
	kICAMessageConnect			= FourCharCode('open');
	kICAMessageDisconnect		= FourCharCode('clos');
	kICAMessageReset			= FourCharCode('rese');
	kICAMessageCheckDevice		= FourCharCode('chkd');


	{  Data type definitions, mapped to AppleEvent types  }
	kICATypeUInt16				= FourCharCode('ui16');						{  UInt16  }
	kICATypeUInt32				= FourCharCode('ui32');						{  UInt32  }
	kICATypeUInt64				= FourCharCode('ui64');						{  UInt64  }
	kICATypeSInt16				= FourCharCode('si16');						{  SInt16  }
	kICATypeSInt32				= FourCharCode('si32');						{  SInt32  }
	kICATypeSInt64				= FourCharCode('si64');						{  SInt64  }
	kICATypeFixed				= FourCharCode('sing');						{  typeIEEE32BitFloatingPoint  }
	kICATypeBoolean				= FourCharCode('bool');						{  typeBoolean  }
	kICATypeString				= FourCharCode('TEXT');						{  typeChar  }
	kICATypeData				= FourCharCode('data');						{  void *  }
	kICATypeThumbnail			= FourCharCode('thum');						{  ICAThumbnail }


	{  Flags for PropertyInfo flag element  }
	kICAFlagReadWriteAccess		= $00000001;
	kICAFlagReadAccess			= $00000002;


	{  Notification types (Refer to section 12.4 of PTP spec)  }
	kICAEventCancelTransaction	= FourCharCode('ecnt');
	kICAEventObjectAdded		= FourCharCode('eoba');
	kICAEventObjectRemoved		= FourCharCode('eobr');
	kICAEventStoreAdded			= FourCharCode('esta');
	kICAEventStoreRemoved		= FourCharCode('estr');
	kICAEventDeviceAdded		= FourCharCode('edea');
	kICAEventDeviceRemoved		= FourCharCode('eder');
	kICAEventDevicePropChanged	= FourCharCode('edpc');
	kICAEventObjectInfoChanged	= FourCharCode('eoic');
	kICAEventDeviceInfoChanged	= FourCharCode('edic');
	kICAEventRequestObjectTransfer = FourCharCode('erot');
	kICAEventStoreFull			= FourCharCode('estf');
	kICAEventDeviceReset		= FourCharCode('edvr');
	kICAEventStorageInfoChanged	= FourCharCode('esic');
	kICAEventCaptureComplete	= FourCharCode('ecpc');
	kICAEventUnreportedStatus	= FourCharCode('eurs');


	{  Used for partial reads via ICAGetPropertyData  }
	kICAStartAtBeginning		= 0;
	kICAEntireLength			= -1;

	{  ICADownloadFile flags  }
	kDeleteAfterDownload		= $00000001;
	kCreateCustomIcon			= $00000002;
	kAddMetaDataToFinderComment	= $00000004;
	kAdjustCreationDate			= $00000008;
	kSetFileTypeAndCreator		= $00000010;
	kEmbedColorSyncProfile		= $00000020;
	kRotateImage				= $00000040;


	{	
	--------------- Structures --------------- 
		}

type
	ICAObjectInfoPtr = ^ICAObjectInfo;
	ICAObjectInfo = record
		objectType:				OSType;									{  i.e. kICAFile }
		objectSubtype:			OSType;									{  i.e. kICAFileImage   }
	end;

	ICAPropertyInfoPtr = ^ICAPropertyInfo;
	ICAPropertyInfo = record
		propertyType:			OSType;
		dataType:				OSType;
		dataSize:				UInt32;
		dataFlags:				UInt32;
	end;

	ICAMessagePtr = ^ICAMessage;
	ICAMessage = record
		messageType:			OSType;									{  <--  i.e. kICAMessageCameraCaptureNewImage  }
		startByte:				UInt32;									{  <--  }
		dataPtr:				Ptr;									{  <--  }
		dataSize:				UInt32;									{  <--  }
		dataType:				OSType;									{  <--  }
	end;

	ICAThumbnailPtr = ^ICAThumbnail;
	ICAThumbnail = record
		width:					UInt32;
		height:					UInt32;
		dataSize:				UInt32;
		data:					SInt8;									{  8-bit RGB data (RGBRGBRGB...) }
	end;


const
	kICAPBVersion				= $00010000;

	{	 
	--------------- Completion Procs --------------- 
		}
	{
	   
	   NOTE: the parameter for the completion proc (ICAHeader*) has to be casted to the appropriate type
	   e.g. (ICAGetChildCountPB*), ...
	   
	}


type
	ICAHeaderPtr = ^ICAHeader;
{$ifc TYPED_FUNCTION_POINTERS}
	ICACompletion = procedure(pb: ICAHeaderPtr);
{$elsec}
	ICACompletion = ProcPtr;
{$endc}

	{	 
	--------------- ICAHeader --------------- 
		}
	ICAHeader = record
		err:					OSErr;									{  -->  }
		refcon:					UInt32;									{  <--  }
	end;

	{	
	--------------- Object parameter blocks --------------- 
		}
	ICAGetChildCountPBPtr = ^ICAGetChildCountPB;
	ICAGetChildCountPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		count:					UInt32;									{  -->  }
	end;

	ICAGetNthChildPBPtr = ^ICAGetNthChildPB;
	ICAGetNthChildPB = record
		header:					ICAHeader;
		parentObject:			ICAObject;								{  <--  }
		index:					UInt32;									{  <-- zero based  }
		childObject:			ICAObject;								{  -->  }
		childInfo:				ICAObjectInfo;							{  -->  }
	end;

	ICAGetObjectInfoPBPtr = ^ICAGetObjectInfoPB;
	ICAGetObjectInfoPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		objectInfo:				ICAObjectInfo;							{  -->  }
	end;

	ICAGetParentOfObjectPBPtr = ^ICAGetParentOfObjectPB;
	ICAGetParentOfObjectPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		parentObject:			ICAObject;								{  -->  }
		parentInfo:				ICAObjectInfo;							{  -->  }
	end;

	ICAGetRootOfObjectPBPtr = ^ICAGetRootOfObjectPB;
	ICAGetRootOfObjectPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		rootObject:				ICAObject;								{  -->  }
		rootInfo:				ICAObjectInfo;							{  -->  }
	end;

	ICAGetObjectRefConPBPtr = ^ICAGetObjectRefConPB;
	ICAGetObjectRefConPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		objectRefCon:			UInt32;									{  -->  }
	end;

	ICASetObjectRefConPBPtr = ^ICASetObjectRefConPB;
	ICASetObjectRefConPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		objectRefCon:			UInt32;									{  <--  }
	end;

	{	
	--------------- Property parameter blocks --------------- 
		}
	ICAGetPropertyCountPBPtr = ^ICAGetPropertyCountPB;
	ICAGetPropertyCountPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		count:					UInt32;									{  -->  }
	end;

	ICAGetNthPropertyPBPtr = ^ICAGetNthPropertyPB;
	ICAGetNthPropertyPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		index:					UInt32;									{  <-- zero based  }
		proprty:				ICAProperty;							{  -->  }
		propertyInfo:			ICAPropertyInfo;						{  -->  }
	end;

	ICAGetPropertyByTypePBPtr = ^ICAGetPropertyByTypePB;
	ICAGetPropertyByTypePB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		propertyType:			OSType;									{  <--  }
		proprty:				ICAProperty;							{  -->  }
		propertyInfo:			ICAPropertyInfo;						{  -->  }
	end;

	ICAGetPropertyInfoPBPtr = ^ICAGetPropertyInfoPB;
	ICAGetPropertyInfoPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		propertyInfo:			ICAPropertyInfo;						{  -->  }
	end;

	ICAGetPropertyDataPBPtr = ^ICAGetPropertyDataPB;
	ICAGetPropertyDataPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		startByte:				UInt32;									{  <--  }
		requestedSize:			UInt32;									{  <--  }
		dataPtr:				Ptr;									{  <->  }
		actualSize:				UInt32;									{  -->  }
		dataType:				OSType;									{  -->  }
	end;

	ICASetPropertyDataPBPtr = ^ICASetPropertyDataPB;
	ICASetPropertyDataPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		startByte:				UInt32;									{  <--  }
		dataPtr:				Ptr;									{  <--  }
		dataSize:				UInt32;									{  <--  }
		dataType:				OSType;									{  <--  }
	end;

	ICAGetParentOfPropertyPBPtr = ^ICAGetParentOfPropertyPB;
	ICAGetParentOfPropertyPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		parentObject:			ICAObject;								{  -->  }
		parentInfo:				ICAObjectInfo;							{  -->  }
	end;

	ICAGetRootOfPropertyPBPtr = ^ICAGetRootOfPropertyPB;
	ICAGetRootOfPropertyPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		rootObject:				ICAObject;								{  -->  }
		rootInfo:				ICAObjectInfo;							{  -->  }
	end;

	ICAGetPropertyRefConPBPtr = ^ICAGetPropertyRefConPB;
	ICAGetPropertyRefConPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		propertyRefCon:			UInt32;									{  -->  }
	end;

	ICASetPropertyRefConPBPtr = ^ICASetPropertyRefConPB;
	ICASetPropertyRefConPB = record
		header:					ICAHeader;
		proprty:				ICAProperty;							{  <--  }
		propertyRefCon:			UInt32;									{  <--  }
	end;

	{	
	--------------- Device parameter blocks --------------- 
		}
	ICAGetDeviceListPBPtr = ^ICAGetDeviceListPB;
	ICAGetDeviceListPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  -->  }
	end;

	ICAObjectSendMessagePBPtr = ^ICAObjectSendMessagePB;
	ICAObjectSendMessagePB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		message:				ICAMessage;								{  <--  }
		result:					UInt32;									{  --> message specific result }
	end;

	ICARegisterEventNotificationPBPtr = ^ICARegisterEventNotificationPB;
	ICARegisterEventNotificationPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		notifyType:				OSType;									{  <--  }
		notifyProc:				ICACompletion;							{  <--  }
	end;

	ICADownloadFilePBPtr = ^ICADownloadFilePB;
	ICADownloadFilePB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		dirFSRef:				FSRefPtr;								{  <--  }
		flags:					UInt32;									{  <--  }
		fileType:				OSType;									{  <--  }
		fileCreator:			OSType;									{  <--  }
		rotationAngle:			Fixed;									{  <--  }
		fileFSRef:				FSRefPtr;								{  --> can be NULL }
	end;

	ICACopyObjectPropertyDictionaryPBPtr = ^ICACopyObjectPropertyDictionaryPB;
	ICACopyObjectPropertyDictionaryPB = record
		header:					ICAHeader;
		objct:					ICAObject;								{  <--  }
		theDict:				^CFDictionaryRef;						{  -->  }
	end;

	{
	   
	   NOTE: for all APIs - pass NULL as completion parameter to make a synchronous call 
	   
	}
	{	
	--------------- Object functions --------------- 
		}

	{
	 *  ICAGetChildCount()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ICAGetChildCount(var pb: ICAGetChildCountPB; completion: ICACompletion): OSErr; external name '_ICAGetChildCount';

{
 *  ICAGetNthChild()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetNthChild(var pb: ICAGetNthChildPB; completion: ICACompletion): OSErr; external name '_ICAGetNthChild';

{
 *  ICAGetObjectInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetObjectInfo(var pb: ICAGetObjectInfoPB; completion: ICACompletion): OSErr; external name '_ICAGetObjectInfo';

{
 *  ICAGetParentOfObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetParentOfObject(var pb: ICAGetParentOfObjectPB; completion: ICACompletion): OSErr; external name '_ICAGetParentOfObject';

{
 *  ICAGetRootOfObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetRootOfObject(var pb: ICAGetRootOfObjectPB; completion: ICACompletion): OSErr; external name '_ICAGetRootOfObject';

{
 *  ICAGetObjectRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetObjectRefCon(var pb: ICAGetObjectRefConPB; completion: ICACompletion): OSErr; external name '_ICAGetObjectRefCon';

{
 *  ICASetObjectRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICASetObjectRefCon(var pb: ICASetObjectRefConPB; completion: ICACompletion): OSErr; external name '_ICASetObjectRefCon';


{
--------------- Property functions --------------- 
}
{
 *  ICAGetPropertyCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetPropertyCount(var pb: ICAGetPropertyCountPB; completion: ICACompletion): OSErr; external name '_ICAGetPropertyCount';

{
 *  ICAGetNthProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetNthProperty(var pb: ICAGetNthPropertyPB; completion: ICACompletion): OSErr; external name '_ICAGetNthProperty';

{
 *  ICAGetPropertyByType()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetPropertyByType(var pb: ICAGetPropertyByTypePB; completion: ICACompletion): OSErr; external name '_ICAGetPropertyByType';

{
 *  ICAGetPropertyInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetPropertyInfo(var pb: ICAGetPropertyInfoPB; completion: ICACompletion): OSErr; external name '_ICAGetPropertyInfo';

{
 *  ICAGetPropertyData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetPropertyData(var pb: ICAGetPropertyDataPB; completion: ICACompletion): OSErr; external name '_ICAGetPropertyData';

{
 *  ICASetPropertyData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICASetPropertyData(var pb: ICASetPropertyDataPB; completion: ICACompletion): OSErr; external name '_ICASetPropertyData';

{
 *  ICAGetParentOfProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetParentOfProperty(var pb: ICAGetParentOfPropertyPB; completion: ICACompletion): OSErr; external name '_ICAGetParentOfProperty';

{
 *  ICAGetRootOfProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetRootOfProperty(var pb: ICAGetRootOfPropertyPB; completion: ICACompletion): OSErr; external name '_ICAGetRootOfProperty';

{
 *  ICAGetPropertyRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetPropertyRefCon(var pb: ICAGetPropertyRefConPB; completion: ICACompletion): OSErr; external name '_ICAGetPropertyRefCon';

{
 *  ICASetPropertyRefCon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICASetPropertyRefCon(var pb: ICASetPropertyRefConPB; completion: ICACompletion): OSErr; external name '_ICASetPropertyRefCon';


{ 
--------------- Device functions --------------- 
}
{
 *  ICAGetDeviceList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAGetDeviceList(var pb: ICAGetDeviceListPB; completion: ICACompletion): OSErr; external name '_ICAGetDeviceList';

{
 *  ICAObjectSendMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAObjectSendMessage(var pb: ICAObjectSendMessagePB; completion: ICACompletion): OSErr; external name '_ICAObjectSendMessage';

{
 *  ICARegisterEventNotification()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICARegisterEventNotification(var pb: ICARegisterEventNotificationPB; completion: ICACompletion): OSErr; external name '_ICARegisterEventNotification';


{ 
------------------------------------------------ 
}
{
 *  ICADownloadFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 }
function ICADownloadFile(var pb: ICADownloadFilePB; completion: ICACompletion): OSErr; external name '_ICADownloadFile';


{
 *  ICACopyObjectPropertyDictionary()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.1 and later
 }
function ICACopyObjectPropertyDictionary(var pb: ICACopyObjectPropertyDictionaryPB; completion: ICACompletion): OSErr; external name '_ICACopyObjectPropertyDictionary';

{$ALIGN MAC68K}


end.
