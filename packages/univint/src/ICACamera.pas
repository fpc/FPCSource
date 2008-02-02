{
     File:       ICACamera.p
 
     Contains:   Digital still camera-specific selectors and structures
 
     Version:    Technology: 1.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit ICACamera;
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
uses MacTypes;
{$ALIGN MAC68K}

{
   -------------------------------------------------------------------------
                                Selectors           
   -------------------------------------------------------------------------
}

const
																{  Camera properties }
																{  Refer to section 13 of the PIMA 15740 (PTP) specification for }
																{  descriptions and usage notes for these standard properties }
	kICAPropertyCameraBatteryLevel = $35303031 (* '5001' *);					{  UInt8   enum/range }
	kICAPropertyCameraFunctionalMode = $35303032 (* '5002' *);					{  UInt16     enum }
	kICAPropertyCameraImageSize	= $35303033 (* '5003' *);						{  CFString     enum/range }
	kICAPropertyCameraCompressionSetting = $35303034 (* '5004' *);				{  UInt8   enum/range }
	kICAPropertyCameraWhiteBalance = $35303035 (* '5005' *);					{  UInt16     enum }
	kICAPropertyCameraRGBGain	= $35303036 (* '5006' *);						{  null terminated string enum/range }
	kICAPropertyCameraFNumber	= $35303037 (* '5007' *);						{  UInt16     enum }
	kICAPropertyCameraFocalLength = $35303038 (* '5008' *);						{  UInt32     enum/range }
	kICAPropertyCameraFocusDistance = $35303039 (* '5009' *);					{  UInt16     enum/range }
	kICAPropertyCameraFocusMode	= $35303041 (* '500A' *);						{  UInt16     enum }
	kICAPropertyCameraExposureMeteringMode = $35303042 (* '500B' *);			{  UInt16     enum }
	kICAPropertyCameraFlashMode	= $35303043 (* '500C' *);						{  UInt16     enum }
	kICAPropertyCameraExposureTime = $35303044 (* '500D' *);					{  UInt32     enum/range }
	kICAPropertyCameraExposureProgramMode = $35303045 (* '500E' *);				{  UInt16     enum }
	kICAPropertyCameraExposureIndex = $35303046 (* '500F' *);					{  UInt16     enum/range }
	kICAPropertyCameraExposureBiasCompensation = $35303130 (* '5010' *);		{  UInt16     enum/range }
	kICAPropertyCameraDateTime	= $35303131 (* '5011' *);						{  null terminated string     none }
	kICAPropertyCameraCaptureDelay = $35303132 (* '5012' *);					{  UInt32     enum/range }
	kICAPropertyCameraStillCaptureMode = $35303133 (* '5013' *);				{  UInt16     enum }
	kICAPropertyCameraContrast	= $35303134 (* '5014' *);						{  UInt8   enum/range }
	kICAPropertyCameraSharpness	= $35303135 (* '5015' *);						{  UInt8   enum/range }
	kICAPropertyCameraDigitalZoom = $35303136 (* '5016' *);						{  UInt8   enum/range }
	kICAPropertyCameraEffectMode = $35303137 (* '5017' *);						{  UInt16     enum }
	kICAPropertyCameraBurstNumber = $35303138 (* '5018' *);						{  UInt16     enum/range }
	kICAPropertyCameraBurstInterval = $35303139 (* '5019' *);					{  UInt16     enum/range }
	kICAPropertyCameraTimelapseNumber = $35303141 (* '501A' *);					{  UInt16     enum/range }
	kICAPropertyCameraTimelapseInterval = $35303142 (* '501B' *);				{  UInt32     enum/range }
	kICAPropertyCameraFocusMeteringMode = $35303143 (* '501C' *);				{  UInt16     enum }

																{  Refer to section 5.5.3 of the PTP spec }
	kICAPropertyCameraStorageType = $73746F72 (* 'stor' *);						{  UInt16 }
	kICAPropertyCameraFilesystemType = $66737973 (* 'fsys' *);					{  UInt16 }
	kICAPropertyCameraAccessCapability = $61636170 (* 'acap' *);				{  UInt16 }
	kICAPropertyCameraMaxCapacity = $6D617863 (* 'maxc' *);						{  UInt64 }
	kICAPropertyCameraFreeSpaceInBytes = $66726573 (* 'fres' *);				{  UInt64 }
	kICAPropertyCameraFreeSpaceInImages = $66726569 (* 'frei' *);				{  UInt32 }
	kICAPropertyCameraStorageDescription = $73746F64 (* 'stod' *);				{  null terminated string }
	kICAPropertyCameraVolumeLabel = $766F6C6C (* 'voll' *);						{  null terminated string }

																{  ICA specific }
	kICAPropertyCameraIcon		= $69636F6E (* 'icon' *);						{  ICAThumbnail }
	kICAPropertyCameraSupportedMessages = $6D736773 (* 'msgs' *);				{  array of OSTypes }

																{  Values for kICAPropertyCameraStorageType }
	kICAStorageFixedROM			= $0001;
	kICAStorageRemovableROM		= $0002;
	kICAStorageFixedRAM			= $0003;
	kICAStorageRemovableRAM		= $0004;

																{  Values for kICAPropertyCameraFilesystemType }
	kICAFileystemGenericFlat	= $0001;
	kICAFileystemGenericHierarchical = $0002;
	kICAFileystemDCF			= $0003;

																{  Values for kICAPropertyCameraAccessCapability }
	kICAAccessReadWrite			= $0000;
	kICAAccessReadOnly			= $0001;
	kICAAccessReadOnlyWithObjectDeletion = $0002;

																{  Camera messages }
	kICAMessageCameraCaptureNewImage = $63636E69 (* 'ccni' *);
	kICAMessageCameraDeleteOne	= $64656C31 (* 'del1' *);
	kICAMessageCameraDeleteAll	= $64656C61 (* 'dela' *);
	kICAMessageCameraSyncClock	= $73636C6B (* 'sclk' *);
	kICAMessageCameraUploadData	= $6C6F6164 (* 'load' *);

{$ALIGN MAC68K}


end.
