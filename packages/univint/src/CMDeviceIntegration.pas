{
     File:       CMDeviceIntegration.p
 
     Contains:   Color Management Device Interfaces - for MacOSX
 
     Version:    Technology: ColorSync 3.1
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

unit CMDeviceIntegration;
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
uses MacTypes,CFBase,CFDictionary,CMTypes,CMApplication,CMICCProfile,CFString;


{$ALIGN MAC68K}

{$ifc TARGET_API_MAC_OSX}
{
    The current versions of the data structure
    containing information on registered devices.
}

const
	cmDeviceInfoVersion1		= $00010000;
	cmDeviceProfileInfoVersion1	= $00010000;

	cmCurrentDeviceInfoVersion	= $00010000;
	cmCurrentProfileInfoVersion	= $00010000;

	{	
	    Certain APIs require a device ID or profile ID.  
	    In some cases, a "default ID" can be used.
		}
	cmDefaultDeviceID			= 0;
	cmDefaultProfileID			= 0;

	{	
	    Possible values for device states accessible by the
	    CMGetDeviceState() and CMSetDeviceState() APIs.
		}
	cmDeviceStateDefault		= $00000000;
	cmDeviceStateOffline		= $00000001;
	cmDeviceStateBusy			= $00000002;
	cmDeviceStateForceNotify	= $80000000;
	cmDeviceStateDeviceRsvdBits	= $00FF0000;
	cmDeviceStateAppleRsvdBits	= $FF00FFFF;

	{	
	    Possible values for flags passed to the
	    CMIterateDeviceProfiles() API.
	    
	    "Factory" profiles are registered via the
	    CMSetDeviceFactoryProfiles() API.
	    
	    "Custom" profiles are those which are meant to take
	    the place of the factory profiles, as a result of
	    customization or calibration.  These profiles are
	    registered via the CMSetDeviceProfiles() API.
	    
	    To retrieve all of the the former for all devices,
	    use cmIterateFactoryDeviceProfiles as the flags
	    value when calling CMIterateDeviceProfiles().
	    
	    To retrieve only the latter for all devices, use
	    the cmIterateCustomDeviceProfiles, as the flags
	    value when calling CMIterateDeviceProfiles().
	    
	    To get the profiles in use for all devices, use
	    cmIterateCurrentDeviceProfiles as the flags value.
	    This will replace the factory profiles with any
	    overrides, yielding the currently used set.
		}
	cmIterateFactoryDeviceProfiles = $00000001;
	cmIterateCustomDeviceProfiles = $00000002;
	cmIterateCurrentDeviceProfiles = $00000003;
	cmIterateDeviceProfilesMask	= $00000003;

	kMaxDeviceNameLength		= 256;
	kMaxProfileNameLength		= 256;

	{	
	    Errors returned by CMDeviceIntegration APIs
		}
	cmDeviceDBNotFoundErr		= -4227;						{  Prefs not found/loaded  }
	cmDeviceAlreadyRegistered	= -4228;						{  Re-registration of device  }
	cmDeviceNotRegistered		= -4229;						{  Device not found  }
	cmDeviceProfilesNotFound	= -4230;						{  Profiles not found  }
	cmInternalCFErr				= -4231;						{  CoreFoundation failure  }

	{	
	    Device state data.
		}

type
	CMDeviceState						= UInt32;
	{	
	    A CMDeviceID must be unique within a device's class.
		}
	CMDeviceID							= UInt32;
	{	
	    A CMDeviceProfileID must only be unique per device.
		}
	CMDeviceProfileID					= UInt32;
	{	
	    DeviceClass type.
		}

const
	cmScannerDeviceClass		= FourCharCode('scnr');
	cmCameraDeviceClass			= FourCharCode('cmra');
	cmDisplayDeviceClass		= FourCharCode('mntr');
	cmPrinterDeviceClass		= FourCharCode('prtr');
	cmProofDeviceClass			= FourCharCode('pruf');


type
	CMDeviceClass						= OSType;
	{	
	    CMDeviceScope
	    Structure specifying a device's or a device setting's scope.
		}
	CMDeviceScopePtr = ^CMDeviceScope;
	CMDeviceScope = record
		deviceUser:				CFStringRef;							{  kCFPreferencesCurrentUser | _AnyUser  }
		deviceHost:				CFStringRef;							{  kCFPreferencesCurrentHost | _AnyHost  }
	end;

	{	
	    CMDeviceInfo
	    Structure containing information on a given device.
		}
	CMDeviceInfoPtr = ^CMDeviceInfo;
	CMDeviceInfo = record
		dataVersion:			UInt32;									{  cmDeviceInfoVersion1  }
		deviceClass:			CMDeviceClass;							{  device class  }
		deviceID:				CMDeviceID;								{  device ID  }
		deviceScope:			CMDeviceScope;							{  device's scope  }
		deviceState:			CMDeviceState;							{  Device State flags  }
		defaultProfileID:		CMDeviceProfileID;						{  Can change  }
		deviceName:				^CFDictionaryRef;						{  Ptr to storage for CFDictionary of  }
																		{  localized device names (could be nil)  }
		profileCount:			UInt32;									{  Count of registered profiles  }
		reserved:				UInt32;									{  Reserved for use by ColorSync  }
	end;

	{	
	    CMDeviceProfileInfo
	    Structure containing information on a device profile.
		}
	CMDeviceProfileInfoPtr = ^CMDeviceProfileInfo;
	CMDeviceProfileInfo = record
		dataVersion:			UInt32;									{  cmProfileInfoVersion1  }
		profileID:				CMDeviceProfileID;						{  The identifier for this profile  }
		profileLoc:				CMProfileLocation;						{  The profile's location  }
		profileName:			CFDictionaryRef;						{  CFDictionary of localized device names  }
		reserved:				UInt32;									{  Reserved for use by ColorSync  }
	end;

	{	
	    CMDeviceProfileArray
	    Structure containing the profiles for a device.
		}
	CMDeviceProfileArrayPtr = ^CMDeviceProfileArray;
	CMDeviceProfileArray = record
		profileCount:			UInt32;									{  Count of profiles in array  }
		profiles:				array [0..0] of CMDeviceProfileInfo;	{  The profile info records  }
	end;

	{	
	    Caller-supplied iterator functions
		}
{$ifc TYPED_FUNCTION_POINTERS}
	CMIterateDeviceInfoProcPtr = function(const (*var*) deviceInfo: CMDeviceInfo; refCon: UnivPtr): OSErr;
{$elsec}
	CMIterateDeviceInfoProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CMIterateDeviceProfileProcPtr = function(const (*var*) deviceInfo: CMDeviceInfo; const (*var*) profileData: CMDeviceProfileInfo; refCon: UnivPtr): OSErr;
{$elsec}
	CMIterateDeviceProfileProcPtr = ProcPtr;
{$endc}

	{	
	    Device Registration
		}
	{
	 *  CMRegisterColorDevice()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         in 3.1 and later
	 	}
function CMRegisterColorDevice(deviceClass: CMDeviceClass; deviceID: CMDeviceID; deviceName: CFDictionaryRef; const (*var*) deviceScope: CMDeviceScope): CMError; external name '_CMRegisterColorDevice';

{
 *  CMUnregisterColorDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMUnregisterColorDevice(deviceClass: CMDeviceClass; deviceID: CMDeviceID): CMError; external name '_CMUnregisterColorDevice';

{
    Default Device accessors
}
{
 *  CMSetDefaultDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMSetDefaultDevice(deviceClass: CMDeviceClass; deviceID: CMDeviceID): CMError; external name '_CMSetDefaultDevice';

{
 *  CMGetDefaultDevice()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDefaultDevice(deviceClass: CMDeviceClass; var deviceID: CMDeviceID): CMError; external name '_CMGetDefaultDevice';

{
    Device Profile Registration & Access
}
{
 *  CMSetDeviceFactoryProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMSetDeviceFactoryProfiles(deviceClass: CMDeviceClass; deviceID: CMDeviceID; defaultProfID: CMDeviceProfileID; const (*var*) deviceProfiles: CMDeviceProfileArray): CMError; external name '_CMSetDeviceFactoryProfiles';

{
 *  CMGetDeviceFactoryProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDeviceFactoryProfiles(deviceClass: CMDeviceClass; deviceID: CMDeviceID; var defaultProfID: CMDeviceProfileID; var arraySize: UInt32; var deviceProfiles: CMDeviceProfileArray): CMError; external name '_CMGetDeviceFactoryProfiles';

{
 *  CMSetDeviceProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMSetDeviceProfiles(deviceClass: CMDeviceClass; deviceID: CMDeviceID; const (*var*) profileScope: CMDeviceScope; const (*var*) deviceProfiles: CMDeviceProfileArray): CMError; external name '_CMSetDeviceProfiles';

{
 *  CMGetDeviceProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDeviceProfiles(deviceClass: CMDeviceClass; deviceID: CMDeviceID; var arraySize: UInt32; var deviceProfiles: CMDeviceProfileArray): CMError; external name '_CMGetDeviceProfiles';

{
 *  CMSetDeviceDefaultProfileID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMSetDeviceDefaultProfileID(deviceClass: CMDeviceClass; deviceID: CMDeviceID; defaultProfID: CMDeviceProfileID): CMError; external name '_CMSetDeviceDefaultProfileID';

{
 *  CMGetDeviceDefaultProfileID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDeviceDefaultProfileID(deviceClass: CMDeviceClass; deviceID: CMDeviceID; var defaultProfID: CMDeviceProfileID): CMError; external name '_CMGetDeviceDefaultProfileID';

{
 *  CMGetDeviceProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDeviceProfile(deviceClass: CMDeviceClass; deviceID: CMDeviceID; profileID: CMDeviceProfileID; var deviceProfLoc: CMProfileLocation): CMError; external name '_CMGetDeviceProfile';

{
 *  CMSetDeviceProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMSetDeviceProfile(deviceClass: CMDeviceClass; deviceID: CMDeviceID; const (*var*) profileScope: CMDeviceScope; profileID: CMDeviceProfileID; const (*var*) deviceProfLoc: CMProfileLocation): CMError; external name '_CMSetDeviceProfile';

{
    Other Device State/Info accessors
}
{
 *  CMSetDeviceState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMSetDeviceState(deviceClass: CMDeviceClass; deviceID: CMDeviceID; deviceState: CMDeviceState): CMError; external name '_CMSetDeviceState';

{
 *  CMGetDeviceState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDeviceState(deviceClass: CMDeviceClass; deviceID: CMDeviceID; var deviceState: CMDeviceState): CMError; external name '_CMGetDeviceState';

{
 *  CMGetDeviceInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMGetDeviceInfo(deviceClass: CMDeviceClass; deviceID: CMDeviceID; var deviceInfo: CMDeviceInfo): CMError; external name '_CMGetDeviceInfo';

{
    Device Data & Profile Iterators
}
{
 *  CMIterateColorDevices()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMIterateColorDevices(proc: CMIterateDeviceInfoProcPtr; var seed: UInt32; var count: UInt32; refCon: UnivPtr): CMError; external name '_CMIterateColorDevices';

{
 *  CMIterateDeviceProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in 3.1 and later
 }
function CMIterateDeviceProfiles(proc: CMIterateDeviceProfileProcPtr; var seed: UInt32; var count: UInt32; flags: UInt32; refCon: UnivPtr): CMError; external name '_CMIterateDeviceProfiles';

{$endc}  {TARGET_API_MAC_OSX}

{$ALIGN MAC68K}


end.
