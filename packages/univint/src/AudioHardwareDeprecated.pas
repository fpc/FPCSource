{==================================================================================================
     File:       CoreAudio/AudioHardwareDeprecated.h

     Copyright:  (c) 1985-2011 by Apple, Inc., all rights reserved.

     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:

                     http://bugs.freepascal.org

==================================================================================================}
{  Initial Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit AudioHardwareDeprecated;
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
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
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
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
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
uses MacTypes, CFRunLoop, CoreAudioTypes, AudioHardwareBase, AudioHardware;
{$endc} {not MACOSALLINCLUDE}


{$ALIGN POWER}

//==================================================================================================
//  Includes


//==================================================================================================


//==================================================================================================
//#pragma mark -
//#pragma mark Property Support Constants

{!
    @enum           Property Address Constants
    @abstract       The valid values for the scope in a property address.
    @constant       kAudioDevicePropertyScopeInput
                        The AudioObjectPropertyScope for properties that apply to the input side of
                        an object.
    @constant       kAudioDevicePropertyScopeOutput
                        The AudioObjectPropertyScope for properties that apply to the output side of
                        an object.
    @constant       kAudioDevicePropertyScopePlayThrough
                        The AudioObjectPropertyScope for properties that apply to the play through
                        side of an object.
}
const
	kAudioDevicePropertyScopeInput = kAudioObjectPropertyScopeInput;
	kAudioDevicePropertyScopeOutput = kAudioObjectPropertyScopeOutput;
	kAudioDevicePropertyScopePlayThrough = kAudioObjectPropertyScopePlayThrough;

{!
    @enum           Property Wildcard Constants
    @abstract       Constants that are used as wildcards in an AudioObjectPropertyAddress.
    @discussion     Wildcards match any and all values for there associated type. They are
                    especially useful for registering listener procs to receive notifications and
                    for querying an AudioObject's list of AudioControls.
    @constant       kAudioPropertyWildcardPropertyID
                        A synonym for kAudioObjectPropertySelectorWildcard.
    @constant       kAudioPropertyWildcardSection
                        The wildcard value for the isInput argument of AudioDeviceGetPropertyInfo(),
                        AudioDeviceGetProperty(), and AudioDeviceSetProperty().
    @constant       kAudioPropertyWildcardChannel
                        A synonym for kAudioObjectPropertyElementWildcard.
}
const
	kAudioPropertyWildcardPropertyID = kAudioObjectPropertySelectorWildcard;
	kAudioPropertyWildcardSection = $FF;
	kAudioPropertyWildcardChannel = kAudioObjectPropertyElementWildcard;

//==================================================================================================
//#pragma mark -
//#pragma mark AudioControl Constants

{!
    @enum           AudioBooleanControl Subclass IDs
    @abstract       The four char codes that identify the various standard subclasses of
                    AudioBooleanControl.
    @constant       kAudioISubOwnerClassID
                        An AudioBooleanControl where true means that the AudioDevice that
                        ultimately owns the control also owns any iSub attached to the CPU.
}
const
	kAudioISubOwnerControlClassID = FourCharCode('atch');

//==================================================================================================
//#pragma mark AudioControl Properties

{!
    @enum           AudioLevelControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioLevelControls.
    @discussion     AudioLevelControl is a subclass of AudioControl and has only the single scope,
                    kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioLevelControlPropertyDecibelsToScalarTransferFunction
                        A UInt32 whose value indicates the transfer function the HAL uses to convert
                        between decibel values and scalar values.
}
const
	kAudioLevelControlPropertyDecibelsToScalarTransferFunction = FourCharCode('lctf');

{!
    @enum           Values for kAudioLevelControlPropertyDecibelsToScalarTransferFunction
    @abstract       The following constants are the only supported values for a volume control's
                    transfer function.
    @discussion     The transfer function implemented in the volume control works by raising the
                    scalar value to an exponent to map it into the decibel range. The constants
                    in this enum express the exponent used in the name as a quotient. For example,
                    kAudioLevelControlTranferFunction3Over4 represents the exponent 0.75.
}
const
	kAudioLevelControlTranferFunctionLinear = 0;
	kAudioLevelControlTranferFunction1Over3 = 1;
	kAudioLevelControlTranferFunction1Over2 = 2;
	kAudioLevelControlTranferFunction3Over4 = 3;
	kAudioLevelControlTranferFunction3Over2 = 4;
	kAudioLevelControlTranferFunction2Over1 = 5;
	kAudioLevelControlTranferFunction3Over1 = 6;
	kAudioLevelControlTranferFunction4Over1 = 7;
	kAudioLevelControlTranferFunction5Over1 = 8;
	kAudioLevelControlTranferFunction6Over1 = 9;
	kAudioLevelControlTranferFunction7Over1 = 10;
	kAudioLevelControlTranferFunction8Over1 = 11;
	kAudioLevelControlTranferFunction9Over1 = 12;
	kAudioLevelControlTranferFunction10Over1 = 13;
	kAudioLevelControlTranferFunction11Over1 = 14;
	kAudioLevelControlTranferFunction12Over1 = 15;

//==================================================================================================
//#pragma mark -
//#pragma mark AudioSystemObject Types

{!
    @typedef        AudioHardwarePropertyID
    @abstract       An AudioHardwarePropertyID is a integer that identifies a specific piece of
                    information about the AudioSystemObject.
}
type
	AudioHardwarePropertyID = AudioObjectPropertySelector;

{!
    @typedef        AudioHardwarePropertyListenerProc
    @abstract       Clients register an AudioHardwarePropertyListenerProc with the AudioSystemObject
                    in order to receive notifications when the properties of the object change.
    @discussion     Note that the same functionality is provided by AudioObjectPropertyListenerProc.
    @param          inPropertyID
                        The AudioHardwarePropertyID of the property that changed.
    @param          inClientData
                        A pointer to client data established when the listener proc was registered
                        with the AudioSystemObject.
    @result         The return value is currently unused and should always be 0.
}
type
	AudioHardwarePropertyListenerProc = function( inPropertyID: AudioHardwarePropertyID; inClientData: UnivPtr ): OSStatus;

//==================================================================================================
//#pragma mark AudioSystemObject Constants

{!
    @defined        kAudioHardwareRunLoopMode
    @discussion     The name of the run loop mode to which only HAL run loop sources and sources
                    added via AudioHardwareAddRunLoopSource() belong. This is the mode in which to
                    task a run loop in order to ensure that just HAL related events are handled.
}
const
	kAudioHardwareRunLoopMode = 'com.apple.audio.CoreAudio';

//==================================================================================================
//#pragma mark AudioSystemObject Properties

{!
    @enum           AudioSystemObject Properties
    @abstract       AudioObjectPropertySelector values that apply to the AudioSystemObject.
    @discussion     The AudioSystemObject has one scope, kAudioObjectPropertyScopeGlobal, and only a
                    master element.
    @constant       kAudioHardwarePropertyRunLoop
                        The CFRunLoopRef the HAL is currently attaching all of it's system
                        notification handlers to. In 10.6 and later, the HAL will use the process's
                        run loop (as defined by CFRunLoopGetMain()) for this task. Whereas in
                        previous releases, the HAL created and managed it's own thread for the task.
                        Clients can set this property to tell the HAL to use a thread of the
                        client's choosing. If the value for this property is set to NULL, the HAL
                        will return to it's pre-10.6 behavior of creating and managing it's own
                        thread for notifications. The caller is responsible for releasing the
                        returned CFObject.
    @constant       kAudioHardwarePropertyDeviceForUID
                        Using an AudioValueTranslation structure, this property translates the input
                        CFStringRef containing a UID into the AudioDeviceID that refers to the
                        AudioDevice with that UID. This property will return kAudioDeviceUnknown if
                        the given UID does not match any currently available AudioDevice.
}
const
	kAudioHardwarePropertyRunLoop = FourCharCode('rnlp');
	kAudioHardwarePropertyDeviceForUID = FourCharCode('duid');

{!
    @enum           AudioSystemObject Properties Implemented via AudioControl objects
    @abstract       AudioObjectPropertySelector values for AudioSystemObject properties that are
                    implemented by AudioControl objects.
    @discussion     These properties are also accessible by locating the AudioControl object
                    attached to the AudioSystemObject and using that object to access the properties
                    of the control.
    @constant       kAudioHardwarePropertyBootChimeVolumeScalar
                        A Float32 that represents the value of the boot chime volume control. The
                        range is between 0.0 and 1.0 (inclusive). This property is implemented by an
                        AudioControl object that is a subclass of AudioBootChimeVolumeControl.
    @constant       kAudioHardwarePropertyBootChimeVolumeDecibels
                        A Float32 that represents the value of the boot chime volume control in dB.
                        This property is implemented by an AudioControl object that is a subclass
                        of AudioBootChimeVolumeControl.
    @constant       kAudioHardwarePropertyBootChimeVolumeRangeDecibels
                        An AudioValueRange that contains the minimum and maximum dB values the
                        boot chime control can have. This property is implemented by an AudioControl
                        object that is a subclass of AudioBootChimeVolumeControl.
    @constant       kAudioHardwarePropertyBootChimeVolumeScalarToDecibels
                        A Float32 that on input contains a scalar volume value for the boot chime
                        and on exit contains the equivalent dB value. This property is implemented
                        by an AudioControl object that is a subclass of AudioBootChimeVolumeControl.
    @constant       kAudioHardwarePropertyBootChimeVolumeDecibelsToScalar
                        A Float32 that on input contains a dB volume value for the boot chime and on
                        exit contains the equivalent scalar value. This property is implemented by
                        an AudioControl object that is a subclass of AudioBootChimeVolumeControl.
    @constant       kAudioHardwarePropertyBootChimeVolumeDecibelsToScalarTransferFunction
                        A UInt32 whose value indicates the transfer function the HAL uses to convert
                        between decibel values and scalar values. This property is implemented by an
                        AudioControl object that is a subclass of AudioBootChimeVolumeControl.
}
const
	kAudioHardwarePropertyBootChimeVolumeScalar = FourCharCode('bbvs');
	kAudioHardwarePropertyBootChimeVolumeDecibels = FourCharCode('bbvd');
	kAudioHardwarePropertyBootChimeVolumeRangeDecibels = FourCharCode('bbd#');
	kAudioHardwarePropertyBootChimeVolumeScalarToDecibels = FourCharCode('bv2d');
	kAudioHardwarePropertyBootChimeVolumeDecibelsToScalar = FourCharCode('bd2v');
	kAudioHardwarePropertyBootChimeVolumeDecibelsToScalarTransferFunction = FourCharCode('bvtf');

//==================================================================================================
//#pragma mark AudioSystemObject Functions

{!
    @functiongroup  AudioSystemObject
}

{!
    @function       AudioHardwareAddRunLoopSource
    @abstract       Add the given CFRunLoopSource to the the HAL's notification CFRunLoop.
    @discussion     The CFRunLoop the HAL uses for notifications is specified by
                    kAudioHardwarePropertyRunLoop. If kAudioHardwarePropertyRunLoop changes,
                    CFRunLoopSources added with this function will automatically be transferred to
                    the new CFRunLoop.
                    Usage of the HAL's notification run loop is deprecated. Please use libdispatch
                    instead.
    @param          inRunLoopSource
                        The CFRunLoopSource to add.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareAddRunLoopSource( inRunLoopSource: CFRunLoopSourceRef ): OSStatus; external name '_AudioHardwareAddRunLoopSource';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3, __MAC_10_7, __IPHONE_2_0, __IPHONE_4_1) *)

{!
    @function       AudioHardwareRemoveRunLoopSource
    @abstract       Remove the given CFRunLoopSource from the the HAL's notification CFRunLoop.
    @discussion     The CFRunLoop the HAL uses for notifications is specified by
                    kAudioHardwarePropertyRunLoop.
                    Usage of the HAL's notification run loop is deprecated. Please use libdispatch
                    instead.
    @param          inRunLoopSource
                        The CFRunLoopSource to remove.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareRemoveRunLoopSource( inRunLoopSource: CFRunLoopSourceRef ): OSStatus; external name '_AudioHardwareRemoveRunLoopSource';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3, __MAC_10_7, __IPHONE_2_0, __IPHONE_4_1) *)

{!
    @function       AudioHardwareGetPropertyInfo
    @abstract       Retrieve information about the given property.
    @discussion     Note that the same functionality is provided by the functions
                    AudioObjectHasProperty(), AudioObjectIsPropertySettable(), and
                    AudioObjectGetPropertyDataSize().
    @param          inPropertyID
                        The AudioHardwarePropertyID of the property to query.
    @param          outSize
                        A pointer to a UInt32 that receives the size of the property data in bytes
                        on exit. This can be NULL if the size information is not being requested.
    @param          outWritable
                        A pointer to a Boolean that receives indication of whether or not the given
                        property can be set. This can be NULL if the writability is not being
                        requested.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareGetPropertyInfo( inPropertyID: AudioHardwarePropertyID; outSize: UInt32Ptr; outWritable: BooleanPtr ): OSStatus; external name '_AudioHardwareGetPropertyInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioHardwareGetProperty
    @abstract       Queries an the AudioSystemObject to get the data of the given property and
                    places it in the provided buffer.
    @discussion     Note that the same functionality is provided by the function
                    AudioObjectGetPropertyData().
    @param          inPropertyID
                        The AudioHardwarePropertyID of the property to query.
    @param          ioDataSize
                        A UInt32 which on entry indicates the size of the buffer pointed to by
                        outData and on exit indicates how much of the buffer was used.
    @param          outData
                        The buffer into which the AudioSystemObject will put the data for the given
                        property.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareGetProperty( inPropertyID: AudioHardwarePropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioHardwareGetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioHardwareSetProperty
    @abstract       Tells the AudioSystemObject to change the value of the given property using the
                    provided data.
    @discussion     Note that the value of the property should not be considered changed until the
                    HAL has called the listeners as many properties values are changed
                    asynchronously. Also note that the same functionality is provided by the
                    function AudioObjectGetPropertyData().
    @param          inPropertyID
                        The AudioHardwarePropertyID of the property to change.
    @param          inDataSize
                        A UInt32 indicating the size of the buffer pointed to by inData.
    @param          inData
                        The buffer containing the data to be used to change the property's value.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareSetProperty( inPropertyID: AudioHardwarePropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioHardwareSetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioHardwareAddPropertyListener
    @abstract       Registers the given AudioHardwarePropertyListenerProc to receive notifications
                    when the given property changes.
    @discussion     Note that the same functionality is provided by AudioObjectAddPropertyListener
                    in conjunction with AudioObjectPropertyListenerProc.
    @param          inPropertyID
                        The AudioHardwarePropertyID of the property to listen to.
    @param          inProc
                        AudioHardwarePropertyListenerProc to call.
    @param          inClientData
                        A pointer to client data that is passed to the listener when it is called.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareAddPropertyListener( inPropertyID: AudioHardwarePropertyID; inProc: AudioHardwarePropertyListenerProc; inClientData: UnivPtr ): OSStatus; external name '_AudioHardwareAddPropertyListener';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioHardwareRemovePropertyListener
    @abstract       Unregisters the given AudioHardwarePropertyListenerProc from receive
                    notifications when the given property changes.
    @discussion     Note that the same functionality is provided by
                    AudioObjectRemovePropertyListener in conjunction with
                    AudioObjectPropertyListenerProc.
    @param          inPropertyID
                        The AudioHardwarePropertyID of the property to stop listening to.
    @param          inProc
                        AudioHardwarePropertyListenerProc to unregister.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareRemovePropertyListener( inPropertyID: AudioHardwarePropertyID; inProc: AudioHardwarePropertyListenerProc ): OSStatus; external name '_AudioHardwareRemovePropertyListener';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

//==================================================================================================
//#pragma mark -
//#pragma mark AudioDevice Types

{!
    @typedef        AudioDeviceID
    @abstract       AudioDevice is the base class for all objects that represent an audio device.
    @discussion     AudioDevice is a subclass of AudioObject. AudioDevices normally contain
                    AudioStreams and AudioControls, but may contain other things depending on the
                    kind of AudioDevice (e.g. aggregate devices contain other AudioDevices).
}
type
	AudioDeviceID = AudioObjectID;

{!
    @typedef        AudioDevicePropertyID
    @abstract       An AudioDevicePropertyID is an integer that identifies a specific piece of
                    information about the object.
}
type
	AudioDevicePropertyID = AudioObjectPropertySelector;

{!
    @typedef        AudioDevicePropertyListenerProc
    @abstract       Clients register an AudioDevicePropertyListenerProc with the AudioDevice object
                    in order to receive notifications when the properties of the object change.
    @discussion     Note that the same functionality is provided by AudioObjectPropertyListenerProc.
    @param          inDevice
                        The AudioDevice whose property has changed.
    @param          inChannel
                        The channel of the property that changed where 0 is the master channel.
    @param          isInput
                        Which section of the AudioDevice changed.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property that changed.
    @param          inClientData
                        A pointer to client data established when the listener proc was registered
                        with the object.
    @result         The return value is currently unused and should always be 0.
}
type
	AudioDevicePropertyListenerProc = function( inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inClientData: UnivPtr ): OSStatus;

//==================================================================================================
//#pragma mark AudioDevice Constants

{!
    @enum           AudioDevice Class Constants
    @abstract       Various constants related to AudioDevices.
    @constant       kAudioDeviceUnknown
                        The AudioObjectID for a nonexistent AudioObject.
}
const
	kAudioDeviceUnknown = kAudioObjectUnknown;

//==================================================================================================
//#pragma mark AudioDevice Properties

{!
    @enum           AudioDevice Properties Implemented via AudioControl objects
    @abstract       AudioObjectPropertySelector values for AudioDevice properties that are
                    implemented by AudioControl objects.
    @discussion     These properties are also accessible by locating the AudioControl object
                    attached to the AudioDevice and using that object to access the properties of
                    the control.
    @constant       kAudioDevicePropertyVolumeDecibelsToScalarTransferFunction
                        A UInt32 whose value indicates the transfer function the HAL uses to convert
                        between decibel values and scalar values. This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl.
    @constant       kAudioDevicePropertyPlayThruVolumeDecibelsToScalarTransferFunction
                        A UInt32 whose value indicates the transfer function the HAL uses to convert
                        between decibel values and scalar values. This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl. Further, the
                        control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyDriverShouldOwniSub
                        A UInt32 where a value of 0 means that the AudioDevice should not claim
                        ownership of any attached iSub and a value of 1 means that it should. Note
                        that this property is only available for built-in devices and for USB Audio
                        devices that use the standard class compliant driver. This property is
                        implemented by an AudioControl object that is a subclass of
                        AudioISubOwnerControl.
    @constant       kAudioDevicePropertySubVolumeDecibelsToScalarTransferFunction
                        A UInt32 whose value indicates the transfer function the HAL uses to convert
                        between decibel values and scalar values. This property is implemented by an
                        AudioControl object that is a subclass of AudioLFEVolumeControl.
}
const
	kAudioDevicePropertyVolumeDecibelsToScalarTransferFunction = FourCharCode('vctf');
	kAudioDevicePropertyPlayThruVolumeDecibelsToScalarTransferFunction = FourCharCode('mvtf');
	kAudioDevicePropertyDriverShouldOwniSub = FourCharCode('isub');
	kAudioDevicePropertySubVolumeDecibelsToScalarTransferFunction = FourCharCode('svtf');

{!
    @enum           AudioDevice Properties That Ought To Some Day Be Deprecated
    @abstract       AudioObjectPropertySelector values whose functionality is better provided by
                    other selectors.
    @discussion     These selectors are still provided for backward compatibility. The description
                    of the property will indicate in parentheses the better selectors to use and
                    why.
    @constant       kAudioDevicePropertyDeviceName
                        A C-string that contains the human readable name of the AudioDevice.
                        (kAudioObjectPropertyName: CFStrings are better for localization.)
    @constant       kAudioDevicePropertyDeviceNameCFString
                        A CFStringRef that contains the human readable name of the AudioDevice. The
                        caller is responsible for releasing the returned CFObject.
                        (kAudioObjectPropertyName: This is just another name for the inherited
                        selector.)
    @constant       kAudioDevicePropertyDeviceManufacturer
                        A C-string that contains the human readable name of the manufacturer of the
                        AudioDevice.
                        (kAudioObjectPropertyManufacturer: CFStrings are better for localization.)
    @constant       kAudioDevicePropertyDeviceManufacturerCFString
                        A CFString that contains the human readable name of the manufacturer of the
                        AudioDevice. The caller is responsible for releasing the returned CFObject.
                        (kAudioObjectPropertyManufacturer: This is just another name for the
                        inherited selector.)
    @constant       kAudioDevicePropertyRegisterBufferList
                        This property allows clients to register a fully populated AudioBufferList
                        that matches the topology described by
                        kAudioDevicePropertyStreamConfiguration for doing input using
                        AudioDeviceRead(). The AudioBufferList will be registered with the call the
                        AudioDeviceSetProperty() and will be unregistered with the call to
                        AudioDeviceGetProperty(). If this property isn't implemented by the
                        AudioDevice, it implies that the AudioDevice also doesn't support
                        AudioDeviceRead().
                        (Aggregate devices make AudioDeviceRead() obsolete for the most part.)
    @constant       kAudioDevicePropertyBufferSize
                        A UInt32 containing the size in bytes of the IO buffer for the AudioStream
                        containing the element.
                        (kAudioDevicePropertyBufferFrameSize: with multiple AudioStreams and the
                        requirement that all streams' buffers represent the same amount of time, it
                        doesn't make sense to set the buffer size in bytes since it will be
                        different for each stream.)
    @constant       kAudioDevicePropertyBufferSizeRange
                        An AudioValueRange specifying the minimum and maximum bytes size for the
                        IO buffer for the AudioStream containing the given element.
                        (kAudioDevicePropertyBufferFrameSizeRange: see
                        kAudioDevicePropertyBufferSize.)
    @constant       kAudioDevicePropertyChannelName
                        A C-string that contains a human readable name for the given element in the
                        given scope. The caller is responsible for releasing the returned CFObject.
                        (kAudioObjectPropertyElementName: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyChannelNameCFString
                        A CFString that contains a human readable name for the given element in the
                        given scope. The caller is responsible for releasing the returned CFObject.
                        (kAudioObjectPropertyElementName: This is just another name for the
                        inherited selector.)
    @constant       kAudioDevicePropertyChannelCategoryName
                        A C-string that contains a human readable name for the category of the given
                        element in the given scope. The caller is responsible for releasing the
                        returned CFObject.
                        (kAudioObjectPropertyElementCategoryName: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyChannelCategoryNameCFString
                        A CFString that contains a human readable name for the category of the given
                        element in the given scope. The caller is responsible for releasing the
                        returned CFObject.
                        (kAudioObjectPropertyElementCategoryName: This is just another name for the
                        inherited selector.)
    @constant       kAudioDevicePropertyChannelNumberName
                        A C-string that contains a human readable name for the number of the given
                        element in the given scope. The caller is responsible for releasing the
                        returned CFObject.
                        (kAudioObjectPropertyElementNumberName: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyChannelNumberNameCFString
                        A CFString that contains a human readable name for the number of the given
                        element in the given scope. The caller is responsible for releasing the
                        returned CFObject.
                        (kAudioObjectPropertyElementNumberName: This is just another name for the
                        inherited selector.)
    @constant       kAudioDevicePropertySupportsMixing
                        A UInt32 where a value of 1 means the AudioDevice supports mixing and a
                        value of 0 means that it doesn't and that all IO is performed in each
                        AudioStream's current physical format. This property is changed indirectly
                        by changing to a format that doesn't support mixing, such as AC-3. (The HAL
                        now vends it's format information with a flag indicating the mixability in
                        order to better support devices with streams that are both mixable and non-
                        mixable.)
    @constant       kAudioDevicePropertyStreamFormat
                        An AudioStreamBasicDescription that describes the current data format for
                        the AudioStream that contains the channel referred to by the element number.
                        (kAudioStreamPropertyVirtualFormat: Managing format information is
                        inherently an operation on AudioStreams, rather than AudioDevices. It is
                        confusing for the client to work with formats at the AudioDevice level and
                        has been shown to lead to programming mistakes by clients when working with
                        devices that have multiple streams.)
    @constant       kAudioDevicePropertyStreamFormats
                        An array of AudioStreamBasicDescriptions that describe the available data
                        formats for the AudioStream that contains the channel referred to by the
                        element number.
                        (kAudioStreamPropertyAvailableVirtualFormats: Managing format information is
                        inherently an operation on AudioStreams, rather than AudioDevices. It is
                        confusing for the client to work with formats at the AudioDevice level and
                        has been shown to lead to programming mistakes by clients when working with
                        devices that have multiple streams.)
    @constant       kAudioDevicePropertyStreamFormatSupported
                        An AudioStreamBasicDescription is passed in to query whether or not the
                        format is supported. A kAudioDeviceUnsupportedFormatError will be returned
                        if the format is not supported and kAudioHardwareNoError will be returned if
                        it is supported. AudioStreamBasicDescription fields set to 0 will be ignored
                        in the query, but otherwise values must match exactly.
                        (kAudioStreamPropertyAvailableVirtualFormats: The proper and most robust way
                        to find a format that the AudioStream can support is to get the list of
                        available formats and look through that rather than using this property.)
    @constant       kAudioDevicePropertyStreamFormatMatch
                        An AudioStreamBasicDescription is passed in and the AudioStream will modify
                        it to describe the best match, in the AudioDevice's opinion, for the given
                        format.
                        (kAudioStreamPropertyAvailableVirtualFormats: The proper and most robust way
                        to find a format that the AudioStream can support is to get the list of
                        available formats and look through that rather than using this property.)
    @constant       kAudioDevicePropertyDataSourceNameForID
                        This property translates the given data source item ID into a human readable
                        name using an AudioValueTranslation structure. The input data is the UInt32
                        holding the item ID to be translated and the output data is a buffer to hold
                        the name as a null terminated C-string.
                        (kAudioDevicePropertyDataSourceNameForIDCFString: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyClockSourceNameForID
                        This property translates the given clock source item ID into a human
                        readable name using an AudioValueTranslation structure. The input data is
                        the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated C-string.
                        (kAudioDevicePropertyClockSourceNameForIDCFString: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyPlayThruDestinationNameForID
                        This property translates the given play through destination item ID into a
                        human readable name using an AudioValueTranslation structure. The input data
                        is the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated C-string.
                        (kAudioDevicePropertyPlayThruDestinationNameForIDCFString: CFStrings are
                        better for localization.)
    @constant       kAudioDevicePropertyChannelNominalLineLevelNameForID
                        This property translates the given nominal line level item ID into a human
                        readable name using an AudioValueTranslation structure. The input data is
                        the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated C-string.
                        (kAudioDevicePropertyChannelNominalLineLevelNameForIDCFString: CFStrings are
                        better for localization.)
    @constant       kAudioDevicePropertyHighPassFilterSettingNameForID
                        This property translates the given high pass filter setting item ID into a
                        human readable name using an AudioValueTranslation structure. The input data
                        is the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated C-string.
                        (kAudioDevicePropertyHighPassFilterSettingNameForIDCFString: CFStrings are
                        better for localization.)
}
const
	kAudioDevicePropertyDeviceName = FourCharCode('name');
	kAudioDevicePropertyDeviceNameCFString = kAudioObjectPropertyName;
	kAudioDevicePropertyDeviceManufacturer = FourCharCode('makr');
	kAudioDevicePropertyDeviceManufacturerCFString = kAudioObjectPropertyManufacturer;
	kAudioDevicePropertyRegisterBufferList = FourCharCode('rbuf');
	kAudioDevicePropertyBufferSize = FourCharCode('bsiz');
	kAudioDevicePropertyBufferSizeRange = FourCharCode('bsz#');
	kAudioDevicePropertyChannelName = FourCharCode('chnm');
	kAudioDevicePropertyChannelNameCFString = kAudioObjectPropertyElementName;
	kAudioDevicePropertyChannelCategoryName = FourCharCode('ccnm');
	kAudioDevicePropertyChannelCategoryNameCFString = kAudioObjectPropertyElementCategoryName;
	kAudioDevicePropertyChannelNumberName = FourCharCode('cnnm');
	kAudioDevicePropertyChannelNumberNameCFString = kAudioObjectPropertyElementNumberName;
	kAudioDevicePropertySupportsMixing = FourCharCode('mix?');
	kAudioDevicePropertyStreamFormat = FourCharCode('sfmt');
	kAudioDevicePropertyStreamFormats = FourCharCode('sfm#');
	kAudioDevicePropertyStreamFormatSupported = FourCharCode('sfm?');
	kAudioDevicePropertyStreamFormatMatch = FourCharCode('sfmm');
	kAudioDevicePropertyDataSourceNameForID = FourCharCode('sscn');
	kAudioDevicePropertyClockSourceNameForID = FourCharCode('cscn');
	kAudioDevicePropertyPlayThruDestinationNameForID = FourCharCode('mddn');
	kAudioDevicePropertyChannelNominalLineLevelNameForID = FourCharCode('cnlv');
	kAudioDevicePropertyHighPassFilterSettingNameForID = FourCharCode('chip');

//==================================================================================================
//#pragma mark AudioDevice Functions

{!
    @functiongroup  AudioDevice
}

{!
    @function       AudioDeviceAddIOProc
    @abstract       Registers the given AudioDeviceIOProc with the AudioDevice.
    @discussion     A client may have multiple IOProcs for a given device, but the device is free to
                    only accept as many as it can handle. Note that it is not recommended for
                    clients to have more than a single IOProc registered at a time as this can be
                    wasteful of system resources. Rather, it is recommended that the client do any
                    necessary mixing itself so that only one IOProc is necessary.
                    This routine has been deprecated in favor of AudioDeviceCreateIOProcID().
    @param          inDevice
                        The AudioDevice to register the IOProc with.
    @param          inProc
                        The AudioDeviceIOProc to register.
    @param          inClientData
                        A pointer to client data that is passed back to the IOProc when it is
                        called.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceAddIOProc( inDevice: AudioDeviceID; inProc: AudioDeviceIOProc; inClientData: UnivPtr ): OSStatus; external name '_AudioDeviceAddIOProc';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceRemoveIOProc
    @abstract       Unregisters the given AudioDeviceIOProc from the AudioDevice.
                    This routine has been deprecated in favor of AudioDeviceDestroyIOProcID().
    @param          inDevice
                        The AudioDevice to unregister the IOProc from.
    @param          inProc
                        The AudioDeviceIOProc to unregister.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceRemoveIOProc( inDevice: AudioDeviceID; inProc: AudioDeviceIOProc ): OSStatus; external name '_AudioDeviceRemoveIOProc';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceRead
    @abstract       Read some data from an AudioDevice starting at the given time.
    @discussion     With the advent of aggregate devices, the need for AudioDeviceRead has gone
                    away. Consequently, this function is now deprecated.
    @param          inDevice
                        The AudioDevice to read from.
    @param          inStartTime
                        An AudioTimeStamp indicating the time from which to read the data. In
                        general, the valid range of time (in frames) is from the current time minus
                        the maximum IO buffer size to the current time minus the safety offset.
    @param          outData
                        An AudioBufferList that must be the same size and shape as that returned by
                        kAudioDevicePropertyStreamConfiguration. Further, the AudioBufferList must
                        have been previously registered with the device via
                        kAudioDevicePropertyRegisterBufferList. On exit, the mDataSize fields will
                        be updated with the amount of data read.
    @result         An OSStatus indicating success or failure.
                    kAudioHardwareUnsupportedOperationError will be returned if the AudioDevice does
                    not support direct reading.
}
function AudioDeviceRead( inDevice: AudioDeviceID; const (*var*) inStartTime: AudioTimeStamp; var outData: AudioBufferList ): OSStatus; external name '_AudioDeviceRead';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_5, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceGetPropertyInfo
    @abstract       Retrieve information about the given property of an AudioDevice.
    @discussion     Note that the same functionality is provided by the functions
                    AudioObjectHasProperty(), AudioObjectIsPropertySettable(), and
                    AudioObjectGetPropertyDataSize().
    @param          inDevice
                        The AudioDevice to query.
    @param          inChannel
                        The channel of the property to query where 0 is the master channel.
    @param          isInput
                        Which section of the AudioDevice to query.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to query.
    @param          outSize
                        A pointer to a UInt32 that receives the size of the property data in bytes
                        on exit. This can be NULL if the size information is not being requested.
    @param          outWritable
                        A pointer to a Boolean that receives indication of whether or not the given
                        property can be set. This can be NULL if the writability is not being
                        requested.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceGetPropertyInfo( inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; outSize: {can be NULL} UInt32Ptr; outWritable: {can be NULL} BooleanPtr ): OSStatus; external name '_AudioDeviceGetPropertyInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceGetProperty
    @abstract       Queries an the AudioDevice object to get the data of the given property and
                    places it in the provided buffer.
    @discussion     Note that the same functionality is provided by the function
                    AudioObjectGetPropertyData().
    @param          inDevice
                        The AudioDevice to query.
    @param          inChannel
                        The channel of the property to query where 0 is the master channel.
    @param          isInput
                        Which section of the AudioDevice to query.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to query.
    @param          ioPropertyDataSize
                        A UInt32 which on entry indicates the size of the buffer pointed to by
                        outData and on exit indicates how much of the buffer was used.
    @param          outPropertyData
                        The buffer into which the object will put the data for the given property.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceGetProperty( inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioDeviceGetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceSetProperty
    @abstract       Tells the AudioDevice object to change the value of the given property using the
                    provided data.
    @discussion     Note that the value of the property should not be considered changed until the
                    HAL has called the listeners as many properties values are changed
                    asynchronously. Also note that the same functionality is provided by the
                    function AudioObjectSetPropertyData().
    @param          inDevice
                        The AudioDevice to change.
    @param          inWhen
                        A pointer to an AudioTimeStamp that says when to change the property's value
                        relative to the device's time base. NULL means execute the change
                        immediately.
    @param          inChannel
                        The channel of the property to change where 0 is the master channel.
    @param          isInput
                        Which section of the AudioDevice to change.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to change.
    @param          inPropertyDataSize
                        A UInt32 indicating the size of the buffer pointed to by inData.
    @param          inPropertyData
                        The buffer containing the data to be used to change the property's value.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceSetProperty( inDevice: AudioDeviceID; {const} inWhen: {can be NULL} AudioTimeStampPtr; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioDeviceSetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceAddPropertyListener
    @abstract       Registers the given AudioDevicePropertyListenerProc to receive notifications
                    when the given property changes.
    @discussion     Note that the same functionality is provided by AudioObjectAddPropertyListener
                    in conjunction with AudioObjectPropertyListenerProc.
    @param          inDevice
                        The AudioDevice with whom to register the listener.
    @param          inChannel
                        The channel of the property to listen to.
    @param          isInput
                        Which section of the AudioDevice to listen to.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to listen to.
    @param          inProc
                        AudioDevicePropertyListenerProc to call.
    @param          inClientData
                        A pointer to client data that is passed to the listener when it is called.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceAddPropertyListener( inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inProc: AudioDevicePropertyListenerProc; inClientData: UnivPtr ): OSStatus; external name '_AudioDeviceAddPropertyListener';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioDeviceRemovePropertyListener
    @abstract       Unregisters the given AudioDevicePropertyListenerProc from receiving
                    notifications when the given property changes.
    @discussion     Note that the same functionality is provided by
                    AudioObjectRemovePropertyListener in conjunction with
                    AudioObjectPropertyListenerProc.
    @param          inDevice
                        The AudioDevice with whom to unregister the listener.
    @param          inChannel
                        The channel of the property to unregister from.
    @param          isInput
                        Which section of the AudioDevice to unregister from.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to stop listening to.
    @param          inProc
                        AudioDevicePropertyListenerProc to unregister.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceRemovePropertyListener( inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inProc: AudioDevicePropertyListenerProc ): OSStatus; external name '_AudioDeviceRemovePropertyListener';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

//==================================================================================================
//#pragma mark -
//#pragma mark AudioStream Types

{!
    @typedef        AudioStreamID
    @abstract       AudioStream is the base class for all objects that represent a stream of data on
                    an audio device.
    @discussion     AudioStream is a subclass of AudioObject and can contain AudioControls.
}
type
	AudioStreamID = AudioObjectID;

{!
    @typedef        AudioStreamPropertyListenerProc
    @abstract       Clients register an AudioStreamPropertyListenerProc with the AudioStream object
                    in order to receive notifications when the properties of the object change.
    @discussion     Note that the same functionality is provided by AudioObjectPropertyListenerProc.
    @param          inStream
                        The AudioStream whose property has changed.
    @param          inChannel
                        The channel of the property that changed where 0 is the master channel.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property that changed.
    @param          inClientData
                        A pointer to client data established when the listener proc was registered
                        with the object.
    @result         The return value is currently unused and should always be 0.
}
type
	AudioStreamPropertyListenerProc = function( inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inClientData: UnivPtr ): OSStatus;

//==================================================================================================
//#pragma mark AudioStream Constants

{!
    @enum           AudioStream Class Constants
    @abstract       Various constants related to AudioStreams.
    @constant       kAudioStreamUnknown
                        The AudioObjectID for a nonexistent AudioObject.
}
const
	kAudioStreamUnknown = kAudioObjectUnknown;

//==================================================================================================
//#pragma mark AudioStream Properties

{!
    @enum           AudioStream Properties That Ought To Some Day Be Deprecated
    @abstract       AudioObjectPropertySelector values whose functionality is better provided by
                    other selectors.
    @discussion     These selectors are still provided for backward compatibility. The description
                    of the property will indicate in parentheses the better selectors to use and
                    why.
    @constant       kAudioStreamPropertyOwningDevice
                        The AudioObjectID of the AudioDevice of which this AudioStream is a part.
                        (kAudioObjectPropertyOwner: This is just another name for the inherited
                        selector.)
    @constant       kAudioStreamPropertyPhysicalFormats
                        An array of AudioStreamBasicDescriptions that describe the available data
                        formats for the AudioStream. The physical format refers to the data format
                        in which the hardware for the owning AudioDevice performs it's IO
                        transactions.
                        (kAudioStreamPropertyAvailablePhysicalFormats: The new name for this
                        property is much clearer for readers of the API to see what is meant and the
                        AudioStreamRangedDescription structure provides better information.)
    @constant       kAudioStreamPropertyPhysicalFormatSupported
                        An AudioStreamBasicDescription is passed in to query whether or not the
                        format is supported. A kAudioDeviceUnsupportedFormatError will be returned
                        if the format is not supported and kAudioHardwareNoError will be returned if
                        it is supported. AudioStreamBasicDescription fields set to 0 will be ignored
                        in the query, but otherwise values must match exactly. The physical format
                        refers to the data format in which the hardware for the owning AudioDevice
                        performs it's IO transactions.
                        (kAudioStreamPropertyAvailablePhysicalFormats: The proper and most robust
                        way to find a format that the AudioStream can support is to get the list of
                        available formats and look through that rather than using this property.)
    @constant       kAudioStreamPropertyPhysicalFormatMatch
                        An AudioStreamBasicDescription is passed in and the AudioStream will modify
                        it to describe the best match, in the AudioDevice's opinion, for the given
                        format. The physical format refers to the data format in which the hardware
                        for the owning AudioDevice performs it's IO transactions.
                        (kAudioStreamPropertyAvailablePhysicalFormats: The proper and most robust
                        way to find a format that the AudioStream can support is to get the list of
                        available formats and look through that rather than using this property.)
}
const
	kAudioStreamPropertyOwningDevice = kAudioObjectPropertyOwner;
	kAudioStreamPropertyPhysicalFormats = FourCharCode('pft#');
	kAudioStreamPropertyPhysicalFormatSupported = FourCharCode('pft?');
	kAudioStreamPropertyPhysicalFormatMatch = FourCharCode('pftm');

//==================================================================================================
//#pragma mark AudioStream Functions

{!
    @functiongroup  AudioStream
}

{!
    @function       AudioStreamGetPropertyInfo
    @abstract       Retrieve information about the given property of an AudioStream.
    @param          inStream
                        The AudioStream to query.
    @param          inChannel
                        The channel of the property to query where 0 is the master channel.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to query.
    @param          outSize
                        A pointer to a UInt32 that receives the size of the property data in bytes
                        on exit. This can be NULL if the size information is not being requested.
    @param          outWritable
                        A pointer to a Boolean that receives indication of whether or not the given
                        property can be set. This can be NULL if the writability is not being
                        requested.
    @result         An OSStatus indicating success or failure.
}
function AudioStreamGetPropertyInfo( inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; outSize: {can be NULL} UInt32Ptr; outWritable: {can be NULL} BooleanPtr ): OSStatus; external name '_AudioStreamGetPropertyInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioStreamGetProperty
    @abstract       Queries an the AudioStream object to get the data of the given property and
                    places it in the provided buffer.
    @discussion     Note that the same functionality is provided by the function
                    AudioObjectGetPropertyData().
    @param          inStream
                        The AudioStream to query.
    @param          inChannel
                        The channel of the property to query where 0 is the master channel.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to query.
    @param          ioPropertyDataSize
                        A UInt32 which on entry indicates the size of the buffer pointed to by
                        outData and on exit indicates how much of the buffer was used.
    @param          outPropertyData
                        The buffer into which the object will put the data for the given property.
    @result         An OSStatus indicating success or failure.
}
function AudioStreamGetProperty( inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; var ioPropertyDataSize: UInt32; outPropertyData: UnivPtr ): OSStatus; external name '_AudioStreamGetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioStreamSetProperty
    @abstract       Tells the AudioStream object to change the value of the given property using the
                    provided data.
    @discussion     Note that the value of the property should not be considered changed until the
                    HAL has called the listeners as many properties values are changed
                    asynchronously. Also note that the same functionality is provided by the
                    function AudioObjectSetPropertyData().
    @param          inStream
                        The AudioStream to change.
    @param          inWhen
                        A pointer to an AudioTimeStamp that says when to change the property's value
                        relative to the device's time base. NULL means execute the change
                        immediately.
    @param          inChannel
                        The channel of the property to change where 0 is the master channel.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to change.
    @param          inPropertyDataSize
                        A UInt32 indicating the size of the buffer pointed to by inData.
    @param          inPropertyData
                        The buffer containing the data to be used to change the property's value.
    @result         An OSStatus indicating success or failure.
}
function AudioStreamSetProperty( inStream: AudioStreamID; {const} inWhen: {can be NULL} AudioTimeStampPtr; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioStreamSetProperty';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioStreamAddPropertyListener
    @abstract       Registers the given AudioStreamPropertyListenerProc to receive notifications
                    when the given property changes.
    @discussion     Note that the same functionality is provided by AudioObjectAddPropertyListener
                    in conjunction with AudioObjectPropertyListenerProc.
    @param          inStream
                        The AudioStream with whom to register the listener.
    @param          inChannel
                        The channel of the property to listen to.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to listen to.
    @param          inProc
                        AudioStreamPropertyListenerProc to call.
    @param          inClientData
                        A pointer to client data that is passed to the listener when it is called.
    @result         An OSStatus indicating success or failure.
}
function AudioStreamAddPropertyListener( inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inProc: AudioStreamPropertyListenerProc; inClientData: UnivPtr ): OSStatus; external name '_AudioStreamAddPropertyListener';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)

{!
    @function       AudioStreamRemovePropertyListener
    @abstract       Unregisters the given AudioStreamPropertyListenerProc from receiving
                    notifications when the given property changes.
    @discussion     Note that the same functionality is provided by
                    AudioObjectRemovePropertyListener in conjunction with
                    AudioObjectPropertyListenerProc.
    @param          inStream
                        The AudioStream with whom to unregister the listener.
    @param          inChannel
                        The channel of the property to unregister from.
    @param          inPropertyID
                        The AudioDevicePropertyID of the property to stop listening to.
    @param          inProc
                        AudioStreamPropertyListenerProc to unregister.
    @result         An OSStatus indicating success or failure.
}
function AudioStreamRemovePropertyListener( inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inProc: AudioStreamPropertyListenerProc ): OSStatus; external name '_AudioStreamRemovePropertyListener';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_6, __IPHONE_2_0, __IPHONE_2_0) *)


//==================================================================================================
//#pragma mark -
//#pragma mark AudioControl Constants

{!
    @enum           AudioControl Base Class IDs
    @abstract       The AudioClassIDs that identify the various AudioControl base classes.
    @constant       kAudioBootChimeVolumeControlClassID
                        An AudioLevelControl for the boot chime of the CPU.
}
const
	kAudioBootChimeVolumeControlClassID = FourCharCode('pram');

//==================================================================================================
//#pragma mark AudioControl Properties

{!
    @enum           AudioControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioControls.
    @discussion     AudioControl is a subclass of AudioObject and has only the single scope,
                    kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioControlPropertyVariant
                        A UInt32 that identifies the specific variant of an AudioControl. This
                        allows the owning AudioObject to support controls that are of the same basic
                        class (that is, the values of  kAudioObjectPropertyClass are the same) but
                        may control a part of the object for which the standard controls do not
                        control.
}
const
	kAudioControlPropertyVariant = FourCharCode('cvar');

{!
    @enum           AudioClockSourceControl Properties
    @abstract       AudioObjectPropertySelector values that apply only to AudioClockSourceControls.
    @discussion     These properties supplement the regular AudioSelectorControl Properties.
    @constant       kAudioClockSourceControlPropertyItemKind
                        This property returns a UInt32 that identifies the kind of clock source
                        the item ID refers to. The qualifier contains the ID of the item. Note that
                        this property is a synonym for kAudioSelectorControlPropertyItemKind.
}
const
	kAudioClockSourceControlPropertyItemKind = kAudioSelectorControlPropertyItemKind;

//==================================================================================================

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
