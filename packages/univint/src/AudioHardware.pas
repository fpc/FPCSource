{==================================================================================================
     File:       CoreAudio/AudioHardware.h

     Contains:   API for communicating with audio hardware.

     Version:    Technology: Mac OS X
                 Release:    Mac OS X

     Copyright:  (c) 1985-2005 by Apple Computer, Inc., all rights reserved.

==================================================================================================}

{	 Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit AudioHardware;
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
uses MacTypes, CFRunLoop, CoreAudioTypes;
{$ALIGN POWER}

//==================================================================================================
//#pragma mark    Overview
{!
    @header AudioHardware
    The audio HAL provides an abstraction through which applications can access audio hardware. To
    do this, the HAL provides a small set of AudioObjects that provide access to the various pieces
    of the system.
    
    AudioObjects all have a set of properties that describe and manipulate their state. A property
    is accessed via an ordered triple. The first ordinate is the selector which describes the
    property. The other two ordinates are the scope and element that identify the particular part of
    the object in which to look for the selector. The AudioObjectPropertyAddress structure
    encapsulates the property address. The value of a property is an untyped block of data whose
    content depends on the specifics of the selector. Some selectors also require the use of a
    qualifier when querying. The qualifier allows for additional information to be provided to be
    used in the manipulation of the property. Changing the value of a property is always considered
    asynchronous.
    
    Applications use the routines AudioObjectHasProperty(), AudioObjectIsPropertySettable() and
    AudioObjectGetPropertyDataSize() to find useful meta-information about the property. Apps use
    AudioObjectGetPropertyData() and AudioObjectSetPropertyData() to manipulate the value of the
    property. Apps use AudioObjectAddPropertyListener() and AudioObjectRemovePropertyListener() to
    register/unregister a function that is to be called when a given property's value changes.
    
    The class of an AudioObject determines the basic functionality of the object in terms of what
    functions will operate on it as well as the set of properties that can be expected to be
    implemented by the object. The set of available classes for objects is limited to those defined
    here. There are no other classes. The set of classes is arranged in a hierarchy such that one
    class inherits the properties/routines of it's super class.
    
    The base class for all AudioObjects is the class AudioObject. As such, each AudioObject will 
    provide basic properties such as it's class, it's human readable name, and the other
    AudioObjects it contains. Other important classes include AudioSystemObject, AudioDevice, and
    AudioStream.
    
    The AudioObjects in the HAL are arranged in a containment hierarchy. The root of the hierarchy
    is the one and only instance of the AudioSystemObject class. The properties of the
    AudioSystemObject describe the process global settings such as the various default devices and
    the notification run loop. The AudioSystemObject also contains all the AudioDevices that are
    available.
    
    Instances of the AudioDevice class encapsulate individual audio devices. An AudioDevice serves
    as the basic unit of IO. It provides a single IO cycle, a timing source based on it, and all the
    buffers synchronized to it. The IO cycle presents all the synchronized buffers to the client in
    the same call out along with time stamps that specify the current time, when the input data was
    acquired and when the output data will be presented.
    
    AudioDevices contain instances of the AudioStream class. An AudioStream represents a single
    buffer of data for transferring across the user/kernel boundary. As such, AudioStreams are the
    gatekeepers of format information. Each has it's own format and list of available formats.
    AudioStreams can provide data in any format, including encoded formats and non-audio formats. If
    the format is a linear PCM format, the data will always be presented as 32 bit, native endian
    floating point. All conversions to and from the true physical format of the hardware is handled
    by the device's driver.
    
    Both AudioDevices and AudioStreams can contain instances of the AudioControl class or it's many
    subclasses. An AudioControl provides properties that describe/manipulate a particular aspect of
    the object such as gain, mute, data source selection, etc. Many common controls are also
    also available as properties on the AudioDevice or AudioStream.
}



//==================================================================================================
//#pragma mark    Error Constants

{!
    @enum           Error Constants
    @abstract       The error constants unique to the HAL.
    @discussion     These are the error constants that are unique to the HAL. Note that the HAL's
                    functions can and will return other codes that are not listed here. While these
                    constants give a general idea of what might have gone wrong during the execution
                    of an API call, if an API call returns anything other than kAudioHardwareNoError
                    it is to be viewed as the same failure regardless of what constant is actually
                    returned.
    @constant       kAudioHardwareNoError
                        The function call completed successfully.
    @constant       kAudioHardwareNotRunningError
                        The function call requires that the hardware be running but it isn't.
    @constant       kAudioHardwareUnspecifiedError
                        The function call failed while doing something that doesn't provide any
                        error messages.
    @constant       kAudioHardwareUnknownPropertyError
                        The AudioObject doesn't know about the property at the given address.
    @constant       kAudioHardwareBadPropertySizeError
                        An improperly sized buffer was provided when accessing the data of a
                        property.
    @constant       kAudioHardwareIllegalOperationError
                        The requested operation couldn't be completed.
    @constant       kAudioHardwareBadObjectError
                        The AudioObjectID passed to the function doesn't map to a valid AudioObject.
    @constant       kAudioHardwareBadDeviceError
                        The AudioDeviceID passed to the function doesn't map to a valid AudioDevice.
    @constant       kAudioHardwareBadStreamError
                        The AudioStreamID passed to the function doesn't map to a valid AudioStream.
    @constant       kAudioHardwareUnsupportedOperationError
                        The AudioObject doesn't support the requested operation.
    @constant       kAudioDeviceUnsupportedFormatError
                        The AudioStream doesn't support the requested format.
    @constant       kAudioDevicePermissionsError
                        The requested operation can't be completed because the process doesn't have
                        permission.
}
const
	kAudioHardwareNoError = 0;
	kAudioHardwareNotRunningError = $73746F70 (* 'stop' *);
	kAudioHardwareUnspecifiedError = $77686174 (* 'what' *);
	kAudioHardwareUnknownPropertyError = $77686F3F (* 'who?' *);
	kAudioHardwareBadPropertySizeError = $2173697A (* '!siz' *);
	kAudioHardwareIllegalOperationError = $6E6F7065 (* 'nope' *);
	kAudioHardwareBadObjectError = $216F626A (* '!obj' *);
	kAudioHardwareBadDeviceError = $21646576 (* '!dev' *);
	kAudioHardwareBadStreamError = $21737472 (* '!str' *);
	kAudioHardwareUnsupportedOperationError = $756E6F70 (* 'unop' *);
	kAudioDeviceUnsupportedFormatError = $21646174 (* '!dat' *);
	kAudioDevicePermissionsError = $21686F67 (* '!hog' *);

//==================================================================================================
//#pragma mark    Property Support Types

{!
    @typedef        AudioObjectPropertySelector
    @abstract       An AudioObjectPropertySelector is a four char code that identifies, along with
                    the AudioObjectPropertyScope and AudioObjectPropertyElement, a specific piece of
                    information about an AudioObject.
    @discussion     The property selector specifies the general classification of the property such
                    as volume, stream format, latency, etc. Note that each class has a different set
                    of selectors. A subclass inherits it's super class's set of selectors, although
                    it may not implement them all.
}
type
	AudioObjectPropertySelector = UInt32;

{!
    @typedef        AudioObjectPropertyScope
    @abstract       An AudioObjectPropertyScope is a four char code that identifies, along with the
                    AudioObjectPropertySelector and AudioObjectPropertyElement, a specific piece of
                    information about an AudioObject.
    @discussion     The scope specifies the section of the object in which to look for the property,
                    such as input, output, global, etc. Note that each class has a different set of
                    scopes. A subclass inherits it's superclass's set of scopes.
}
type
	AudioObjectPropertyScope = UInt32;

{!
    @typedef        AudioObjectPropertyElement
    @abstract       An AudioObjectPropertyElement is an integer that identifies, along with the
                    AudioObjectPropertySelector and AudioObjectPropertyScope, a specific piece of
                    information about an AudioObject.
    @discussion     The element selects one of possibly many items in the section of the object in
                    which to look for the property. Elements are number sequentially where 0
                    represents the master element. Elements are particular to an instance of a
                    class, meaning that two instances can have different numbers of elements in the
                    same scope. There is no inheritance of elements.
}
type
	AudioObjectPropertyElement = UInt32;

{!
    @struct         AudioObjectPropertyAddress
    @abstract       An AudioObjectPropertyAddress collects the three parts that identify a specific
                    property together in a struct for easy transmission.
    @field          mSelector
                        The AudioObjectPropertySelector for the property.
    @field          mScope
                        The AudioObjectPropertyScope for the property.
    @field          mElement
                        The AudioObjectPropertyElement for the property.
}
type
	AudioObjectPropertyAddress = record
		mSelector: AudioObjectPropertySelector;
		mScope: AudioObjectPropertyScope;
		mElement: AudioObjectPropertyElement;
	end;
	AudioObjectPropertyAddressPtr = ^AudioObjectPropertyAddress;

//==================================================================================================
//#pragma mark    Property Support Constants

{!
    @enum           Property Wildcard Constants
    @abstract       Constants that are used as wildcards in an AudioObjectPropertyAddress.
    @discussion     Wildcards match any and all values for there associated type. They are
                    especially useful for registering listener procs to receive notifications and
                    for querying an AudioObject's list of AudioControls.
    @constant       kAudioObjectPropertySelectorWildcard
                        The wildcard value for AudioObjectPropertySelectors.
    @constant       kAudioObjectPropertyScopeWildcard
                        The wildcard value for AudioObjectPropertyScopes.
    @constant       kAudioObjectPropertyElementWildcard
                        The wildcard value for AudioObjectPropertyElements.
	@constant		kAudioPropertyWildcardPropertyID
						A synonym for kAudioObjectPropertySelectorWildcard.
	@constant		kAudioPropertyWildcardSection
						The wildcard value for the isInput argument of AudioDeviceGetPropertyInfo(),
						AudioDeviceGetProperty(), and AudioDeviceSetProperty().
	@constant		kAudioPropertyWildcardChannel
						A synonym for kAudioObjectPropertyElementWildcard.
}
const
	kAudioObjectPropertySelectorWildcard = $2A2A2A2A (* '****' *);
	kAudioObjectPropertyScopeWildcard = $2A2A2A2A (* '****' *);
	kAudioObjectPropertyElementWildcard = $FFFFFFFF;
	kAudioPropertyWildcardPropertyID = kAudioObjectPropertySelectorWildcard;
	kAudioPropertyWildcardSection = $FF;
	kAudioPropertyWildcardChannel = kAudioObjectPropertyElementWildcard;

//==================================================================================================
//#pragma mark    AudioObject Types

{!
    @typedef        AudioClassID
    @abstract       AudioClassIDs are used to identify the class of an AudioObject.
}
type
	AudioClassID = UInt32;

{!
    @typedef        AudioObjectID
    @abstract       AudioObject is the base class for all the objects in the HAL.
    @discussion     AudioObjects have properties and can contain other AudioObjects.
}
type
	AudioObjectID = UInt32;

{!
    @typedef        AudioObjectPropertyListenerProc
    @abstract       Clients register an AudioObjectPropertyListenerProc with an AudioObject in order
                    to receive notifications when the properties of the object change.
    @discussion     Listeners will be called when possibly many properties have changed.
                    Consequently, the implementation of a listener must go through the array of
                    addresses to see what exactly has changed. Note that the array of addresses will
                    always have at least one address in it for which the listener is signed up to
                    receive notifications about but may contain addresses for properties for which
                    the listener is not signed up to receive notifications.
    @param          inObjectID
                        The AudioObject whose properties have changed.
    @param          inNumberAddresses
                        The number of elements in the inAddresses array.
    @param          inAddresses
                        An array of AudioObjectPropertyAddresses indicating which properties
                        changed.
    @param          inClientData
                        A pointer to client data established when the listener proc was registered
                        with the AudioObject.
    @result         The return value is currently unused and should always be 0.
}
type
	AudioObjectPropertyListenerProc = function( inObjectID: AudioObjectID; inNumberAddresses: UInt32; {const} inAddresses: {variable-size-array} AudioObjectPropertyAddressPtr; inClientData: UnivPtr ): OSStatus;

//==================================================================================================
//#pragma mark    AudioObject Constants

{!
    @enum           AudioObject Class Constants
    @abstract       Various constants related to AudioObjects.
    @constant       kAudioObjectPropertyScopeGlobal
                        The AudioObjectPropertyScope for properties that apply to the object as a
                        whole. All AudioObjects have a global scope and for some it is their only
                        scope.
    @constant       kAudioObjectPropertyElementMaster
                        The AudioObjectPropertyElement value for properties that apply to the master
                        element or to the entire scope.
    @constant       kAudioObjectClassID
                        The AudioClassID that identifies the AudioObject class.
    @constant       kAudioObjectClassIDWildcard
                        The wildcard value for AudioClassIDs.
    @constant       kAudioObjectUnknown
                        The AudioObjectID for a non-existant AudioObject.
}
const
	kAudioObjectPropertyScopeGlobal = $676C6F62 (* 'glob' *);
	kAudioObjectPropertyElementMaster = 0;
	kAudioObjectClassID = $616F626A (* 'aobj' *);
	kAudioObjectClassIDWildcard = $2A2A2A2A (* '****' *);
	kAudioObjectUnknown = 0;

//==================================================================================================
//#pragma mark    AudioObject Properties

{!
    @enum           AudioObject Property Selectors
    @abstract       AudioObjectPropertySelector values that apply to all AudioObjects.
    @constant       kAudioObjectPropertyClass
                        An AudioClassID that identifies the class of the AudioObject.
    @constant       kAudioObjectPropertyOwner
                        An AudioObjectID that identifies the the AudioObject that owns the given
                        AudioObject. Note that all AudioObjects are owned by some other AudioObject.
                        The only exception is the AudioSystemObject, for which the value of this
                        property is kAudioObjectUnknown.
    @constant       kAudioObjectPropertyCreator
                        A CFString that contains the bundle ID of the plug-in that instantiated the
                        object.
    @constant       kAudioObjectPropertyObjectName
                        A CFString that contains the human readable name of the object. The caller
                        is responsible for releasing the returned CFObject.
    @constant       kAudioObjectPropertyManufacturer
                        A CFString that contains the human readable name of the manufacturer of the
                        hardware the AudioObject is a part of. The caller is responsible for
                        releasing the returned CFObject.
    @constant       kAudioObjectPropertyElementName
                        A CFString that contains a human readable name for the given element in the
                        given scope. The caller is responsible for releasing the returned CFObject.
    @constant       kAudioObjectPropertyElementCategoryName
                        A CFString that contains a human readable name for the category of the given
                        element in the given scope. The caller is responsible for releasing the
                        returned CFObject.
    @constant       kAudioObjectPropertyElementNumberName
                        A CFString that contains a human readable name for the number of the given
                        element in the given scope. The caller is responsible for releasing the
                        returned CFObject.
    @constant       kAudioObjectPropertyOwnedObjects
                        An array of AudioObjectIDs that represent all the AudioObjects owned by the
                        given object. The qualifier is an array of AudioClassIDs. If it is
                        non-empty, the returned array of AudioObjectIDs will only refer to objects
                        whose class is in the qualifier array or whose is a subclass of one in the
                        qualifier array.
    @constant       kAudioObjectPropertyListenerAdded
                        An AudioObjectPropertyAddress indicating the address to which a new listener
                        was added. Note that this property is not for applications to use. Rather,
                        this property is for the HAL shell to notify AudioObjects implemented by an
                        AudioPlugIn when a listener is added.
    @constant       kAudioObjectPropertyListenerRemoved
                        An AudioObjectPropertyAddress indicating the address to which a listener was
                        removed. Note that this property is not for applications to use. Rather,
                        this property is for the HAL shell to notify AudioObjects implemented by an
                        AudioPlugIn when a listener is removed.
}
const
	kAudioObjectPropertyClass = $636C6173 (* 'clas' *);
	kAudioObjectPropertyOwner = $73746476 (* 'stdv' *);
	kAudioObjectPropertyCreator = $6F706C67 (* 'oplg' *);
	kAudioObjectPropertyName = $6C6E616D (* 'lnam' *);
	kAudioObjectPropertyManufacturer = $6C6D616B (* 'lmak' *);
	kAudioObjectPropertyElementName = $6C63686E (* 'lchn' *);
	kAudioObjectPropertyElementCategoryName = $6C63636E (* 'lccn' *);
	kAudioObjectPropertyElementNumberName = $6C636E6E (* 'lcnn' *);
	kAudioObjectPropertyOwnedObjects = $6F776E64 (* 'ownd' *);
	kAudioObjectPropertyListenerAdded = $6C697361 (* 'lisa' *);
	kAudioObjectPropertyListenerRemoved = $6C697372 (* 'lisr' *);

//==================================================================================================
//#pragma mark    AudioObject Functions

{!
    @functiongroup  AudioObject
}

{!
    @function       AudioObjectShow
    @abstract       Prints to standard out a textural description of the AudioObject.
    @param          inObjectID
                        The AudioObject to show.
}
procedure AudioObjectShow( inObjectID: AudioObjectID ); external name '_AudioObjectShow';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectHasProperty
    @abstract       Queries an AudioObject about whether or not it has the given property.
    @param          inObjectID
                        The AudioObject to query.
    @param          inAddress
                        An AudioObjectPropertyAddress indicating which property is being queried.
    @result         A Boolean indicating whether or not the AudioObject has the given property.
}
function AudioObjectHasProperty( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress ): Boolean; external name '_AudioObjectHasProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectIsPropertySettable
    @abstract       Queries an AudioObject about whether or not the given property can be set using
                    AudioObjectSetPropertyData.
    @param          inObjectID
                        The AudioObject to query.
    @param          inAddress
                        An AudioObjectPropertyAddress indicating which property is being queried.
    @param          outIsSettable
                        A Boolean indicating whether or not the property can be set.
    @result         An OSStatus indicating success or failure.
}
function AudioObjectIsPropertySettable( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress; var outIsSettable: Boolean ): OSStatus; external name '_AudioObjectIsPropertySettable';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectGetPropertyDataSize
    @abstract       Queries an AudioObject to find the size of the data for the given property.
    @param          inObjectID
                        The AudioObject to query.
    @param          inAddress
                        An AudioObjectPropertyAddress indicating which property is being queried.
    @param          inQualifierDataSize
                        A UInt32 indicating the size of the buffer pointed to by inQualifierData.
                        Note that not all properties require qualification, in which case this
                        value will be 0.
    @param          inQualifierData,
                        A buffer of data to be used in determining the data of the property being
                        queried. Note that not all properties require qualification, in which case
                        this value will be NULL.
    @param          outDataSize
                        A UInt32 indicating how many bytes the data for the given property occupies.
    @result         An OSStatus indicating success or failure.
}
function AudioObjectGetPropertyDataSize( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress; inQualifierDataSize: UInt32; inQualifierData: {const} UnivPtr; var outDataSize: UInt32 ): OSStatus; external name '_AudioObjectGetPropertyDataSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectGetPropertyData
    @abstract       Queries an AudioObject to get the data of the given property and places it in
                    the provided buffer.
    @param          inObjectID
                        The AudioObject to query.
    @param          inAddress
                        An AudioObjectPropertyAddress indicating which property is being queried.
    @param          inQualifierDataSize
                        A UInt32 indicating the size of the buffer pointed to by inQualifierData.
                        Note that not all properties require qualification, in which case this
                        value will be 0.
    @param          inQualifierData,
                        A buffer of data to be used in determining the data of the property being
                        queried. Note that not all properties require qualification, in which case
                        this value will be NULL.
    @param          ioDataSize
                        A UInt32 which on entry indicates the size of the buffer pointed to by
                        outData and on exit indicates how much of the buffer was used.
    @param          outData
                        The buffer into which the AudioObject will put the data for the given
                        property.
    @result         An OSStatus indicating success or failure.
}
function AudioObjectGetPropertyData( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress; inQualifierDataSize: UInt32; inQualifierData: {const} UnivPtr; var ioDataSize: UInt32; outData: UnivPtr ): OSStatus; external name '_AudioObjectGetPropertyData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectSetPropertyData
    @abstract       Tells an AudioObject to change the value of the given property using the
                    provided data.
    @discussion     Note that the value of the property should not be considered changed until the
                    HAL has called the listeners as many properties values are changed
                    asynchronously.
    @param          inObjectID
                        The AudioObject to change.
    @param          inAddress
                        An AudioObjectPropertyAddress indicating which property is being changed.
    @param          inQualifierDataSize
                        A UInt32 indicating the size of the buffer pointed to by inQualifierData.
                        Note that not all properties require qualification, in which case this
                        value will be 0.
    @param          inQualifierData,
                        A buffer of data to be used in determining the data of the property being
                        queried. Note that not all properties require qualification, in which case
                        this value will be NULL.
    @param          inDataSize
                        A UInt32 indicating the size of the buffer pointed to by inData.
    @param          inData
                        The buffer containing the data to be used to change the property's value.
    @result         An OSStatus indicating success or failure.
}
function AudioObjectSetPropertyData( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress; inQualifierDataSize: UInt32; inQualifierData: {const} UnivPtr; inDataSize: UInt32; inData: {const} UnivPtr ): OSStatus; external name '_AudioObjectSetPropertyData';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectAddPropertyListener
    @abstract       Registers the given AudioObjectPropertyListenerProc to receive notifications
                    when the given properties change.
    @param          inObjectID
                        The AudioObject to register the listener with.
    @param          inAddress
                        The AudioObjectPropertyAddresses indicating which property the listener
                        should be notified about.
    @param          inListener
                        The AudioObjectPropertyListenerProc to call.
    @param          inClientData
                        A pointer to client data that is passed to the listener when it is called.
    @result         An OSStatus indicating success or failure.
}
function AudioObjectAddPropertyListener( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress; inListener: AudioObjectPropertyListenerProc; inClientData: UnivPtr ): OSStatus; external name '_AudioObjectAddPropertyListener';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{!
    @function       AudioObjectRemovePropertyListener
    @abstract       Unregisters the given AudioObjectPropertyListenerProc from receiving
                    notifications when the given properties change.
    @param          inObjectID
                        The AudioObject to unregister the listener from.
    @param          inNumberAddresses
                        The number of elements in the inAddresses array.
    @param          inAddresses
                        The AudioObjectPropertyAddress indicating which property the listener should
                        be removed from.
    @param          inListener
                        The AudioObjectPropertyListenerProc being removed.
    @param          inClientData
                        A pointer to client data that is passed to the listener when it is called.
    @result         An OSStatus indicating success or failure.
}
function AudioObjectRemovePropertyListener( inObjectID: AudioObjectID; const (*var*) inAddress: AudioObjectPropertyAddress; inListener: AudioObjectPropertyListenerProc; inClientData: UnivPtr ): OSStatus; external name '_AudioObjectRemovePropertyListener';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

//==================================================================================================
//#pragma mark    AudioControl Constants

{!
    @enum           AudioControl Base Class IDs
    @abstract       The AudioClassIDs that identify the various AudioControl base classes.
    @constant       kAudioControlClassID
                        The AudioClassID that identifies the AudioControl class.
    @constant       kAudioLevelControlClassID
                        The AudioClassID that identifies the AudioLevelControl class which is a
                        subclass of AudioControl. AudioLevelControls manipulate gain/attenuation
                        stages in the hardware.
    @constant       kAudioBooleanControlClassID
                        The AudioClassID that identifies the AudioBooleanControl class which is a
                        subclass of AudioControl. AudioBooleanControls manipulate on/off switches
                        in the hardware.
    @constant       kAudioSelectorControlClassID
                        The AudioClassID that identifies the AudioSelectorControl class which is a
                        subclass of AudioControl. AudioSelectorControls manipulate controls that
                        have multiple, but discreet values.
    @constant       kAudioStereoPanControlClassID
                        The AudioClassID that identifies the AudioStereoPanControl class which is
                        a subclass of AudioControl. AudioStereoPanControls manipulate the pot for
                        panning a mono signal between a left/right pair of outputs.
}
const
	kAudioControlClassID = $6163746C (* 'actl' *);
	kAudioLevelControlClassID = $6C65766C (* 'levl' *);
	kAudioBooleanControlClassID = $746F676C (* 'togl' *);
	kAudioSelectorControlClassID = $736C6374 (* 'slct' *);
	kAudioStereoPanControlClassID = $7370616E (* 'span' *);

{!
    @enum           AudioLevelControl Subclass IDs
    @abstract       The four char codes that identify the various standard subclasses of
                    AudioLevelControl.
    @constant       kAudioVolumeControlClassID
                        An AudioLevelControl for a general gain/attenuation stage.
    @constant       kAudioLFEVolumeControlClassID
                        An AudioLevelControl for an LFE channel that results from bass management
                        such as the iSub. Note that LFE channels that are represented as normal
                        audio channels (in other words, real data is being fed them in an IOProc)
                        will use kAudioVolumeControlClassID to manipulate the level.
    @constant       kAudioBootChimeVolumeControlClassID
                        An AudioLevelControl for the boot chime of the CPU.
}
const
	kAudioVolumeControlClassID = $766C6D65 (* 'vlme' *);
	kAudioLFEVolumeControlClassID = $73756276 (* 'subv' *);
	kAudioBootChimeVolumeControlClassID = $7072616D (* 'pram' *);

{!
    @enum           AudioBooleanControl Subclass IDs
    @abstract       The four char codes that identify the various standard subclasses of
                    AudioBooleanControl.
    @constant       kAudioMuteControlClassID
                        An AudioBooleanControl where a true value means that mute is enabled making
                        that element inaudible.
    @constant       kAudioSoloControlClassID
                        An AudioBooleanControl where a true value means that solo is enabled making
                        just that element audible and the other elements inaudible.
    @constant       kAudioJackControlClassID
                        An AudioBooleanControl where a true value means something is plugged into
                        that element.
    @constant       kAudioLFEMuteControlClassID
                        An AudioBooleanControl where true means that mute is enabled make that LFE
                        element inaudible. This control is for LFE channels that result from bass
                        management such as the iSub. Note that LFE channels that are represented as
                        normal audio channels (in other words, real data is being fed them in an
                        IOProc) will use kAudioVolumeControlClassID to manipulate mute.
    @constant       kAudioISubOwnerClassID
                        An AudioBooleanControl where true means that the AudioDevice that
                        ultimately owns the control also owns any iSub attached to the CPU.
}
const
	kAudioMuteControlClassID = $6D757465 (* 'mute' *);
	kAudioSoloControlClassID = $736F6C6F (* 'solo' *);
	kAudioJackControlClassID = $6A61636B (* 'jack' *);
	kAudioLFEMuteControlClassID = $7375626D (* 'subm' *);
	kAudioISubOwnerControlClassID = $61746368 (* 'atch' *);

{!
    @enum           AudioSelectorControl Subclass IDs
    @abstract       The four char codes that identify the various standard subclasses of
                    AudioSelectorControl.
    @constant       kAudioDataSourceControlClassID
                        An AudioSelectorControl that identifies where the data for the element is
                        coming from.
    @constant       kAudioDataDestinationControlClassID
                        An AudioSelectorControl that identifies where the data for the element is
                        going.
    @constant       kAudioClockSourceControlClassID
                        An AudioSelectorControl that identifies where the timing info for the object
                        is coming from.
    @constant       kAudioLineLevelControlClassID
                        An AudioSelectorControl that identifies the nominal line level for the
                        element. Note that this is not a gain stage but rather indicating the
                        voltage standard (if any) used for the element, such as +4dBu, -10dBV,
                        instrument, etc.
}
const
	kAudioDataSourceControlClassID = $64737263 (* 'dsrc' *);
	kAudioDataDestinationControlClassID = $64657374 (* 'dest' *);
	kAudioClockSourceControlClassID = $636C636B (* 'clck' *);
	kAudioLineLevelControlClassID = $6E6C766C (* 'nlvl' *);

//==================================================================================================
//#pragma mark    AudioControl Properties

{!
    @enum           AudioControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioControls.
    @discussion     AudioControl is a subclass of AudioObject and has only the single scope,
                    kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioControlPropertyScope
                        The AudioObjectPropertyScope in the owning AudioObject that contains the
                        AudioControl.
    @constant       kAudioControlPropertyElement
                        The AudioObjectPropertyElement in the owning AudioObject that contains the
                        AudioControl.
    @constant       kAudioControlPropertyVariant
                        A UInt32 that identifies the specific variant of an AudioControl. This
                        allows the owning AudioObject to support controls that are of the same basic
                        class (that is, the values of  kAudioObjectPropertyClass are the same) but
                        may control a part of the object for which the standard controls do not
                        control.
}
const
	kAudioControlPropertyScope = $63736370 (* 'cscp' *);
	kAudioControlPropertyElement = $63656C6D (* 'celm' *);
	kAudioControlPropertyVariant = $63766172 (* 'cvar' *);

{!
    @enum           AudioLevelControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioLevelControls.
    @discussion     AudioLevelControl is a subclass of AudioControl and has only the single scope,
                    kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioLevelControlPropertyScalarValue
                        A Float32 that represents the value of the boot chime volume control. The
                        range is between 0.0 and 1.0 (inclusive).
    @constant       kAudioLevelControlPropertyDecibelValue
                        A Float32 that represents the value of the boot chime volume control in dB.
    @constant       kAudioLevelControlPropertyDecibelRange
                        An AudioValueRange that contains the minimum and maximum dB values the
                        boot chime control can have.
    @constant       kAudioLevelControlPropertyConvertScalarToDecibels
                        A Float32 that on input contains a scalar volume value for the boot chime
                        and on exit contains the equivalent dB value.
    @constant       kAudioLevelControlPropertyConvertDecibelsToScalar
                        A Float32 that on input contains a dB volume value for the boot chime and on
                        exit contains the equivalent scalar value.
}
const
	kAudioLevelControlPropertyScalarValue = $6C637376 (* 'lcsv' *);
	kAudioLevelControlPropertyDecibelValue = $6C636476 (* 'lcdv' *);
	kAudioLevelControlPropertyDecibelRange = $6C636472 (* 'lcdr' *);
	kAudioLevelControlPropertyConvertScalarToDecibels = $6C637364 (* 'lcsd' *);
	kAudioLevelControlPropertyConvertDecibelsToScalar = $6C636473 (* 'lcds' *);

{!
    @enum           AudioBooleanControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioBooleanControls.
    @discussion     AudioBooleanControl is a subclass of AudioControl and has only the single scope,
                    kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioBooleanControlPropertyValue
                        A UInt32 where 0 means false and 1 means true.
}
const
	kAudioBooleanControlPropertyValue = $6263766C (* 'bcvl' *);

{!
    @enum           AudioSelectorControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioSelectorControls.
    @discussion     AudioSelectorControl is a subclass of AudioControl and has only the single
                    scope, kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioSelectorControlPropertyCurrentItem
                        A UInt32 that is the ID of the item currently selected.
    @constant       kAudioSelectorControlPropertyAvailableItems
                        An array of UInt32s that represent the IDs of all the items available.
    @constant       kAudioSelectorControlPropertyItemName
                        This property translates the given item ID into a human readable name. The
                        qualifier contains the ID of the item to be translated and name is returned
                        as a CFString as the property data. The caller is responsible for releasing
                        the returned CFObject.
}
const
	kAudioSelectorControlPropertyCurrentItem = $73636369 (* 'scci' *);
	kAudioSelectorControlPropertyAvailableItems = $73636169 (* 'scai' *);
	kAudioSelectorControlPropertyItemName = $7363696E (* 'scin' *);

{!
    @enum           AudioClockSourceControl Properties
    @abstract       AudioObjectPropertySelector values that apply only to AudioClockSourceControls.
    @discussion     These properties supplement the regular AudioSelectorControl Properties.
    @constant       kAudioClockSourceControlPropertyItemKind
                        This property returns a UInt32 that identifies the kind of clock source
                        the item ID refers to. The qualifier contains the ID of the item. Values for
                        this property are defined in <IOAudio/audio/IOAudioTypes.h>.
}
const
	kAudioClockSourceControlPropertyItemKind = $636C6B6B (* 'clkk' *);

{!
    @enum           AudioStereoPanControl Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioStereoPanControls.
    @discussion     AudioStereoPanControl is a subclass of AudioControl and has only the single
                    scope, kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioStereoPanControlPropertyValue
                        A Float32 where 0.0 is full left, 1.0 is full right, and 0.5 is center.
    @constant       kAudioStereoPanControlPropertyPanningChannels
                        An array of two UInt32s that indicate which elements of the owning object
                        the signal is being panned between.
}
const
	kAudioStereoPanControlPropertyValue = $73706376 (* 'spcv' *);
	kAudioStereoPanControlPropertyPanningChannels = $73706363 (* 'spcc' *);

//==================================================================================================
//#pragma mark    AudioSystemObject Types

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
//#pragma mark    AudioSystemObject Constants

{!
    @enum           AudioSystemObject Class Constants
    @abstract       Various constants related to the AudioSystemObject.
    @constant       kAudioSystemObjectClassID
                        The AudioClassID that identifies the AudioSystemObject class.
    @constant       kAudioObjectSystemObject
                        The AudioObjectID that always refers to the one and only instance of the
                        AudioSystemObject.
}
const
	kAudioSystemObjectClassID = $61737973 (* 'asys' *);
	kAudioObjectSystemObject = 1;

//==================================================================================================
//#pragma mark    AudioSystemObject Properties

{!
    @enum           AudioSystemObject Properties
    @abstract       AudioObjectPropertySelector values that apply to the AudioSystemObject.
    @discussion     The AudioSystemObject has one scope, kAudioObjectPropertyScopeGlobal, and only a
                    master element.
    @constant       kAudioHardwarePropertyProcessIsMaster
                        A UInt32 where 1 means that the current process contains the master instance
                        of the HAL. The master instance of the HAL is the only instance in which
                        plug-ins should save/restore their devices' settings.
    @constant       kAudioHardwarePropertyIsInitingOrExiting
                        A UInt32 whose value will be non-zero if the HAL is either in the midst of
                        initializing or in the midst of exiting the process.
    @constant       kAudioHardwarePropertyDevices
                        An array of the AudioDeviceIDs that represent all the devices currently
                        available to the system.
    @constant       kAudioHardwarePropertyDefaultInputDevice
                        The AudioDeviceID of the default input AudioDevice.
    @constant       kAudioHardwarePropertyDefaultOutputDevice
                        The AudioDeviceID of the default output AudioDevice.
    @constant       kAudioHardwarePropertyDefaultOutputDevice
                        The AudioDeviceID of the output AudioDevice to use for system related sound
                        from the alert sound to digital call progress.
    @constant       kAudioHardwarePropertyDeviceForUID
                        Using an AudioValueTranslation structure, this property translates the input
                        CFStringRef containing a UID into the AudioDeviceID that refers to the
                        AudioDevice with that UID. This property will return kAudioDeviceUnknown if
                        the given UID does not match any currently available AudioDevice.
    @constant       kAudioHardwarePropertySleepingIsAllowed
                        A UInt32 where 1 means that the process will allow the CPU to idle sleep
                        even if there is audio IO in progress. A 0 means that the CPU will not be
                        allowed to idle sleep. Note that this property won't affect when the CPU is
                        forced to sleep.
    @constant       kAudioHardwarePropertyUnloadingIsAllowed
                        A UInt32 where 1 means that this process wants the HAL to unload itself
                        after a period of inactivity where there are no IOProcs and no listeners
                        registered with any AudioObject.
    @constant       kAudioHardwarePropertyHogModeIsAllowed
                        A UInt32 where 1 means that this process wants the HAL to automatically take
                        hog mode and 0 means that the HAL should not automatically take hog mode on
                        behalf of the process. Processes that only ever use the default device are
                        the sort of that should set this property's value to 0.
    @constant       kAudioHardwarePropertyRunLoop
                        The CFRunLoopRef the HAL is currently attaching all of it's system
                        notification handlers to. By default, the HAL will create and manage it's
                        own thread for this job. Clients can set this property to tell the HAL to
                        use a thread of the client's choosing. The caller is responsible for
                        releasing the returned CFObject.
    @constant       kAudioHardwarePropertyPlugInForBundleID
                        Using an AudioValueTranslation structure, this property translates the input
                        CFString containing a bundle ID into the AudioObjectID of the AudioPlugIn
                        that corresponds to it. This property will return kAudioObjectUnkown if the
                        given bundle ID doesn't match any AudioPlugIns.
}
const
	kAudioHardwarePropertyProcessIsMaster = $6D617374 (* 'mast' *);
	kAudioHardwarePropertyIsInitingOrExiting = $696E6F74 (* 'inot' *);
	kAudioHardwarePropertyDevices = $64657623 (* 'dev#' *);
	kAudioHardwarePropertyDefaultInputDevice = $64496E20 (* 'dIn ' *);
	kAudioHardwarePropertyDefaultOutputDevice = $644F7574 (* 'dOut' *);
	kAudioHardwarePropertyDefaultSystemOutputDevice = $734F7574 (* 'sOut' *);
	kAudioHardwarePropertyDeviceForUID = $64756964 (* 'duid' *);
	kAudioHardwarePropertySleepingIsAllowed = $736C6570 (* 'slep' *);
	kAudioHardwarePropertyUnloadingIsAllowed = $756E6C64 (* 'unld' *);
	kAudioHardwarePropertyHogModeIsAllowed = $686F6772 (* 'hogr' *);
	kAudioHardwarePropertyRunLoop = $726E6C70 (* 'rnlp' *);
	kAudioHardwarePropertyPlugInForBundleID = $70696269 (* 'pibi' *);

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
}
const
	kAudioHardwarePropertyBootChimeVolumeScalar = $62627673 (* 'bbvs' *);
	kAudioHardwarePropertyBootChimeVolumeDecibels = $62627664 (* 'bbvd' *);
	kAudioHardwarePropertyBootChimeVolumeRangeDecibels = $62626423 (* 'bbd#' *);
	kAudioHardwarePropertyBootChimeVolumeScalarToDecibels = $62763264 (* 'bv2d' *);
	kAudioHardwarePropertyBootChimeVolumeDecibelsToScalar = $62643276 (* 'bd2v' *);

//==================================================================================================
//#pragma mark    AudioSystemObject Functions

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
    @param          inRunLoopSource
                        The CFRunLoopSource to add.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareAddRunLoopSource( inRunLoopSource: CFRunLoopSourceRef ): OSStatus; external name '_AudioHardwareAddRunLoopSource';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{!
    @function       AudioHardwareRemoveRunLoopSource
    @abstract       Remove the given CFRunLoopSource from the the HAL's notification CFRunLoop.
    @discussion     The CFRunLoop the HAL uses for notifications is specified by
                    kAudioHardwarePropertyRunLoop.
    @param          inRunLoopSource
                        The CFRunLoopSource to remove.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareRemoveRunLoopSource( inRunLoopSource: CFRunLoopSourceRef ): OSStatus; external name '_AudioHardwareRemoveRunLoopSource';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{!
    @function       AudioHardwareUnload
    @abstract       When this routine is called, all IO on all devices within a process will be
                    terminated and all resources capable of being released will be released. This
                    routine essentially returns the HAL to it's uninitialized state.
    @result         An OSStatus indicating success or failure.
}
function AudioHardwareUnload: OSStatus; external name '_AudioHardwareUnload';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

//==================================================================================================
//#pragma mark    AudioPlugIn Constants

{!
    @enum           AudioPlugIn Class Constants
    @abstract       Various constants related to AudioPlugIns.
    @constant       kAudioPlugInClassID
                        The AudioClassID that identifies the AudioPlugIn class.
}
const
	kAudioPlugInClassID = $61706C67 (* 'aplg' *);

//==================================================================================================
//#pragma mark    AudioPlugIn Properties

{!
    @enum           AudioPlugIn Properties
    @abstract       AudioObjectPropertySelector values that apply to AudioPlugIns.
    @discussion     AudioPlugIn is a subclass of AudioObject that represents a plug-in loaded by the
                    HAL that conforms to the API in <CoreAudio/AudioHardwarePlugIn.h>. AudioPlugIns
                    have one scope, kAudioObjectPropertyScopeGlobal, and only a master element.
    @constant       kAudioPlugInPropertyBundleID
                        A CFString that contains the bundle identifier for the AudioPlugIn. The
                        caller is responsible for releasing the returned CFObject.
    @constant       kAudioPlugInCreateAggregateDevice
                        This property is used to tell a plug-in to create a new
                        AudioAggregateDevice. It's value is only read. The qualifier data for this
                        property is a CFDictionary containing a description of the
                        AudioAggregateDevice to create. The keys for the CFDictionary are defined in
                        the AudioAggregateDevice Constants section. The value of the property that
                        gets returned is the AudioObjectID of the newly created device.
    @constant       kAudioPlugInDestroyAggregateDevice
                        This property is used to tell a plug-in to destroy an AudioAggregateDevice.
                        Like kAudioPlugInCreateAggregateDevice, this property is read only. The
                        value of the property is the AudioObjectID of the AudioAggregateDevice to
                        destroy.
}
const
	kAudioPlugInPropertyBundleID = $70696964 (* 'piid' *);
	kAudioPlugInCreateAggregateDevice = $63616767 (* 'cagg' *);
	kAudioPlugInDestroyAggregateDevice = $64616767 (* 'dagg' *);

//==================================================================================================
//#pragma mark    AudioDevice Types

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
    @struct         AudioHardwareIOProcStreamUsage
    @abstract       This structure describes which streams a given AudioDeviceIOProc will use. It is
                    used in conjunction with kAudioDevicePropertyIOProcStreamUsage.
    @field          mIOProc
                        The IOProc whose stream usage is being specified.
    @field          mNumberStreams
                        The number of streams being specified.
    @field          mStreamIsOn
                        An array of UInt32's whose length is specified by mNumberStreams. Each
                        element of the array corresponds to a stream. A value of 0 means the stream
                        is not to be enabled. Any other value means the stream is to be used.
}
type
	AudioHardwareIOProcStreamUsage = record
		mIOProc: UnivPtr;
		mNumberStreams: UInt32;
        mStreamIsOn: array[0..0] of UInt32;
	end;

{!
    @typedef        AudioDeviceIOProc
    @abstract       An AudioDeviceIOProc is called by an AudioDevice to provide input data read from
                    the device and collect output data to be written to the device for the current
                    IO cycle.
    @param          inDevice
                        The AudioDevice doing the IO.
    @param          inNow
                        An AudioTimeStamp that indicates the IO cycle started. Note that this time
                        includes any scheduling latency that may have been incurred waking the
                        thread on which IO is being done.
    @param          inInputData
                        An AudioBufferList containing the input data for the current IO cycle. For
                        streams that are disabled, the AudioBuffer's mData field will be NULL but
                        the mDataByteSize field will still say how much data would have been there
                        if it was enabled. Note that the contents of this structure should never be
                        modified.
    @param          inInputTime
                        An AudioTimeStamp that indicates the time at which the first frame in the
                        data was acquired from the hardware. If the device has no input streams, the
                        time stamp will be zeroed out.
    @param          outOutputData
                        An AudioBufferList in which the output data for the current IO cycle is to
                        be placed. On entry, each AudioBuffer's mDataByteSize field indicates the
                        maximum amount of data that can be placed in the buffer and the buffer's
                        memory has been zeroed out. For formats where the number of bytes per packet
                        can vary (as with AC-3, for example), the client has to fill out on exit
                        each mDataByteSize field in each AudioBuffer with the amount of data that
                        was put in the buffer. Otherwise, the mDataByteSize field should not be
                        changed. For streams that are disabled, the AudioBuffer's mData field will
                        be NULL but the mDataByteSize field will still say how much data would have
                        been there if it was enabled. Except as noted above, the contents of this
                        structure should not other wise be modified.
    @param          inOutputTime
                        An AudioTimeStamp that indicates the time at which the first frame in the
                        data will be passed to the hardware. If the device has no output streams,
                        the time stamp will be zeroed out.
    @param          inClientData
                        A pointer to client data established when the AudioDeviceIOProc was
                        registered with the AudioDevice.
    @result         The return value is currently unused and should always be 0.
}
type
	AudioDeviceIOProc = function( inDevice: AudioDeviceID; const (*var*) inNow: AudioTimeStamp; const (*var*) inInputData: AudioBufferList; const (*var*) inInputTime: AudioTimeStamp; var outOutputData: AudioBufferList; const (*var*) inOutputTime: AudioTimeStamp; inClientData: UnivPtr ): OSStatus;

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
//#pragma mark    AudioDevice Constants

{!
    @enum           AudioDevice Class Constants
    @abstract       Various constants related to AudioDevices.
    @constant       kAudioDevicePropertyScopeInput
                        The AudioObjectPropertyScope for properties that apply to the input signal
                        paths of the AudioDevice.
    @constant       kAudioDevicePropertyScopeOutput
                        The AudioObjectPropertyScope for properties that apply to the output signal
                        paths of the AudioDevice.
    @constant       kAudioDevicePropertyScopePlayThrough
                        The AudioObjectPropertyScope for properties that apply to the play through
                        signal paths of the AudioDevice.
    @constant       kAudioDeviceClassID
                        The AudioClassID that identifies the AudioDevice class.
    @constant       kAudioDeviceUnknown
                        The AudioObjectID for a nonexistent AudioObject.
}
const
	kAudioDevicePropertyScopeInput = $696E7074 (* 'inpt' *);
	kAudioDevicePropertyScopeOutput = $6F757470 (* 'outp' *);
	kAudioDevicePropertyScopePlayThrough = $70747275 (* 'ptru' *);
	kAudioDeviceClassID = $61646576 (* 'adev' *);
	kAudioDeviceUnknown = kAudioObjectUnknown;

{!
    @enum           StartAtTime/GetNearestStartTime Flags
    @abstract       The flags that can be passed to control the behavior of AudioDeviceStartAtTime()
                    andAudioDeviceGetNearestStartTime().
    @constant       kAudioDeviceStartTimeIsInputFlag
                        Set to indicate that the requested time refers to an input time. Clear to
                        indicate that it is an output time.
    @constant       kAudioDeviceStartTimeDontConsultDeviceFlag
                        Set to indicate that the device should not be consulted when determining the
                        start time. Clear to indicate that the device should be consulted. This flag
                        cannot be set if kAudioDeviceStartTimeDontConsultHALFlag is set.
    @constant       kAudioDeviceStartTimeDontConsultHALFlag
                        Set to indicate that the HAL should not be consulted when determining the
                        start time. Clear to indicate that the HAL should be consulted. This flag
                        cannot be set if kAudioDeviceStartTimeDontConsultDeviceFlag is set.
}
const
	kAudioDeviceStartTimeIsInputFlag = 1 shl 0;
	kAudioDeviceStartTimeDontConsultDeviceFlag = 1 shl 1;
	kAudioDeviceStartTimeDontConsultHALFlag = 1 shl 2;

//==================================================================================================
//#pragma mark    AudioDevice Properties

{!
    @enum           AudioDevice Properties
    @abstract       AudioObjectPropertySelector values that apply to AudioDevice objects.
    @discussion     AudioDevices have four scopes: kAudioDevicePropertyScopeGlobal,
                    kAudioDevicePropertyScopeInput, kAudioDevicePropertyScopeOutput, and
                    kAudioDevicePropertyScopePlayThrough. They have a master element and an element
                    for each channel in each stream numbered according to the starting channel
                    number of each stream.
    @constant       kAudioDevicePropertyPlugIn
                        An OSStatus that contains any error codes generated by loading the IOAudio
                        driver plug-in for the AudioDevice or kAudioHardwareNoError if the plug-in
                        loaded successfully. This property only exists for IOAudio-based
                        AudioDevices whose driver has specified a plug-in to load.
    @constant       kAudioDevicePropertyConfigurationApplication
                        A CFString that contains the bundle ID for an application that provides a
                        GUI for configuring the AudioDevice. By default, the value of this property
                        is the bundle ID for Audio MIDI Setup. The caller is responsible for
                        releasing the returned CFObject.
    @constant       kAudioDevicePropertyDeviceUID
                        A CFString that contains a persistent identifier for the AudioDevice. An
                        AudioDevice's UID is persistent across boots. The content of the UID string
                        is a black box and may contain information that is unique to a particular
                        instance of an AudioDevice's hardware or unique to the CPU. Therefore they
                        are not suitable for passing between CPUs or for identifying similar models
                        of hardware.
    @constant       kAudioDevicePropertyModelUID
                        A CFString that contains a persistent identifier for the model of an
                        AudioDevice. The identifier is unique such that the identifier from two
                        AudioDevices are equal if and only if the two AudioDevices are the exact
                        same model from the same manufacturer. Further, the identifier has to be the
                        same no matter on what machine the AudioDevice appears.
    @constant       kAudioDevicePropertyTransportType
                        A UInt32 whose value indicates how the AudioDevice is connected to the CPU.
                        Constants for some of the values for this property can be found in
                        <IOKit/audio/IOAudioTypes.h>.
    @constant       kAudioDevicePropertyRelatedDevices
                        An array of AudioDeviceIDs for devices related to the AudioDevice. For
                        IOAudio-based devices, a AudioDevices are related if they share the same
                        IOAudioDevice object.
    @constant       kAudioDevicePropertyClockDomain
                        A UInt32 whose value indicates the clock domain to which this AudioDevice
                        belongs. AudioDevices that have the same value for this property are able to
                        be synchronized in hardware. However, a value of 0 indicates that the clock
                        domain for the device is unspecified and should be assumed to be separate
                        from every other device's clock domain, even if they have the value of 0 as
                        their clock domain as well.
    @constant       kAudioDevicePropertyDeviceIsAlive
                        A UInt32 where a value of 1 means the device is ready and available and 0
                        means the device is usable and will most likely go away shortly.
    @constant       kAudioDevicePropertyDeviceHasChanged
                        The type of this property is a UInt32, but it's value has no meaning. This
                        property exists so that clients can listen to it and be told when the
                        configuration of the AudioDevice has changed in ways that cannot otherwise
                        be conveyed through other notifications. In response to this notification,
                        clients should re-evaluate everything they need to know about the device,
                        particularly the layout and values of the controls.
    @constant       kAudioDevicePropertyDeviceIsRunning
                        A UInt32 where a value of 0 means the AudioDevice is not performing IO and
                        a value of 1 means that it is. Note that the device can be running even if
                        there are no active IOProcs such as by calling AudioDeviceStart() and
                        passing a NULL IOProc. Note that the notification for this property is
                        usually sent from the AudioDevice's IO thread.
    @constant       kAudioDevicePropertyDeviceIsRunningSomewhere
                        A UInt32 where 1 means that the AudioDevice is running in at least one
                        process on the system and 0 means that it isn't running at all.
    @constant       kAudioDevicePropertyDeviceCanBeDefaultDevice
                        A UInt32 where 1 means that the AudioDevice is a possible selection for
                        kAudioHardwarePropertyDefaultInputDevice or
                        kAudioHardwarePropertyDefaultOutputDevice depending on the scope.
    @constant       kAudioDevicePropertyDeviceCanBeDefaultSystemDevice
                        A UInt32 where 1 means that the AudioDevice is a possible selection for
                        kAudioHardwarePropertyDefaultSystemOutputDevice.
    @constant       kAudioDeviceProcessorOverload
                        A UInt32 where the value has no meaning. This property exists so that
                        clients can be notified when the AudioDevice detects that an IO cycle has
                        run past it's deadline. Note that the notification for this property is
                        usually sent from the AudioDevice's IO thread.
    @constant       kAudioDevicePropertyHogMode
                        A pid_t indicating the process that currently owns exclusive access to the
                        AudioDevice or a value of -1 indicating that the device is currently
                        available to all processes. If the AudioDevice is in a non-mixable mode,
                        the HAL will automatically take hog mode on behalf of the first process to
                        start an IOProc.
    @constant       kAudioDevicePropertyLatency
                        A UInt32 containing the number of frames of latency in the AudioDevice. Note
                        that input and output latency may differ. Further, the AudioDevice's
                        AudioStreams may have additional latency so they should be queried as well.
                        If both the device and the stream say they have latency, then the total
                        latency for the stream is the device latency summed with the stream latency.
    @constant       kAudioDevicePropertyBufferFrameSize
                        A UInt32 whose value indicates the number of frames in the IO buffers.
    @constant       kAudioDevicePropertyBufferFrameSizeRange
                        An AudioValueRange indicating the minimum and maximum values, inclusive, for
                        kAudioDevicePropertyBufferFrameSize.
    @constant       kAudioDevicePropertyUsesVariableBufferFrameSizes
                        A UInt32 that, if implemented by a device, indicates that the sizes of the
                        buffers passed to an IOProc will vary by a small amount. The value of this
                        property will indicate the largest buffer that will be passed and
                        kAudioDevicePropertyBufferFrameSize will indicate the smallest buffer that
                        will get passed to the IOProc. The usage of this property is narrowed to
                        only allow for devices whose buffer sizes vary by small amounts greater than
                        kAudioDevicePropertyBufferFrameSize. It is not intended to be a license for
                        devices to be able to send buffers however they please. Rather, it is
                        intended to allow for hardware whose natural rhythms lead to this necessity.
    @constant       kAudioDevicePropertyStreams
                        An array of AudioStreamIDs that represent the AudioStreams of the
                        AudioDevice. Note that if a notification is received for this property, any
                        cached AudioStreamIDs for the device become invalid and need to be
                        re-fetched.
    @constant       kAudioDevicePropertySafetyOffset
                        A UInt32 whose value indicates the number for frames in ahead (for output)
                        or behind (for input the current hardware position that is safe to do IO.
    @constant       kAudioDevicePropertyIOCycleUsage
                        A Float32 whose range is from 0 to 1. This value indicates how much of the
                        client portion of the IO cycle the process will use. The client portion of
                        the IO cycle is the portion of the cycle in which the device calls the
                        IOProcs so this property does not the apply to the duration of the entire
                        cycle.
    @constant       kAudioDevicePropertyStreamConfiguration
                        This property returns the stream configuration of the device in an
                        AudioBufferList (with the buffer pointers set to NULL) which describes the
                        list of streams and the number of channels in each stream. This corresponds
                        to what will be passed into the IOProc.
    @constant       kAudioDevicePropertyIOProcStreamUsage
                        An AudioHardwareIOProcStreamUsage structure which details the stream usage
                        of a given IO proc. If a stream is marked as not being used, the given
                        IOProc will see a corresponding NULL buffer pointer in the AudioBufferList
                        passed to it's IO proc. Note that the number of streams detailed in the
                        AudioHardwareIOProcStreamUsage must include all the streams of that
                        direction on the device. Also, when getting the value of the property, one
                        must fill out the mIOProc field of the AudioHardwareIOProcStreamUsage with
                        the address of the of the IOProc whose stream usage is to be retrieved.
    @constant       kAudioDevicePropertyPreferredChannelsForStereo
                        An array of two UInt32s, the first for the left channel, the second for the
                        right channel, that indicate the channel numbers to use for stereo IO on the
                        device. The value of this property can be different for input and output and
                        there are no restrictions on the channel numbers that can be used.
    @constant       kAudioDevicePropertyPreferredChannelLayout
                        An AudioChannelLayout that indicates how each channel of the AudioDevice
                        should be used.
    @constant       kAudioDevicePropertyNominalSampleRate
                        A Float64 that indicates the current nominal sample rate of the AudioDevice.
    @constant       kAudioDevicePropertyAvailableNominalSampleRates
                        An array of AudioValueRange structs that indicates the valid ranges for the
                        nominal sample rate of the AudioDevice.
    @constant       kAudioDevicePropertyActualSampleRate
                        A Float64 that indicates the current actual sample rate of the AudioDevice
                        as measured by it's time stamps.
}
const
	kAudioDevicePropertyPlugIn = $706C7567 (* 'plug' *);
	kAudioDevicePropertyConfigurationApplication = $63617070 (* 'capp' *);
	kAudioDevicePropertyDeviceUID = $75696420 (* 'uid ' *);
	kAudioDevicePropertyModelUID = $6D756964 (* 'muid' *);
	kAudioDevicePropertyTransportType = $7472616E (* 'tran' *);
	kAudioDevicePropertyRelatedDevices = $616B696E (* 'akin' *);
	kAudioDevicePropertyClockDomain = $636C6B64 (* 'clkd' *);
	kAudioDevicePropertyDeviceIsAlive = $6C69766E (* 'livn' *);
	kAudioDevicePropertyDeviceHasChanged = $64696666 (* 'diff' *);
	kAudioDevicePropertyDeviceIsRunning = $676F696E (* 'goin' *);
	kAudioDevicePropertyDeviceIsRunningSomewhere = $676F6E65 (* 'gone' *);
	kAudioDevicePropertyDeviceCanBeDefaultDevice = $64666C74 (* 'dflt' *);
	kAudioDevicePropertyDeviceCanBeDefaultSystemDevice = $73666C74 (* 'sflt' *);
	kAudioDeviceProcessorOverload = $6F766572 (* 'over' *);
	kAudioDevicePropertyHogMode = $6F696E6B (* 'oink' *);
	kAudioDevicePropertyLatency = $6C746E63 (* 'ltnc' *);
	kAudioDevicePropertyBufferFrameSize = $6673697A (* 'fsiz' *);
	kAudioDevicePropertyBufferFrameSizeRange = $66737A23 (* 'fsz#' *);
	kAudioDevicePropertyUsesVariableBufferFrameSizes = $7666737A (* 'vfsz' *);
	kAudioDevicePropertyStreams = $73746D23 (* 'stm#' *);
	kAudioDevicePropertySafetyOffset = $73616674 (* 'saft' *);
	kAudioDevicePropertyIOCycleUsage = $6E637963 (* 'ncyc' *);
	kAudioDevicePropertyStreamConfiguration = $736C6179 (* 'slay' *);
	kAudioDevicePropertyIOProcStreamUsage = $73757365 (* 'suse' *);
	kAudioDevicePropertyPreferredChannelsForStereo = $64636832 (* 'dch2' *);
	kAudioDevicePropertyPreferredChannelLayout = $73726E64 (* 'srnd' *);
	kAudioDevicePropertyNominalSampleRate = $6E737274 (* 'nsrt' *);
	kAudioDevicePropertyAvailableNominalSampleRates = $6E737223 (* 'nsr#' *);
	kAudioDevicePropertyActualSampleRate = $61737274 (* 'asrt' *);

{!
    @enum           AudioDevice Properties Implemented via AudioControl objects
    @abstract       AudioObjectPropertySelector values for AudioDevice properties that are
                    implemented by AudioControl objects.
    @discussion     These properties are also accessible by locating the AudioControl object
                    attached to the AudioDevice and using that object to access the properties of
                    the control.
    @constant       kAudioDevicePropertyJackIsConnected
                        A UInt32 where a value of 0 means that there isn't anything plugged into the
                        jack associated withe given element and scope. This property is implemented
                        by an AudioJackControl, a subclass of AudioBooleanControl.
    @constant       kAudioDevicePropertyVolumeScalar
                        A Float32 that represents the value of the volume control. The range is
                        between 0.0 and 1.0 (inclusive). This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl.
    @constant       kAudioDevicePropertyVolumeDecibels
                        A Float32 that represents the value of the volume control in dB. This
                        property is implemented by an AudioControl object that is a subclass of
                        AudioVolumeControl.
    @constant       kAudioDevicePropertyVolumeRangeDecibels
                        An AudioValueRange that contains the minimum and maximum dB values the
                        control can have. This property is implemented by an AudioControl object
                        that is a subclass of AudioVolumeControl.
    @constant       kAudioDevicePropertyVolumeScalarToDecibels
                        A Float32 that on input contains a scalar volume value for the and on exit
                        contains the equivalent dB value. This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl.
    @constant       kAudioDevicePropertyVolumeDecibelsToScalar
                        A Float32 that on input contains a dB volume value for the and on exit
                        contains the equivalent scalar value. This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl.
    @constant       kAudioDevicePropertyStereoPan
                        A Float32 where 0.0 is full left, 1.0 is full right, and 0.5 is center. This
                        property is implemented by an AudioControl object that is a subclass of
                        AudioStereoPanControl.
    @constant       kAudioDevicePropertyStereoPanChannels
                        An array of two UInt32s that indicate which elements of the owning object
                        the signal is being panned between. This property is implemented by an
                        AudioControl object that is a subclass of AudioStereoPanControl.
    @constant       kAudioDevicePropertyMute
                        A UInt32 where a value of 1 means that mute is enabled making that element
                        inaudible. The property is implemented by an AudioControl object that is a
                        subclass of AudioMuteControl.
    @constant       kAudioDevicePropertySolo
                        A UInt32 where a value of 1 means that just that element is audible and the
                        other elements are inaudible. The property is implemented by an AudioControl
                        object that is a subclass of AudioSoloControl.
    @constant       kAudioDevicePropertyDataSource
                        A UInt32 whose value is the item ID for the currently selected data source.
                        This property is implemented by an AudioControl object that is a subclass of
                        AudioDataSourceControl.
    @constant       kAudioDevicePropertyDataSources
                        An array of UInt32s that are represent all the IDs of all the data sources
                        currently available. This property is implemented by an AudioControl object
                        that is a subclass of AudioDataSourceControl.
    @constant       kAudioDevicePropertyDataSourceNameForIDCFString
                        This property translates the given data source item ID into a human readable
                        name using an AudioValueTranslation structure. The input data is the UInt32
                        containing the item ID to translated and the output data is a CFString. The
                        caller is responsible for releasing the returned CFObject. This property is
                        implemented by an AudioControl object that is a subclass of
                        AudioDataSourceControl.
    @constant       kAudioDevicePropertyClockSource
                        A UInt32 whose value is the item ID for the currently selected clock source.
                        This property is implemented by an AudioControl object that is a subclass of
                        AudioClockControl.
    @constant       kAudioDevicePropertyClockSources
                        An array of UInt32s that are represent all the IDs of all the clock sources
                        currently available. This property is implemented by an AudioControl object
                        that is a subclass of AudioClockControl.
    @constant       kAudioDevicePropertyClockSourceNameForIDCFString
                        This property translates the given clock source item ID into a human
                        readable name using an AudioValueTranslation structure. The input data is
                        the UInt32 containing the item ID to translated and the output data is a
                        CFString. The caller is responsible for releasing the returned CFObject.
                        This property is implemented by an AudioControl object that is a subclass of
                        AudioClockControl.
    @constant       kAudioDevicePropertyClockSourceKindForID
                        This property returns a UInt32 that identifies the kind of clock source
                        the item ID refers to using an AudioValueTranslation structure. The input
                        data is the UInt32 containing the item ID and the output data is the UInt32.
                        Values for this property are defined in <IOAudio/audio/IOAudioTypes.h>. 
    @constant       kAudioDevicePropertyPlayThru
                        A UInt32 where a value of 0 means that play through is off and a value of 1
                        means that it is on. This property is implemented by an AudioControl object
                        that is a subclass of AudioMuteControl. Further, the control that implements
                        this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruSolo
                        A UInt32 where a value of 1 means that just that play through element is
                        audible and the other elements are inaudible. The property is implemented by
                        an AudioControl object that is a subclass of AudioSoloControl. Further, the
                        control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruVolumeScalar
                        A Float32 that represents the value of the volume control. The range is
                        between 0.0 and 1.0 (inclusive). This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl.Further, the
                        control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruVolumeDecibels
                        A Float32 that represents the value of the volume control in dB. This
                        property is implemented by an AudioControl object that is a subclass of
                        AudioVolumeControl. Further, the control that implements this property is
                        only available through kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruVolumeRangeDecibels
                        An AudioValueRange that contains the minimum and maximum dB values the
                        control can have. This property is implemented by an AudioControl object
                        that is a subclass of AudioVolumeControl. Further, the control that
                        implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruVolumeScalarToDecibels
                        A Float32 that on input contains a scalar volume value for the and on exit
                        contains the equivalent dB value. This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl. Further, the
                        control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruVolumeDecibelsToScalar
                        A Float32 that on input contains a dB volume value for the and on exit
                        contains the equivalent scalar value. This property is implemented by an
                        AudioControl object that is a subclass of AudioVolumeControl. Further, the
                        control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruStereoPan
                        A Float32 where 0.0 is full left, 1.0 is full right, and 0.5 is center. This
                        property is implemented by an AudioControl object that is a subclass of
                        AudioStereoPanControl. Further, the control that implements this property is
                        only available through kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruStereoPanChannels
                        An array of two UInt32s that indicate which elements of the owning object
                        the signal is being panned between. This property is implemented by an
                        AudioControl object that is a subclass of AudioStereoPanControl. Further,
                        the control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruDestination
                        A UInt32 whose value is the item ID for the currently selected play through
                        data destination. This property is implemented by an AudioControl object
                        that is a subclass of AudioDataDestinationControl. Further, the control that
                        implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruDestinations
                        An array of UInt32s that are represent all the IDs of all the play through
                        data destinations currently available. This property is implemented by an
                        AudioControl object that is a subclass of AudioDataDestinationControl.
                        Further, the control that implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyPlayThruDestinationNameForIDCFString
                        This property translates the given play through data destination item ID
                        into a human readable name using an AudioValueTranslation structure. The
                        input data is the UInt32 containing the item ID to translated and the output
                        data is a CFString. The caller is responsible for releasing the returned
                        CFObject. This property is implemented by an AudioControl object that is a
                        subclass of AudioDataDestinationControl. Further, the control that
                        implements this property is only available through
                        kAudioDevicePropertyScopePlayThrough.
    @constant       kAudioDevicePropertyChannelNominalLineLevel
                        A UInt32 whose value is the item ID for the currently selected nominal line
                        level. This property is implemented by an AudioControl object that is a
                        subclass of AudioLineLevelControl.
    @constant       kAudioDevicePropertyChannelNominalLineLevels
                        An array of UInt32s that represent all the IDs of all the nominal line
                        levels currently available. This property is implemented by an AudioControl
                        object that is a subclass of AudioLineLevelControl.
    @constant       kAudioDevicePropertyChannelNominalLineLevelNameForIDCFString
                        This property translates the given nominal line level item ID into a human
                        readable name using an AudioValueTranslation structure. The input data is
                        the UInt32 containing the item ID to be translated and the output data is a
                        CFString. The caller is responsible for releasing the returned CFObject.
                        This property is implemented by an AudioCOntrol object that is a subclass of
                        AudioLineLevelControl.
    @constant       kAudioDevicePropertyDriverShouldOwniSub
                        A UInt32 where a value of 0 means that the AudioDevice should not claim
                        ownership of any attached iSub and a value of 1 means that it should. Note
                        that this property is only available for built-in devices and for USB Audio
                        devices that use the standard class compliant driver. This property is
                        implemented by an AudioControl object that is a subclass of
                        AudioISubOwnerControl.
    @constant       kAudioDevicePropertySubVolumeScalar
                        A Float32 that represents the value of the LFE volume control. The range is
                        between 0.0 and 1.0 (inclusive). This property is implemented by an
                        AudioControl object that is a subclass of AudioLFEVolumeControl.
    @constant       kAudioDevicePropertySubVolumeDecibels
                        A Float32 that represents the value of the LFE volume control in dB. This
                        property is implemented by an AudioControl object that is a subclass of
                        AudioLFE VolumeControl.
    @constant       kAudioDevicePropertySubVolumeRangeDecibels
                        An AudioValueRange that contains the minimum and maximum dB values the
                        control can have. This property is implemented by an AudioControl object
                        that is a subclass of AudioLFEVolumeControl.
    @constant       kAudioDevicePropertySubVolumeScalarToDecibels
                        A Float32 that on input contains a scalar volume value for the and on exit
                        contains the equivalent dB value. This property is implemented by an
                        AudioControl object that is a subclass of AudioLFEVolumeControl.
    @constant       kAudioDevicePropertySubVolumeDecibelsToScalar
                        A Float32 that on input contains a dB volume value for the and on exit
                        contains the equivalent scalar value. This property is implemented by an
                        AudioControl object that is a subclass of AudioLFEVolumeControl.
    @constant       kAudioDevicePropertySubMute
                        A UInt32 where a value of 1 means that mute is enabled making the LFE on
                        that element inaudible. The property is implemented by an AudioControl
                        object that is a subclass of AudioLFEMuteControl.
}
const
	kAudioDevicePropertyJackIsConnected = $6A61636B (* 'jack' *);
	kAudioDevicePropertyVolumeScalar = $766F6C6D (* 'volm' *);
	kAudioDevicePropertyVolumeDecibels = $766F6C64 (* 'vold' *);
	kAudioDevicePropertyVolumeRangeDecibels = $76646223 (* 'vdb#' *);
	kAudioDevicePropertyVolumeScalarToDecibels = $76326462 (* 'v2db' *);
	kAudioDevicePropertyVolumeDecibelsToScalar = $64623276 (* 'db2v' *);
	kAudioDevicePropertyStereoPan = $7370616E (* 'span' *);
	kAudioDevicePropertyStereoPanChannels = $73706E23 (* 'spn#' *);
	kAudioDevicePropertyMute = $6D757465 (* 'mute' *);
	kAudioDevicePropertySolo = $736F6C6F (* 'solo' *);
	kAudioDevicePropertyDataSource = $73737263 (* 'ssrc' *);
	kAudioDevicePropertyDataSources = $73736323 (* 'ssc#' *);
	kAudioDevicePropertyDataSourceNameForIDCFString = $6C73636E (* 'lscn' *);
	kAudioDevicePropertyClockSource = $63737263 (* 'csrc' *);
	kAudioDevicePropertyClockSources = $63736323 (* 'csc#' *);
	kAudioDevicePropertyClockSourceNameForIDCFString = $6C63736E (* 'lcsn' *);
	kAudioDevicePropertyClockSourceKindForID = $6373636B (* 'csck' *);
	kAudioDevicePropertyPlayThru = $74687275 (* 'thru' *);
	kAudioDevicePropertyPlayThruSolo = $74687273 (* 'thrs' *);
	kAudioDevicePropertyPlayThruVolumeScalar = $6D767363 (* 'mvsc' *);
	kAudioDevicePropertyPlayThruVolumeDecibels = $6D766462 (* 'mvdb' *);
	kAudioDevicePropertyPlayThruVolumeRangeDecibels = $6D766423 (* 'mvd#' *);
	kAudioDevicePropertyPlayThruVolumeScalarToDecibels = $6D763264 (* 'mv2d' *);
	kAudioDevicePropertyPlayThruVolumeDecibelsToScalar = $6D763273 (* 'mv2s' *);
	kAudioDevicePropertyPlayThruStereoPan = $6D73706E (* 'mspn' *);
	kAudioDevicePropertyPlayThruStereoPanChannels = $6D737023 (* 'msp#' *);
	kAudioDevicePropertyPlayThruDestination = $6D646473 (* 'mdds' *);
	kAudioDevicePropertyPlayThruDestinations = $6D646423 (* 'mdd#' *);
	kAudioDevicePropertyPlayThruDestinationNameForIDCFString = $6D646463 (* 'mddc' *);
	kAudioDevicePropertyChannelNominalLineLevel = $6E6C766C (* 'nlvl' *);
	kAudioDevicePropertyChannelNominalLineLevels = $6E6C7623 (* 'nlv#' *);
	kAudioDevicePropertyChannelNominalLineLevelNameForIDCFString = $6C636E6C (* 'lcnl' *);
	kAudioDevicePropertyDriverShouldOwniSub = $69737562 (* 'isub' *);
	kAudioDevicePropertySubVolumeScalar = $73766C6D (* 'svlm' *);
	kAudioDevicePropertySubVolumeDecibels = $73766C64 (* 'svld' *);
	kAudioDevicePropertySubVolumeRangeDecibels = $73766423 (* 'svd#' *);
	kAudioDevicePropertySubVolumeScalarToDecibels = $73763264 (* 'sv2d' *);
	kAudioDevicePropertySubVolumeDecibelsToScalar = $73643276 (* 'sd2v' *);
	kAudioDevicePropertySubMute = $736D7574 (* 'smut' *);

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
                        A CFString that contains a human readable name for the given element in the
                        given scope. The caller is responsible for releasing the returned CFObject.
                        (kAudioObjectPropertyElementName: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyChannelNameCFString
                        A CFString that contains a human readable name for the given element in the
                        given scope. The caller is responsible for releasing the returned CFObject.
                        (kAudioObjectPropertyElementName: This is just another name for the
                        inherited selector.)
    @constant       kAudioDevicePropertyChannelCategoryName
                        A CFString that contains a human readable name for the category of the given
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
                        A CFString that contains a human readable name for the number of the given
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
                        the name as a null terminated c-string.
                        (kAudioDevicePropertyDataSourceNameForIDCFString: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyClockSourceNameForID
                        This property translates the given clock source item ID into a human
                        readable name using an AudioValueTranslation structure. The input data is
                        the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated c-string.
                        (kAudioDevicePropertyClockSourceNameForIDCFString: CFStrings are better for
                        localization.)
    @constant       kAudioDevicePropertyPlayThruDestinationNameForID
                        This property translates the given play through destination item ID into a
                        human readable name using an AudioValueTranslation structure. The input data
                        is the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated c-string.
                        (kAudioDevicePropertyPlayThruDestinationNameForIDCFString: CFStrings are
                        better for localization.)
    @constant       kAudioDevicePropertyChannelNominalLineLevelNameForID
                        This property translates the given nominal line level item ID into a human
                        readable name using an AudioValueTranslation structure. The input data is
                        the UInt32 holding the item ID to be translated and the output data is a
                        buffer to hold the name as a null terminated c-string.
                        (kAudioDevicePropertyChannelNominalLineLevelNameForIDCFString: CFStrings are
                        better for localization.)
}
const
	kAudioDevicePropertyDeviceName = $6E616D65 (* 'name' *);
	kAudioDevicePropertyDeviceNameCFString = kAudioObjectPropertyName;
	kAudioDevicePropertyDeviceManufacturer = $6D616B72 (* 'makr' *);
	kAudioDevicePropertyDeviceManufacturerCFString = kAudioObjectPropertyManufacturer;
	kAudioDevicePropertyRegisterBufferList = $72627566 (* 'rbuf' *);
	kAudioDevicePropertyBufferSize = $6273697A (* 'bsiz' *);
	kAudioDevicePropertyBufferSizeRange = $62737A23 (* 'bsz#' *);
	kAudioDevicePropertyChannelName = $63686E6D (* 'chnm' *);
	kAudioDevicePropertyChannelNameCFString = kAudioObjectPropertyElementName;
	kAudioDevicePropertyChannelCategoryName = $63636E6D (* 'ccnm' *);
	kAudioDevicePropertyChannelCategoryNameCFString = kAudioObjectPropertyElementCategoryName;
	kAudioDevicePropertyChannelNumberName = $636E6E6D (* 'cnnm' *);
	kAudioDevicePropertyChannelNumberNameCFString = kAudioObjectPropertyElementNumberName;
	kAudioDevicePropertySupportsMixing = $6D69783F (* 'mix?' *);
	kAudioDevicePropertyStreamFormat = $73666D74 (* 'sfmt' *);
	kAudioDevicePropertyStreamFormats = $73666D23 (* 'sfm#' *);
	kAudioDevicePropertyStreamFormatSupported = $73666D3F (* 'sfm?' *);
	kAudioDevicePropertyStreamFormatMatch = $73666D6D (* 'sfmm' *);
	kAudioDevicePropertyDataSourceNameForID = $7373636E (* 'sscn' *);
	kAudioDevicePropertyClockSourceNameForID = $6373636E (* 'cscn' *);
	kAudioDevicePropertyPlayThruDestinationNameForID = $6D64646E (* 'mddn' *);
	kAudioDevicePropertyChannelNominalLineLevelNameForID = $636E6C76 (* 'cnlv' *);

//==================================================================================================
//#pragma mark    AudioDevice Functions

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceRemoveIOProc
    @abstract       Unregisters the given AudioDeviceIOProc from the AudioDevice.
    @param          inDevice
                        The AudioDevice to unregister the IOProc from.
    @param          inProc
                        The AudioDeviceIOProc to unregister.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceRemoveIOProc( inDevice: AudioDeviceID; inProc: AudioDeviceIOProc ): OSStatus; external name '_AudioDeviceRemoveIOProc';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceStart
    @abstract       Starts IO for the given AudioDeviceIOProc.
    @param          inDevice
                        The AudioDevice to start the IOProc on.
    @param          inProc
                        The AudioDeviceIOProc to start. Note that this can be NULL, which starts the
                        hardware regardless of whether or not there are any IOProcs registered. This
                        is necessary if any of the AudioDevice's timing services are to be used. A
                        balancing call to AudioDeviceStop with a NULL IOProc is required to stop the
                        hardware.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceStart( inDevice: AudioDeviceID; inProc: AudioDeviceIOProc ): OSStatus; external name '_AudioDeviceStart';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceStartAtTime
    @abstract       Starts IO for the given AudioDeviceIOProc and aligns the IO cycle of the
                    AudioDevice with the given time.
    @param          inDevice
                        The AudioDevice to start the IOProc on.
    @param          inProc
                        The AudioDeviceIOProc to start. Note that this can be NULL, which starts the
                        hardware regardless of whether or not there are any IOProcs registered.
    @param          ioRequestedStartTime
                        A pointer to an AudioTimeStamp that, on entry, is the requested time to
                        start the IOProc. On exit, it will be the actual time the IOProc will start.
    @param          inFlags
                        A UInt32 containing flags that modify how this function behaves.
    @result         An OSStatus indicating success or failure.
                    kAudioHardwareUnsupportedOperationError will be returned if the AudioDevice does
                    not support starting at a specific time and inProc and ioRequestedStartTime are
                    not NULL.
}
function AudioDeviceStartAtTime( inDevice: AudioDeviceID; inProc: AudioDeviceIOProc; ioRequestedStartTime: AudioTimeStampPtr; inFlags: UInt32 ): OSStatus; external name '_AudioDeviceStartAtTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{!
    @function       AudioDeviceStop
    @abstract       Stops IO for the given AudioDeviceIOProc.
    @param          inDevice
                        The AudioDevice to stop the IOProc on.
    @param          inProc
                        The AudioDeviceIOProc to stop.
    @result         An OSStatus indicating success or failure.
}
function AudioDeviceStop( inDevice: AudioDeviceID; inProc: AudioDeviceIOProc ): OSStatus; external name '_AudioDeviceStop';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceRead
    @abstract       Read some data from an AudioDevice starting at the given time.
    @discussion     With the advent of aggregate devices, the need for AudioDeviceRead has gone
                    away. Consequently, this function is a good candidate for deprecation some day.
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
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{!
    @function       AudioDeviceGetCurrentTime
    @abstract       Retrieves the current time from an AudioDevice. Note that the device has to be
                    running.
    @param          inDevice
                        The AudioDevice to from which to get the time.
    @param          outTime
                        An AudioTimeStamp into which the current time is put.
    @result         An OSStatus indicating success or failure. kAudioHardwareNotRunningError will be
                    returned if the AudioDevice isn't running.
}
function AudioDeviceGetCurrentTime( inDevice: AudioDeviceID; var outTime: AudioTimeStamp ): OSStatus; external name '_AudioDeviceGetCurrentTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceTranslateTime
    @abstract       Translates the time in the AudioDevice's time base from one representation to
                    another. Note that the device has to be running
    @param          inDevice
                        The AudioDevice whose time base governs the translation.
    @param          inTime
                        An AudioTimeStamp containing the time to be translated.
    @param          outTime
                        An AudioTimeStamp into which the translated time is put. On entry, the
                        mFlags field specifies which representations to translate the input time
                        into. Because not every device supports all time representations, on exit,
                        the mFlags field will indicate which translations were actually done.
    @result         An OSStatus indicating success or failure. kAudioHardwareNotRunningError will be
                    returned if the AudioDevice isn't running.
}
function AudioDeviceTranslateTime( inDevice: AudioDeviceID; const (*var*) inTime: AudioTimeStamp; var outTime: AudioTimeStamp ): OSStatus; external name '_AudioDeviceTranslateTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceGetNearestStartTime
    @abstract       Query an AudioDevice to get a time equal to or later than the given time that is
                    the best time to start IO.
    @discussion     The time that is returned is dictated by the constraints of the device and the
                    system. For instance, the driver of a device that provides both audio and video
                    data may only allow start times that coincide with the edge of a video frame.
                    Also, if the device already has one or more active IOProcs, the start time will
                    be shifted to the beginning of the next IO cycle so as not to cause
                    discontinuities in the existing IOProcs. Another reason the start time may shift
                    is to allow for aligning the buffer accesses in an optimal fashion. Note that
                    the device must be running to use this function.
    @param          inDevice
                        The AudioDevice to query.
    @param          ioRequestedStartTime
                        A pointer to an AudioTimeStamp that, on entry, is the requested start time.
                        On exit, it will have the a time equal to or later than the requested time,
                        as dictated by the device's constraints.
    @param          inFlags
                        A UInt32 containing flags that modify how this function behaves.
    @result         An OSStatus indicating success or failure. kAudioHardwareNotRunningError will be
                    returned if the AudioDevice isn't running.
                    kAudioHardwareUnsupportedOperationError will be returned if the AudioDevice does
                    not support starting at a specific time.
}
function AudioDeviceGetNearestStartTime( inDevice: AudioDeviceID; var ioRequestedStartTime: AudioTimeStamp; inFlags: UInt32 ): OSStatus; external name '_AudioDeviceGetNearestStartTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

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
function AudioDeviceGetPropertyInfo( inDevice: AudioDeviceID; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; outSize: UInt32Ptr; outWritable: BooleanPtr ): OSStatus; external name '_AudioDeviceGetPropertyInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

{!
    @function       AudioDeviceSetProperty
    @abstract       Tells the AudioDevice object to change the value of the given property using the
                    provided data.
    @discussion     Note that the value of the property should not be considered changed until the
                    HAL has called the listeners as many properties values are changed
                    asynchronously. Also note that the same functionality is provided by the
                    function AudioObjectGetPropertyData().
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
function AudioDeviceSetProperty( inDevice: AudioDeviceID; inWhen: {const} AudioTimeStampPtr; inChannel: UInt32; isInput: Boolean; inPropertyID: AudioDevicePropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioDeviceSetProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)

//==================================================================================================
//#pragma mark    AudioStream Types

{!
    @typedef        AudioStreamID
    @abstract       AudioStream is the base class for all objects that represent a stream of data on
                    an audio device.
    @discussion     AudioStream is a subclass of AudioObject and can contain AudioControls.
}
type
	AudioStreamID = AudioObjectID;

{!
    @struct         AudioStreamRangedDescription
    @abstract       This structure allows a specific sample rate range to be associated with an
                    AudioStreamBasicDescription that specifies it's sample rate as
                    kAudioStreamAnyRate.
    @discussion     Note that this structure is only used to desicribe the the available formats
                    for a stream. It is not used for the current format.
    @field          mFormat
                        The AudioStreamBasicDescription that describes the format of the stream.
                        Note that the mSampleRate field of the structure will be the same as the
                        the values in mSampleRateRange when only a single sample rate is supported.
                        It will be kAudioStreamAnyRate when there is a range with more elements. 
    @field          mSampleRateRange
                        The AudioValueRange that describes the minimum and maximum sample rate for
                        the stream. If the mSampleRate field of mFormat is kAudioStreamAnyRate the
                        format supports the range of sample rates described by this structure.
                        Otherwise, the minimum will be the same as the maximum which will be the
                        same as the mSampleRate field of mFormat.
}
type
	AudioStreamRangedDescription = record
		mFormat: AudioStreamBasicDescription;
		mSampleRateRange: AudioValueRange;
	end;

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
//#pragma mark    AudioStream Constants

{!
    @enum           AudioStream Class Constants
    @abstract       Various constants related to AudioStreams.
    @constant       kAudioStreamClassID
                        The AudioClassID that identifies the AudioStream class.
    @constant       kAudioStreamUnknown
                        The AudioObjectID for a nonexistent AudioObject.
}
const
	kAudioStreamClassID = $61737472 (* 'astr' *);
	kAudioStreamUnknown = kAudioObjectUnknown;

//==================================================================================================
//#pragma mark    AudioStream Properties

{!
    @enum           AudioStream Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioStreams.
    @discussion     AudioStream is a subclass of AudioObject and has only the single scope,
                    kAudioObjectPropertyScopeGlobal. They have a master element and an element for
                    each channel in the stream numbered upward from 1. Note that AudioStream objects
                    share AudioControl objects with their owning AudioDevice. Consequently, all the
                    standard AudioControl related property selectors implemented by AudioDevices are
                    also implemented by AudioStreams. The same constants are to be used for such
                    properties.
    @constant       kAudioStreamPropertyDirection
                        A UInt32 where a value of 0 means that this AudioStream is an output stream
                        and a value of 1 means that it is an input stream.
    @constant       kAudioStreamPropertyTerminalType
                        A UInt32 whose value describes the general kind of functionality attached
                        to the AudioStream. Constants that describe some of the values of this
                        property are defined in <IOKit/audio/IOAudioTypes.h>
    @constant       kAudioStreamPropertyStartingChannel
                        A UInt32 that specifies the first element in the owning device that
                        corresponds to element one of this stream.
    @constant       kAudioStreamPropertyLatency
                        A UInt32 containing the number of frames of latency in the AudioStream. Note
                        that the owning AudioDevice may have additional latency so it should be
                        queried as well. If both the device and the stream say they have latency,
                        then the total latency for the stream is the device latency summed with the
                        stream latency.
    @constant       kAudioStreamPropertyVirtualFormat
                        An AudioStreamBasicDescription that describes the current data format for
                        the AudioStream. The virtual format refers to the data format in which all
                        IOProcs for the owning AudioDevice will perform IO transactions.
    @constant       kAudioStreamPropertyAvailableVirtualFormats
                        An array of AudioStreamRangedDescriptions that describe the available data
                        formats for the AudioStream. The virtual format refers to the data format in
                        which all IOProcs for the owning AudioDevice will perform IO transactions.
    @constant       kAudioStreamPropertyPhysicalFormat
                        An AudioStreamBasicDescription that describes the current data format for
                        the AudioStream. The physical format refers to the data format in which the
                        hardware for the owning AudioDevice performs it's IO transactions.
    @constant       kAudioStreamPropertyAvailablePhysicalFormats
                        An array of AudioStreamRangedDescriptions that describe the available data
                        formats for the AudioStream. The physical format refers to the data format
                        in which the hardware for the owning AudioDevice performs it's IO
                        transactions.
}
const
	kAudioStreamPropertyDirection = $73646972 (* 'sdir' *);
	kAudioStreamPropertyTerminalType = $7465726D (* 'term' *);
	kAudioStreamPropertyStartingChannel = $7363686E (* 'schn' *);
	kAudioStreamPropertyLatency = kAudioDevicePropertyLatency;
	kAudioStreamPropertyVirtualFormat = $73666D74 (* 'sfmt' *);
	kAudioStreamPropertyAvailableVirtualFormats = $73666D61 (* 'sfma' *);
	kAudioStreamPropertyPhysicalFormat = $70667420 (* 'pft ' *);
	kAudioStreamPropertyAvailablePhysicalFormats = $70667461 (* 'pfta' *);


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
	kAudioStreamPropertyPhysicalFormats = $70667423 (* 'pft#' *);
	kAudioStreamPropertyPhysicalFormatSupported = $7066743F (* 'pft?' *);
	kAudioStreamPropertyPhysicalFormatMatch = $7066746D (* 'pftm' *);

//==================================================================================================
//#pragma mark    AudioStream Functions

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
function AudioStreamGetPropertyInfo( inStream: AudioStreamID; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; var outSize: UInt32; var outWritable: Boolean ): OSStatus; external name '_AudioStreamGetPropertyInfo';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{!
    @function       AudioStreamSetProperty
    @abstract       Tells the AudioStream object to change the value of the given property using the
                    provided data.
    @discussion     Note that the value of the property should not be considered changed until the
                    HAL has called the listeners as many properties values are changed
                    asynchronously. Also note that the same functionality is provided by the
                    function AudioObjectGetPropertyData().
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
function AudioStreamSetProperty( inStream: AudioStreamID; inWhen: AudioTimeStampPtr; inChannel: UInt32; inPropertyID: AudioDevicePropertyID; inPropertyDataSize: UInt32; inPropertyData: {const} UnivPtr ): OSStatus; external name '_AudioStreamSetProperty';
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

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
(* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

//==================================================================================================
//#pragma mark    AudioAggregateDevice Constants

{!
    @enum           AudioAggregateDevice Class Constants
    @abstract       Various constants related to AudioAggregateDevices.
    @constant       kAudioAggregateDeviceClassID
                        The AudioClassID that identifies the AudioAggregateDevice class.
    @constant       kAudioDeviceTransportTypeAggregate
                        The transport type ID (see kAudioDevicePropertyTransportType) for aggregate
                        devices.
    @constant       kAudioDeviceTransportTypeAutoAggregate
                        The transport type ID (see kAudioDevicePropertyTransportType) for
                        automatically generated aggregate devices.
}
const
	kAudioAggregateDeviceClassID = $61616767 (* 'aagg' *);
	kAudioDeviceTransportTypeAggregate = $67727570 (* 'grup' *);
	kAudioDeviceTransportTypeAutoAggregate = $66677270 (* 'fgrp' *);

{!
    @defined        kAudioAggregateDeviceUIDKey
    @discussion     The key used in a CFDictionary that describes the composition of an
                    AudioAggregateDevice. The value for this key is a CFString that contains the UID
                    of the AudioAggregateDevice.
}
const
	kAudioAggregateDeviceUIDKey = 'uid';

{!
    @defined        kAudioAggregateDeviceNameKey
    @discussion     The key used in a CFDictionary that describes the composition of an
                    AudioAggregateDevice. The value for this key is a CFString that contains the
                    human readable name of the AudioAggregateDevice.
}
const
	kAudioAggregateDeviceNameKey = 'name';

{!
    @defined        kAudioAggregateDeviceSubDeviceListKey
    @discussion     The key used in a CFDictionary that describes the composition of an
                    AudioAggregateDevice. The value for this key is a CFArray of CFDictionaries that
                    describe each sub-device in the AudioAggregateDevice. The keys for this
                    CFDictionary are defined in the AudioSubDevice section.
}
const
	kAudioAggregateDeviceSubDeviceListKey = 'subdevices';

{!
    @defined        kAudioAggregateDeviceMasterSubDeviceKey
    @discussion     The key used in a CFDictionary that describes the composition of an
                    AudioAggregateDevice. The value for this key is a CFString that contains the
                    UID for the sub-device that is the master time source for the
                    AudioAggregateDevice.
}
const
	kAudioAggregateDeviceMasterSubDeviceKey = 'master';

{!
    @defined        kAudioAggregateDeviceIsPrivateKey
    @discussion     The key used in a CFDictionary that describes the composition of an
                    AudioAggregateDevice. The value for this key is a CFNumber where a value of 0
                    means that the AudioAggregateDevice is to be published to the entire system and
                    a value of 1 means that the AudioAggregateDevice is private to the process that
                    created it. Note that a private AudioAggregateDevice is not persistent across
                    launches of the process that created it. Note that if this key is not present,
                    it implies that the AudioAggregateDevice is published to the entire system.
}
const
	kAudioAggregateDeviceIsPrivateKey = 'private';

//==================================================================================================
//#pragma mark    AudioAggregateDevice Properties

{!
    @enum           AudioAggregateDevice Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioAggregateDevices.
    @discussion     AudioAggregateDevice is a subclass of AudioDevice.
    @constant       kAudioAggregateDevicePropertyFullSubDeviceList
                        A CFArray of CFStrings that contain the UIDs of all the devices, active or
                        inactive, contained in the AudioAggregateDevice. The order of the items in
                        the array is significant and is used to determine the order of the streams
                        of the AudioAggregateDevice. The caller is responsible for releasing the
                        returned CFObject.
    @constant       kAudioAggregateDevicePropertyActiveSubDeviceList
                        An array of AudioObjectIDs for all the active sub-devices in the aggregate
                        device.
    @constant       kAudioAggregateDevicePropertyComposition
                        A CFDictionary that describes the composition of the AudioAggregateDevice.
                        The keys for this CFDicitionary are defined in the AudioAggregateDevice
                        Constants section.
}
const
	kAudioAggregateDevicePropertyFullSubDeviceList = $67727570 (* 'grup' *);
	kAudioAggregateDevicePropertyActiveSubDeviceList = $61677270 (* 'agrp' *);
	kAudioAggregateDevicePropertyComposition = $61636F6D (* 'acom' *);

{!
    @enum           AudioAggregateDevice Properties Implemented via AudioControl objects
    @abstract       AudioObjectPropertySelector values for AudioAggregateDevice properties that are
                    implemented by AudioControl objects.
    @discussion     These properties are also accessible by locating the AudioControl object
                    attached to the AudioAggregateDevice and using that object to access the
                    properties of the control.
    @constant       kAudioAggregateDevicePropertyMasterSubDevice
                        A CFString that contains the UID for the AudioDevice that is currently
                        serving as the master time base of the aggregate device. This property is
                        also implemented by the AudioClockSourceControl on the master element of the
                        global scope of the AudioAggregateDevice.
}
const
	kAudioAggregateDevicePropertyMasterSubDevice = $616D7374 (* 'amst' *);

//==================================================================================================
//#pragma mark    AudioSubDevice Constants

{!
    @enum           AudioSubDevice Class Constants
    @abstract       Various constants related to AudioSubDevices.
    @constant       kAudioSubDeviceClassID
                        The AudioClassID that identifies the AudioSubDevice class.
}
const
	kAudioSubDeviceClassID = $61737562 (* 'asub' *);

{!
    @enum           AudioSubDevice Clock Drift Compensation Methods
    @abstract       Constants that describe the range of values the property
                    kAudioSubDevicePropertyDriftCompensation. It is a continuous range from
                    kAudioSubDeviceDriftCompensationMinQuality to
                    kAudioSubDeviceDriftCompensationMaxQuality, with some commonly used settings
                    called out.
}
const
	kAudioSubDeviceDriftCompensationMinQuality = 0;
	kAudioSubDeviceDriftCompensationLowQuality = $20;
	kAudioSubDeviceDriftCompensationMediumQuality = $40;
	kAudioSubDeviceDriftCompensationHighQuality = $60;
	kAudioSubDeviceDriftCompensationMaxQuality = $7F;

{!
    @defined        kAudioSubDeviceUIDKey
    @discussion     The key used in a CFDictionary that describes the state of an AudioSubDevice.
                    The value for this key is a CFString that contains the UID for the
                    AudioSubDevice.
}
const
	kAudioSubDeviceUIDKey = 'uid';

//==================================================================================================
//#pragma mark    AudioSubDevice Properties

{!
    @enum           AudioSubDevice Properties
    @abstract       AudioObjectPropertySelector values that apply to all AudioSubDevices.
    @discussion     AudioSubDevice is a subclass of AudioDevice that is collected together with
                    other sub-devices in an AudioAggregateDevice. AudioSubDevice objects do not
                    implement an IO path nor any AudioDevice properties associated with the IO path.
                    They also don't have any streams.
    @constant       kAudioSubDevicePropertyExtraLatency
                        A Float64 indicating the number of sample frames to add to or subtract from
                        the latency compensation used for this AudioSubDevice.
    @constant       kAudioSubDevicePropertyDriftCompensation
                        A UInt32 where a value of 0 indicates that no drift compensation should be
                        done for this AudioSubDevice and a value of 1 means that it should.
    @constant       kAudioSubDevicePropertyDriftCompensationQuality
                        A UInt32 that controls the trade-off between quality and CPU load in the
                        drift compensation. The range of values is from 0 to 128, where the lower
                        the number, the worse the quality but also the less CPU is used to do the
                        compensation.
}
const
	kAudioSubDevicePropertyExtraLatency = $786C7463 (* 'xltc' *);
	kAudioSubDevicePropertyDriftCompensation = $64726674 (* 'drft' *);
	kAudioSubDevicePropertyDriftCompensationQuality = $64726671 (* 'drfq' *);

//==================================================================================================


end.
