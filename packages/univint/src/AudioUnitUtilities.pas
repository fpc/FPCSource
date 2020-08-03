{!
	@file		AudioUnitUtilities.h
	@framework	AudioToolbox.framework
	@copyright	(c) 2002-2015 by Apple, Inc., all rights reserved.
	@abstract	Higher-level utility functions for the use of AudioUnit clients.

    @discussion

	The AU Parameter Listener is designed to provide notifications when an Audio Unit's parameters
	or other state changes.  It makes it unnecessary for UI components to continually poll an Audio
	Unit to determine if a parameter value has been changed. In order for this notification
	mechanism to work properly, parameter values should be changed using the AUParameterSet call
	(discussed below). This also makes it unnecessary for an Audio Unit to provide and support a
	notification mechanism, particularly as AudioUnitSetParameter may be received by an Audio Unit
	during the render process.

	The AUEventListener API's extend the AUParameterListener API's by supporting event types
	other than parameter changes. Events, including parameter changes are delivered serially to the 
	listener, preserving the time order of the events and parameter changes.

	There are also some utilities for converting between non-linear and linear value ranges. These
	are useful for displaying a non-linear parameter (such as one whose units are in Hertz or
	decibels) using a linear control mechanism, such as a slider, to ensure that the user has a
	wider perceived range of control over the parameter value.
}
{  Pascal Translation: Jonas Maebe <jonas@freepascal.org>, July 2019 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit AudioUnitUtilities;
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
{$ifc defined iphonesim}
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
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
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
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
uses MacTypes,CFBase,CFRunLoop,AUComponent;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}



//CF_ASSUME_NONNULL_BEGIN


{ ============================================================================= }

{!
    @enum       
    @constant   kAUParameterListener_AnyParameter
                    A wildcard value for an AudioUnitParameterID. Note that this is
                    only valid when sending a notification (with AUParameterListenerNotify),
                    not when registering to receive one.
}
const
	kAUParameterListener_AnyParameter = $FFFFFFFF; 

{!
    @enum       AudioUnitEventType
    
    @abstract   Types of Audio Unit Events.
    
    @constant   kAudioUnitEvent_ParameterValueChange
                    The event is a change to a parameter value
    @constant   kAudioUnitEvent_BeginParameterChangeGesture
                    The event signifies a gesture (e.g. mouse-down) beginning a potential series of
                    related parameter value change events.
    @constant   kAudioUnitEvent_EndParameterChangeGesture
                    The event signifies a gesture (e.g. mouse-up) ending a series of related
                    parameter value change events.
    @constant   kAudioUnitEvent_PropertyChange
                    The event is a change to a property value.
}
type
	AudioUnitEventType = UInt32;
	AudioUnitEventTypePtr = ^AudioUnitEventType;
const
	kAudioUnitEvent_ParameterValueChange = 0;
	kAudioUnitEvent_BeginParameterChangeGesture = 1;
	kAudioUnitEvent_EndParameterChangeGesture = 2;
	kAudioUnitEvent_PropertyChange = 3; 

{ ============================================================================= }

{!
    @typedef        AUParameterListenerRef
    @abstract       An object which receives notifications of Audio Unit parameter value changes.
    @discussion
}
type
	AUListenerBase = record end;
	AUParameterListenerRef = ^AUListenerBase;
    // opaque
    // old-style listener, may not be passed to new functions

{!
    @typedef        AUEventListenerRef
    @abstract       An object which receives Audio Unit events.
    @discussion     An AUEventListenerRef may be passed to API's taking an AUEventListenerRef
                    as an argument.
}
type
	AUEventListenerRef = AUParameterListenerRef;
    // new-style listener, can be passed to both old and new functions

{!
    @struct     AudioUnitEvent
    @abstract   Describes a change to an Audio Unit's state.
    @field      mEventType
        The type of event.
    @field      mArgument
        Specifies the parameter or property which has changed.
}  
type
	AudioUnitEvent = record
		mEventType: AudioUnitEventType;
		mArgument: record
			case byte of
				0: (mParameter: AudioUnitParameter); // for parameter value change, begin and end gesture
		  		1: (mProperty: AudioUnitProperty);  // for kAudioUnitEvent_PropertyChange
		end
	end;
	AudioUnitEventPtr = ^AudioUnitEvent;


{!
    @typedef    AUParameterListenerProc
    @abstract   A function called when a parameter value changes.
    @param  inUserData
                The value passed to AUListenerCreate when the callback function was installed.
    @param  inObject
                The object which generated the parameter change.
    @param  inParameter
                Signifies the parameter whose value changed.
    @param  inValue
                The parameter's new value.
}
type
	AUParameterListenerProc = procedure( inUserData: UnivPtr {__nullable}; inObject: UnivPtr {__nullable}; const (*var*) inParameter: AudioUnitParameter; inValue: AudioUnitParameterValue );

{!
    @typedef    AUEventListenerProc
    @abstract   A function called when an Audio Unit event occurs.
    @param  inUserData
                The value passed to AUListenerCreate when the callback function was installed.
    @param  inObject
                The object which generated the parameter change.
    @param  inEvent
                The event which occurred.
    @param  inEventHostTime
                The host time at which the event occurred.
    @param  inParameterValue
                If the event is parameter change, the parameter's new value (otherwise, undefined).
}
type
	AUEventListenerProc = procedure( inUserData: UnivPtr {__nullable}; inObject: UnivPtr {__nullable}; const (*var*) inEvent: AudioUnitEvent; inEventHostTime: UInt64; inParameterValue: AudioUnitParameterValue );


{ ============================================================================= }

{!
    @functiongroup  AUListener
}


{!
    @function   AUListenerCreate
    @abstract   Create an object for fielding notifications when AudioUnit parameter values change.
    @param      inProc
                    Function called when the parameter's value changes.
    @param      inUserData
                    A reference value for the use of the callback function.
    @param      inRunLoop
                    The run loop on which the callback is called.  If NULL,
                    CFRunLoopGetCurrent() is used.
    @param      inRunLoopMode
                    The run loop mode in which the callback's underlying run loop source will be
                    attached.  If NULL, kCFRunLoopDefaultMode is used.
    @param      inNotificationInterval
                    The minimum time interval, in seconds, at which the callback will be called.
                    If multiple parameter value changes occur within this time interval, the
                    listener will only receive a notification for the last value change that
                    occurred before the callback.  If inNotificationInterval is 0, the inRunLoop
                    and inRunLoopMode arguments are ignored, and the callback will be issued
                    immediately, on the thread on which the parameter was changed.
    @param      outListener
                    On successful return, an AUParameterListenerRef.
    @discussion 
        Note that only parameter changes issued through AUParameterSet will generate
        notifications to listeners; thus, in most cases, AudioUnit clients should use
        AUParameterSet in preference to AudioUnitSetParameter.
}
function AUListenerCreate( inProc: AUParameterListenerProc; inUserData: UnivPtr; inRunLoop: CFRunLoopRef {__nullable}; inRunLoopMode: CFStringRef {__nullable}; inNotificationInterval: Float32; var outListener: AUParameterListenerRef {__nullable * __nonnull} ): OSStatus; external name '_AUListenerCreate';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)

{!
    @function   AUListenerDispose
    @abstract   Dispose a parameter listener object.
    @param      inListener
                    The parameter listener to dispose.
    @discussion 
}
function AUListenerDispose( inListener: AUParameterListenerRef ): OSStatus; external name '_AUListenerDispose';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)

{!
    @function   AUListenerAddParameter
    @abstract   Connect a parameter to a listener.
    @param      inListener
                    The parameter listener which will receive the callback.
    @param      inObject
                    The object which is interested in the value of the parameter.  This will be
                    passed as the inObject parameter to the listener callback function when the
                    parameter changes.
    @param      inParameter
                    The parameter whose value changes are to generate callbacks.
    @discussion 
        Associates an arbitrary object (often a user interface widget) with an
        AudioUnitParameter, and delivers notifications to the specified listener, telling it
        that the object needs to be informed of the parameter's value change.
}
function AUListenerAddParameter( inListener: AUParameterListenerRef; inObject: UnivPtr {__nullable}; const (*var*) inParameter: AudioUnitParameter ): OSStatus; external name '_AUListenerAddParameter';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)

{!
    @function   AUListenerRemoveParameter
    @abstract   Remove a parameter/listener connection.
    @param      inListener
                    The parameter listener to stop receiving callbacks.
    @param      inObject
                    The object which is no longer interested in the value of the parameter.
    @param      inParameter
                    The parameter whose value changes are to stop generating callbacks.
    @discussion 
}
function AUListenerRemoveParameter( inListener: AUParameterListenerRef; inObject: UnivPtr {__nullable}; const (*var*) inParameter: AudioUnitParameter ): OSStatus; external name '_AUListenerRemoveParameter';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)


{!
    @function   AUParameterSet
    @abstract   Set an AudioUnit parameter value and notify listeners.
    @param      inSendingListener
                    A parameter listener generating the change and which does not want to
                    receive a callback as a result of it. May be NULL.
    @param      inSendingObject
                    The object generating the change and which does not want to receive a
                    callback as a result of it. NULL is treated specially when inListener is
                    non-null; it signifies that none of the specified listener's objects will
                    receive notifications.
    @param      inParameter
                    The parameter being changed.
    @param      inValue
                    The new value of the parameter.
	@param		inBufferOffsetInFrames
					The offset into the next rendered buffer at which the parameter change will take
					effect.
    @discussion 
        Calls AudioUnitSetParameter, and performs/schedules notification callbacks to all
        parameter listeners, for that parameter -- except that no callback will be generated to
        the inListener/inObject pair.
}
function AUParameterSet( inSendingListener: AUParameterListenerRef {__nullable}; inSendingObject: UnivPtr {__nullable}; const (*var*) inParameter: AudioUnitParameter; inValue: AudioUnitParameterValue; inBufferOffsetInFrames: UInt32 ): OSStatus; external name '_AUParameterSet';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)

{!
    @function   AUParameterListenerNotify
    @abstract   Notify listeners of a past parameter change.
    @param      inSendingListener
                    A parameter listener generating the change and which does not want to
                    receive a callback as a result of it. May be NULL.
    @param      inSendingObject
                    The object generating the change and which does not want to receive a
                    callback as a result of it. NULL is treated specially when inListener is
                    non-null; it signifies that none of the specified listener's objects will
                    receive notifications.
    @param      inParameter
                    The parameter which was changed.
    @discussion 
        Performs and schedules the notification callbacks of AUParameterSet, without
        actually setting an AudioUnit parameter value.
        
        Clients scheduling ramped parameter changes to AudioUnits must make this call
        dynamically during playback in order for AudioUnitViews to be updated.  When the view's
        listener receives a notification, it will be passed the current value of the parameter.

        A special meaning is applied if the mParameterID value of inParameter is equal to
        kAUParameterListener_AnyParameter. In this case, ANY listener for ANY parameter value
        changes on the specified AudioUnit will be notified of the current value of that
        parameter.
}
function AUParameterListenerNotify( inSendingListener: AUParameterListenerRef {__nullable}; inSendingObject: UnivPtr {__nullable}; const (*var*) inParameter: AudioUnitParameter ): OSStatus; external name '_AUParameterListenerNotify';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)

{ ============================================================================= }

{!
    @functiongroup  AUEventListener
}


{!
    @function   AUEventListenerCreate
    @abstract   Creates an Audio Unit event listener.
    @param      inProc
                    Function called when an event occurs.
    @param      inUserData
                    A reference value for the use of the callback function.
    @param      inRunLoop
                    The run loop on which the callback is called.  If NULL,
                    CFRunLoopGetCurrent() is used.
    @param      inRunLoopMode
                    The run loop mode in which the callback's underlying run loop source will be
                    attached.  If NULL, kCFRunLoopDefaultMode is used.
    @param      inNotificationInterval
                    The minimum time interval, in seconds, at which the callback will be called.
    @param      inValueChangeGranularity
                    Determines how parameter value changes occurring within this interval are
                    queued; when an event follows a previous one by a smaller time interval than
                    the granularity, then the listener will only be notified for the second
                    parameter change.
    @param      outListener
                    On successful return, an AUEventListenerRef.
    
    @discussion
        See the discussion of AUEventListenerCreateWithDispatchQueue.
}
function AUEventListenerCreate( inProc: AUEventListenerProc; inUserData: UnivPtr {__nullable}; inRunLoop: CFRunLoopRef {__nullable}; inRunLoopMode: CFStringRef {__nullable}; inNotificationInterval: Float32 { seconds }; inValueChangeGranularity: Float32 { seconds }; var outListener: AUEventListenerRef {__nullable * __nonnull} ): OSStatus; external name '_AUEventListenerCreate';
(* API_AVAILABLE(macos(10.3), ios(6.0), watchos(2.0), tvos(9.0)) *)

{!
    @function   AUEventListenerAddEventType
    @abstract   Begin delivering a particular type of events to a listener.
    @param      inListener
                    The parameter listener which will receive the events.
    @param      inObject
                    The object which is interested in the value of the parameter.  This will be
                    passed as the inObject parameter to the listener callback function when the
                    parameter changes.
    @param      inEvent
                    The type of event to listen for.
    @result     An OSStatus error code.
}
function AUEventListenerAddEventType( inListener: AUEventListenerRef; inObject: UnivPtr {__nullable}; const (*var*) inEvent: AudioUnitEvent ): OSStatus; external name '_AUEventListenerAddEventType';
(* API_AVAILABLE(macos(10.3), ios(6.0), watchos(2.0), tvos(9.0)) *)
    
{!
    @function   AUEventListenerRemoveEventType
    @abstract   Stop delivering a particular type of events to a listener.
    @param      inListener
                    The parameter listener to stop receiving events.
    @param      inObject
                    The object which is no longer interested in the value of the parameter.
    @param      inEvent
                    The type of event to stop listening for.
    @result     An OSStatus error code.
}
function AUEventListenerRemoveEventType( inListener: AUEventListenerRef; inObject: UnivPtr {__nullable}; const (*var*) inEvent: AudioUnitEvent ): OSStatus; external name '_AUEventListenerRemoveEventType';
(* API_AVAILABLE(macos(10.3), ios(6.0), watchos(2.0), tvos(9.0)) *)           

{!
    @function   AUEventListenerNotify
    @abstract   Deliver an AudioUnitEvent to all listeners registered to receive it.
    @discussion This is only to be used for notifications about parameter changes (and gestures).
                It can not be used for notifying changes to property values as these are 
                internal to an audio unit and should not be issued outside of the audio unit itself.
    @param      inSendingListener
                    A parameter listener generating the change and which does not want to
                    receive a callback as a result of it. May be NULL.
    @param      inSendingObject
                    The object generating the change and which does not want to receive a
                    callback as a result of it. NULL is treated specially when inListener is
                    non-null; it signifies that none of the specified listener's objects will
                    receive notifications.
    @param      inEvent
                    The event to be delivered.
    @result     An OSStatus error code.
}
function AUEventListenerNotify( inSendingListener: AUEventListenerRef {__nullable}; inSendingObject: UnivPtr {__nullable}; const (*var*) inEvent: AudioUnitEvent ): OSStatus; external name '_AUEventListenerNotify';
(* API_AVAILABLE(macos(10.3), ios(6.0), watchos(2.0), tvos(9.0)) *)
                                    
{ ============================================================================= }

{!
    @functiongroup  Parameter value utilities
}


{!
    @function   AUParameterValueFromLinear
    @abstract   Converts a linear value to a parameter value according to the parameter's units.
    
    @param      inLinearValue
                    The linear value (0.0-1.0) to convert.
    @param      inParameter
                    The parameter, including its Audio Unit, that will define the conversion of
                    the supplied linear value to a value that is natural to that parameter.
    @result
                The converted parameter value, in the parameter's natural units.
    @discussion 
}
function AUParameterValueFromLinear( inLinearValue: Float32 { // 0-1 }; const (*var*) inParameter: AudioUnitParameter ): AudioUnitParameterValue; external name '_AUParameterValueFromLinear';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)

{!
    @function   AUParameterValueToLinear
    @abstract   Converts a parameter value to a linear value according to the parameter's units.
    
    @param      inParameterValue
                    The value in the natural units of the specified parameter.
        
    @param      inParameter
                    The parameter, including its Audio Unit, that will define the conversion of
                    the supplied parameter value to a corresponding linear value.
    @result
                A number 0.0-1.0.
    @discussion 
}
function AUParameterValueToLinear( inParameterValue: AudioUnitParameterValue; const (*var*) inParameter: AudioUnitParameter ): Float32; external name '_AUParameterValueToLinear';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)
                                        // returns 0-1

{!
    @function   AUParameterFormatValue
    @abstract   Format a parameter value into a string.
    @param      inParameterValue
                    The parameter value to be formatted.
    @param      inParameter
                    The Audio Unit, scope, element, and parameter whose value this is.
    @param      inTextBuffer
                    The character array to receive the formatted text.  Should be at least 32
                    characters.
    @param      inDigits
                    The resolution of the string (see example above).
    @result
                <tt>inTextBuffer</tt>
    @discussion 
        Formats a floating point value into a string.  Computes a power of 10 to which the value
        will be rounded and displayed as follows:  if the the parameter is logarithmic (Hertz),
        the number of significant digits is inDigits - pow10(inParameterValue) + 1.  Otherwise,
        it is inDigits - pow10(maxValue - minValue) + 1.

        Example for inDigits=3:
<pre>
        pow10   range           digits after decimal place display
        -2      .0100-.0999     4
        -1      .100-.999       3
        0       1.00-9.99       2
        1       10.0-99.9       1
        2       100-999         0
        3       1000-9990       -1
        4       10000-99900     -2</pre>
}                              
function AUParameterFormatValue( inParameterValue: Float64; const (*var*) inParameter: AudioUnitParameter; var inTextBuffer: char; inDigits: UInt32 ): CStringPtr; external name '_AUParameterFormatValue';
(* API_AVAILABLE(macos(10.2), ios(6.0), watchos(2.0), tvos(9.0)) *)


//CF_ASSUME_NONNULL_END
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
