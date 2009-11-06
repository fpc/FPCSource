{
     File:      AudioUnit/AudioComponent.h
 
     Contains:  API for finding and opening audio components
  
     Copyright: (c) 2007 - 2008 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
}
{	  Pascal Translation:  Gorazd Krosl <gorazd_1957@yahoo.ca>, October 2009 }

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

unit AudioComponents;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := TFALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes,CoreAudioTypes,Components,CFBase;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


//=====================================================================================================================
////#pragma mark Overview

{!
    @header AudioComponent

	@discussion
	This file defines a collection of APIs that are designed to be used when finding and opening audio components
	(such as audio units, audio codecs, audio file components).
	
	Traditionally, the ComponentManager has been used for the registration, discovery, and packaging of these loadable
	code modules. However, this may not always be the case. In order to provide an API that will be supported going forward
	from SnowLeopard (10.6) and iPhone 2.0, it is advised that applications use the AudioComponent APIs to find and load(open)
	audio components such as audio units. 
	
	The type "AudioComponent" or "AudioComponentInstance" should be seen and used as a distinct type from the Component Manager
	types of "Component" and "ComponentInstance". It is not the case that it is safe to assume a direct cast is compatible between
	this type and the other.
	
	The AudioComponent APIs provide a general, and mostly complete service for doing the basic task of finding and using audio components. 
	If however, you find that you need functionality that is not provided by this API that was previously supported by the ComponentMgr then
	you should use the ComponentMgr APIs exclusively to find and open the Component Mgr components. In other words, you should not assume that
	an audio component instance is always interchangeable with the existing Component Instance type.
}


//=====================================================================================================================
//#pragma mark Data Types

{!
	@struct			AudioComponentDescription
	@discussion		A structure used to describe the unique and identifying IDs of an audio component 
	@field			componentType
						A unique 4-byte code indentifying the generic type of an audio component
	@field			componentSubType
						the particular flavor of this instance
	@field			componentManufacturer
						vendor identification
	@field			componentFlags
						must be set to zero unless a known specific value is requested
	@field			componentFlagsMask
						must be set to zero unless a known specific value is requested
}
//#pragma pack(push, 4)

type
	AudioComponentDescription = record
		componentType: OSType;
		componentSubType: OSType;
		componentManufacturer: OSType;
		componentFlags: UInt32;
		componentFlagsMask: UInt32;
	end;
	AudioComponentDescriptionPtr = ^AudioComponentDescription;
//#pragma pack(pop)


{!
	@typedef		AudioComponent
	@abstract		The type used to represent a class of particular audio components
	@discussion		An audio component is usually found through a search and is then uniquely identified by the triple of
					an audio component's type, subtype and manufacturer. 
					
					It can have properties associated with it (such as a name, a version).
					
					It is then used as a factory (like a class in an OO language) from which to create particular instances. The
					instances are used to do the actualy work.
					
					For example:
					'aufx', 'dely', 'appl' describes the digital delay, audio unit effect from Apple Inc. You can find this component
					through searching explicitly for the audio component that matches this pattern (this is an unique identifier - there is
					only one match to this triple ID). Then once found, the Apple delay effect audio unit can be created from its audio component
					and used to apply that effect to an audio signal. While the audio component is a singleton, you can of course create as many
					instances of a given audio component that you need to do the work you need to do.
}
type
	AudioComponent = ^SInt32; { an opaque type }

{!
	@typedef		AudioComponentInstance
	@abstract		The type used to represent an instance of a particular audio component
	@discussion		An audio component instance is created from its factory/producer audio component. It is the body of code that does the work.
	
					A special note: While on the desktop this is typedef'd to a ComponentInstanceRecord *, you should not always assume that this
					will always be compatible and usable with ComponentMgr calls. 
}
//#if TARGET_OS_IPHONE
{$ifc TARGET_OS_IPHONE}
type
	AudioComponentInstance = ^SInt32; { an opaque type }
//#else
{$elsec}
type
	AudioComponentInstance = ComponentInstanceRecordPtr;
//#endif
{$endc}


//=====================================================================================================================
//#pragma mark Functions


{!
    @function       AudioComponentFindNext
    @abstract       find an audio component
    @discussion     This function is used to find an audio component that is the closest match to the provide values.
    @param          inComponent
						If NULL, then the search starts from the beginning until an audio component is found that matches
						the description provided by inDesc
						If not-NULL, then the search starts (continues) from the previously found audio component specified
						by inComponent, and will return the nextfound audio component
	@param			inDesc
						The type, subtype and manufacturer fields are used to specify the audio component to search for.
						A value of 0 (zero) for any of these fiels is a wildcard, so the first match found is returned
	@result			An audio component that matches the search parameters, or NULL if none found
}
function AudioComponentFindNext( inComponent: AudioComponent; const (*var*) inDesc: AudioComponentDescription ): AudioComponent; external name '_AudioComponentFindNext';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentCount
    @abstract       number of audio components
    @discussion     the number of AudioComponenents that match the specified AudioComponentDescription. 
	@param			inDesc
						The type, subtype and manufacturer fields are used to specify the audio components to count
						A value of 0 (zero) for any of these fiels is a wildcard, so will match any value for this field
	@result			a UInt32. 0 (zero) means no audio components were found that matched the search parameters.
}
function AudioComponentCount( const (*var*) inDesc: AudioComponentDescription ): UInt32; external name '_AudioComponentCount';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentCopyName
    @abstract       used to retrieve the name of an audio component
    @discussion     the name of an audio component
	@param			inComponent
						the audio component (must not be NULL)
	@param			outName
						a CFString that is the name of the audio component. This string should be released by the caller.
    @result			an OSStatus result code.
}
function AudioComponentCopyName( inComponent: AudioComponent; var outName: CFStringRef ): OSStatus; external name '_AudioComponentCopyName';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentGetDescription
    @abstract       used to retrieve the audio component's description
    @discussion     will return the fully specified audio component description for the provided audio component
	@param			inComponent
						the audio component (must not be NULL)
	@param			outDesc
						the audio component description for the specified audio component
    @result			an OSStatus result code.
}
function AudioComponentGetDescription( inComponent: AudioComponent; var outDesc: AudioComponentDescription ): OSStatus; external name '_AudioComponentGetDescription';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentGetVersion
    @abstract       used to retrieve the audio component's version
    @discussion     will return the version number of a given audio component
	@param			inComponent
						the audio component (must not be NULL)
	@param			outVersion
						the audio component's version in the form of 0xMMMMmmDD (Major, Minor, Dot)
    @result			an OSStatus result code.
}
function AudioComponentGetVersion( inComponent: AudioComponent; var outVersion: UInt32 ): OSStatus; external name '_AudioComponentGetVersion';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentInstanceNew
    @abstract       used to create an audio component instance
    @discussion     this call creates an instance of a given audio component. The audio component instance is the object
					that does all of the work, whereas the audio component is the way that a program finds and then makes this object
					to do this work. For example, an audio unit is a type of audio component instance, so to use an audio unit, one
					finds its audio component, and then creates a new instance of that component. This instance is then used
					to process, mix, synthesise, etc the audio tasks for which it was designed.
	@param			inComponent
						the audio component (must not be NULL)
	@param			outInstance
						the audio component instance
    @result			an OSStatus result code.
}
function AudioComponentInstanceNew( inComponent: AudioComponent; var outInstance: AudioComponentInstance ): OSStatus; external name '_AudioComponentInstanceNew';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentInstanceDispose
    @abstract       used to dispose of an audio component instance
    @discussion     this call will dispose the audio component instance that was created with the New call. It will deallocate any resources
					that the instance was using.
	@param			inInstance
						the audio component instance to dispose (must not be NULL)
    @result			an OSStatus result code.
}
function AudioComponentInstanceDispose( inInstance: AudioComponentInstance ): OSStatus; external name '_AudioComponentInstanceDispose';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

// retrieves the class object assosciated with the instance
{!
    @function       AudioComponentInstanceGetComponent
    @abstract       used to retrieve the audio component from its instance
    @discussion     allows the application at any time to retrieve the audio component that is the factory object of a given instance 
					(ie, the audio component that was used to create the instnace in the first place). This allows the application to
					retrieve general information about a particular audio component (its name, version, etc) when one just has an
					audio component instance to work with 
	@param			inInstance
						the audio component instance (must not be NULL, and instance must be valid - that is, not disposed)
    @result			a valid audio component or NULL if no component was found.
}
function AudioComponentInstanceGetComponent( inInstance: AudioComponentInstance ): AudioComponent; external name '_AudioComponentInstanceGetComponent';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_2_0) *)

{!
    @function       AudioComponentInstanceCanDo
    @discussion     this call is used to determine if an audio component instance implements a particular component API call
					as signified by the specified selector identifier token.
	@param			inInstance
						the audio component instance
	@param			inSelectorID
						a number to signify the audio component API (component selector) as appropriate for the instance's component type.
    @result			a boolean
}
function AudioComponentInstanceCanDo( inInstance: AudioComponentInstance; inSelectorID: SInt16 ): Boolean; external name '_AudioComponentInstanceCanDo';
(* __OSX_AVAILABLE_STARTING(__MAC_10_6,__IPHONE_3_0) *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
