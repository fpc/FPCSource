{
     File:       QuickTime/HIMovieView.h
 
     Contains:   HIView-based movie playback
 
     Version:    QuickTime 7.7.1
 
     Copyright:  © 2004-2012 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{  Pascal Translation:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit HIMovieView;
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
uses MacTypes,HIGeometry,HIObject,HIView,Movies;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{ QuickTime is not available to 64-bit clients }

{$ifc not TARGET_CPU_64}


{
 *  kHIMovieViewClassID
 *  
 *  Summary:
 *    Class ID for HIMovieView
 }
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kHIMovieViewClassID CFSTRP('com.apple.quicktime.HIMovieView')}
{$endc}

{
 *  Summary:
 *    HIMovieView Event class
 }
const
{
   * Events related to movie views.
   }
	kEventClassMovieView = FourCharCode('moov');


{
 *  kEventClassMovieView / kEventMovieViewOptimalBoundsChanged
 *  
 *  Summary:
 *    Sent when the movie size changes.
 *  
 *  Parameters:
 *    
 *    --> kEventParamDirectObject (in, typeControlRef)
 *          The movie view whose size is changing.
 *    
 *    --> kEventParamControlOptimalBounds (in, typeHIRect)
 *          The new optimal bounds.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 }
const
	kEventMovieViewOptimalBoundsChanged = 1;


{
 *  Summary:
 *    HIMovieView attributes
 }
const
{
   * No attributes
   }
	kHIMovieViewNoAttributes = 0;

  {
   * Movie controller bar is visible below visual content
   }
	kHIMovieViewControllerVisibleAttribute = 1 shl 0;

  {
   * Automatically call MCIdle() at appropriate times
   }
	kHIMovieViewAutoIdlingAttribute = 1 shl 1;

  {
   * Accepts keyboard focus
   }
	kHIMovieViewAcceptsFocusAttribute = 1 shl 2;

  {
   * Movie editing enabled
   }
	kHIMovieViewEditableAttribute = 1 shl 3;

  {
   * Handles editing HI commands such as cut, copy and paste
   }
	kHIMovieViewHandleEditingHIAttribute = 1 shl 4;

  {
   * Combination of kHIMovieViewControllerVisibleAttribute,
   * kHIMovieViewAutoIdlingAttribute, and
   * kHIMovieViewAcceptsFocusAttribute
   }
	kHIMovieViewStandardAttributes = kHIMovieViewControllerVisibleAttribute or kHIMovieViewAutoIdlingAttribute or kHIMovieViewAcceptsFocusAttribute;

{
 *  HIMovieViewCreate()
 *  
 *  Summary:
 *    Creates an HIMovieView object
 *  
 *  Discussion:
 *    If successful, the created view will have a single retain count.
 *  
 *  Parameters:
 *    
 *    inMovie:
 *      [in]  Initial movie to view, may be NULL
 *    
 *    inAttributes:
 *      [in]  Initial HIMovieView attributes
 *    
 *    outMovieView:
 *      [out] Points to variable to receive new HIMovieView
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewCreate( inMovie: Movie; inAttributes: OptionBits; var outMovieView: HIViewRef ): OSStatus; external name '_HIMovieViewCreate';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewGetMovie()
 *  
 *  Summary:
 *    Returns the view's current movie.
 *  
 *  Parameters:
 *    
 *    inView:
 *      [in]  The HIMovieView
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewGetMovie( inView: HIViewRef ): Movie; external name '_HIMovieViewGetMovie';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewSetMovie()
 *  
 *  Summary:
 *    Sets the view's current movie.
 *  
 *  Parameters:
 *    
 *    inView:
 *      [in]  The HIMovieView
 *    
 *    inMovie:
 *      [in]  The new movie to display
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewSetMovie( inView: HIViewRef; inMovie: Movie ): OSStatus; external name '_HIMovieViewSetMovie';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewGetAttributes()
 *  
 *  Summary:
 *    Returns the view's current attributes.
 *  
 *  Parameters:
 *    
 *    inView:
 *      [in]  The HIMovieView
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewGetAttributes( inView: HIViewRef ): OptionBits; external name '_HIMovieViewGetAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewChangeAttributes()
 *  
 *  Summary:
 *    Changes the views attributes.
 *  
 *  Discussion:
 *    Setting an attribute takes precedence over clearing the attribute.
 *  
 *  Parameters:
 *    
 *    inView:
 *      [in]  The HIMovieView
 *    
 *    inAttributesToSet:
 *      [in]  Attributes to set
 *    
 *    inAttributesToClear:
 *      [in]  Attributes to clear
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewChangeAttributes( inView: HIViewRef; inAttributesToSet: OptionBits; inAttributesToClear: OptionBits ): OSStatus; external name '_HIMovieViewChangeAttributes';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewGetMovieController()
 *  
 *  Summary:
 *    Returns the view's current movie controller.
 *  
 *  Parameters:
 *    
 *    inView:
 *      [in]  The HIMovieView
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewGetMovieController( inView: HIViewRef ): MovieController; external name '_HIMovieViewGetMovieController';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewGetControllerBarSize()
 *  
 *  Summary:
 *    Returns the size of the visible movie controller bar.
 *  
 *  Parameters:
 *    
 *    inView:
 *      [in]  The HIMovieView
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewGetControllerBarSize( inView: HIViewRef ): HISize; external name '_HIMovieViewGetControllerBarSize';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewPlay()
 *  
 *  Summary:
 *    Convenience routine to play the view's current movie.
 *  
 *  Discussion:
 *    If the movie is already playing, this function does nothing.
 *  
 *  Parameters:
 *    
 *    movieView:
 *      [in]  The movie view.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewPlay( movieView: HIViewRef ): OSStatus; external name '_HIMovieViewPlay';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{
 *  HIMovieViewPause()
 *  
 *  Summary:
 *    Convenience routine to pause the view's current movie.
 *  
 *  Discussion:
 *    If the movie is already paused, this function does nothing.
 *  
 *  Parameters:
 *    
 *    movieView:
 *      [in]  The movie view.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 (or QuickTime 7.0) and later in QuickTime.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function HIMovieViewPause( movieView: HIViewRef ): OSStatus; external name '_HIMovieViewPause';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{$endc} {TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
