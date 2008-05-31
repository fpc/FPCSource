(*
     File:       OSServices/SystemSound.h
 
     Contains:   SystemSound include file
 
     Version:    OSServices-97~320
 
     Copyright:  © 2000-2005 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
*)
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2005 }
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

unit SystemSound;
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
uses MacTypes,Files,CFBase,CFRunLoop;
{$ALIGN POWER}


(* ================================================================================ *)
(* Errors                                                                           *)
(* ================================================================================ *)
const
	kSystemSoundNoError = 0;
	kSystemSoundUnspecifiedError = -1500;
	kSystemSoundClientTimedOutError = -1501;

(* ================================================================================ *)
(* Types                                                                            *)
(* ================================================================================ *)
type
	SystemSoundActionID = UInt32;
type
	SystemSoundCompletionProcPtr = function( actionID: SystemSoundActionID; userData: UnivPtr ): OSStatus;
type
	SystemSoundCompletionUPP = SystemSoundCompletionProcPtr;
(*
 *  NewSystemSoundCompletionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 *)
function NewSystemSoundCompletionUPP( userRoutine: SystemSoundCompletionProcPtr ): SystemSoundCompletionUPP; external name '_NewSystemSoundCompletionUPP'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

(*
 *  DisposeSystemSoundCompletionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 *)
procedure DisposeSystemSoundCompletionUPP( userUPP: SystemSoundCompletionUPP ); external name '_DisposeSystemSoundCompletionUPP'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

(*
 *  InvokeSystemSoundCompletionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.3 and later
 *    Non-Carbon CFM:   available as macro/inline
 *)
function InvokeSystemSoundCompletionUPP( actionID: SystemSoundActionID; userData: UnivPtr; userUPP: SystemSoundCompletionUPP ): OSStatus; external name '_InvokeSystemSoundCompletionUPP'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

(* ================================================================================ *)
(* Public APIs                                                                      *)
(* ================================================================================ *)
(*
 *  AlertSoundPlay()
 *  
 *  Summary:
 *    Play an Alert Sound
 *  
 *  Discussion:
 *    Play the user's current alert sound, interrupting any previously
 *    playing alert sound.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
procedure AlertSoundPlay; external name '_AlertSoundPlay'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


(*
 *  AlertSoundPlayCustomSound()
 *  
 *  Summary:
 *    Play a User designated Alert Sound
 *  
 *  Discussion:
 *    Play a sound, designated by a SystemSoundActionID, with the
 *    behavior of AlertSoundPlay().
 *  
 *  Parameters:
 *    
 *    inAction:
 *      A SystemSoundActionID indicating the desired Sound to be played
 *      with AlertSound behavior.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
procedure AlertSoundPlayCustomSound( inAction: SystemSoundActionID ); external name '_AlertSoundPlayCustomSound'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


(*
 *  SystemSoundPlay()
 *  
 *  Summary:
 *    Play a System Sound
 *  
 *  Discussion:
 *    Immediately play the sound designated by actionID. Use for one
 *    time actions that do not require a duration or modification
 *    during playback. Sustain loops in the sound will be ignored.
 *  
 *  Parameters:
 *    
 *    inAction:
 *      A SystemSoundActionID indicating the desired System Sound to be
 *      played.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
procedure SystemSoundPlay( inAction: SystemSoundActionID ); external name '_SystemSoundPlay'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


(*
 *  SystemSoundGetActionID()
 *  
 *  Summary:
 *    Create a 'custom' System Sound by providing an audio file.
 *  
 *  Discussion:
 *    If the user wants to add a sound that can be played via
 *    SystemSoundPlay(), an FSRef for an audio file can be passed and a
 *    SystemSoundActionID, which can be passed to SystemSoundPlay() or
 *    AlertSoundPlayCustomSound(), will be returned. It is important
 *    that SystemSoundRemoveActionID() be called when the action is no
 *    longer needed by the client application so the System Sound
 *    Server can release any resources dedicated to the returned action
 *    id.
 *  
 *  Parameters:
 *    
 *    userFile:
 *      An const FSRef * for the audio file to be used as a System
 *      Sound. Any audio file supported by the AudioFile APIs in the
 *      AudioToolbox framework may be used.
 *    
 *    outAction:
 *      If successful, a SystemSoundActionID will be returned, which in
 *      turn can be passed to SystemSoundPlay().
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
function SystemSoundGetActionID( const (*var*) userFile: FSRef; var outAction: SystemSoundActionID ): OSStatus; external name '_SystemSoundGetActionID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


(*
 *  SystemSoundRemoveActionID()
 *  
 *  Summary:
 *    Remove a 'custom' System Sound.
 *  
 *  Discussion:
 *    If the user no longer needs to use the custom system sound that
 *    was created via SystemSoundGetActionID, this function should be
 *    called so the SystemSoundServer can release resources that are no
 *    longer needed.
 *  
 *  Parameters:
 *    
 *    inAction:
 *      A SystemSoundActionID indicating the desired System Sound to be
 *      removed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
function SystemSoundRemoveActionID( inAction: SystemSoundActionID ): OSStatus; external name '_SystemSoundRemoveActionID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


(*
 *  SystemSoundSetCompletionRoutine()
 *  
 *  Summary:
 *    Call the provided Completion Routine when the provided
 *    SystemSoundActionID finishes playing in the server.
 *  
 *  Discussion:
 *    Once set, the System Sound Server will send a message to the
 *    System Sound Client indicating which SystemSoundActionID has
 *    finished playing.
 *  
 *  Parameters:
 *    
 *    inAction:
 *      The SystemSoundActionID that the completion routine will be
 *      associated with.
 *    
 *    inRunLoop:
 *      A CFRunLoopRef indicating the desired run loop the completion
 *      routine should be run on. Pass NULL for the main run loop.
 *    
 *    inRunLoopMode:
 *      A CFStringRef indicating the run loop mode for the runloop mode
 *      for the runloop where the completion routine will be executed.
 *      Pass NULL to use kCFRunLoopDefaultMode.
 *    
 *    inCompletionRoutine:
 *      A SystemSoundCompletionProc for the completion routine proc to
 *      be called when the provided SystemSoundActionID has completed
 *      playing in the server.
 *    
 *    inUserData:
 *      A void * to pass user data to the completion routine.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
function SystemSoundSetCompletionRoutine( inAction: SystemSoundActionID; inRunLoop: CFRunLoopRef; inRunLoopMode: CFStringRef; inCompletionRoutine: SystemSoundCompletionUPP; inUserData: UnivPtr ): OSStatus; external name '_SystemSoundSetCompletionRoutine'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


(*
 *  SystemSoundRemoveCompletionRoutine()
 *  
 *  Summary:
 *    Remove the Completion Routine being used for the provided
 *    SystemSoundActionID.
 *  
 *  Discussion:
 *    To be called when it is no longer desired for the Completion
 *    Routine to be called when a System Sound action has finished
 *    playing.
 *  
 *  Parameters:
 *    
 *    inAction:
 *      A SystemSoundActionID that currently has an associated
 *      completion routine.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 *)
procedure SystemSoundRemoveCompletionRoutine( inAction: SystemSoundActionID ); external name '_SystemSoundRemoveCompletionRoutine'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


end.
