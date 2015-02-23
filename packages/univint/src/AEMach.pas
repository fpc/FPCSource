{
     File:       AE/AEMach.h
 
     Contains:   AppleEvent over mach_msg interfaces
 
    
 
     Copyright:  © 2000-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, September 2012 }
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

unit AEMach;
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
uses MacTypes,MacOSXPosix,AEDataModel;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{-
 * AE Mach API --
 *
 * AppleEvents on OS X are implemented in terms of mach messages.
 * To facilitate writing server processes that can send and receive
 * AppleEvents, the following APIs are provided.
 *
 * AppleEvents are directed to a well known port uniquely tied to a
 * process.  The AE framework will discover this port based on the
 * keyAddressAttr of the event (as specifed in AECreateAppleEvent by
 * the target parameter.)  If a port cannot be found,
 * procNotFound (-600) will be returned on AESend.
 *
 * Of note is a new attribute for an AppleEvent, keyReplyPortAttr.
 * This specifies the mach_port_t to which an AppleEvent reply
 * should be directed.  By default, replies are sent to the
 * processes' registered port where they are culled from the normal  
 * event stream if there is an outstanding AESend + kAEWaitReply.
 * But it may be desirable for a client to specify their own port to
 * receive queud replies.
 *
 * In the case of AESendMessage with kAEWaitReply specified, an 
 * anonymous port will be used to block until the reply is received.
 *
 * Not supplied is a convenience routine to block a server and
 * process AppleEvents.  This implementation will be detailed in a
 * tech note.
 *
 * In general, the AppleEvent APIs are thread safe, but the mechanism
 * of their delivery (AEProcessAppleEvent, AEResumeTheCurrentEvent)
 * are not.  If you're attemping to write a thread safe server, you
 * should avoid AppleEvent routines that don't explicitly state their
 * thread safety.
 *
 *}
const
	keyReplyPortAttr = FourCharCode('repp');

{ typeReplyPortAttr was misnamed and is deprecated; use keyReplyPortAttr instead. }
const
	typeReplyPortAttr = keyReplyPortAttr;

{
 *  AEGetRegisteredMachPort()
 *  
 *  Discussion:
 *    Return the mach_port_t that was registered by the AppleEvent
 *    framework for this process.  This port is considered public, and
 *    will be used by other applications to target your process.  You
 *    are free to use this mach_port_t to add to a port set, if and
 *    only if, you are not also using routines from HIToolbox.  In that
 *    case, HIToolbox retains control of this port and AppleEvents are
 *    dispatched through the main event loop.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function AEGetRegisteredMachPort: mach_port_t; external name '_AEGetRegisteredMachPort';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AEDecodeMessage()
 *  
 *  Discussion:
 *    Decode a mach_msg into an AppleEvent and its related reply.  (The
 *    reply is set up from fields of the event.)  You can call this
 *    routine if you wish to dispatch or handle the event yourself.  To
 *    return a reply to the sender, you should call:
 *     AESendMessage(reply, NULL, kAENoReply, kAENormalPriority,
 *    kAEDefaultTimeout);
 *    If this message is a reply, the 'reply' parameter will be
 *    initialized to ( typeNull, 0 ), and the 'event' parameter will be
 *    the AppleEvent reply with a event class attribute of
 *    typeAppleEvent, class typeAppleEventReply:
 *    The contents of the header are invalid after this call.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    header:
 *      The incoming mach message to be dispatched
 *    
 *    event:
 *      The AppleEvent to decode the message in header into
 *    
 *    reply:
 *      The AppleEvent reply is decoded into reply
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function AEDecodeMessage( var header: mach_msg_header_t; var event: AppleEvent; reply: AppleEventPtr { can be NULL } ): OSStatus; external name '_AEDecodeMessage';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AEProcessMessage()
 *  
 *  Discussion:
 *    Decodes and dispatches an event to an event handler.  Handles
 *    packaging and returning the reply to the sender.
 *    The contents of the header are invalid after this call.
 *  
 *  Mac OS X threading:
 *    Not thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    header:
 *      The incoming mach message to be dispatched.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function AEProcessMessage( var header: mach_msg_header_t ): OSStatus; external name '_AEProcessMessage';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)


{
 *  AESendMessage()
 *  
 *  Discussion:
 *    Send an AppleEvent to a target process.  If the target is the
 *    current process (as specified by using typeProcessSerialNumber of
 *    ( 0, kCurrentProcess ) it is dispatched directly to the
 *    appropriate event handler in your process and not serialized.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    event:
 *      The event to be sent
 *    
 *    reply:
 *      The reply for the event, if non-NULL
 *    
 *    sendMode:
 *      The mode to send the event
 *    
 *    timeOutInTicks:
 *      The timeout for sending the event, in ticks.  If 0, there is no
 *      timeout.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function AESendMessage( const (*var*) event: AppleEvent; reply: AppleEventPtr { can be NULL }; sendMode: AESendMode; timeOutInTicks: SIGNEDLONG ): OSStatus; external name '_AESendMessage';
(* __OSX_AVAILABLE_STARTING( __MAC_10_0, __IPHONE_NA ) *)

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
