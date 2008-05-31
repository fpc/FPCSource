{
     File:       AEMach.p
 
     Contains:   AppleEvent over mach_msg interfaces
 
     Version:    Technology: For Mac OS X
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

unit AEMach;
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
uses MacTypes,MixedMode,AEDataModel;


{$ALIGN MAC68K}

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
 * Of note is a new attribute for an AppleEvent, typeReplyPortAttr.
 * This specifies the mach_port_t to which an AppleEvent reply
 * should be directed.  By default, replies are sent to the
 * processes registered port where they are culled from the normal  
 * event stream if there is an outstanding AESend + kAEWaitReply.
 * But it may be desirable for a client to specify their own port to
 * receive quued replies.
 * (In the case of AESendMessage with kAEWaitReply specified, an 
 * anonymous port will be used to block until the reply is received.)
 *
 * Not supplied is a convenience routine to block a server and
 * process AppleEvents.  This implementation will be detailed in a
 * tech note.
 *}

const
	typeReplyPortAttr			= FourCharCode('repp');

{$ifc TARGET_RT_MAC_MACHO}
{$ifc not undefined _MACH_MESSAGE_H_}
	{	-
	 * Return the mach_port_t that was registered with the bootstrap
	 * server for this process.  This port is considered public, and
	 * will be used by other applications to target your process.  You
	 * are free to use this mach_port_t to add to a port set, if and
	 * only if, you are not also using routines from HIToolbox.  In that
	 * case, HIToolbox retains control of this port and AppleEvents are
	 * dispatched through the main event loop.  
	 *	}
	{
	 *  AEGetRegisteredMachPort()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         in version 10.0 and later
	 	}
function AEGetRegisteredMachPort: mach_port_t; external name '_AEGetRegisteredMachPort';

{-
 * Decode a mach_msg into an AppleEvent and its related reply.  (The
 * reply is set up from fields of the event.)  You can call this
 * routine if you wish to dispatch or handle the event yourself.  To
 * return a reply to the sender, you should call:
 *
 *  AESendMessage(reply, NULL, kAENoReply, kAENormalPriority, kAEDefaultTimeout);
 *
 * The contents of the header are invalid after this call.  
 *}
{
 *  AEDecodeMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function AEDecodeMessage(var header: mach_msg_header_t; var event: AppleEvent; reply: AppleEventPtr): OSStatus; external name '_AEDecodeMessage';

{-
 * Decodes and dispatches an event to an event handler.  Handles
 * packaging and returning the reply to the sender.
 *
 * The contents of the header are invalid after this call.
 *}
{
 *  AEProcessMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function AEProcessMessage(var header: mach_msg_header_t): OSStatus; external name '_AEProcessMessage';

{-
 * Send an AppleEvent to a target process.  If the target is the
 * current process (as specified by using typeProcessSerialNumber of
 * ( 0, kCurrentProcess ) it is dispatched directly to the
 * appropriate event handler in your process and not serialized.
 *}
{
 *  AESendMessage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         in version 10.0 and later
 }
function AESendMessage(const (*var*) event: AppleEvent; reply: AppleEventPtr; sendMode: AESendMode; timeOutInTicks: SInt32): OSStatus; external name '_AESendMessage';

{$endc}  {_MACH_MESSAGE_H_}
{$endc}  {TARGET_RT_MAC_MACHO}

{$ALIGN MAC68K}


end.
