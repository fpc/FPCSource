{
 * Copyright (c) 2000-2003 Apple Computer, Inc. All Rights Reserved.
 * 
 * The contents of this file constitute Original Code as defined in and are
 * subject to the Apple Public Source License Version 1.2 (the 'License').
 * You may not use this file except in compliance with the License. Please obtain
 * a copy of the License at http://www.apple.com/publicsource and read it before
 * using this file.
 * 
 * This Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY of ANY KIND, EITHER EXPRESS
 * OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT
 * LIMITATION, ANY WARRANTIES of MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT. Please see the License for the
 * specific language governing rights and limitations under the License.
 }
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


{
 *  AuthSession.h
 *  AuthSession - APIs for managing login, authorization, and security Sessions.
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

unit AuthSession;
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
uses MacTypes,Authorization;
{$ALIGN MAC68K}
{!
	@header AuthSession

	The Session API provides specialized applications access to Session management and inquiry
    functions. This is a specialized API that should not be of interest to most people.
	
	The Security subsystem separates all processes into Security "sessions". Each process is in
	exactly one session, and session membership inherits across fork/exec. Sessions form boundaries
	for security-related state such as authorizations, keychain lock status, and the like.
	Typically, each successful login (whether graphical or through ssh & friends) creates
	a separate session. System daemons (started at system startup) belong to the "root session"
	which has no user nor graphics access.
    
	Sessions are identified with SecuritySessionIds. A session has a set of attributes
	that are set on creation and can be retrieved with SessionGetInfo().
	
	There are similar session concepts in the system, related but not necessarily
	completely congruous. In particular, graphics sessions track security sessions
	(but only for graphic logins).
}


{!
	@typedef SecuritySessionId
	These are externally visible identifiers for authorization sessions.
        Different sessions have different identifiers; beyond that, you can't
        tell anything from these values.
    SessionIds can be compared for equality as you'd expect, but you should be careful
        to use attribute bits wherever appropriate.
}
type
	SecuritySessionId = UInt32;
	SecuritySessionIdPtr = ^SecuritySessionId;


{!
    @enum SecuritySessionId
    Here are some special values for SecuritySessionId. You may specify those
        on input to SessionAPI functions. They will never be returned from such
        functions.
}
const
	noSecuritySession                      = 0;     { definitely not a valid SecuritySessionId }
	callerSecuritySession = -1;     { the Session I (the caller) am in }


{!
    @enum SessionAttributeBits
    Each Session has a set of attribute bits. You can get those from the
        SessionGetInfo API function.
 }
type
	SessionAttributeBits = UInt32;
	SessionAttributeBitsPtr = ^SessionAttributeBits;
 
const
    sessionIsRoot                          = $0001; { is the root session (startup/system programs) }
    sessionHasGraphicAccess                = $0010; { graphic subsystem (CoreGraphics et al) available }
    sessionHasTTY                          = $0020; { /dev/tty is available }
    sessionIsRemote                        = $1000; { session was established over the network }

	// the following bits are used internally; do not try to set them
    sessionWasInitialized                  = $8000;  { session has been set up by its leader }


{!
    @enum SessionCreationFlags
    These flags control how a new session is created by SessionCreate.
        They have no permanent meaning beyond that.
 }
type
	SessionCreationFlags = UInt32;
	SessionCreationFlagsPtr = ^SessionCreationFlags;
 
const
    sessionKeepCurrentBootstrap             = $8000; { caller has allocated sub-bootstrap (expert use only) }
 
 
{!
	@enum SessionStatus
	Error codes returned by AuthSession API.
    Note that the AuthSession APIs can also return Authorization API error codes.
}
const
	errSessionSuccess                       = 0;      { all is well }
	errSessionInvalidId                     = -60500; { invalid session id specified }
	errSessionInvalidAttributes             = -60501; { invalid set of requested attribute bits }
	errSessionAuthorizationDenied           = -60502; { you are not allowed to do this }

	errSessionInternal                      = errAuthorizationInternal;	{ internal error }
	errSessionInvalidFlags                  = errAuthorizationInvalidFlags; { invalid flags/options }


{!
    @function SessionGetInfo
    Obtain information about a session. You can ask about any session whose
	identifier you know. Use the callerSecuritySession constant to ask about
	your own session (the one your process is in).

    @param session (input) The Session you are asking about. Can be one of the
        special constants defined above.
	
	@param sessionId (output/optional) The actual SecuritySessionId for the session you asked about.
        Will never be one of those constants.
        
    @param attributes (output/optional) Receives the attribute bits for the session.

    @result An OSStatus indicating success (noErr) or an error cause.
    
    errSessionInvalidId -60500 Invalid session id specified

}
function SessionGetInfo( session: SecuritySessionId; sessionId: SecuritySessionIdPtr; attributes: SessionAttributeBitsPtr ): OSStatus; external name '_SessionGetInfo';
    

{!
    @function SessionCreate
    This (very specialized) function creates a security session.
	Upon completion, the new session contains the calling process (and none other).
	You cannot create a session for someone else, and cannot avoid being placed
	into the new session. This is (currently) the only call that changes a process's
	session membership.
    By default, a new bootstrap subset port is created for the calling process. The process
    acquires this new port as its bootstrap port, which all its children will inherit.
    If you happen to have created the subset port on your own, you can pass the
    sessionKeepCurrentBootstrap flag, and SessionCreate will use it. Note however that
    you cannot supersede a prior SessionCreate call that way; only a single SessionCreate
    call is allowed for each Session (however made).
	This call will discard any security information established for the calling process.
	In particular, any authorization handles acquired will become invalid, and so will any
	keychain related information. We recommend that you call SessionCreate before
	making any other security-related calls that establish rights of any kind, to the
	extent this is practical. Also, we strongly recommend that you do not perform
	security-related calls in any other threads while calling SessionCreate.
    
    @param flags Flags controlling how the session is created.
    
    @param attributes The set of attribute bits to set for the new session.
        Not all bits can be set this way.
    
    @result An OSStatus indicating success (noErr) or an error cause.
    
    errSessionInvalidAttributes -60501 Attempt to set invalid attribute bits	
    errSessionAuthorizationDenied -60502 Attempt to re-initialize a session
    errSessionInvalidFlags -60011 Attempt to specify unsupported flag bits
    
}
function SessionCreate( flags: SessionCreationFlags; attributes: SessionAttributeBits ): OSStatus; external name '_SessionCreate';

end.
