{
 * Copyright (c) 2000-2002 Apple Computer, Inc. All Rights Reserved.
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
 *  Authorization.h -- APIs for implementing access control in applications
 *  and daemons.
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

unit Authorization;
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
uses MacTypes;
{$ALIGN MAC68K}
{!
	@header Authorization
	Version 1.0 10/16/2000

	The Authorization API contains all the APIs that a application or tool that need pre-authorization or need an authorization desision made.
	
	A typical use cases are a preference panel that would start off calling AuthorizationCreate() (without UI) to get an authorization object.  Then call AuthorizationCopyRights() to figure out what is currently allowed.
	
	If any of the operations that the preference panel wishes to perform are currently not allowed the lock icon in the window would show up in the locked state.  Otherwise it would show up unlocked.
	
	When the user locks the lock AuthorizationFree() is called with the kAuthorizationFlagDestroyRights to destroy any authorization rights that have been aquired.
	
	When the user unlocks the lock AuthorizationCreate() is called with the kAuthorizationFlagInteractionAllowed and kAuthorizationFlagExtendRights flags to obtain all required rights.  The old authorization object can be freed by calling AuthorizationFree() with no flags.

}


{!
	@defined kAuthorizationEmptyEnvironment
	Parameter to specify to AuthorizationCreate when no environment is being provided.
}
const kAuthorizationEmptyEnvironment = nil;


{!
	@enum AuthorizationStatus
	Error codes returned by Authorization API.
}

{
    Note: the comments that appear after these errors are used to create SecErrorMessages.strings.
    The comments must not be multi-line, and should be in a form meaningful to an end user. If
    a different or additional comment is needed, it can be put in the header doc format, or on a
    line that does not start with errZZZ.
}

const
	errAuthorizationSuccess					= 0;      { The operation completed successfully. }
	errAuthorizationInvalidSet				= -60001; { The set parameter is invalid. }
	errAuthorizationInvalidRef				= -60002; { The authorization parameter is invalid. }
	errAuthorizationInvalidTag				= -60003; { The tag parameter is invalid. }
	errAuthorizationInvalidPointer			= -60004; { The authorizedRights parameter is invalid. }
	errAuthorizationDenied					= -60005; { The authorization was denied. }
	errAuthorizationCanceled				= -60006; { The authorization was cancelled by the user. }
	errAuthorizationInteractionNotAllowed	= -60007; { The authorization was denied since no user interaction was possible. }
	errAuthorizationInternal                = -60008; { something else went wrong }
	errAuthorizationExternalizeNotAllowed	= -60009; { authorization externalization denied }
	errAuthorizationInternalizeNotAllowed	= -60010; { authorization internalization denied }
	errAuthorizationInvalidFlags            = -60011; { invalid option flag(s) }
	errAuthorizationToolExecuteFailure      = -60031; { cannot execute privileged tool }
	errAuthorizationToolEnvironmentError    = -60032; { privileged tool environment error }
	errAuthorizationBadAddress				= -60033; { invalid socket address requested }


{!
	@enum AuthorizationFlags
	Optional flags passed in to serveral Authorization APIs. See the description of AuthorizationCreate, AuthorizationCopyRights and AuthorizationFree for a description of how they affect those calls.
}
const
	kAuthorizationFlagDefaults              = 0;
	kAuthorizationFlagInteractionAllowed	= (1 shl 0);
	kAuthorizationFlagExtendRights			= (1 shl 1);
	kAuthorizationFlagPartialRights			= (1 shl 2);
	kAuthorizationFlagDestroyRights			= (1 shl 3);
	kAuthorizationFlagPreAuthorize			= (1 shl 4);
	
	// private bits (do not use)
	kAuthorizationFlagNoData                = (1 shl 20);


{!
	@typedef AuthorizationFlags
	Optional flags passed in to AuthorizationCreate.
}
type AuthorizationFlags = UInt32;


{!
	@enum AuthorizationRightFlags
	Flags returned in the flags field of ItemSet Items when calling AuthorizationCopyRights().
}
const
	kAuthorizationFlagCanNotPreAuthorize = (1 shl 0);


{!
	@typedef AuthorizationRef
	Opaque reference to an authorization object.
}
type
	AuthorizationRef							= ^SInt32;


{!
	@typedef AuthorizationString
	A zero terminated string in UTF-8 encoding.
}
type AuthorizationString = CStringPtr;


{!
	@struct AuthorizationItem
	Each AuthorizationItem describes a single string-named item with optional
	parameter value. The value must be contiguous memory of valueLength bytes;
	internal structure is defined separately for each name.

	@field name name of the item, as an AuthorizationString. Mandatory.
	@field valueLength Number of bytes in parameter value. Must be 0 if no parameter value.
	@field value Pointer to the optional parameter value associated with name.
	Must be NULL if no parameter value.
	@field flags Reserved field. Must be set to 0 on creation. Do not modify after that.
}
type
	AuthorizationItem = record
		name: AuthorizationString;
		valueLength: UInt32;
		value: Ptr;
		flags: UInt32;
	end;
	AuthorizationItemPtr = ^AuthorizationItem;


{!
	@struct AuthorizationItemSet
	An AuthorizationItemSet structure represents a set of zero or more AuthorizationItems.  Since it is a set it should not contain any identical AuthorizationItems.

	@field count Number of items identified by items.
	@field items Pointer to an array of items.
}
type
	AuthorizationItemSet = record
		count: UInt32;
		items: AuthorizationItemPtr;
	end;
	AuthorizationItemSetPtr = ^AuthorizationItemSet;

{!
	@struct AuthorizationExternalForm
	An AuthorizationExternalForm structure can hold the externalized form of
	an AuthorizationRef. As such, it can be transmitted across IPC channels
	to other processes, which can re-internalize it to recover a valid AuthorizationRef
	handle.
	The data contained in an AuthorizationExternalForm should be considered opaque.

	SECURITY NOTE: Applications should take care to not disclose the AuthorizationExternalForm to
	potential attackers since it would authorize rights to them.
}
const kAuthorizationExternalFormLength = 32;

type
	AuthorizationExternalForm = record
		bytes: packed array[0..(kAuthorizationExternalFormLength)-1] of char;
	end;


{!
	@typedef AuthorizationRights
	An AuthorizationItemSet representing a set of rights each with an associated argument (value).
	Each argument value is as defined for the specific right they belong to.  Argument values may not contain pointers as the should be copyable to different address spaces.
}
type AuthorizationRights = AuthorizationItemSet;
type AuthorizationRightsPtr = ^AuthorizationRights;
type AuthorizationRightsPtrPtr = ^AuthorizationRightsPtr;


{!
	@typedef AuthorizationEnvironment
	An AuthorizationItemSet representing environmental information of potential use
	to authorization decisions.
}
type AuthorizationEnvironment = AuthorizationItemSet;
type AuthorizationEnvironmentPtr = ^AuthorizationEnvironment;


{!
    @function AuthorizationCreate
    Create a new autorization object which can be used in other authorization calls.  When the authorization is no longer needed AuthorizationFree should be called.

	When the kAuthorizationFlagInteractionAllowed flag is set, user interaction will happen when required.  Failing to set this flag will result in this call failing with a errAuthorizationInteractionNotAllowed status when interaction is required.

	Setting the kAuthorizationFlagExtendRights flag will extend the currently available rights. If this flag is set the returned AuthorizationRef will grant all the rights requested when errAuthorizationSuccess is returned. If this flag is not set the operation will almost certainly succeed, but no attempt will be made to make the requested rights availible.
		Call AuthorizationCopyRights to figure out which of the requested rights are granted by the returned AuthorizationRef.

	Setting the kAuthorizationFlagPartialRights flag will cause this call to succeed if only some of the requested rights are being granted by the returned AuthorizationRef. Unless this flag is set this API will fail if not all the requested rights could be obtained.

	Setting the kAuthorizationFlagDestroyRights flag will prevent any rights obtained during this call from being preserved after returning from this API (This is most useful when the authorization parameter is NULL and the caller doesn't want to affect the session state in any way).

	Setting the kAuthorizationFlagPreAuthorize flag will pre authorize the requested rights so that at a later time -- by calling AuthorizationMakeExternalForm() follow by AuthorizationCreateFromExternalForm() -- the obtained rights can be used in a different process.  Rights that can't be preauthorized will be treated as if they were authorized for the sake of returning an error (in other words if all rights are either authorized or could not be preauthorized this call will still succeed).
		The rights which could not be preauthorized are not currently authorized and may fail to authorize when a later call to AuthorizationCopyRights() is made, unless the kAuthorizationFlagExtendRights and kAuthorizationFlagInteractionAllowed flags are set.  Even then they might still fail if the user does not supply the correct credentials.
		The reason for passing in this flag is to provide correct audit trail information and to avoid unnecessary user interaction.

    @param rights (input/optional) An AuthorizationItemSet containing rights for which authorization is being requested.  If none are specified the resulting AuthorizationRef will authorize nothing at all.
    @param environment (input/optional) An AuthorizationItemSet containing enviroment state used when making the autorization decision.  See the AuthorizationEnvironment type for details.
    @param flags (input) options specified by the AuthorizationFlags enum.  set all unused bits to zero to allow for future expansion.
    @param authorization (output optional) A pointer to an AuthorizationRef to be returned.  When the returned AuthorizationRef is no longer needed AuthorizationFree should be called to prevent anyone from using the aquired rights.  If NULL is specified no new rights are returned, but the system will attempt to authorize all the requested rights and return the appropriate status.

    @result errAuthorizationSuccess 0 authorization or all requested rights succeeded.

	errAuthorizationDenied -60005 The authorization for one or more of the requested rights was denied.

	errAuthorizationCanceled -60006 The authorization was cancelled by the user.

	errAuthorizationInteractionNotAllowed -60007 The authorization was denied since no interaction with the user was allowed.
}
function AuthorizationCreate(rights: AuthorizationRightsPtr; environment: AuthorizationEnvironmentPtr; flags: AuthorizationFlags; var authorization: AuthorizationRef): OSStatus; external name '_AuthorizationCreate';


{!
    @function AuthorizationFree
    Destroy an AutorizationRef object. If the kAuthorizationFlagDestroyRights flag is passed,
	any rights associated with the authorization are lost. Otherwise, only local resources
	are released, and the rights may still be available to other clients.

	Setting the kAuthorizationFlagDestroyRights flag will prevent any rights that were obtained by the specified authorization object to be preserved after returning from this API.  This effectivaly locks down all potentially shared authorizations.

    @param authorization (input) The authorization object on which this operation is performed.
	
	@param flags (input) Bit mask of option flags to this call.

    @result errAuthorizationSuccess 0 No error.

    errAuthorizationInvalidRef -60002 The authorization parameter is invalid.
}
function AuthorizationFree(authorization: AuthorizationRef;flags: AuthorizationFlags): OSStatus; external name '_AuthorizationFree';

{!
	@function AuthorizationCopyRights
    Given a set of rights, return the subset that is currently authorized
    by the AuthorizationRef given.

	When the kAuthorizationFlagInteractionAllowed flag is set, user interaction will happen when required.  Failing to set this flag will result in this call failing with a errAuthorizationInteractionNotAllowed status when interaction is required.

	Setting the kAuthorizationFlagExtendRights flag will extend the currently available rights.

	Setting the kAuthorizationFlagPartialRights flag will cause this call to succeed if only some of the requested rights are being granted by the returned AuthorizationRef.  Unless this flag is set this API will fail if not all the requested rights could be obtained.

	Setting the kAuthorizationFlagDestroyRights flag will prevent any additional rights obtained during this call from being preserved after returning from this API.

	Setting the kAuthorizationFlagPreAuthorize flag will pre authorize the requested rights so that at a later time -- by calling AuthorizationMakeExternalForm() follow by AuthorizationCreateFromExternalForm() -- the obtained rights can be used in a different process.  Rights that can't be preauthorized will be treated as if they were authorized for the sake of returning an error (in other words if all rights are either authorized or could not be preauthorized this call will still succeed), and they will be returned in authorizedRights with their kAuthorizationFlagCanNotPreAuthorize bit in the flags field set to 1.
		The rights which could not be preauthorized are not currently authorized and may fail to authorize when a later call to AuthorizationCopyRights() is made, unless the kAuthorizationFlagExtendRights and kAuthorizationFlagInteractionAllowed flags are set.  Even then they might still fail if the user does not supply the correct credentials.
		The reason for passing in this flag is to provide correct audit trail information and to avoid unnecessary user interaction.

	Setting the kAuthorizationFlagPreAuthorize flag will pre authorize the requested rights so that at a later time -- by calling AuthorizationMakeExternalForm() follow by AuthorizationCreateFromExternalForm() -- the obtained rights can be used in a different process.  When this flags is specified rights that can't be preauthorized will be returned as if they were authorized with their kAuthorizationFlagCanNotPreAuthorize bit in the flags field set to 1.  These rights are not currently authorized and may fail to authorize later unless kAuthorizationFlagExtendRights and kAuthorizationFlagInteractionAllowed flags are set when the actual authorization is done.  And even then they might still fail if the user does not supply the correct credentials.

    @param authorization (input) The authorization object on which this operation is performed.
    @param rights (input) A rights set (see AuthorizationCreate).
    @param environment (input/optional) An AuthorizationItemSet containing enviroment state used when making the autorization decision.  See the AuthorizationEnvironment type for details.
    @param flags (input) options specified by the AuthorizationFlags enum.  set all unused bits to zero to allow for future expansion.
    @param authorizedRights (output/optional) A pointer to a newly allocated AuthorizationInfoSet in which the authorized subset of rights are returned (authorizedRights should be deallocated by calling AuthorizationFreeItemSet() when it is no longer needed).  If NULL the only information returned is the status.  Note that if the kAuthorizationFlagPreAuthorize flag was specified rights that could not be preauthorized are returned in authorizedRights, but their flags contains the kAuthorizationFlagCanNotPreAuthorize bit.

    @result errAuthorizationSuccess 0 No error.

	errAuthorizationInvalidRef -60002 The authorization parameter is invalid.

    errAuthorizationInvalidSet -60001 The rights parameter is invalid.

    errAuthorizationInvalidPointer -60004 The authorizedRights parameter is invalid.
}
function AuthorizationCopyRights(authorization: AuthorizationRef; const (*var*) rights: AuthorizationRights; environment: AuthorizationEnvironmentPtr; flags: AuthorizationFlags; authorizedRights: AuthorizationRightsPtrPtr): OSStatus; external name '_AuthorizationCopyRights';

{!
	@function AuthorizationCopyInfo
	Returns sideband information (e.g. access credentials) obtained from a call to AuthorizationCreate.  The format of this data depends of the tag specified.
	
    @param authorization (input) The authorization object on which this operation is performed.
    @param tag (input/optional) An optional string tag specifing which sideband information should be returned.  When NULL is specified all available information is returned.
    @param flags (input) options specified by the AuthorizationFlags enum.  set all unused bits to zero to allow for future expansion.
    @param info (output) A pointer to a newly allocated AuthorizationInfoSet in which the requested sideband infomation is returned (info should be deallocated by calling AuthorizationFreeItemSet() when it is no longer needed).

    @result errAuthorizationSuccess 0 No error.

    errAuthorizationInvalidRef -60002 The authorization parameter is invalid.

    errAuthorizationInvalidTag -60003 The tag parameter is invalid.

    errAuthorizationInvalidPointer -60004 The info parameter is invalid.
}
function AuthorizationCopyInfo(authorization: AuthorizationRef; tag: AuthorizationString; var info: AuthorizationItemSetPtr): OSStatus; external name '_AuthorizationCopyInfo';

{!
	@function AuthorizationMakeExternalForm
	Turn an Authorization into an external "byte blob" form so it can be
	transmitted to another process.
	Note that *storing* the external form somewhere will probably not do what
	you want, since authorizations are bounded by sessions, processes, and possibly
	time limits. This is for online transmission of authorizations.
	
	@param authorization The (valid) authorization reference to externalize
	@param extForm Pointer to an AuthorizationExternalForm variable to fill.
	
        @result errAuthorizationSuccess 0 No error.

        errAuthorizationExternalizeNotAllowed -60009 Externalizing this authorization is not allowed.

        errAuthorizationInvalidRef -60002 The authorization parameter is invalid.


}
function AuthorizationMakeExternalForm(authorization: AuthorizationRef; var extForm: AuthorizationExternalForm): OSStatus; external name '_AuthorizationMakeExternalForm';


{!
	@function AuthorizationCreateFromExternalForm
	Turn an Authorization into an external "byte blob" form so it can be
	transmitted to another process.
	Note that *storing* the external form somewhere will probably not do what
	you want, since authorizations are bounded by sessions, processes, and possibly
	time limits. This is for online transmission of authorizations.
	
	@param extForm Pointer to an AuthorizationExternalForm value.
	@param authorization Will be filled with a valid AuthorizationRef on success.
	
	@result errAuthorizationInternalizeNotAllowed -60010 Internalizing this authorization is not allowed.
}
function AuthorizationCreateFromExternalForm(const (*var*) extForm: AuthorizationExternalForm; var authorization: AuthorizationRef): OSStatus; external name '_AuthorizationCreateFromExternalForm';


{!
	@function AuthorizationFreeItemSet
	Release the memory allocated for an AuthorizationItemSet that was allocated
	by an API call.
	
    @param set The AuthorizationItemSet to deallocate.

    @result errAuthorizationSuccess 0 No error.

    errAuthorizationInvalidSet -60001 The set parameter is invalid.
}
function AuthorizationFreeItemSet(var setx: AuthorizationItemSet): OSStatus; external name '_AuthorizationFreeItemSet';


{!
	@function AuthorizationExecuteWithPrivileges
	Run an executable tool with enhanced privileges after passing
	suitable authorization procedures.
	
	@param authorization An authorization reference that is used to authorize
	access to the enhanced privileges. It is also passed to the tool for
	further access control.
	@param pathToTool Full pathname to the tool that should be executed
	with enhanced privileges.
	@param options Option bits (reserved). Must be zero.
	@param arguments An argv-style vector of strings to be passed to the tool.
	@param communicationsPipe Assigned a UNIX stdio FILE pointer for
	a bidirectional pipe to communicate with the tool. The tool will have
	this pipe as its standard I/O channels (stdin/stdout). If NULL, do not
	establish a communications pipe.
 }
type
  Arg10000Type = array[0..10000] of CStringPtr;
  Arg10000TypePtr = ^Arg10000Type;
function AuthorizationExecuteWithPrivileges(authorization: AuthorizationRef; pathToTool: CStringPtr; options: AuthorizationFlags; arguments: Arg10000TypePtr; communicationsPipe: UnivPtr): OSStatus; external name '_AuthorizationExecuteWithPrivileges';
// communicationsPipe not yet supported


{!
	@function AuthorizationCopyPrivilegedReference
	From within a tool launched via the AuthorizationExecuteWithPrivileges function
	ONLY, retrieve the AuthorizationRef originally passed to that function.
	While AuthorizationExecuteWithPrivileges already verified the authorization to
	launch your tool, the tool may want to avail itself of any additional pre-authorizations
	the caller may have obtained through that reference.
 }
function AuthorizationCopyPrivilegedReference(var authorization: AuthorizationRef; flags: AuthorizationFlags): OSStatus; external name '_AuthorizationCopyPrivilegedReference';

end.
