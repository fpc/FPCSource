{	CFPreferences.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
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

unit CFPreferences;
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
uses MacTypes,CFDictionary,CFBase,CFArray,CFPropertyList,CFString;
{$ALIGN POWER}


var kCFPreferencesAnyApplication: CFStringRef; external name '_kCFPreferencesAnyApplication'; (* attribute const *)
var kCFPreferencesCurrentApplication: CFStringRef; external name '_kCFPreferencesCurrentApplication'; (* attribute const *)
var kCFPreferencesAnyHost: CFStringRef; external name '_kCFPreferencesAnyHost'; (* attribute const *)
var kCFPreferencesCurrentHost: CFStringRef; external name '_kCFPreferencesCurrentHost'; (* attribute const *)
var kCFPreferencesAnyUser: CFStringRef; external name '_kCFPreferencesAnyUser'; (* attribute const *)
var kCFPreferencesCurrentUser: CFStringRef; external name '_kCFPreferencesCurrentUser'; (* attribute const *)

{ NOTE: All CFPropertyListRef values returned from
         CFPreferences API should be assumed to be immutable.
}

{	The "App" functions search the various sources of defaults that
	apply to the given application, and should never be called with
	kCFPreferencesAnyApplication - only kCFPreferencesCurrentApplication
	or an application's ID (its bundle identifier).
}

{ Searches the various sources of application defaults to find the
value for the given key. key must not be NULL.  If a value is found,
it returns it; otherwise returns NULL.  Caller must release the
returned value }
function CFPreferencesCopyAppValue( key: CFStringRef; applicationID: CFStringRef ): CFPropertyListRef; external name '_CFPreferencesCopyAppValue';

{ Convenience to interpret a preferences value as a boolean directly.
Returns false if the key doesn't exist, or has an improper format; under
those conditions, keyExistsAndHasValidFormat (if non-NULL) is set to false }
function CFPreferencesGetAppBooleanValue( key: CFStringRef; applicationID: CFStringRef; var keyExistsAndHasValidFormat: Boolean ): Boolean; external name '_CFPreferencesGetAppBooleanValue';

{ Convenience to interpret a preferences value as an integer directly.
Returns 0 if the key doesn't exist, or has an improper format; under
those conditions, keyExistsAndHasValidFormat (if non-NULL) is set to false }
function CFPreferencesGetAppIntegerValue( key: CFStringRef; applicationID: CFStringRef; var keyExistsAndHasValidFormat: Boolean ): CFIndex; external name '_CFPreferencesGetAppIntegerValue';

{ Sets the given value for the given key in the "normal" place for
application preferences.  key must not be NULL.  If value is NULL,
key is removed instead. }
procedure CFPreferencesSetAppValue( key: CFStringRef; value: CFPropertyListRef; applicationID: CFStringRef ); external name '_CFPreferencesSetAppValue';

{ Adds the preferences for the given suite to the app preferences for
   the specified application.  To write to the suite domain, use
   CFPreferencesSetValue(), below, using the suiteName in place
   of the appName }
procedure CFPreferencesAddSuitePreferencesToApp( applicationID: CFStringRef; suiteID: CFStringRef ); external name '_CFPreferencesAddSuitePreferencesToApp';

procedure CFPreferencesRemoveSuitePreferencesFromApp( applicationID: CFStringRef; suiteID: CFStringRef ); external name '_CFPreferencesRemoveSuitePreferencesFromApp';

{ Writes all changes in all sources of application defaults.
Returns success or failure. }
function CFPreferencesAppSynchronize( applicationID: CFStringRef ): Boolean; external name '_CFPreferencesAppSynchronize';

{ The primitive get mechanism; all arguments must be non-NULL
(use the constants above for common values).  Only the exact
location specified by app-user-host is searched.  The returned
CFType must be released by the caller when it is finished with it. }
function CFPreferencesCopyValue( key: CFStringRef; applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef ): CFPropertyListRef; external name '_CFPreferencesCopyValue';

{ Convenience to fetch multiple keys at once.  Keys in 
keysToFetch that are not present in the returned dictionary
are not present in the domain.  If keysToFetch is NULL, all
keys are fetched. }
function CFPreferencesCopyMultiple( keysToFetch: CFArrayRef; applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef ): CFDictionaryRef; external name '_CFPreferencesCopyMultiple';

{ The primitive set function; all arguments except value must be
non-NULL.  If value is NULL, the given key is removed }
procedure CFPreferencesSetValue( key: CFStringRef; value: CFPropertyListRef; applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef ); external name '_CFPreferencesSetValue';

{ Convenience to set multiple values at once.  Behavior is undefined
if a key is in both keysToSet and keysToRemove }
procedure CFPreferencesSetMultiple( keysToSet: CFDictionaryRef; keysToRemove: CFArrayRef; applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef ); external name '_CFPreferencesSetMultiple';

function CFPreferencesSynchronize( applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef ): Boolean; external name '_CFPreferencesSynchronize';

{ Constructs and returns the list of the name of all applications
which have preferences in the scope of the given user and host.
The returned value must be released by the caller; neither argument
may be NULL. }
function CFPreferencesCopyApplicationList( userName: CFStringRef; hostName: CFStringRef ): CFArrayRef; external name '_CFPreferencesCopyApplicationList';

{ Constructs and returns the list of all keys set in the given
location.  The returned value must be released by the caller;
all arguments must be non-NULL }
function CFPreferencesCopyKeyList( applicationID: CFStringRef; userName: CFStringRef; hostName: CFStringRef ): CFArrayRef; external name '_CFPreferencesCopyKeyList';

{#if MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}

{ Function to determine whether or not a given key has been imposed on the
user - In cases where machines and/or users are under some kind of management,
callers should use this function to determine whether or not to disable UI elements
corresponding to those preference keys. }
function CFPreferencesAppValueIsForced( key: CFStringRef; applicationID: CFStringRef ): Boolean; external name '_CFPreferencesAppValueIsForced';

{#endif}


end.
