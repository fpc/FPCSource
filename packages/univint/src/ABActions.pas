//
//  ABActionsC.h
//  AddressBook Framework
//
//  Copyright (c) 2003 Apple Computer. All rights reserved.
//
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


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

unit ABActions;
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
uses MacTypes,ABAddressBook,CFBase;
{$ALIGN MAC68K}

// --------------------------------------------------------------------------------
//      Action Support
// --------------------------------------------------------------------------------
// This API allows developers to populate AddressBook.app's roll-over menus with custom
// entries. Your CFBundle must implement a function named ABActionRegisterCallbacks which
// will return a pointer to an ABActionCallbacks struct. This struct should be filled out
// as follows:
//
// version: The version of this structure is 0.
//
// proprty: A pointer to a function that returns the AddressBook property this action applies
// to. Only items with labels may have actions at this time. (emails, phones, birthdays, etc)
//
// title: A pointer to a function which returns a copy of the title to be displayed. This function
// takes two parameters, the selected person and item identifier. The item identifier will be NULL
// for single value properties. AddressBook will release this string when it's done with it.
//
// enabled: A pointer to a function which returns YES if the action should be enabled for the
// passed ABPersonRef and item identifier. The item identifier will be NULL for single value
// properties. This field may be NULL. Actions with NULL enabled callbacks will always be enabled.
//
// selected. A pointer to a function which will be called when the user selects this action.
// It's passed an ABPersonRef and item identifier. The item identifier will be NULL for single
// value properties.
//
// Action plugins are stored in ~/Library/Address Book Plug-Ins or /Library/Address Book Plug-Ins
//
// There can be only 1 Action plug in per bundle.

type ABActionGetPropertyCallback = function: CFStringRef;
type ABActionCopyTitleCallback = function( person: ABPersonRef; identifier: CFStringRef ): CFStringRef;
type ABActionEnabledCallback = function( person: ABPersonRef; identifier: CFStringRef ): Boolean;
type ABActionSelectedCallback = procedure( person: ABPersonRef; identifier: CFStringRef );

type ABActionCallbacks = record
    version: CFIndex;
    proprty: ABActionGetPropertyCallback;
    title: ABActionCopyTitleCallback;
    enabled: ABActionEnabledCallback;
    selected: ABActionSelectedCallback;
end;

// Your CFBundle MUST include a function named ABActionRegisterCallbacks which returns a pointer
// to a filled out ABActionCallbacks struct:
//
// ABActionCallbacks* ABActionRegisterCallbacks(void);

end.
