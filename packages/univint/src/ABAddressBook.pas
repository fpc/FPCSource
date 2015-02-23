{
//  ABAddressBookC.h
//  AddressBook Framework
//
//  Copyright (c) 2003-2007 Apple Inc.  All rights reserved.
//
//
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{	  Pascal Translation Updated:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, November 2009 }

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

unit ABAddressBook;
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
uses MacTypes,ABTypedefs,ABGlobals,CFBase,CFArray,CFDictionary,CFData;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}



type
	ABRecordRef = UnivPtr;
	ABPersonRef = ^__ABPerson; { an opaque type }
	__ABPerson = record end;
	ABGroupRef = ^__ABGroup; { an opaque type }
	__ABGroup = record end;
	ABSearchElementRef = ^__ABSearchElementRef; { an opaque type }
	__ABSearchElementRef = record end;
	ABAddressBookRef = ^__ABAddressBookRef; { an opaque type }
	__ABAddressBookRef = record end;
	ABMultiValueRef = ^__ABMultiValue; { an opaque type }
	__ABMultiValue = record end;
	ABMutableMultiValueRef = ^__ABMultiValue; { an opaque type }

// --------------------------------------------------------------------------------
//	LSOpenCFURLRef support
// --------------------------------------------------------------------------------
// An application can open the AddressBook app and select (and edit) a specific
// person by using the LSOpenCFURLRef API.
//
// To launch (or bring to front) the Address Book app and select a given person
//
// CFStringRef uniqueId = ABRecordCopyUniqueId(aPerson);
// CFStringRef urlString = CFStringCreateWithFormat(NULL, CFSTR(addressbook://%@), uniqueId);
// CFURLRef urlRef = CFURLCreateWithString(NULL, urlString, NULL);
// LSOpenCFURLRef(urlRef, NULL);
// CFRelease(uniqueId);
// CFRelease(urlRef);
// CFRelease(urlString);
//
// To launch (or bring to front) the Address Book app and edit a given person
//
// CFStringRef uniqueId = ABRecordCopyUniqueId(aPerson);
// CFStringRef urlString = CFStringCreateWithFormat(NULL, CFSTR(addressbook://%@?edit), uniqueId);
// CFURLRef urlRef = CFURLCreateWithString(NULL, urlString, NULL);
// LSOpenCFURLRef(urlRef, NULL);
// CFRelease(uniqueId);
// CFRelease(urlRef);
// CFRelease(urlString);

// --------------------------------------------------------------------------------
//      AddressBook
// --------------------------------------------------------------------------------

    // --- There is only one Address Book
function ABGetSharedAddressBook: ABAddressBookRef; external name '_ABGetSharedAddressBook';

    // --- Searching
function ABCopyArrayOfMatchingRecords( addressBook: ABAddressBookRef; search: ABSearchElementRef ): CFArrayRef; external name '_ABCopyArrayOfMatchingRecords';

    // --- Saving
function ABSave( addressBook: ABAddressBookRef ): CBool; external name '_ABSave';
function ABHasUnsavedChanges( addressBook: ABAddressBookRef ): CBool; external name '_ABHasUnsavedChanges';

    // --- Me
function ABGetMe( addressBook: ABAddressBookRef ): ABPersonRef; external name '_ABGetMe'; // Not retain???
procedure ABSetMe( addressBook: ABAddressBookRef; moi: ABPersonRef ); external name '_ABSetMe';

    // Returns the record class Name for a particular uniqueId
function ABCopyRecordTypeFromUniqueId( addressBook: ABAddressBookRef; uniqueId: CFStringRef ): CFStringRef; external name '_ABCopyRecordTypeFromUniqueId';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

    // --- Properties
    // Property names must be unique for a record type
function ABAddPropertiesAndTypes( addressBook: ABAddressBookRef; recordType: CFStringRef; propertiesAnTypes: CFDictionaryRef ): CFIndex; external name '_ABAddPropertiesAndTypes';
function ABRemoveProperties( addressBook: ABAddressBookRef; recordType: CFStringRef; properties: CFArrayRef ): CFIndex; external name '_ABRemoveProperties';
function ABCopyArrayOfPropertiesForRecordType( addressBook: ABAddressBookRef; recordType: CFStringRef ): CFArrayRef; external name '_ABCopyArrayOfPropertiesForRecordType';
function ABTypeOfProperty( addressBook: ABAddressBookRef; recordType: CFStringRef; proprty: CFStringRef ): ABPropertyType; external name '_ABTypeOfProperty';

    // --- Records (Person, Group)
function ABCopyRecordForUniqueId( addressBook: ABAddressBookRef; uniqueId: CFStringRef ): ABRecordRef; external name '_ABCopyRecordForUniqueId';
function ABAddRecord( addressBook: ABAddressBookRef; recrd: ABRecordRef ): CBool; external name '_ABAddRecord';
function ABRemoveRecord( addressBook: ABAddressBookRef; recrd: ABRecordRef ): CBool; external name '_ABRemoveRecord';

    // --- People
function ABCopyArrayOfAllPeople( addressBook: ABAddressBookRef ): CFArrayRef; external name '_ABCopyArrayOfAllPeople';                  // Array of ABPerson

    // --- Groups
function ABCopyArrayOfAllGroups( addressBook: ABAddressBookRef ): CFArrayRef; external name '_ABCopyArrayOfAllGroups';                  // Array of ABGroup

// --------------------------------------------------------------------------------
//      ABRecord
// --------------------------------------------------------------------------------

function ABRecordCreateCopy( recrd: ABRecordRef ): ABRecordRef; external name '_ABRecordCreateCopy';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

function ABRecordCopyRecordType( recrd: ABRecordRef ): CFStringRef; external name '_ABRecordCopyRecordType';

    // --- Property value
function ABRecordCopyValue( recrd: ABRecordRef; proprty: CFStringRef ): CFTypeRef; external name '_ABRecordCopyValue';
    // returns a CFDictionary for multi-value properties
function ABRecordSetValue( recrd: ABRecordRef; proprty: CFStringRef; value: CFTypeRef ): CBool; external name '_ABRecordSetValue';
    // takes a CFDictionary for multi-value properties
function ABRecordRemoveValue( recrd: ABRecordRef; proprty: CFStringRef ): CBool; external name '_ABRecordRemoveValue';
   // is the record read only
function ABRecordIsReadOnly( recrd: ABRecordRef ): CBool; external name '_ABRecordIsReadOnly';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

    // ---- Unique ID access convenience
function ABRecordCopyUniqueId( recrd: ABRecordRef ): CFStringRef; external name '_ABRecordCopyUniqueId';

// --------------------------------------------------------------------------------
//      ABPerson
// --------------------------------------------------------------------------------

function ABPersonCreate: ABPersonRef; external name '_ABPersonCreate';

function ABPersonCreateWithVCardRepresentation( vCard: CFDataRef ): ABPersonRef; external name '_ABPersonCreateWithVCardRepresentation';
function ABPersonCopyVCardRepresentation( person: ABPersonRef ): CFDataRef; external name '_ABPersonCopyVCardRepresentation';

function ABPersonCopyParentGroups( person: ABPersonRef ): CFArrayRef; external name '_ABPersonCopyParentGroups'; // Groups this person belongs to

    // --- Search elements
function ABPersonCreateSearchElement( proprty: CFStringRef; labl: CFStringRef; key: CFStringRef; value: CFTypeRef; comparison: ABSearchComparison ): ABSearchElementRef; external name '_ABPersonCreateSearchElement';

// --------------------------------------------------------------------------------
//      ABGroups
// --------------------------------------------------------------------------------

function ABGroupCreate: ABGroupRef; external name '_ABGroupCreate';

    // --- Dealing with Persons
function ABGroupCopyArrayOfAllMembers( group: ABGroupRef ): CFArrayRef; external name '_ABGroupCopyArrayOfAllMembers';
function ABGroupAddMember( group: ABGroupRef; personToAdd: ABPersonRef ): CBool; external name '_ABGroupAddMember';
function ABGroupRemoveMember( group: ABGroupRef; personToRemove: ABPersonRef ): CBool; external name '_ABGroupRemoveMember';

    // --- Dealing with Groups
function ABGroupCopyArrayOfAllSubgroups( group: ABGroupRef ): CFArrayRef; external name '_ABGroupCopyArrayOfAllSubgroups';
function ABGroupAddGroup( group: ABGroupRef; groupToAdd: ABGroupRef ): CBool; external name '_ABGroupAddGroup';
function ABGroupRemoveGroup( group: ABGroupRef; groupToRemove: ABGroupRef ): CBool; external name '_ABGroupRemoveGroup';

    // --- Dealing with Parents
function ABGroupCopyParentGroups( group: ABGroupRef ): CFArrayRef; external name '_ABGroupCopyParentGroups';

    // --- Distribution list
function ABGroupSetDistributionIdentifier( group: ABGroupRef; person: ABPersonRef; proprty: CFStringRef; identifier: CFStringRef ): CBool; external name '_ABGroupSetDistributionIdentifier';
function ABGroupCopyDistributionIdentifier( group: ABGroupRef; person: ABPersonRef; proprty: CFStringRef ): CFStringRef; external name '_ABGroupCopyDistributionIdentifier';

    // --- Search elements
function ABGroupCreateSearchElement( proprty: CFStringRef; labl: CFStringRef; key: CFStringRef; value: CFTypeRef; comparison: ABSearchComparison ): ABSearchElementRef; external name '_ABGroupCreateSearchElement';

// --------------------------------------------------------------------------------
//      ABSearchElement
// --------------------------------------------------------------------------------

function ABSearchElementCreateWithConjunction( conjunction: ABSearchConjunction; childrenSearchElement: CFArrayRef ): ABSearchElementRef; external name '_ABSearchElementCreateWithConjunction';

function ABSearchElementMatchesRecord( searchElement: ABSearchElementRef; recrd: ABRecordRef ): CBool; external name '_ABSearchElementMatchesRecord';

// --------------------------------------------------------------------------------
//      ABMultiValue
// --------------------------------------------------------------------------------

function ABMultiValueCreate: ABMultiValueRef; external name '_ABMultiValueCreate';
function ABMultiValueCount( multiValue: ABMultiValueRef ): CFIndex; external name '_ABMultiValueCount';
function ABMultiValueCopyValueAtIndex( multiValue: ABMultiValueRef; index: CFIndex ): CFTypeRef; external name '_ABMultiValueCopyValueAtIndex';
function ABMultiValueCopyLabelAtIndex( multiValue: ABMultiValueRef; index: CFIndex ): CFStringRef; external name '_ABMultiValueCopyLabelAtIndex';
function ABMultiValueCopyPrimaryIdentifier( multiValue: ABMultiValueRef ): CFStringRef; external name '_ABMultiValueCopyPrimaryIdentifier';
function ABMultiValueIndexForIdentifier( multiValue: ABMultiValueRef; identifier: CFStringRef ): CFIndex; external name '_ABMultiValueIndexForIdentifier';
function ABMultiValueCopyIdentifierAtIndex( multiValue: ABMultiValueRef; index: CFIndex ): CFStringRef; external name '_ABMultiValueCopyIdentifierAtIndex';
function ABMultiValuePropertyType( multiValue: ABMultiValueRef ): ABPropertyType; external name '_ABMultiValuePropertyType';
function ABMultiValueCreateCopy( multiValue: ABMultiValueRef ): ABMultiValueRef; external name '_ABMultiValueCreateCopy';

// --------------------------------------------------------------------------------
//      ABMutableMultiValue
// --------------------------------------------------------------------------------

function ABMultiValueCreateMutable: ABMutableMultiValueRef; external name '_ABMultiValueCreateMutable';
function ABMultiValueAdd( multiValue: ABMutableMultiValueRef; value: CFTypeRef; labl: CFStringRef; var outIdentifier: CFStringRef ): CBool; external name '_ABMultiValueAdd';
function ABMultiValueInsert( multiValue: ABMutableMultiValueRef; value: CFTypeRef; labl: CFStringRef; index: CFIndex; var outIdentifier: CFStringRef ): CBool; external name '_ABMultiValueInsert';
function ABMultiValueRemove( multiValue: ABMutableMultiValueRef; index: CFIndex ): CBool; external name '_ABMultiValueRemove';
function ABMultiValueReplaceValue( multiValue: ABMutableMultiValueRef; value: CFTypeRef; index: CFIndex ): CBool; external name '_ABMultiValueReplaceValue';
function ABMultiValueReplaceLabel( multiValue: ABMutableMultiValueRef; labl: CFStringRef; index: CFIndex ): CBool; external name '_ABMultiValueReplaceLabel';
function ABMultiValueSetPrimaryIdentifier( multiValue: ABMutableMultiValueRef; identifier: CFStringRef ): CBool; external name '_ABMultiValueSetPrimaryIdentifier';
function ABMultiValueCreateMutableCopy( multiValue: ABMultiValueRef ): ABMutableMultiValueRef; external name '_ABMultiValueCreateMutableCopy';

// --------------------------------------------------------------------------------
//      Localization of properties or labels
// --------------------------------------------------------------------------------

function ABCopyLocalizedPropertyOrLabel( labelOrProperty: CFStringRef ): CFStringRef; external name '_ABCopyLocalizedPropertyOrLabel';

// --- Address formatting
function ABCreateFormattedAddressFromDictionary( addressBook: ABAddressBookRef; address: CFDictionaryRef ): CFStringRef; external name '_ABCreateFormattedAddressFromDictionary';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
function ABCopyDefaultCountryCode( addressBook: ABAddressBookRef ): CFStringRef; external name '_ABCopyDefaultCountryCode';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

// --------------------------------------------------------------------------------
//      Person Image Loading
// --------------------------------------------------------------------------------

function ABPersonSetImageData( person: ABPersonRef; imageData: CFDataRef ): CBool; external name '_ABPersonSetImageData';
function ABPersonCopyImageData( person: ABPersonRef ): CFDataRef; external name '_ABPersonCopyImageData';

type
	ABImageClientCallback = procedure( imageData: CFDataRef; tag: CFIndex; refcon: UnivPtr );

function ABBeginLoadingImageDataForClient( person: ABPersonRef; callback: ABImageClientCallback; refcon: UnivPtr ): CFIndex; external name '_ABBeginLoadingImageDataForClient';
procedure ABCancelLoadingImageDataForTag( tag: CFIndex ); external name '_ABCancelLoadingImageDataForTag';

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
