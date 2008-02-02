//
//  ABAddressBookC.h
//  AddressBook Framework
//
//  Copyright (c) 2002-2003 Apple Computer. All rights reserved.
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

unit ABAddressBook;
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
uses MacTypes,ABTypedefs,ABGlobals,CFBase,CFArray,CFDictionary,CFData;
{$ALIGN MAC68K}

type
	ABRecordRef    = ^SInt32; { an opaque 32-bit type }
	ABPersonRef    = ^SInt32; { an opaque 32-bit type }
	ABGroupRef    = ^SInt32; { an opaque 32-bit type }
	ABSearchElementRef    = ^SInt32; { an opaque 32-bit type }
	ABAddressBookRef    = ^SInt32; { an opaque 32-bit type }
	ABMultiValueRef    = ^SInt32; { an opaque 32-bit type }
	ABMutableMultiValueRef    = ^SInt32; { an opaque 32-bit type }

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
function ABSave( addressBook: ABAddressBookRef ): Boolean; external name '_ABSave';
function ABHasUnsavedChanges( addressBook: ABAddressBookRef ): Boolean; external name '_ABHasUnsavedChanges';

    // --- Me
function ABGetMe( addressBook: ABAddressBookRef ): ABPersonRef; external name '_ABGetMe'; // Not retain???
procedure ABSetMe( addressBook: ABAddressBookRef; moi: ABPersonRef ); external name '_ABSetMe';

    // Returns the record class Name for a particular uniqueId
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function ABCopyRecordTypeFromUniqueId( addressBook: ABAddressBookRef; uniqueId: CFStringRef ): CFStringRef; external name '_ABCopyRecordTypeFromUniqueId';

    // --- Properties
    // Property names must be unique for a record type
function ABAddPropertiesAndTypes( addressBook: ABAddressBookRef; recordType: CFStringRef; propertiesAnTypes: CFDictionaryRef ): SInt32; external name '_ABAddPropertiesAndTypes';
function ABRemoveProperties( addressBook: ABAddressBookRef; recordType: CFStringRef; properties: CFArrayRef ): SInt32; external name '_ABRemoveProperties';
function ABCopyArrayOfPropertiesForRecordType( addressBook: ABAddressBookRef; recordType: CFStringRef ): CFArrayRef; external name '_ABCopyArrayOfPropertiesForRecordType';
function ABTypeOfProperty( addressBook: ABAddressBookRef; recordType: CFStringRef; proprty: CFStringRef ): ABPropertyType; external name '_ABTypeOfProperty';

    // --- Records (Person, Group)
function ABCopyRecordForUniqueId( addressBook: ABAddressBookRef; uniqueId: CFStringRef ): ABRecordRef; external name '_ABCopyRecordForUniqueId';
function ABAddRecord( addressBook: ABAddressBookRef; recrd: ABRecordRef ): Boolean; external name '_ABAddRecord';
function ABRemoveRecord( addressBook: ABAddressBookRef; recrd: ABRecordRef ): Boolean; external name '_ABRemoveRecord';

    // --- People
function ABCopyArrayOfAllPeople( addressBook: ABAddressBookRef ): CFArrayRef; external name '_ABCopyArrayOfAllPeople';                  // Array of ABPerson

    // --- Groups
function ABCopyArrayOfAllGroups( addressBook: ABAddressBookRef ): CFArrayRef; external name '_ABCopyArrayOfAllGroups';                  // Array of ABGroup

// --------------------------------------------------------------------------------
//      ABRecord
// --------------------------------------------------------------------------------

function ABRecordCopyRecordType( recrd: ABRecordRef ): CFStringRef; external name '_ABRecordCopyRecordType';

    // --- Property value
function ABRecordCopyValue( recrd: ABRecordRef; proprty: CFStringRef ): CFTypeRef; external name '_ABRecordCopyValue';
    // returns a CFDictionary for multi-value properties
function ABRecordSetValue( recrd: ABRecordRef; proprty: CFStringRef; value: CFTypeRef ): Boolean; external name '_ABRecordSetValue';
    // takes a CFDictionary for multi-value properties
function ABRecordRemoveValue( recrd: ABRecordRef; proprty: CFStringRef ): Boolean; external name '_ABRecordRemoveValue';

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
function ABGroupAddMember( group: ABGroupRef; personToAdd: ABPersonRef ): Boolean; external name '_ABGroupAddMember';
function ABGroupRemoveMember( group: ABGroupRef; personToRemove: ABPersonRef ): Boolean; external name '_ABGroupRemoveMember';

    // --- Dealing with Groups
function ABGroupCopyArrayOfAllSubgroups( group: ABGroupRef ): CFArrayRef; external name '_ABGroupCopyArrayOfAllSubgroups';
function ABGroupAddGroup( group: ABGroupRef; groupToAdd: ABGroupRef ): Boolean; external name '_ABGroupAddGroup';
function ABGroupRemoveGroup( group: ABGroupRef; groupToRemove: ABGroupRef ): Boolean; external name '_ABGroupRemoveGroup';

    // --- Dealong with Parents
function ABGroupCopyParentGroups( group: ABGroupRef ): CFArrayRef; external name '_ABGroupCopyParentGroups';

    // --- Distribution list
function ABGroupSetDistributionIdentifier( group: ABGroupRef; person: ABPersonRef; proprty: CFStringRef; identifier: CFStringRef ): Boolean; external name '_ABGroupSetDistributionIdentifier';
function ABGroupCopyDistributionIdentifier( group: ABGroupRef; person: ABPersonRef; proprty: CFStringRef ): CFStringRef; external name '_ABGroupCopyDistributionIdentifier';

    // --- Search elements
function ABGroupCreateSearchElement( proprty: CFStringRef; labl: CFStringRef; key: CFStringRef; value: CFTypeRef; comparison: ABSearchComparison ): ABSearchElementRef; external name '_ABGroupCreateSearchElement';

// --------------------------------------------------------------------------------
//      ABSearchElement
// --------------------------------------------------------------------------------

function ABSearchElementCreateWithConjunction( conjunction: ABSearchConjunction; childrenSearchElement: CFArrayRef ): ABSearchElementRef; external name '_ABSearchElementCreateWithConjunction';

function ABSearchElementMatchesRecord( searchElement: ABSearchElementRef; recrd: ABRecordRef ): Boolean; external name '_ABSearchElementMatchesRecord';

// --------------------------------------------------------------------------------
//      ABMultiValue
// --------------------------------------------------------------------------------

function ABMultiValueCreate: ABMultiValueRef; external name '_ABMultiValueCreate';
function ABMultiValueCount( multiValue: ABMultiValueRef ): UInt32; external name '_ABMultiValueCount';
function ABMultiValueCopyValueAtIndex( multiValue: ABMultiValueRef; index: SInt32 ): CFTypeRef; external name '_ABMultiValueCopyValueAtIndex';
function ABMultiValueCopyLabelAtIndex( multiValue: ABMultiValueRef; index: SInt32 ): CFStringRef; external name '_ABMultiValueCopyLabelAtIndex';
function ABMultiValueCopyPrimaryIdentifier( multiValue: ABMultiValueRef ): CFStringRef; external name '_ABMultiValueCopyPrimaryIdentifier';
function ABMultiValueIndexForIdentifier( multiValue: ABMultiValueRef; identifier: CFStringRef ): SInt32; external name '_ABMultiValueIndexForIdentifier';
function ABMultiValueCopyIdentifierAtIndex( multiValue: ABMultiValueRef; index: SInt32 ): CFStringRef; external name '_ABMultiValueCopyIdentifierAtIndex';
function ABMultiValuePropertyType( multiValue: ABMultiValueRef ): ABPropertyType; external name '_ABMultiValuePropertyType';
function ABMultiValueCreateCopy( multiValue: ABMultiValueRef ): ABMultiValueRef; external name '_ABMultiValueCreateCopy';

// --------------------------------------------------------------------------------
//      ABMutableMultiValue
// --------------------------------------------------------------------------------

function ABMultiValueCreateMutable: ABMutableMultiValueRef; external name '_ABMultiValueCreateMutable';
function ABMultiValueAdd( multiValue: ABMutableMultiValueRef; value: CFTypeRef; labl: CFStringRef; var outIdentifier: CFStringRef ): Boolean; external name '_ABMultiValueAdd';
function ABMultiValueInsert( multiValue: ABMutableMultiValueRef; value: CFTypeRef; labl: CFStringRef; index: SInt32; var outIdentifier: CFStringRef ): Boolean; external name '_ABMultiValueInsert';
function ABMultiValueRemove( multiValue: ABMutableMultiValueRef; index: SInt32 ): Boolean; external name '_ABMultiValueRemove';
function ABMultiValueReplaceValue( multiValue: ABMutableMultiValueRef; value: CFTypeRef; index: SInt32 ): Boolean; external name '_ABMultiValueReplaceValue';
function ABMultiValueReplaceLabel( multiValue: ABMutableMultiValueRef; labl: CFStringRef; index: SInt32 ): Boolean; external name '_ABMultiValueReplaceLabel';
function ABMultiValueSetPrimaryIdentifier( multiValue: ABMutableMultiValueRef; identifier: CFStringRef ): Boolean; external name '_ABMultiValueSetPrimaryIdentifier';
function ABMultiValueCreateMutableCopy( multiValue: ABMultiValueRef ): ABMutableMultiValueRef; external name '_ABMultiValueCreateMutableCopy';

// --------------------------------------------------------------------------------
//      Localization of properties or labels
// --------------------------------------------------------------------------------

function ABCopyLocalizedPropertyOrLabel( labelOrProperty: CFStringRef ): CFStringRef; external name '_ABCopyLocalizedPropertyOrLabel';

// --- Address formatting
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function ABCreateFormattedAddressFromDictionary( addressBook: ABAddressBookRef; address: CFDictionaryRef ): CFStringRef; external name '_ABCreateFormattedAddressFromDictionary';
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function ABCopyDefaultCountryCode( addressBook: ABAddressBookRef ): CFStringRef; external name '_ABCopyDefaultCountryCode';

// --------------------------------------------------------------------------------
//      Person Image Loading
// --------------------------------------------------------------------------------

function ABPersonSetImageData( person: ABPersonRef; imageData: CFDataRef ): Boolean; external name '_ABPersonSetImageData';
function ABPersonCopyImageData( person: ABPersonRef ): CFDataRef; external name '_ABPersonCopyImageData';

type ABImageClientCallback = procedure ( imageData: CFDataRef; tag: SInt32; refcon: UnivPtr );

function ABBeginLoadingImageDataForClient( person: ABPersonRef; callback: ABImageClientCallback; refcon: UnivPtr ): SInt32; external name '_ABBeginLoadingImageDataForClient';
procedure ABCancelLoadingImageDataForTag( tag: SInt32 ); external name '_ABCancelLoadingImageDataForTag';

end.
