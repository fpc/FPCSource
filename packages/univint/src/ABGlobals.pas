{
 *  ABGlobals.h
 *  AddressBook Framework
 *
 *  Copyright (c) 2002-2003 Apple Computer. All rights reserved.
 *
 }
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, Feburary 2006 }

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

unit ABGlobals;
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
uses MacTypes,CFBase;
{$ALIGN POWER}

// NOTE: This header is for C programmers. For Objective-C use ABGlobals.h

// ================================================================
//      Global Table properties
// ================================================================

// ----- Properties common to all Records

var kABUIDProperty: CFStringRef; external name '_kABUIDProperty'; (* attribute const *)                   // The UID property - kABStringProperty
var kABCreationDateProperty: CFStringRef; external name '_kABCreationDateProperty'; (* attribute const *)          // Creation Date (when first saved) - kABDateProperty
var kABModificationDateProperty: CFStringRef; external name '_kABModificationDateProperty'; (* attribute const *)      // Last saved date - kABDateProperty

// ----- Person specific properties

var kABFirstNameProperty: CFStringRef; external name '_kABFirstNameProperty'; (* attribute const *)             // First name - kABStringProperty
var kABLastNameProperty: CFStringRef; external name '_kABLastNameProperty'; (* attribute const *)              // Last name - kABStringProperty

var kABFirstNamePhoneticProperty: CFStringRef; external name '_kABFirstNamePhoneticProperty'; (* attribute const *)     // First name Phonetic - kABStringProperty
var kABLastNamePhoneticProperty: CFStringRef; external name '_kABLastNamePhoneticProperty'; (* attribute const *)      // Last name Phonetic - kABStringProperty

var kABNicknameProperty: CFStringRef; external name '_kABNicknameProperty'; (* attribute const *)              // kABStringProperty
var kABMaidenNameProperty: CFStringRef; external name '_kABMaidenNameProperty'; (* attribute const *)            // kABStringProperty

var kABBirthdayProperty: CFStringRef; external name '_kABBirthdayProperty'; (* attribute const *)              // Birth date - kABDateProperty

var kABOrganizationProperty: CFStringRef; external name '_kABOrganizationProperty'; (* attribute const *)          // Company name - kABStringProperty

var kABJobTitleProperty: CFStringRef; external name '_kABJobTitleProperty'; (* attribute const *)              // Job Title - kABStringProperty

// Deprecated in Mac OS 10.4. You should use kABURLsProperty.
var kABHomePageProperty: CFStringRef; external name '_kABHomePageProperty'; (* attribute const *)              // Home Web page - kABStringProperty

var kABURLsProperty: CFStringRef; external name '_kABURLsProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)          // URLs - kABMultiStringProperty
var kABHomePageLabel: CFStringRef; external name '_kABHomePageLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *) // Homepage URL

var kABEmailProperty: CFStringRef; external name '_kABEmailProperty'; (* attribute const *)                 // Email(s) - kABMultiStringProperty
var kABEmailWorkLabel: CFStringRef; external name '_kABEmailWorkLabel'; (* attribute const *)        // Home email
var kABEmailHomeLabel: CFStringRef; external name '_kABEmailHomeLabel'; (* attribute const *)        // Work email

var kABAddressProperty: CFStringRef; external name '_kABAddressProperty'; (* attribute const *)                // Street Addresses - kABMultiDictionaryProperty
var kABAddressStreetKey: CFStringRef; external name '_kABAddressStreetKey'; (* attribute const *)           // Street
var kABAddressCityKey: CFStringRef; external name '_kABAddressCityKey'; (* attribute const *)             // City
var kABAddressStateKey: CFStringRef; external name '_kABAddressStateKey'; (* attribute const *)            // State
var kABAddressZIPKey: CFStringRef; external name '_kABAddressZIPKey'; (* attribute const *)              // Zip
var kABAddressCountryKey: CFStringRef; external name '_kABAddressCountryKey'; (* attribute const *)          // Country
var kABAddressCountryCodeKey: CFStringRef; external name '_kABAddressCountryCodeKey'; (* attribute const *)      // Country Code
var kABAddressHomeLabel: CFStringRef; external name '_kABAddressHomeLabel'; (* attribute const *)       // Home Address
var kABAddressWorkLabel: CFStringRef; external name '_kABAddressWorkLabel'; (* attribute const *)       // Work Address

{
 * kABAddressCountryCodeKey code must be one of the following:
 * iso country codes
 *
 *    ae = United Arab Emirates
 *    ar = Argentina
 *    at = Austria
 *    au = Australia
 *    ba = Bosnia and Herzegovina
 *    be = Belgium
 *    bg = Bulgaria
 *    bh = Bahrain
 *    br = Brazil
 *    ca = Canada
 *    ch = Switzerland
 *    cn = China
 *    cs = Czech
 *    de = Germany
 *    dk = Denmark
 *    eg = Egypt
 *    es = Spain
 *    fi = Finland
 *    fr = France
 *    gr = Greece
 *    gl = Greenland
 *    hk = Hong Kong
 *    hr = Croatia
 *    hu = Hungary
 *    ie = Ireland
 *    il = Israel
 *    id = Indonesia
 *    in = India
 *    is = Iceland
 *    it = Italy
 *    ja = Japan
 *    jo = Jordan
 *    kr = South Korea
 *    kw = Kuwait
 *    lb = Lebanon
 *    lu = Luxembourg
 *    mk = Macedonia
 *    mx = Mexico
 *    nl = Netherlands
 *    no = Norway
 *    nz = New Zealand
 *    om = Oman
 *    pl = Poland
 *    pt = Portugal
 *    qa = Qatar
 *    ro = Romania
 *    ru = Russian Federation
 *    sa = Saudi Arabia
 *    se = Sweden
 *    sg = Singapore
 *    si = Slovenia
 *    sk = Slovakia
 *    sy = Syrian Arab Republic
 *    tw = Taiwan
 *    tr = Turkey
 *    ua = Ukraine
 *    uk = United Kingdom
 *    us = United States
 *    ye = Yemen
 *    yu = Serbia and Montenegro
 *    za = South Africa
 *
 }

var kABOtherDatesProperty: CFStringRef; external name '_kABOtherDatesProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)       // Dates associated with this person - kABMultiDateProperty - (Person)
var kABAnniversaryLabel: CFStringRef; external name '_kABAnniversaryLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

var kABRelatedNamesProperty: CFStringRef; external name '_kABRelatedNamesProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)       // names - kABMultiStringProperty
var kABFatherLabel: CFStringRef; external name '_kABFatherLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABMotherLabel: CFStringRef; external name '_kABMotherLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABParentLabel: CFStringRef; external name '_kABParentLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABBrotherLabel: CFStringRef; external name '_kABBrotherLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABSisterLabel: CFStringRef; external name '_kABSisterLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABChildLabel: CFStringRef; external name '_kABChildLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABFriendLabel: CFStringRef; external name '_kABFriendLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABSpouseLabel: CFStringRef; external name '_kABSpouseLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABPartnerLabel: CFStringRef; external name '_kABPartnerLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABAssistantLabel: CFStringRef; external name '_kABAssistantLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABManagerLabel: CFStringRef; external name '_kABManagerLabel'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

var kABDepartmentProperty: CFStringRef; external name '_kABDepartmentProperty'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)     // Department name - (Person)

var kABPersonFlags: CFStringRef; external name '_kABPersonFlags'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)     // Various flags - kABIntegerProperty

const	
	kABShowAsMask = 7;
	kABShowAsPerson = 0;
	kABShowAsCompany = 1;
	
	kABNameOrderingMask = 7 shl 3;
	kABDefaultNameOrdering = 0 shl 3;
	kABFirstNameFirst = 4 shl 3;
	kABLastNameFirst = 2 shl 3;

var kABPhoneProperty: CFStringRef; external name '_kABPhoneProperty'; (* attribute const *)                  // Generic phone number - kABMultiStringProperty
var kABPhoneWorkLabel: CFStringRef; external name '_kABPhoneWorkLabel'; (* attribute const *)         // Work phone
var kABPhoneHomeLabel: CFStringRef; external name '_kABPhoneHomeLabel'; (* attribute const *)         // Home phone
var kABPhoneMobileLabel: CFStringRef; external name '_kABPhoneMobileLabel'; (* attribute const *)       // Cell phone
var kABPhoneMainLabel: CFStringRef; external name '_kABPhoneMainLabel'; (* attribute const *)         // Main phone
var kABPhoneHomeFAXLabel: CFStringRef; external name '_kABPhoneHomeFAXLabel'; (* attribute const *)      // FAX number
var kABPhoneWorkFAXLabel: CFStringRef; external name '_kABPhoneWorkFAXLabel'; (* attribute const *)      // FAX number
var kABPhonePagerLabel: CFStringRef; external name '_kABPhonePagerLabel'; (* attribute const *)        // Pager number

var kABAIMInstantProperty: CFStringRef; external name '_kABAIMInstantProperty'; (* attribute const *)             // AIM Instant Messaging - kABMultiStringProperty
var kABAIMWorkLabel: CFStringRef; external name '_kABAIMWorkLabel'; (* attribute const *)
var kABAIMHomeLabel: CFStringRef; external name '_kABAIMHomeLabel'; (* attribute const *)

var kABJabberInstantProperty: CFStringRef; external name '_kABJabberInstantProperty'; (* attribute const *)          // Jabber Instant Messaging - kABMultiStringProperty
var kABJabberWorkLabel: CFStringRef; external name '_kABJabberWorkLabel'; (* attribute const *)
var kABJabberHomeLabel: CFStringRef; external name '_kABJabberHomeLabel'; (* attribute const *)

var kABMSNInstantProperty: CFStringRef; external name '_kABMSNInstantProperty'; (* attribute const *)             // MSN Instant Messaging  - kABMultiStringProperty
var kABMSNWorkLabel: CFStringRef; external name '_kABMSNWorkLabel'; (* attribute const *)
var kABMSNHomeLabel: CFStringRef; external name '_kABMSNHomeLabel'; (* attribute const *)

var kABYahooInstantProperty: CFStringRef; external name '_kABYahooInstantProperty'; (* attribute const *)           // Yahoo Instant Messaging  - kABMultiStringProperty
var kABYahooWorkLabel: CFStringRef; external name '_kABYahooWorkLabel'; (* attribute const *)
var kABYahooHomeLabel: CFStringRef; external name '_kABYahooHomeLabel'; (* attribute const *)

var kABICQInstantProperty: CFStringRef; external name '_kABICQInstantProperty'; (* attribute const *)             // ICQ Instant Messaging  - kABMultiStringProperty
var kABICQWorkLabel: CFStringRef; external name '_kABICQWorkLabel'; (* attribute const *)
var kABICQHomeLabel: CFStringRef; external name '_kABICQHomeLabel'; (* attribute const *)

var kABNoteProperty: CFStringRef; external name '_kABNoteProperty'; (* attribute const *)                   // Note - kABStringProperty

var kABMiddleNameProperty: CFStringRef; external name '_kABMiddleNameProperty'; (* attribute const *)             // kABStringProperty
var kABMiddleNamePhoneticProperty: CFStringRef; external name '_kABMiddleNamePhoneticProperty'; (* attribute const *)     // kABStringProperty
var kABTitleProperty: CFStringRef; external name '_kABTitleProperty'; (* attribute const *)                  // kABStringProperty "Sir" "Duke" "General" "Lord"
var kABSuffixProperty: CFStringRef; external name '_kABSuffixProperty'; (* attribute const *)                 // kABStringProperty "Sr." "Jr." "III"

// ----- Group Specific Properties

var kABGroupNameProperty: CFStringRef; external name '_kABGroupNameProperty'; (* attribute const *)              // Name of the group - kABStringProperty

// ================================================================
//      Generic Labels
// ================================================================

    // All kABXXXXWorkLabel are equivalent to this label
var kABWorkLabel: CFStringRef; external name '_kABWorkLabel'; (* attribute const *)

    // All kABXXXXHomeLabel are equivalent to this label
var kABHomeLabel: CFStringRef; external name '_kABHomeLabel'; (* attribute const *)

    // Can be used with any multi-value property
var kABOtherLabel: CFStringRef; external name '_kABOtherLabel'; (* attribute const *)

// ================================================================
//      Notifications published when something changes
// ================================================================
// These notifications are not sent until ABGetSharedAddressBook()
// has been called somewhere

    // This process has changed the DB
var kABDatabaseChangedNotification: CFStringRef; external name '_kABDatabaseChangedNotification'; (* attribute const *)

    // Another process has changed the DB
var kABDatabaseChangedExternallyNotification: CFStringRef; external name '_kABDatabaseChangedExternallyNotification'; (* attribute const *)

    // The user info (dictionary) in the above notification will contain
    // the following 3 keys. Value for each keys is an array of
    // uniqueId of the Inserted/Updated/Deleted Records.
    // If all three values are nil assume that everything has changed (could be the case
    // when restoring from backup)
var kABInsertedRecords: CFStringRef; external name '_kABInsertedRecords'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABUpdatedRecords: CFStringRef; external name '_kABUpdatedRecords'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
var kABDeletedRecords: CFStringRef; external name '_kABDeletedRecords'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)


// ================================================================
//      Localization of property or label
// ================================================================

    // Returns the localized version of built in properties, labels or keys
    // Returns propertyOrLabel if not found (e.g. if not built in)
function ABLocalizedPropertyOrLabel( propertyOrLabel: CFStringRef ): CFStringRef; external name '_ABLocalizedPropertyOrLabel';

end.
