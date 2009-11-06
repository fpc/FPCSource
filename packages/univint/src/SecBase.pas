{
 * Copyright (c) 2000-2008 Apple Inc. All Rights Reserved.
 * 
 * @APPLE_LICENSE_HEADER_START@
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 }
{      Pascal Translation:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit SecBase;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := TFALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes,CFBase;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ifc TARGET_CPU_64}
{$packrecords C}
{$elsec}
{$ALIGN POWER}
{$endc} {TARGET_CPU_64}

{!
	@header SecBase
	SecBase contains common declarations for the Security functions. 
}


{!
    @typedef SecKeychainRef
    @abstract Contains information about a keychain.
}
type
	SecKeychainRef = ^OpaqueSecKeychainRef; { an opaque type }
	OpaqueSecKeychainRef = record end;
	SecKeychainRefPtr = ^SecKeychainRef;  { when a var xx:SecKeychainRef parameter can be nil, it is changed to xx: SecKeychainRefPtr }

{!
    @typedef SecKeychainItemRef
    @abstract Contains information about a keychain item.
}
type
	SecKeychainItemRef = ^OpaqueSecKeychainItemRef; { an opaque type }
	OpaqueSecKeychainItemRef = record end;
	SecKeychainItemRefPtr = ^SecKeychainItemRef;  { when a var xx:SecKeychainItemRef parameter can be nil, it is changed to xx: SecKeychainItemRefPtr }

{!
    @typedef SecKeychainSearchRef
    @abstract Contains information about a keychain search.
}
type
	SecKeychainSearchRef = ^OpaqueSecKeychainSearchRef; { an opaque type }
	OpaqueSecKeychainSearchRef = record end;
	SecKeychainSearchRefPtr = ^SecKeychainSearchRef;  { when a var xx:SecKeychainSearchRef parameter can be nil, it is changed to xx: SecKeychainSearchRefPtr }

{!
    @typedef SecKeychainAttrType
    @abstract Represents a keychain attribute type.
}
type
	SecKeychainAttrType = OSType;

{!
    @struct SecKeychainAttribute
    @abstract Contains keychain attributes. 
    @field tag A 4-byte attribute tag.
    @field length The length of the buffer pointed to by data.
    @field data A pointer to the attribute data.
}
type
	SecKeychainAttribute = record
		tag: SecKeychainAttrType;
		length: UInt32;
		data: UnivPtr;
	end;

{!
    @typedef SecKeychainAttributePtr
    @abstract Represents a pointer to a keychain attribute structure.
}
	SecKeychainAttributePtr = ^SecKeychainAttribute;

{!
    @typedef SecKeychainAttributeList
    @abstract Represents a list of keychain attributes.
    @field count An unsigned 32-bit integer that represents the number of keychain attributes in the array.
    @field attr A pointer to the first keychain attribute in the array.
}
type
	SecKeychainAttributeListPtr = ^SecKeychainAttributeList;
	SecKeychainAttributeList = record
		count: UInt32;
		attr: SecKeychainAttributePtr;
	end;

{!
    @typedef SecKeychainStatus
    @abstract Represents the status of a keychain.
}
type
	SecKeychainStatus = UInt32;

{!
    @typedef SecTrustedApplicationRef
    @abstract Contains information about a trusted application.
}
type
	SecTrustedApplicationRef = ^OpaqueSecTrustedApplicationRef; { an opaque type }
	OpaqueSecTrustedApplicationRef = record end;

{!
    @typedef SecPolicyRef
    @abstract Contains information about a policy.
}
type
	SecPolicyRef = ^OpaqueSecPolicyRef; { an opaque type }
	OpaqueSecPolicyRef = record end;

{!
    @typedef SecCertificateRef
    @abstract Contains information about a certificate.
}
type
	SecCertificateRef = ^OpaqueSecCertificateRef; { an opaque type }
	OpaqueSecCertificateRef = record end;

{!
    @typedef SecAccessRef
    @abstract Contains information about an access.
}
type
	SecAccessRef = ^OpaqueSecAccessRef; { an opaque type }
	OpaqueSecAccessRef = record end;

{!
    @typedef SecIdentityRef
    @abstract Contains information about an identity.
}
type
	SecIdentityRef = ^OpaqueSecIdentityRef; { an opaque type }
	OpaqueSecIdentityRef = record end;

{!
    @typedef SecKeyRef
    @abstract Contains information about a key.
}
type
	SecKeyRef = ^OpaqueSecKeyRef; { an opaque type }
	OpaqueSecKeyRef = record end;

{!
    @typedef SecACLRef
    @abstract Contains information about an access control list (ACL) entry.
}
type
	SecACLRef = ^OpaqueSecTrustRef; { an opaque type }
	OpaqueSecTrustRef = record end;

{!
    @typedef SecPasswordRef
    @abstract Contains information about a password.
}
type
	SecPasswordRef = ^OpaqueSecPasswordRef; { an opaque type }
	OpaqueSecPasswordRef = record end;

{!
    @typedef SecKeychainAttributeInfo
    @abstract Represents an attribute. 
    @field count The number of tag-format pairs in the respective arrays. 
    @field tag A pointer to the first attribute tag in the array.
    @field format A pointer to the first CSSM_DB_ATTRIBUTE_FORMAT in the array.
    @discussion Each tag and format item form a pair.  
}
type
	SecKeychainAttributeInfoPtr = ^SecKeychainAttributeInfo;
	SecKeychainAttributeInfo = record
		count: UInt32;
		tag: UInt32Ptr;
		format: UInt32Ptr;
	end;

{!
    @function SecCopyErrorMessageString
    @abstract Returns a string describing the specified error result code.
    @param status An error result code of type OSStatus or CSSM_RETURN, as returned by a Security or CSSM function.
    @reserved Reserved for future use. Your code should pass NULL in this parameter.
    @result A reference to an error string, or NULL if no error string is available for the specified result code. Your code must release this reference by calling the CFRelease function.
}
function SecCopyErrorMessageString( status: OSStatus; reserved: UnivPtr ): CFStringRef; external name '_SecCopyErrorMessageString';

{!
@enum Security Error Codes 
@abstract Result codes returned from Security framework functions.
@constant errSecSuccess No error.
@constant errSecUnimplemented Function or operation not implemented.
@constant errSecParam One or more parameters passed to a function were not valid.
@constant errSecAllocate Failed to allocate memory.
@constant errSecNotAvailable No keychain is available.
@constant errSecReadOnly Read only error.
@constant errSecAuthFailed Authorization/Authentication failed.
@constant errSecNoSuchKeychain The keychain does not exist.
@constant errSecInvalidKeychain The keychain is not valid.
@constant errSecDuplicateKeychain A keychain with the same name already exists.
@constant errSecDuplicateCallback The specified callback is already installed.
@constant errSecInvalidCallback The specified callback is not valid.
@constant errSecDuplicateItem The item already exists.
@constant errSecItemNotFound The item cannot be found.
@constant errSecBufferTooSmall The buffer is too small.
@constant errSecDataTooLarge The data is too large.
@constant errSecNoSuchAttr The attribute does not exist.
@constant errSecInvalidItemRef The item reference is invalid.
@constant errSecInvalidSearchRef The search reference is invalid.
@constant errSecNoSuchClass The keychain item class does not exist.
@constant errSecNoDefaultKeychain A default keychain does not exist.
@constant errSecInteractionNotAllowed User interaction is not allowed.
@constant errSecReadOnlyAttr The attribute is read only.
@constant errSecWrongSecVersion The version is incorrect.
@constant errSecKeySizeNotAllowed The key size is not allowed.
@constant errSecNoStorageModule There is no storage module available.
@constant errSecNoCertificateModule There is no certificate module available.
@constant errSecNoPolicyModule There is no policy module available.
@constant errSecInteractionRequired User interaction is required.
@constant errSecDataNotAvailable The data is not available.
@constant errSecDataNotModifiable The data is not modifiable.
@constant errSecCreateChainFailed The attempt to create a certificate chain failed.
@constant errSecACLNotSimple The access control list is not in standard simple form.
@constant errSecPolicyNotFound The policy specified cannot be found.
@constant errSecInvalidTrustSetting The specified trust setting is invalid.
@constant errSecNoAccessForItem The specified item has no access control.
@constant errSecInvalidOwnerEdit Invalid attempt to change the owner of this item.
@constant errSecTrustNotAvailable No trust results are available.
@constant errSecUnsupportedFormat Import/Export format unsupported.
@constant errSecUnknownFormat Unknown format in import.
@constant errSecKeyIsSensitive Key material must be wrapped for export.
@constant errSecMultiplePrivKeys An attempt was made to import multiple private keys.
@constant errSecPassphraseRequired Passphrase is required for import/export.
@constant errSecInvalidPasswordRef The password reference was invalid.
@constant errSecInvalidTrustSettings The Trust Settings Record was corrupted.
@constant errSecNoTrustSettings No Trust Settings were found. 
@constant errSecPkcs12VerifyFailure MAC verification failed during PKCS12 Import.
@constant errSecDecode Unable to decode the provided data.

@discussion The assigned error space is discontinuous: e.g. -25240..-25279, -25290..25329, and so on.
}

{
    Note: the comments that appear after these errors are used to create SecErrorMessages.strings.
    The comments must not be multi-line, and should be in a form meaningful to an end user. If
    a different or additional comment is needed, it can be put in the header doc format, or on a
    line that does not start with errZZZ.
}

const
	errSecSuccess = 0;       { No error. }
	errSecUnimplemented = -4;      { Function or operation not implemented. }
	errSecParam = -50;     { One or more parameters passed to a function were not valid. }
	errSecAllocate = -108;    { Failed to allocate memory. }

	errSecNotAvailable = -25291;	{ No keychain is available. You may need to restart your computer. }
	errSecReadOnly = -25292;	{ This keychain cannot be modified. }
	errSecAuthFailed = -25293;	{ The user name or passphrase you entered is not correct. }
	errSecNoSuchKeychain = -25294;	{ The specified keychain could not be found. }
	errSecInvalidKeychain = -25295;	{ The specified keychain is not a valid keychain file. }
	errSecDuplicateKeychain = -25296;	{ A keychain with the same name already exists. }
	errSecDuplicateCallback = -25297;	{ The specified callback function is already installed. }
	errSecInvalidCallback = -25298;	{ The specified callback function is not valid. }
	errSecDuplicateItem = -25299;	{ The specified item already exists in the keychain. }
	errSecItemNotFound = -25300;	{ The specified item could not be found in the keychain. }
	errSecBufferTooSmall = -25301;	{ There is not enough memory available to use the specified item. }
	errSecDataTooLarge = -25302;	{ This item contains information which is too large or in a format that cannot be displayed. }
	errSecNoSuchAttr = -25303;	{ The specified attribute does not exist. }
	errSecInvalidItemRef = -25304;	{ The specified item is no longer valid. It may have been deleted from the keychain. }
	errSecInvalidSearchRef = -25305;	{ Unable to search the current keychain. }
	errSecNoSuchClass = -25306;	{ The specified item does not appear to be a valid keychain item. }
	errSecNoDefaultKeychain = -25307;	{ A default keychain could not be found. }
	errSecInteractionNotAllowed = -25308;	{ User interaction is not allowed. }
	errSecReadOnlyAttr = -25309;	{ The specified attribute could not be modified. }
	errSecWrongSecVersion = -25310;	{ This keychain was created by a different version of the system software and cannot be opened. }
	errSecKeySizeNotAllowed = -25311;	{ This item specifies a key size which is too large. }
	errSecNoStorageModule = -25312;	{ A required component (data storage module) could not be loaded. You may need to restart your computer. }
	errSecNoCertificateModule = -25313;	{ A required component (certificate module) could not be loaded. You may need to restart your computer. }
	errSecNoPolicyModule = -25314;	{ A required component (policy module) could not be loaded. You may need to restart your computer. }
	errSecInteractionRequired = -25315;	{ User interaction is required, but is currently not allowed. }
	errSecDataNotAvailable = -25316;	{ The contents of this item cannot be retrieved. }
	errSecDataNotModifiable = -25317;	{ The contents of this item cannot be modified. }
	errSecCreateChainFailed = -25318;	{ One or more certificates required to validate this certificate cannot be found. }
	errSecInvalidPrefsDomain = -25319;  { The specified preferences domain is not valid. }
	
	errSecACLNotSimple = -25240;	{ The specified access control list is not in standard (simple) form. }
	errSecPolicyNotFound = -25241;	{ The specified policy cannot be found. }
	errSecInvalidTrustSetting = -25242;	{ The specified trust setting is invalid. }
	errSecNoAccessForItem = -25243;	{ The specified item has no access control. }
	errSecInvalidOwnerEdit = -25244;  { Invalid attempt to change the owner of this item. }
	errSecTrustNotAvailable = -25245;	{ No trust results are available. }
	errSecUnsupportedFormat = -25256;  { Import/Export format unsupported. }
	errSecUnknownFormat = -25257;  { Unknown format in import. }
	errSecKeyIsSensitive = -25258;  { Key material must be wrapped for export. }
	errSecMultiplePrivKeys = -25259;  { An attempt was made to import multiple private keys. }
	errSecPassphraseRequired = -25260;  { Passphrase is required for import/export. }
	errSecInvalidPasswordRef = -25261;  { The password reference was invalid. }
	errSecInvalidTrustSettings = -25262;	{ The Trust Settings Record was corrupted. }
	errSecNoTrustSettings = -25263;	{ No Trust Settings were found. }
	errSecPkcs12VerifyFailure = -25264;	{ MAC verification failed during PKCS12 Import. }
	
	errSecDecode = -26275;  { Unable to decode the provided data. }

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
