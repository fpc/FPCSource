{
 * Copyright (c) 2002-2009 Apple Inc. All Rights Reserved.
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
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, September 2010 }
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

unit SecTrust;
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
	{$setc TARGET_CPU_PPC := FALSE}
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
uses MacTypes,SecBase,cssmtype,cssmapple,CFBase,CFArray,CFData,CFDate;
{$endc} {not MACOSALLINCLUDE}



{!
	@header SecTrust
	The functions and data types in SecTrust implement trust computation and allow the user to apply trust decisions to the trust configuration.
}


{!
	@typedef SecTrustResultType
	@abstract Specifies the trust result type.
	@constant kSecTrustResultInvalid Indicates an invalid setting or result.
	@constant kSecTrustResultProceed Indicates you may proceed.  This value may be returned by the SecTrustEvaluate function or stored as part of the user trust settings. 
	@constant kSecTrustResultConfirm Indicates confirmation with the user is required before proceeding.  This value may be returned by the SecTrustEvaluate function or stored as part of the user trust settings. 
	@constant kSecTrustResultDeny Indicates a user-configured deny; do not proceed. This value may be returned by the SecTrustEvaluate function or stored as part of the user trust settings. 
	@constant kSecTrustResultUnspecified Indicates user intent is unknown. This value may be returned by the SecTrustEvaluate function or stored as part of the user trust settings. 
	@constant kSecTrustResultRecoverableTrustFailure Indicates a trust framework failure; retry after fixing inputs. This value may be returned by the SecTrustEvaluate function but not stored as part of the user trust settings. 
	@constant kSecTrustResultFatalTrustFailure Indicates a trust framework failure; no "easy" fix. This value may be returned by the SecTrustEvaluate function but not stored as part of the user trust settings.
	@constant kSecTrustResultOtherError Indicates a failure other than that of trust evaluation. This value may be returned by the SecTrustEvaluate function but not stored as part of the user trust settings.
 }
type
	SecTrustResultType = UInt32;

const
	kSecTrustResultInvalid = 0;
	kSecTrustResultProceed = 1;
	kSecTrustResultConfirm = 2;
	kSecTrustResultDeny = 3;
	kSecTrustResultUnspecified = 4;
	kSecTrustResultRecoverableTrustFailure = 5;
	kSecTrustResultFatalTrustFailure = 6;
	kSecTrustResultOtherError = 7;

{!
	@typedef SecTrustUserSetting
	@abstract Specifies user-specified trust settings.
}
type
	SecTrustUserSetting = SecTrustResultType;

{!
	@typedef SecTrustRef
	@abstract A pointer to an opaque trust management structure.
}
type
	SecTrustRef = ^OpaqueSecTrustRef; { an opaque type }
	{ already defined in SecBase 
	OpaqueSecTrustRef = record end; }

{!
	@function SecTrustGetTypeID
	@abstract Returns the type identifier of SecTrust instances.
	@result The CFTypeID of SecTrust instances.
}
function SecTrustGetTypeID: CFTypeID; external name '_SecTrustGetTypeID';

{!
	@function SecTrustCreateWithCertificates
	@abstract Creates a trust based on the given certificates and policies.
    @param certificates The group of certificates to verify.
    @param policies An array of one or more policies. You may pass a SecPolicyRef to represent a single policy.
	@param trustRef On return, a pointer to the trust management reference.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustCreateWithCertificates( certificates: CFArrayRef; policies: CFTypeRef; var trustRef: SecTrustRef ): OSStatus; external name '_SecTrustCreateWithCertificates';

{!
    @function SecTrustSetPolicies
    @abstract Set (replace) the set of policies to evaluate.
    @param trust The reference to the trust to change.
    @param policies An array of one or more policies. A single SecPolicyRef may also be passed, representing an array of one policy.
    @result A result code. See "Security Error Codes" (SecBase.h).
}    
function SecTrustSetPolicies( trust: SecTrustRef; policies: CFTypeRef ): OSStatus; external name '_SecTrustSetPolicies';

{!
	@function SecTrustSetParameters
	@abstract Sets the action and action data for a trust.
	@param trustRef The reference to the trust to change.
	@param action A CSSM trust action.
	@param actionData A reference to action data.
	@result A result code. See "Security Error Codes" (SecBase.h).
 }
function SecTrustSetParameters( trustRef: SecTrustRef; action: CSSM_TP_ACTION; actionData: CFDataRef ): OSStatus; external name '_SecTrustSetParameters';

{!
	@function SecTrustSetAnchorCertificates
	@abstract Sets the anchor certificates for a given trust.
	@param trust A reference to a trust.
	@param anchorCertificates An array of anchor certificates.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustSetAnchorCertificates( trust: SecTrustRef; anchorCertificates: CFArrayRef ): OSStatus; external name '_SecTrustSetAnchorCertificates';

{!
	@function SecTrustSetAnchorCertificatesOnly
	@abstract Reenables trusting anchor certificates in addition to those passed in
    via the SecTrustSetAnchorCertificates API.
	@param trust A reference to a trust object.
	@param anchorCertificatesOnly If true, disables trusting any anchors other
    than the ones passed in via SecTrustSetAnchorCertificates().  If false,
    the built in anchor certificates are also trusted.
	@result A result code.  See "Security Error Codes" (SecBase.h).
}
function SecTrustSetAnchorCertificatesOnly( trust: SecTrustRef; anchorCertificatesOnly: Boolean ): OSStatus; external name '_SecTrustSetAnchorCertificatesOnly';

{!
	@function SecTrustSetKeychains
	@abstract Sets the keychains for a given trust.
	@param trust A reference to a trust.
    @param keychainOrArray A reference to an array of keychains to search, a single keychain or NULL to search the user's default keychain search list.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustSetKeychains( trust: SecTrustRef; keychainOrArray: CFTypeRef ): OSStatus; external name '_SecTrustSetKeychains';

{!
	@function SecTrustSetVerifyDate
	@abstract Specifies the date for verification of a given trust.
	@param trust A reference to the trust to verify.
	@param verifyDate The date to verify.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustSetVerifyDate( trust: SecTrustRef; verifyDate: CFDateRef ): OSStatus; external name '_SecTrustSetVerifyDate';

{!
	@function SecTrustGetVerifyTime
	@abstract Returns the verify time.
	@param trust A reference to the trust object to verify.
	@result A CFAbsoluteTime value representing the time at which certificates
	should be checked for validity.
}
function SecTrustGetVerifyTime( trust: SecTrustRef ): CFAbsoluteTime; external name '_SecTrustGetVerifyTime';

{!
	@function SecTrustEvaluate
	@abstract Evaluates a trust.
	@param trust A reference to the trust to evaluate.
	@param result A pointer to a result type.
	@result A result code. See "Security Error Codes" (SecBase.h).	
}
function SecTrustEvaluate( trust: SecTrustRef; var result: SecTrustResultType ): OSStatus; external name '_SecTrustEvaluate';

{!
	@function SecTrustGetResult
	@abstract Returns detail information on the outcome of a call to SecTrustEvaluate.
	@param trustRef A reference to a trust.
	@param result A pointer to the result from the call to SecTrustEvaluate.
	@param certChain On return, a pointer to the certificate chain used to validate the input certificate. Call the CFRelease function to release this pointer.
	@param statusChain On return, a pointer to the status of the certificate chain. Do not attempt to free this pointer; it remains valid until the trust is destroyed or the next call to SecTrustEvaluate.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustGetResult( trustRef: SecTrustRef; var result: SecTrustResultType; var certChain: CFArrayRef; statusChain: CSSM_TP_APPLE_EVIDENCE_INFOArrayPtr): OSStatus; external name '_SecTrustGetResult';

{!
	@function SecTrustGetCssmResult
	@abstract Gets the CSSM trust result.
	@param trust A reference to a trust.
	@param result On return, a pointer to the CSSM trust result.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustGetCssmResult( trust: SecTrustRef; var result: CSSM_TP_VERIFY_CONTEXT_RESULT_PTR ): OSStatus; external name '_SecTrustGetCssmResult';

{!
	@function SecTrustGetCssmResultCode
	@abstract Gets the result code from the most recent call to SecTrustEvaluate for the specified trust.
	@param trust A reference to a trust.
	@param resultCode On return, the result code produced by the most recent evaluation of the given trust (cssmerr.h). The value of resultCode is undefined if SecTrustEvaluate has not been called.
	@result A result code. See "Security Error Codes" (SecBase.h). Returns errSecTrustNotAvailable if SecTrustEvaluate has not been called for the specified trust.
}
function SecTrustGetCssmResultCode( trust: SecTrustRef; var resultCode: OSStatus ): OSStatus; external name '_SecTrustGetCssmResultCode';

{!
	@function SecTrustGetTPHandle
	@abstract Gets the CSSM trust handle
	@param trust A reference to a trust.
	@param handle On return, a CSSM trust handle.
	@result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustGetTPHandle( trust: SecTrustRef; var handle: CSSM_TP_HANDLE ): OSStatus; external name '_SecTrustGetTPHandle';

{!
    @function SecTrustCopyPolicies
    @abstract Returns an array of policies used by a given trust.
    @param trust  A reference to a trust.
    @param policies On return, an array of policies used by this trust. Call the CFRelease function to release this reference.
    @result A result code. See "Security Error Codes" (SecBase.h).
}    
function SecTrustCopyPolicies( trust: SecTrustRef; var policies: CFArrayRef ): OSStatus; external name '_SecTrustCopyPolicies';

{!
    @function SecTrustCopyCustomAnchorCertificates
    @abstract Returns an array of custom anchor certificates used by a given trust, as set by a prior call to SecTrustSetAnchorCertificates, or NULL if no custom anchors have been specified.
    @param trust  A reference to a trust.
    @param anchors On return, an array of custom anchor certificates (roots) used by this trust, or NULL if no custom anchors have been specified. Call the CFRelease function to release this reference.
    @result A result code. See "Security Error Codes" (SecBase.h).
	@availability Mac OS X version 10.5.
}    
function SecTrustCopyCustomAnchorCertificates( trust: SecTrustRef; var anchors: CFArrayRef ): OSStatus; external name '_SecTrustCopyCustomAnchorCertificates';

{!
    @function SecTrustCopyAnchorCertificates
    @abstract Returns an array of default anchor (root) certificates used by the system.
    @param anchors On return, an array containing the system's default anchors (roots). Call the CFRelease function to release this pointer.
    @result A result code. See "Security Error Codes" (SecBase.h).
}
function SecTrustCopyAnchorCertificates( var anchors: CFArrayRef ): OSStatus; external name '_SecTrustCopyAnchorCertificates';

{!
	@function SecTrustGetCSSMAnchorCertificates
	@abstract Retrieves the CSSM anchor certificates.
	@param cssmAnchors A pointer to an array of anchor certificates.
	@param cssmAnchorCount A pointer to the number of certificates in anchors.
	@result A result code. See "Security Error Codes" (SecBase.h).
	@availability Mac OS X version 10.4. Deprecated in Mac OS X version 10.5.
}
function SecTrustGetCSSMAnchorCertificates( {const} cssmAnchors: CSSM_DATAArrayPtr; var cssmAnchorCount: UInt32 ): OSStatus; external name '_SecTrustGetCSSMAnchorCertificates';
(* DEPRECATED_IN_MAC_OS_X_VERSION_10_5_AND_LATER *)

{!
	@function SecTrustGetUserTrust
	@abstract Gets the user-specified trust settings of a certificate and policy.
	@param certificate A reference to a certificate.
	@param policy A reference to a policy.
	@param trustSetting On return, a pointer to the user specified trust settings.
	@result A result code. See "Security Error Codes" (SecBase.h).
	@availability Mac OS X version 10.4. Deprecated in Mac OS X version 10.5.
}
function SecTrustGetUserTrust( certificate: SecCertificateRef; policy: SecPolicyRef; var trustSetting: SecTrustUserSetting ): OSStatus; external name '_SecTrustGetUserTrust';
(* DEPRECATED_IN_MAC_OS_X_VERSION_10_5_AND_LATER *)

{!
	@function SecTrustSetUserTrust
	@abstract Sets the user-specified trust settings of a certificate and policy.
	@param certificate A reference to a certificate.
	@param policy A reference to a policy.
	@param trustSetting The user-specified trust settings.
	@result A result code. See "Security Error Codes" (SecBase.h).
	@availability Mac OS X version 10.4. Deprecated in Mac OS X version 10.5.
	@discussion as of Mac OS version 10.5, this will result in a call to 
	 SecTrustSettingsSetTrustSettings(). 
}
function SecTrustSetUserTrust( certificate: SecCertificateRef; policy: SecPolicyRef; trustSetting: SecTrustUserSetting ): OSStatus; external name '_SecTrustSetUserTrust';
(* DEPRECATED_IN_MAC_OS_X_VERSION_10_5_AND_LATER *)

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
