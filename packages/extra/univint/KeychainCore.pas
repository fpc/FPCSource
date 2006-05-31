{
     File:       KeychainCore.p
 
     Contains:   Keychain low-level Interfaces
 
     Version:    Technology: Keychain 3.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
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

unit KeychainCore;
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
uses MacTypes,Files,Aliases,CodeFragments,MacErrors,Processes,Events;

{$ALIGN MAC68K}

{ Data structures and types }

type
	SecKeychainRef    = ^SInt32; { an opaque 32-bit type }
	SecKeychainRefPtr = ^SecKeychainRef;  { when a var xx:SecKeychainRef parameter can be nil, it is changed to xx: SecKeychainRefPtr }
	SecKeychainItemRef    = ^SInt32; { an opaque 32-bit type }
	SecKeychainItemRefPtr = ^SecKeychainItemRef;  { when a var xx:SecKeychainItemRef parameter can be nil, it is changed to xx: SecKeychainItemRefPtr }
	SecKeychainSearchRef    = ^SInt32; { an opaque 32-bit type }
	SecKeychainSearchRefPtr = ^SecKeychainSearchRef;  { when a var xx:SecKeychainSearchRef parameter can be nil, it is changed to xx: SecKeychainSearchRefPtr }
	SecKeychainAttrType					= OSType;
	SecKeychainStatus					= UInt32;
	SecKeychainAttributePtr = ^SecKeychainAttribute;
	SecKeychainAttribute = record
		tag:					SecKeychainAttrType;					{  4-byte attribute tag  }
		length:					UInt32;									{  Length of attribute data  }
		data:					Ptr;									{  Pointer to attribute data  }
	end;

	SecKeychainAttributeListPtr = ^SecKeychainAttributeList;
	SecKeychainAttributeList = record
		count:					UInt32;									{  How many attributes in the array  }
		attr:					SecKeychainAttributePtr;				{  Pointer to first attribute in array  }
	end;

	KCRef								= SecKeychainRef;
	KCItemRef							= SecKeychainItemRef;
	KCSearchRef							= SecKeychainSearchRef;
	KCRefPtr							= ^KCRef;
	KCItemRefPtr						= ^KCItemRef;
	KCSearchRefPtr						= ^KCSearchRef;
	KCAttribute							= SecKeychainAttribute;
	KCAttributePtr 						= ^KCAttribute;
	KCAttributeList						= SecKeychainAttributeList;
	KCAttributeListPtr 					= ^KCAttributeList;
	KCAttrType							= SecKeychainAttrType;
	KCStatus							= SecKeychainStatus;
	KCEvent 					= UInt16;
const
	kIdleKCEvent				= 0;							{  null event  }
	kLockKCEvent				= 1;							{  a keychain was locked  }
	kUnlockKCEvent				= 2;							{  a keychain was unlocked  }
	kAddKCEvent					= 3;							{  an item was added to a keychain  }
	kDeleteKCEvent				= 4;							{  an item was deleted from a keychain  }
	kUpdateKCEvent				= 5;							{  an item was updated  }
	kPasswordChangedKCEvent		= 6;							{  the keychain identity was changed  }
	kSystemKCEvent				= 8;							{  the keychain client can process events  }
	kDefaultChangedKCEvent		= 9;							{  the default keychain was changed  }
	kDataAccessKCEvent			= 10;							{  a process has accessed a keychain item's data  }
	kKeychainListChangedKCEvent	= 11;							{  the list of keychains has changed  }


type
	KCEventMask 				= UInt16;
const
	kIdleKCEventMask			= $01;
	kLockKCEventMask			= $02;
	kUnlockKCEventMask			= $04;
	kAddKCEventMask				= $08;
	kDeleteKCEventMask			= $10;
	kUpdateKCEventMask			= $20;
	kPasswordChangedKCEventMask	= $40;
	kSystemEventKCEventMask		= $0100;
	kDefaultChangedKCEventMask	= $0200;
	kDataAccessKCEventMask		= $0400;
	kEveryKCEventMask			= $FFFF;						{  all of the above }


type
	AFPServerSignature					= packed array [0..15] of UInt8;
	AFPServerSignaturePtr				= ^AFPServerSignature; { when a VAR xx: AFPServerSignature parameter can be nil, it is changed to xx: AFPServerSignaturePtr }
	KCPublicKeyHash						= packed array [0..19] of UInt8;
	KCCallbackInfoPtr = ^KCCallbackInfo;
	KCCallbackInfo = record
		version:				UInt32;
		item:					KCItemRef;
		processID:				ProcessSerialNumber;
		event:					EventRecord;
		keychain:				KCRef;
	end;


const
	kUnlockStateKCStatus		= 1;
	kRdPermKCStatus				= 2;
	kWrPermKCStatus				= 4;


	kCertificateKCItemClass		= $63657274 (* 'cert' *);						{  Certificate  }
	kAppleSharePasswordKCItemClass = $61736870 (* 'ashp' *);					{  Appleshare password  }
	kInternetPasswordKCItemClass = $696E6574 (* 'inet' *);						{  Internet password  }
	kGenericPasswordKCItemClass	= $67656E70 (* 'genp' *);						{  Generic password  }


type
	KCItemClass							= FourCharCode;

const
																{  Common attributes  }
	kClassKCItemAttr			= $636C6173 (* 'clas' *);						{  Item class (KCItemClass)  }
	kCreationDateKCItemAttr		= $63646174 (* 'cdat' *);						{  Date the item was created (UInt32)  }
	kModDateKCItemAttr			= $6D646174 (* 'mdat' *);						{  Last time the item was updated (UInt32)  }
	kDescriptionKCItemAttr		= $64657363 (* 'desc' *);						{  User-visible description string (string)  }
	kCommentKCItemAttr			= $69636D74 (* 'icmt' *);						{  User's comment about the item (string)  }
	kCreatorKCItemAttr			= $63727472 (* 'crtr' *);						{  Item's creator (OSType)  }
	kTypeKCItemAttr				= $74797065 (* 'type' *);						{  Item's type (OSType)  }
	kScriptCodeKCItemAttr		= $73637270 (* 'scrp' *);						{  Script code for all strings (ScriptCode)  }
	kLabelKCItemAttr			= $6C61626C (* 'labl' *);						{  Item label (string)  }
	kInvisibleKCItemAttr		= $696E7669 (* 'invi' *);						{  Invisible (boolean)  }
	kNegativeKCItemAttr			= $6E656761 (* 'nega' *);						{  Negative (boolean)  }
	kCustomIconKCItemAttr		= $63757369 (* 'cusi' *);						{  Custom icon (boolean)  }
	kAccountKCItemAttr			= $61636374 (* 'acct' *);						{  User account (string)  }
																{  Unique Generic password attributes  }
	kServiceKCItemAttr			= $73766365 (* 'svce' *);						{  Service (string)  }
	kGenericKCItemAttr			= $67656E61 (* 'gena' *);						{  User-defined attribute (untyped bytes)  }
																{  Unique Internet password attributes  }
	kSecurityDomainKCItemAttr	= $73646D6E (* 'sdmn' *);						{  Security domain (string)  }
	kServerKCItemAttr			= $73727672 (* 'srvr' *);						{  Server's domain name or IP address (string)  }
	kAuthTypeKCItemAttr			= $61747970 (* 'atyp' *);						{  Authentication Type (KCAuthType)  }
	kPortKCItemAttr				= $706F7274 (* 'port' *);						{  Port (UInt16)  }
	kPathKCItemAttr				= $70617468 (* 'path' *);						{  Path (string)  }
																{  Unique Appleshare password attributes  }
	kVolumeKCItemAttr			= $766C6D65 (* 'vlme' *);						{  Volume (string)  }
	kAddressKCItemAttr			= $61646472 (* 'addr' *);						{  Server address (IP or domain name) or zone name (string)  }
	kSignatureKCItemAttr		= $73736967 (* 'ssig' *);						{  Server signature block (AFPServerSignature)  }
																{  Unique AppleShare and Internet attributes  }
	kProtocolKCItemAttr			= $7074636C (* 'ptcl' *);						{  Protocol (KCProtocolType)  }
																{  Certificate attributes  }
	kSubjectKCItemAttr			= $7375626A (* 'subj' *);						{  Subject distinguished name (DER-encoded data)  }
	kCommonNameKCItemAttr		= $636E2020 (* 'cn  ' *);						{  Common Name (UTF8-encoded string)  }
	kIssuerKCItemAttr			= $69737375 (* 'issu' *);						{  Issuer distinguished name (DER-encoded data)  }
	kSerialNumberKCItemAttr		= $736E6272 (* 'snbr' *);						{  Certificate serial number (DER-encoded data)  }
	kEMailKCItemAttr			= $6D61696C (* 'mail' *);						{  E-mail address (ASCII-encoded string)  }
	kPublicKeyHashKCItemAttr	= $68706B79 (* 'hpky' *);						{  Hash of public key (KCPublicKeyHash), 20 bytes max.  }
	kIssuerURLKCItemAttr		= $6975726C (* 'iurl' *);						{  URL of the certificate issuer (ASCII-encoded string)  }
																{  Shared by keys and certificates  }
	kEncryptKCItemAttr			= $656E6372 (* 'encr' *);						{  Encrypt (Boolean)  }
	kDecryptKCItemAttr			= $64656372 (* 'decr' *);						{  Decrypt (Boolean)  }
	kSignKCItemAttr				= $7369676E (* 'sign' *);						{  Sign (Boolean)  }
	kVerifyKCItemAttr			= $76657269 (* 'veri' *);						{  Verify (Boolean)  }
	kWrapKCItemAttr				= $77726170 (* 'wrap' *);						{  Wrap (Boolean)  }
	kUnwrapKCItemAttr			= $756E7772 (* 'unwr' *);						{  Unwrap (Boolean)  }
	kStartDateKCItemAttr		= $73646174 (* 'sdat' *);						{  Start Date (UInt32)  }
	kEndDateKCItemAttr			= $65646174 (* 'edat' *);						{  End Date (UInt32)  }


type
	KCItemAttr							= FourCharCode;

const
	kKCAuthTypeNTLM				= $6E746C6D (* 'ntlm' *);
	kKCAuthTypeMSN				= $6D736E61 (* 'msna' *);
	kKCAuthTypeDPA				= $64706161 (* 'dpaa' *);
	kKCAuthTypeRPA				= $72706161 (* 'rpaa' *);
	kKCAuthTypeHTTPDigest		= $68747464 (* 'httd' *);
	kKCAuthTypeDefault			= $64666C74 (* 'dflt' *);


type
	KCAuthType							= FourCharCode;

const
	kKCProtocolTypeFTP			= $66747020 (* 'ftp ' *);
	kKCProtocolTypeFTPAccount	= $66747061 (* 'ftpa' *);
	kKCProtocolTypeHTTP			= $68747470 (* 'http' *);
	kKCProtocolTypeIRC			= $69726320 (* 'irc ' *);
	kKCProtocolTypeNNTP			= $6E6E7470 (* 'nntp' *);
	kKCProtocolTypePOP3			= $706F7033 (* 'pop3' *);
	kKCProtocolTypeSMTP			= $736D7470 (* 'smtp' *);
	kKCProtocolTypeSOCKS		= $736F7820 (* 'sox ' *);
	kKCProtocolTypeIMAP			= $696D6170 (* 'imap' *);
	kKCProtocolTypeLDAP			= $6C646170 (* 'ldap' *);
	kKCProtocolTypeAppleTalk	= $61746C6B (* 'atlk' *);
	kKCProtocolTypeAFP			= $61667020 (* 'afp ' *);
	kKCProtocolTypeTelnet		= $74656C6E (* 'teln' *);


type
	KCProtocolType						= FourCharCode;
	KCCertAddOptions 			= UInt32;
const
	kSecOptionReserved			= $000000FF;					{  First byte reserved for SecOptions flags  }
	kCertUsageShift				= 8;							{  start at bit 8  }
	kCertUsageSigningAdd		= $0100;
	kCertUsageSigningAskAndAdd	= $0200;
	kCertUsageVerifyAdd			= $0400;
	kCertUsageVerifyAskAndAdd	= $0800;
	kCertUsageEncryptAdd		= $1000;
	kCertUsageEncryptAskAndAdd	= $2000;
	kCertUsageDecryptAdd		= $4000;
	kCertUsageDecryptAskAndAdd	= $8000;
	kCertUsageKeyExchAdd		= $00010000;
	kCertUsageKeyExchAskAndAdd	= $00020000;
	kCertUsageRootAdd			= $00040000;
	kCertUsageRootAskAndAdd		= $00080000;
	kCertUsageSSLAdd			= $00100000;
	kCertUsageSSLAskAndAdd		= $00200000;
	kCertUsageAllAdd			= $7FFFFF00;


type
	KCVerifyStopOn 				= UInt16;
const
	kPolicyKCStopOn				= 0;
	kNoneKCStopOn				= 1;
	kFirstPassKCStopOn			= 2;
	kFirstFailKCStopOn			= 3;


type
	KCCertSearchOptions 		= UInt32;
const
	kCertSearchShift			= 0;							{  start at bit 0  }
	kCertSearchSigningIgnored	= 0;
	kCertSearchSigningAllowed	= $01;
	kCertSearchSigningDisallowed = $02;
	kCertSearchSigningMask		= $03;
	kCertSearchVerifyIgnored	= 0;
	kCertSearchVerifyAllowed	= $04;
	kCertSearchVerifyDisallowed	= $08;
	kCertSearchVerifyMask		= $0C;
	kCertSearchEncryptIgnored	= 0;
	kCertSearchEncryptAllowed	= $10;
	kCertSearchEncryptDisallowed = $20;
	kCertSearchEncryptMask		= $30;
	kCertSearchDecryptIgnored	= 0;
	kCertSearchDecryptAllowed	= $40;
	kCertSearchDecryptDisallowed = $80;
	kCertSearchDecryptMask		= $C0;
	kCertSearchWrapIgnored		= 0;
	kCertSearchWrapAllowed		= $0100;
	kCertSearchWrapDisallowed	= $0200;
	kCertSearchWrapMask			= $0300;
	kCertSearchUnwrapIgnored	= 0;
	kCertSearchUnwrapAllowed	= $0400;
	kCertSearchUnwrapDisallowed	= $0800;
	kCertSearchUnwrapMask		= $0C00;
	kCertSearchPrivKeyRequired	= $1000;
	kCertSearchAny				= 0;

	{	 Other constants 	}
	kAnyPort					= 0;

	kAnyProtocol				= 0;
	kAnyAuthType				= 0;

	{	 Opening and getting information about the Keychain Manager 	}
	{
	 *  KCGetKeychainManagerVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         not available
	 	}
function KCGetKeychainManagerVersion(var returnVers: UInt32): OSStatus; external name '_KCGetKeychainManagerVersion';

{$ifc TARGET_RT_MAC_CFM}
{
        KeychainManagerAvailable() is a macro/inline available only in C/C++.  
        To get the same functionality from pascal or assembly, you need
        to test if KCGetKeychainManagerVersion function is not NULL.  For instance:
        
            gKeychainManagerAvailable = FALSE;
            IF @KCGetKeychainManagerVersion <> kUnresolvedCFragSymbolAddress THEN
                gKeychainManagerAvailable = TRUE;
            end
    
    }
{$elsec}
  {$ifc TARGET_RT_MAC_MACHO}
  {$endc}
{$endc}

{ Managing the Human Interface }
{
 *  KCSetInteractionAllowed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCSetInteractionAllowed(state: boolean): OSStatus; external name '_KCSetInteractionAllowed';

{
 *  KCIsInteractionAllowed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCIsInteractionAllowed: boolean; external name '_KCIsInteractionAllowed';

{ Creating references to keychains }
{
 *  KCMakeKCRefFromFSSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCMakeKCRefFromFSSpec(var keychainFSSpec: FSSpec; var keychain: KCRef): OSStatus; external name '_KCMakeKCRefFromFSSpec';

{
 *  KCMakeKCRefFromAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCMakeKCRefFromAlias(keychainAlias: AliasHandle; var keychain: KCRef): OSStatus; external name '_KCMakeKCRefFromAlias';

{
 *  KCMakeAliasFromKCRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCMakeAliasFromKCRef(keychain: KCRef; var keychainAlias: AliasHandle): OSStatus; external name '_KCMakeAliasFromKCRef';

{
 *  KCReleaseKeychain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCReleaseKeychain(var keychain: KCRef): OSStatus; external name '_KCReleaseKeychain';

{ Specifying the default keychain }
{
 *  KCGetDefaultKeychain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetDefaultKeychain(var keychain: KCRef): OSStatus; external name '_KCGetDefaultKeychain';

{
 *  KCSetDefaultKeychain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCSetDefaultKeychain(keychain: KCRef): OSStatus; external name '_KCSetDefaultKeychain';

{ Getting information about a keychain }
{
 *  KCGetStatus()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetStatus(keychain: KCRef; var keychainStatus: UInt32): OSStatus; external name '_KCGetStatus';

{
 *  KCGetKeychain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetKeychain(item: KCItemRef; var keychain: KCRef): OSStatus; external name '_KCGetKeychain';

{
 *  KCGetKeychainName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetKeychainName(keychain: KCRef; keychainName: StringPtr): OSStatus; external name '_KCGetKeychainName';

{ Enumerating available keychains }
{
 *  KCCountKeychains()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCCountKeychains: UInt16; external name '_KCCountKeychains';

{
 *  KCGetIndKeychain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetIndKeychain(index: UInt16; var keychain: KCRef): OSStatus; external name '_KCGetIndKeychain';


type
{$ifc TYPED_FUNCTION_POINTERS}
	KCCallbackProcPtr = function(keychainEvent: KCEvent; var info: KCCallbackInfo; userContext: UnivPtr): OSStatus;
{$elsec}
	KCCallbackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	KCCallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	KCCallbackUPP = UniversalProcPtr;
{$endc}	

const
	uppKCCallbackProcInfo = $00000FB0;
	{
	 *  NewKCCallbackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewKCCallbackUPP(userRoutine: KCCallbackProcPtr): KCCallbackUPP; external name '_NewKCCallbackUPP'; { old name was NewKCCallbackProc }
{
 *  DisposeKCCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeKCCallbackUPP(userUPP: KCCallbackUPP); external name '_DisposeKCCallbackUPP';
{
 *  InvokeKCCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeKCCallbackUPP(keychainEvent: KCEvent; var info: KCCallbackInfo; userContext: UnivPtr; userRoutine: KCCallbackUPP): OSStatus; external name '_InvokeKCCallbackUPP'; { old name was CallKCCallbackProc }
{ Keychain Manager callbacks }
{
 *  KCAddCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCAddCallback(callbackProc: KCCallbackUPP; eventMask: KCEventMask; userContext: UnivPtr): OSStatus; external name '_KCAddCallback';

{
 *  KCRemoveCallback()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCRemoveCallback(callbackProc: KCCallbackUPP): OSStatus; external name '_KCRemoveCallback';


{ Creating and editing a keychain item }
{
 *  KCNewItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCNewItem(itemClass: KCItemClass; itemCreator: OSType; length: UInt32; data: UnivPtr; var item: KCItemRef): OSStatus; external name '_KCNewItem';

{
 *  KCSetAttribute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCSetAttribute(item: KCItemRef; var attr: KCAttribute): OSStatus; external name '_KCSetAttribute';

{
 *  KCGetAttribute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetAttribute(item: KCItemRef; var attr: KCAttribute; var actualLength: UInt32): OSStatus; external name '_KCGetAttribute';

{
 *  KCSetData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCSetData(item: KCItemRef; length: UInt32; data: UnivPtr): OSStatus; external name '_KCSetData';

{ Managing keychain items }
{
 *  KCUpdateItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCUpdateItem(item: KCItemRef): OSStatus; external name '_KCUpdateItem';

{
 *  KCReleaseItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCReleaseItem(var item: KCItemRef): OSStatus; external name '_KCReleaseItem';

{
 *  KCCopyItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCCopyItem(item: KCItemRef; destKeychain: KCRef; var copy: KCItemRef): OSStatus; external name '_KCCopyItem';

{ Searching and enumerating keychain items }
{
 *  KCFindFirstItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCFindFirstItem(keychain: KCRef; attrList: {Const}KCAttributeListPtr; var search: KCSearchRef; var item: KCItemRef): OSStatus; external name '_KCFindFirstItem';

{
 *  KCFindNextItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCFindNextItem(search: KCSearchRef; var item: KCItemRef): OSStatus; external name '_KCFindNextItem';

{
 *  KCReleaseSearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCReleaseSearch(var search: KCSearchRef): OSStatus; external name '_KCReleaseSearch';


{ Managing keychain items }
{
 *  KCDeleteItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCDeleteItem(item: KCItemRef): OSStatus; external name '_KCDeleteItem';

{
 *  KCGetData()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCGetData(item: KCItemRef; maxLength: UInt32; data: UnivPtr; var actualLength: UInt32): OSStatus; external name '_KCGetData';

{ Storing and retrieving AppleShare passwords }
{
 *  KCAddAppleSharePassword()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCAddAppleSharePassword(var serverSignature: AFPServerSignature; serverAddress: StringPtr; serverName: StringPtr; volumeName: StringPtr; accountName: StringPtr; passwordLength: UInt32; passwordData: UnivPtr; item: KCItemRefPtr): OSStatus; external name '_KCAddAppleSharePassword';

{
 *  KCFindAppleSharePassword()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCFindAppleSharePassword(serverSignature: AFPServerSignaturePtr; serverAddress: StringPtr; serverName: StringPtr; volumeName: StringPtr; accountName: StringPtr; maxLength: UInt32; passwordData: UnivPtr; var actualLength: UInt32; item: KCItemRefPtr): OSStatus; external name '_KCFindAppleSharePassword';

{ Storing and retrieving Internet passwords }
{
 *  KCAddInternetPassword()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCAddInternetPassword(serverName: StringPtr; securityDomain: StringPtr; accountName: StringPtr; port: UInt16; protocol: OSType; authType: OSType; passwordLength: UInt32; passwordData: UnivPtr; item: KCItemRefPtr): OSStatus; external name '_KCAddInternetPassword';

{
 *  KCAddInternetPasswordWithPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCAddInternetPasswordWithPath(serverName: StringPtr; securityDomain: StringPtr; accountName: StringPtr; path: StringPtr; port: UInt16; protocol: OSType; authType: OSType; passwordLength: UInt32; passwordData: UnivPtr; item: KCItemRefPtr): OSStatus; external name '_KCAddInternetPasswordWithPath';

{
 *  KCFindInternetPassword()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCFindInternetPassword(serverName: StringPtr; securityDomain: StringPtr; accountName: StringPtr; port: UInt16; protocol: OSType; authType: OSType; maxLength: UInt32; passwordData: UnivPtr; var actualLength: UInt32; item: KCItemRefPtr): OSStatus; external name '_KCFindInternetPassword';

{
 *  KCFindInternetPasswordWithPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCFindInternetPasswordWithPath(serverName: StringPtr; securityDomain: StringPtr; accountName: StringPtr; path: StringPtr; port: UInt16; protocol: OSType; authType: OSType; maxLength: UInt32; passwordData: UnivPtr; var actualLength: UInt32; item: KCItemRefPtr): OSStatus; external name '_KCFindInternetPasswordWithPath';

{ Storing and retrieving other types of passwords }
{
 *  KCAddGenericPassword()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCAddGenericPassword(serviceName: StringPtr; accountName: StringPtr; passwordLength: UInt32; passwordData: UnivPtr; item: KCItemRefPtr): OSStatus; external name '_KCAddGenericPassword';

{
 *  KCFindGenericPassword()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCFindGenericPassword(serviceName: StringPtr; accountName: StringPtr; maxLength: UInt32; passwordData: UnivPtr; var actualLength: UInt32; item: KCItemRefPtr): OSStatus; external name '_KCFindGenericPassword';

{
 *  KCLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in KeychainLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function KCLock(keychain: KCRef): OSStatus; external name '_KCLock';


{$ALIGN MAC68K}


end.
