{
     File:       HIServices/InternetConfig.h
 
     Contains:   Internet Config interfaces
 
     Version:    HIServices-416~44
 
     Copyright:  � 1999-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://bugs.freepascal.org
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
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

unit InternetConfig;
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
uses MacTypes,Files,Aliases,Components,AEDataModel;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{

    ***DEPRECATION NOTICE***

    The Internet Config APIs are officially deprecated in 10.7.

    You can replace your use of Internet Config APIs with LaunchServices APIs.
    For example, to find which application is currently preferred for a
    particular MIME type, use LSCopyApplicationForMIMEType().

}

{
    IMPORTANT NOTES ABOUT THE C HEADERS
    -----------------------------------

    o   When you see the parameter 'y *x', you should be aware that
        you *cannot pass in nil*.  In future this restriction may be eased,
        especially for the attr parameter to ICGetPref.  Parameters where nil
        is legal are declared using the explicit pointer type, ie 'yPtr x'.

    o   Strings are *Pascal* strings.  This means that they must be word aligned.
        MPW and Think C do this automatically.  The last time I checked, Metrowerks
        C does not.  If it still doesn't, then IMHO it's a bug in their compiler
        and you should report it to them.  [IC 1.4 and later no longer require
        word aligned strings.  You can ignore this warning if you require IC 1.4
        or greater.]
}
{*********************************************************************************************}


{$ALIGN MAC68K}

{***********************************************************************************************
  IC error codes
 ***********************************************************************************************}

const
	icPrefNotFoundErr = -666; { preference not found (duh!)  }
	icPermErr = -667; { cannot set preference  }
	icPrefDataErr = -668; { problem with preference data  }
	icInternalErr = -669; { hmm, this is not good  }
	icTruncatedErr = -670; { more data was present than was returned  }
	icNoMoreWritersErr = -671; { you cannot begin a write session because someone else is already doing it  }
	icNothingToOverrideErr = -672; { no component for the override component to capture  }
	icNoURLErr = -673; { no URL found  }
	icConfigNotFoundErr = -674; { no configuration was found  }
	icConfigInappropriateErr = -675; { incorrect manufacturer code  }
	icProfileNotFoundErr = -676; { profile not found  }
	icTooManyProfilesErr = -677;  { too many profiles in database  }

{***********************************************************************************************
  IC versions (not necessarily, but historically, from a component)
 ***********************************************************************************************}

const
	kICComponentInterfaceVersion0 = $00000000; { IC >= 1.0  }
	kICComponentInterfaceVersion1 = $00010000; { IC >= 1.1  }
	kICComponentInterfaceVersion2 = $00020000; { IC >= 1.2  }
	kICComponentInterfaceVersion3 = $00030000; { IC >= 2.0  }
	kICComponentInterfaceVersion4 = $00040000; { IC >= 2.5  }
	kICComponentInterfaceVersion = kICComponentInterfaceVersion4; { current version number is 4  }

{***********************************************************************************************
  opaque type for preference reference
 ***********************************************************************************************}

type
	ICInstance = ^OpaqueICInstance; { an opaque type }
	OpaqueICInstance = record end;
	ICInstancePtr = ^ICInstance;  { when a var xx:ICInstance parameter can be nil, it is changed to xx: ICInstancePtr }
{$ifc not TARGET_CPU_64}
{***********************************************************************************************
  a record that specifies a folder, an array of such records, and a pointer to such an array
 ***********************************************************************************************}
type
	ICDirSpec = record
		vRefNum: SInt16;
		dirID: SIGNEDLONG;
	end;
	ICDirSpecPtr = ^ICDirSpec;

	ICDirSpecArray = array [0..3] of ICDirSpec;
	ICDirSpecArrayPtr = ^ICDirSpecArray;
{$endc} {not TARGET_CPU_64}

{***********************************************************************************************
  preference attributes type, bit number constants, and mask constants
 ***********************************************************************************************}
type
	ICAttr = UInt32;
const
	kICAttrLockedBit = 0;
	kICAttrVolatileBit = 1;

const
	kICAttrNoChange = $FFFFFFFF; { pass this to ICSetPref to tell it not to change the attributes  }
	kICAttrLockedMask = $00000001;
	kICAttrVolatileMask = $00000002;

{***********************************************************************************************
  permissions for use with ICBegin
 ***********************************************************************************************}
type
	ICPerm = UInt8;
const
	icNoPerm = 0;
	icReadOnlyPerm = 1;
	icReadWritePerm = 2;


{***********************************************************************************************
  profile IDs
 ***********************************************************************************************}
type
	ICProfileID = SInt32;
	ICProfileIDPtr = ^ICProfileID;
const
	kICNilProfileID = 0;

{***********************************************************************************************
  other constants
 ***********************************************************************************************}
const
	kICNoUserInteractionBit = 0;

const
	kICNoUserInteractionMask = $00000001;

const
	kICFileType = FourCharCode('ICAp');
	kICCreator = FourCharCode('ICAp');

{***********************************************************************************************
  Apple event constants
 ***********************************************************************************************}
const
	kInternetEventClass = FourCharCode('GURL');
	kAEGetURL = FourCharCode('GURL');
	kAEFetchURL = FourCharCode('FURL');
	keyAEAttaching = FourCharCode('Atch');

{ AERegistry.i defines a compatible keyAEDestination }
const
	kICEditPreferenceEventClass = FourCharCode('ICAp');
	kICEditPreferenceEvent = FourCharCode('ICAp');
	keyICEditPreferenceDestination = FourCharCode('dest');

{***********************************************************************************************
  constants for use with ICGetVersion
 ***********************************************************************************************}
const
	kICComponentVersion = 0;    { Return a component version, comparable to kICComponentInterfaceVersion  }
	kICNumVersion = 1;     { Return a NumVersion structure  }

{***********************************************************************************************
  types and constants for use with kICDocumentFont, et. al.
 ***********************************************************************************************}
type
	ICFontRecord = record
		size: SInt16;
		face: SInt8;
		pad: SInt8;
		font: Str255;
	end;
	ICFontRecordPtr = ^ICFontRecord;
type
	ICFontRecordHandle = ^ICFontRecordPtr;

{***********************************************************************************************
  types and constants for use with kICCharacterSet, et. al.
 ***********************************************************************************************}
type
	ICCharTable = record
		netToMac: packed array [0..255] of UInt8;
		macToNet: packed array [0..255] of UInt8;
	end;
	ICCharTablePtr = ^ICCharTable;
type
	ICCharTableHandle = ^ICCharTablePtr;

{***********************************************************************************************
  types and constants for use with kICHelper, et. al.
 ***********************************************************************************************}
type
	ICAppSpec = record
		fCreator: OSType;
		name: Str63;
	end;
	ICAppSpecPtr = ^ICAppSpec;
type
	ICAppSpecHandle = ^ICAppSpecPtr;
	ICAppSpecList = record
		numberOfItems: SInt16;
		appSpecs: array [0..0] of ICAppSpec;
	end;
	ICAppSpecListPtr = ^ICAppSpecList;
type
	ICAppSpecListHandle = ^ICAppSpecListPtr;

{***********************************************************************************************
  types and constants for use with kICDownloadFolder, et. al.
 ***********************************************************************************************}
type
	ICFileSpec = record
		volName: Str31;                { this field should be ignored, use the alias }
		volCreationDate: SInt32;        { this field should be ignored, use the alias }
		fss: FSSpec;                    { this field should be ignored, use the alias }
		alias: AliasRecord;
                                              { plus extra data, aliasSize 0 means no alias manager present when}
                                              { ICFileSpecification was created}
	end;
	ICFileSpecPtr = ^ICFileSpec;
type
	ICFileSpecHandle = ^ICFileSpecPtr;
const
	kICFileSpecHeaderSize = SizeOf(ICFileSpec) - sizeof(AliasRecord);

{***********************************************************************************************
  types and constants for use with ICMapFilename, et. al.
 ***********************************************************************************************}
type
	ICMapEntryFlags = SInt32;
	ICFixedLength = SInt16;
	ICMapEntry = record
		totalLength: SInt16;
		fixedLength: ICFixedLength;
		version: SInt16;
		fileType: OSType;
		fileCreator: OSType;
		postCreator: OSType;
		flags: ICMapEntryFlags;
                                              { variable part starts here}
		extension: Str255;
		creatorAppName: Str255;
		postAppName: Str255;
		MIMEType: Str255;
		entryName: Str255;
	end;
	ICMapEntryPtr = ^ICMapEntry;
type
	ICMapEntryHandle = ^ICMapEntryPtr;
const
	kICMapFixedLength = 22;

const
	kICMapBinaryBit = 0;    { file should be transfered in binary as opposed to text mode}
	kICMapResourceForkBit = 1;    { the resource fork of the file is significant}
	kICMapDataForkBit = 2;    { the data fork of the file is significant}
	kICMapPostBit = 3;    { post process using post fields}
	kICMapNotIncomingBit = 4;    { ignore this mapping for incoming files}
	kICMapNotOutgoingBit = 5;     { ignore this mapping for outgoing files}

const
	kICMapBinaryMask = $00000001; { file should be transfered in binary as opposed to text mode}
	kICMapResourceForkMask = $00000002; { the resource fork of the file is significant}
	kICMapDataForkMask = $00000004; { the data fork of the file is significant}
	kICMapPostMask = $00000008; { post process using post fields}
	kICMapNotIncomingMask = $00000010; { ignore this mapping for incoming files}
	kICMapNotOutgoingMask = $00000020; { ignore this mapping for outgoing files}

{***********************************************************************************************
  types and constants for use with kICServices, et. al.
 ***********************************************************************************************}
type
	ICServiceEntryFlags = SInt16;
	ICServiceEntry = record
		name: Str255;
		port: SInt16;
		flags: ICServiceEntryFlags;
	end;
	ICServiceEntryPtr = ^ICServiceEntry;
type
	ICServiceEntryHandle = ^ICServiceEntryPtr;

const
	kICServicesTCPBit = 0;
	kICServicesUDPBit = 1;     { both bits can be set, which means the service is both TCP and UDP, eg daytime}

const
	kICServicesTCPMask = $00000001;
	kICServicesUDPMask = $00000002; { both bits can be set, which means the service is both TCP and UDP, eg daytime}

type
	ICServices = record
		count: SInt16;
		services: array [0..0] of ICServiceEntry;
	end;
	ICServicesPtr = ^ICServices;
type
	ICServicesHandle = ^ICServicesPtr;
{***********************************************************************************************
  keys
 ***********************************************************************************************}
{ 
    key reserved for use by Internet Config 
}
const
	kICReservedKey = 'kICReservedKey';
{
    STR# -- formatted, list of Archie servers  
}
const
	kICArchieAll = 'ArchieAll';
{
    PString -- formatted, preferred Archie server   
}
const
	kICArchiePreferred = 'ArchiePreferred';
{
    ICCharTable -- Mac-to-Net and Net-to-Mac character translation   
}
const
	kICCharacterSet = 'CharacterSet';
{
    ICFontRecord -- font used for proportional text   
}
const
	kICDocumentFont = 'DocumentFont';
{
    ICFileSpec -- where to put newly downloaded files   
}
const
	kICDownloadFolder = 'DownloadFolder';
{
    PString -- user@host.domain, email address of user, ie return address   
}
const
	kICEmail = 'Email';
{
    PString -- host.domain, default FTP server   
}
const
	kICFTPHost = 'FTPHost';
{
    PString -- second level FTP proxy authorisation   
}
const
	kICFTPProxyAccount = 'FTPProxyAccount';
{
    PString -- host.domain   
}
const
	kICFTPProxyHost = 'FTPProxyHost';
{
    PString -- scrambled, password for FTPProxyUser   
}
const
	kICFTPProxyPassword = 'FTPProxyPassword';
{
    PString -- first level FTP proxy authorisation   
}
const
	kICFTPProxyUser = 'FTPProxyUser';
{
    PString -- host.domain, default finger server   
}
const
	kICFingerHost = 'FingerHost';
{
    PString -- host.domain, default Gopher server   
}
const
	kICGopherHost = 'GopherHost';
{
    PString -- host.domain, see note in Prog Docs   
}
const
	kICGopherProxy = 'GopherProxy';
{
    PString -- host.domain   
}
const
	kICHTTPProxyHost = 'HTTPProxyHost';
{
    ICAppSpec -- helpers for URL schemes   
}
const
	kICHelper = 'Helper�';
{
    PString -- description for URL scheme   
}
const
	kICHelperDesc = 'HelperDesc�';
{
    ICAppSpecList -- list of common helpers for URL schemes   
}
const
	kICHelperList = 'HelperList�';
{
    PString -- host.domain, Internet Relay Chat server   
}
const
	kICIRCHost = 'IRCHost';
{
    STR# -- formatted, list of Info-Mac servers   
}
const
	kICInfoMacAll = 'InfoMacAll';
{
    PString -- formatted, preferred Info-Mac server   
}
const
	kICInfoMacPreferred = 'InfoMacPreferred';
{
    PString -- string LDAP thing   
}
const
	kICLDAPSearchbase = 'LDAPSearchbase';
{
    PString -- host.domain   
}
const
	kICLDAPServer = 'LDAPServer';
{
    ICFontRecord -- font used for lists of items (eg news article lists)   
}
const
	kICListFont = 'ListFont';
{
    PString -- host for MacSearch queries   
}
const
	kICMacSearchHost = 'MacSearchHost';
{
    PString -- user@host.domain, account from which to fetch mail   
}
const
	kICMailAccount = 'MailAccount';
{
    TEXT -- extra headers for mail messages   
}
const
	kICMailHeaders = 'MailHeaders';
{
    PString -- scrambled, password for MailAccount   
}
const
	kICMailPassword = 'MailPassword';
{
    ICMapEntries -- file type mapping, see documentation   
}
const
	kICMapping = 'Mapping';
{
    PString -- host.domain, NNTP server   
}
const
	kICNNTPHost = 'NNTPHost';
{
    PString -- host.domain, Network Time Protocol (NTP)   
}
const
	kICNTPHost = 'NTPHost';
{
    Boolean   
}
const
	kICNewMailDialog = 'NewMailDialog';
{
    Boolean -- how to announce new mail   
}
const
	kICNewMailFlashIcon = 'NewMailFlashIcon';
{
    Boolean   
}
const
	kICNewMailPlaySound = 'NewMailPlaySound';
{
    PString   
}
const
	kICNewMailSoundName = 'NewMailSoundName';
{
    PString -- scrambled, password for NewsAuthUsername   
}
const
	kICNewsAuthPassword = 'NewsAuthPassword';
{
    PString -- user name for authorised news servers   
}
const
	kICNewsAuthUsername = 'NewsAuthUsername';
{
    TEXT -- extra headers for news messages   
}
const
	kICNewsHeaders = 'NewsHeaders';
{
    STR# -- list of domains not to be proxied   
}
const
	kICNoProxyDomains = 'NoProxyDomains';
{
    PString -- for X-Organization string   
}
const
	kICOrganization = 'Organization';
{
    PString -- host.domain, default Ph server   
}
const
	kICPhHost = 'PhHost';
{
    TEXT -- default response for finger servers   
}
const
	kICPlan = 'Plan';
{
    ICFontRecord -- font used to print ScreenFont   
}
const
	kICPrinterFont = 'PrinterFont';
{
    PString -- used to quote responses in news and mail   
}
const
	kICQuotingString = 'QuotingString';
{
    PString -- real name of user   
}
const
	kICRealName = 'RealName';
{
    PString -- RTSP Proxy Host
}
const
	kICRTSPProxyHost = 'RTSPProxyHost';
{
    PString -- host.domain, SMTP server   
}
const
	kICSMTPHost = 'SMTPHost';
{
    ICFontRecord -- font used for monospaced text (eg news articles)   
}
const
	kICScreenFont = 'ScreenFont';
{
    ICServices -- TCP and IP port-to-name mapping   
}
const
	kICServices = 'Services';
{
    TEXT -- append to news and mail messages   
}
const
	kICSignature = 'Signature';
{
    TEXT -- preferred mailing address   
}
const
	kICSnailMailAddress = 'SnailMailAddress';
{
    PString -- host.domain, remember that host.domain format allows ":port" and " port"  
}
const
	kICSocksHost = 'SocksHost';
{
    PString -- host.domain, default Telnet address   
}
const
	kICTelnetHost = 'TelnetHost';
{
    STR# -- formatted, list of UMich servers   
}
const
	kICUMichAll = 'UMichAll';
{
    PString -- formatted, preferred UMich server   
}
const
	kICUMichPreferred = 'UMichPreferred';
{
    Boolean   
}
const
	kICUseFTPProxy = 'UseFTPProxy';
{
    Boolean   
}
const
	kICUseGopherProxy = 'UseGopherProxy';
{
    Boolean   
}
const
	kICUseHTTPProxy = 'UseHTTPProxy';
{
    Boolean -- use PASV command for FTP transfers   
}
const
	kICUsePassiveFTP = 'UsePassiveFTP';
{
    Boolean
}
const
	kICUseRTSPProxy = 'UseRTSPProxy';
{
    Boolean   
}
const
	kICUseSocks = 'UseSocks';
{
    PString -- no idea   
}
const
	kICWAISGateway = 'WAISGateway';
{
    PString -- URL, users default WWW page   
}
const
	kICWWWHomePage = 'WWWHomePage';
{
    RGBColor -- background colour for web pages   
}
const
	kICWebBackgroundColour = 'WebBackgroundColour';
{
    RGBColor -- colour for read links   
}
const
	kICWebReadColor = '646F6777쩦ebReadColor';
{
    PString -- URL, users default search page   
}
const
	kICWebSearchPagePrefs = 'WebSearchPagePrefs';
{
    RGBColor -- colour for normal text   
}
const
	kICWebTextColor = 'WebTextColor';
{
    Boolean -- whether to underline links   
}
const
	kICWebUnderlineLinks = '646F6777쩦ebUnderlineLinks';
{
    RGBColor -- colour for unread links   
}
const
	kICWebUnreadColor = '646F6777쩦ebUnreadColor';
{
    PString -- host.domain, default whois server   
}
const
	kICWhoisHost = 'WhoisHost';

{***********************************************************************************************

      FUNCTIONS

      What do the annotations after each API mean?
      --------------------------------------------

      [r1] Requires IC 1.1 or higher.
      [r2] Requires IC 1.2 or higher.
      [r3] Requires IC 2.0 or higher.
      [r4] Requires IC 2.5 or higher.
      
      IMPORTANT:

      In IC 2.5, instances automatically use the default configuration.
      You no longer need to configure an instance explicitly, except
      if your code might run with an older version of IC.  So the following
      notes only apply to IC 2.0 and earlier.

      [c1]  You must have specified a configuration before calling this routine.
      
      [c2]  You must have specified the default configuration before calling this
            routine.
      
      [c3]  You do not need to specify a configuration before calling this routine.
      
      [b1]  You must be inside a Begin/End pair when calling this routine.
      
      [b2]  You must be inside a Begin/End read/write pair when calling this routine.
      
      [b3]  You do not need to be inside a Begin/End pair when calling this routine.
      
      [b4]  If you are getting or setting multiple preferences, you should make this
            call inside a Begin/End pair. If you do not make this call inside a Begin/End
            pair, the call will automatically do it for you.
      
      [b5]  It is illegal to call this routine inside a Begin/End pair.

 ***********************************************************************************************}

{ ***** Starting Up and Shutting Down *****  }
{
 *  ICStart()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICStart( var inst: ICInstance; signature: OSType ): OSStatus; external name '_ICStart';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ Call this at application initialisation. Set signature to a value
   * which has been regsitered with DTS to allow for future expansion
   * of the IC system. Returns inst as a connection to the IC system.
   }
{
 *  ICStop()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICStop( inst: ICInstance ): OSStatus; external name '_ICStop';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [b5] 
   * Call this at application initialisation, after which inst
   * is no longer valid connection to IC.
   }
{
 *  ICGetVersion()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetVersion( inst: ICInstance; whichVersion: SIGNEDLONG; var version: UInt32 ): OSStatus; external name '_ICGetVersion';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r4] [c3] [b3] 
   * Returns the version of Internet Config.  Pass kICComponentVersion
   * to get the version as previously returned by GetComponenVerson.
   * Pass kICNumVersion to get a NumVersion structure.
   }
{
 *  ICGetConfigName()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetConfigName( inst: ICInstance; longname: Boolean; var name: Str255 ): OSStatus; external name '_ICGetConfigName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r2] [c1] [b3] 
   * Returns a string that describes the current configuration at a user
   * level. Set longname to true if you want a long name, up to 255
   * characters, or false if you want a short name, typically about 32
   * characters.
   * The returned string is for user display only. If you rely on the
   * exact format of it, you will conflict with any future IC
   * implementation that doesn't use explicit preference files.
   }
{ ***** Getting Information *****  }
{
 *  ICGetSeed()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetSeed( inst: ICInstance; var seed: SIGNEDLONG ): OSStatus; external name '_ICGetSeed';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c3] [b3] 
   * Returns the current seed for the IC prefs database.
   * This seed changes each time a non-volatile preference is changed.
   * You can poll this to determine if any cached preferences change.
   }
{
 *  ICGetPerm()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetPerm( inst: ICInstance; var perm: ICPerm ): OSStatus; external name '_ICGetPerm';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c3] [b3] 
   * Returns the access permissions currently associated with this instance.
   * While applications normally know what permissions they have,
   * this routine is designed for use by override components.
   }
{ ***** Reading and Writing Preferences *****  }
{
 *  ICBegin()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICBegin( inst: ICInstance; perm: ICPerm ): OSStatus; external name '_ICBegin';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b5] 
   * Starting reading or writing multiple preferences.
   * A call to this must be balanced by a call to ICEnd.
   * Do not call WaitNextEvent between these calls.
   * The perm specifies whether you intend to read or read/write.
   * Only one writer is allowed per instance.
   * Note that this may open resource files that are not closed
   * until you call ICEnd.
   }
{
 *  ICGetPref()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetPref( inst: ICInstance; const (*var*) key: Str255; var attr: ICAttr; buf: UnivPtr; var size: SIGNEDLONG ): OSStatus; external name '_ICGetPref';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b4] 
   * Reads the preference specified by key from the IC database to the
   * buffer pointed to by buf and size.
   * key must not be the empty string.
   * If buf is nil then no data is returned.
   * size must be non-negative.
   * attr and size are always set on return. On errors (except icTruncatedErr)
   * attr is set to ICattr_no_change and size is set to 0.
   * size is the actual size of the data.
   * attr is set to the attributes associated with the preference.
   * If this routine returns icTruncatedErr then the other returned
   * values are valid except that only the first size bytes have been
   * return. size is adjusted to reflect the true size of the preference.
   * Returns icPrefNotFound if there is no preference for the key.
   }
{
 *  ICSetPref()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICSetPref( inst: ICInstance; const (*var*) key: Str255; attr: ICAttr; buf: {const} UnivPtr; size: SIGNEDLONG ): OSStatus; external name '_ICSetPref';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b4] 
   * Sets the preference specified by key from the IC database to the
   * value pointed to by buf and size.
   * key must not be the empty string.
   * size must be non-negative. 
   * If buf is nil then the preference value is not set and size is ignored.
   * If buf is not nil then the preference value is set to the size
   * bytes pointed to by buf.
   * If attr is ICattr_no_change then the preference attributes are not set.
   * Otherwise the preference attributes are set to attr.
   * Returns icPermErr if the previous ICBegin was passed icReadOnlyPerm.
   * Returns icPermErr if current attr is locked, new attr is locked and buf <> nil.
   }
{
 *  ICFindPrefHandle()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICFindPrefHandle( inst: ICInstance; const (*var*) key: Str255; var attr: ICAttr; prefh: Handle ): OSStatus; external name '_ICFindPrefHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r2] [c1] [b4] 
   * This routine effectively replaces ICGetPrefHandle.
   * Reads the preference specified by key from the IC database into
   * a handle, prefh.
   * key must not be the empty string.
   * attr is set to the attributes associated with the preference.
   * You must set prefh to a non-nil handle before calling this routine.
   * If the preference does not exist, icPrefNotFoundErr is returned.
   }
{
 *  ICGetPrefHandle()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetPrefHandle( inst: ICInstance; const (*var*) key: Str255; var attr: ICAttr; var prefh: Handle ): OSStatus; external name '_ICGetPrefHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b4] 
   * This routine is now obsolete. Use ICFindPrefHandle instead.
   * Reads the preference specified by key from the IC database into
   * a newly created handle, prefh.
   * key must not be the empty string.
   * attr is set to the attributes associated with the preference.
   * The incoming value of prefh is ignored.
   * A new handle is created in the current heap and returned in prefh.
   * If the routine returns an error, prefh is set to nil.
   * If the preference does not exist, no error is returned and prefh is set
   * to an empty handle.
   }
{
 *  ICSetPrefHandle()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICSetPrefHandle( inst: ICInstance; const (*var*) key: Str255; attr: ICAttr; prefh: Handle ): OSStatus; external name '_ICSetPrefHandle';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b4] 
   * Sets the preference specified by key from the IC database to the
   * value contained in prefh.
   * key must not be the empty string.
   * If prefh is nil then the preference value is not set.
   * If prefh is not nil then the preference value is set to the data
   * contained in it.
   * If attr is ICattr_no_change then the preference attributes are not set.
   * Otherwise the preference attributes are set to attr.
   * Returns icPermErr if the previous ICBegin was passed icReadOnlyPerm.
   * Returns icPermErr if current attr is locked, new attr is locked and prefh <> nil.
   }
{
 *  ICCountPref()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICCountPref( inst: ICInstance; var count: SIGNEDLONG ): OSStatus; external name '_ICCountPref';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b1] 
   * Counts the total number of preferences.
   * If the routine returns an error, count is set to 0.
   }
{
 *  ICGetIndPref()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetIndPref( inst: ICInstance; index: SIGNEDLONG; var key: Str255 ): OSStatus; external name '_ICGetIndPref';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b1] 
   * Returns the key of the index'th preference.
   * index must be positive.
   * Returns icPrefNotFoundErr if index is greater than the total number of preferences.
   * If the routine returns an error, key is undefined.
   }
{
 *  ICDeletePref()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICDeletePref( inst: ICInstance; const (*var*) key: Str255 ): OSStatus; external name '_ICDeletePref';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b2] 
   * Deletes the preference specified by key.
   * key must not be the empty string.
   * Returns icPrefNotFound if the preference specified by key is not present.
   }
{
 *  ICEnd()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICEnd( inst: ICInstance ): OSStatus; external name '_ICEnd';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [c1] [b1] 
   * Terminates a preference session, as started by ICBegin.
   * You must have called ICBegin before calling this routine.
   }
{
 *  ICGetDefaultPref()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetDefaultPref( inst: ICInstance; const (*var*) key: Str255; prefH: Handle ): OSStatus; external name '_ICGetDefaultPref';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r4] [c3] [b5] 
   * Returns a default preference value for the specified key.  You
   * must pass in a valid prefH, which is resized to fit the data.
   }
{ ***** User Interface Stuff *****  }
{
 *  ICEditPreferences()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICEditPreferences( inst: ICInstance; const (*var*) key: Str255 ): OSStatus; external name '_ICEditPreferences';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Instructs IC to display the user interface associated with editing
   * preferences and focusing on the preference specified by key.
   * If key is the empty string then no preference should be focused upon.
   * You must have specified a configuration before calling this routine.
   * You do not need to call ICBegin before calling this routine.
   * In the current implementation this launches the IC application
   * (or brings it to the front) and displays the window containing
   * the preference specified by key.
   * It may have a radically different implementation in future
   * IC systems.
   }
{ ***** URL Handling *****  }
{
 *  ICLaunchURL()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICLaunchURL( inst: ICInstance; const (*var*) hint: Str255; data: {const} UnivPtr; len: SIGNEDLONG; var selStart: SIGNEDLONG; var selEnd: SIGNEDLONG ): OSStatus; external name '_ICLaunchURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Parses a URL out of the specified text and feeds it off to the
   * appropriate helper.
   * hint indicates the default scheme for URLs of the form "name@address".
   * If hint is the empty string then URLs of that form are not allowed.
   * data points to the start of the text. It must not be nil.
   * len indicates the length of the text. It must be non-negative.
   * selStart and selEnd should be passed in as the current selection of
   * the text. This selection is given in the same manner as TextEdit,
   * ie if selStart = selEnd then there is no selection only an insertion
   * point. Also selStart � selEnd and 0 � selStart � len and 0 � selEnd � len.
   * selStart and selEnd are returned as the bounds of the URL. If the
   * routine returns an error then these new boundaries may be
   * invalid but they will be close.
   * The URL is parsed out of the text and passed off to the appropriate
   * helper using the GURL AppleEvent.
   }
{
 *  ICParseURL()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICParseURL( inst: ICInstance; const (*var*) hint: Str255; data: {const} UnivPtr; len: SIGNEDLONG; var selStart: SIGNEDLONG; var selEnd: SIGNEDLONG; url: Handle ): OSStatus; external name '_ICParseURL';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Parses a URL out of the specified text and returns it in a canonical form
   * in a handle.
   * hint indicates the default scheme for URLs of the form "name@address".
   * If hint is the empty string then URLs of that form are not allowed.
   * data points to the start of the text. It must not be nil.
   * len indicates the length of the text. It must be non-negative.
   * selStart and selEnd should be passed in as the current selection of
   * the text. This selection is given in the same manner as TextEdit,
   * ie if selStart = selEnd then there is no selection only an insertion
   * point. Also selStart � selEnd and 0 � selStart � len and 0 � selEnd � len.
   * selStart and selEnd are returned as the bounds of the URL. If the
   * routine returns an error then these new boundaries may be
   * invalid but they will be close.
   * The incoming url handle must not be nil.  The resulting URL is normalised
   * and copied into the url handle, which is resized to fit.
   }
{
 *  ICCreateGURLEvent()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICCreateGURLEvent( inst: ICInstance; helperCreator: OSType; urlH: Handle; var theEvent: AppleEvent ): OSStatus; external name '_ICCreateGURLEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r4] [c1] [b3] 
   * Creates a GURL Apple event, targetted at the application whose creator
   * code is helperCreator, with a direct object containing the URL text from urlH.
   }
{
 *  ICSendGURLEvent()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICSendGURLEvent( inst: ICInstance; var theEvent: AppleEvent ): OSStatus; external name '_ICSendGURLEvent';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r4] [c1] [b3] 
   * Sends theEvent to the target application.
   }
{ ***** Mappings Routines *****
 * 
 * Routines for interrogating mappings database.
 * 
 * ----- High Level Routines -----
  }
{
 *  ICMapFilename()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICMapFilename( inst: ICInstance; const (*var*) filename: Str255; var entry: ICMapEntry ): OSStatus; external name '_ICMapFilename';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b4] 
   * Takes the name of an incoming file and returns the most appropriate
   * mappings database entry, based on its extension.
   * filename must not be the empty string.
   * Returns icPrefNotFoundErr if no suitable entry is found.
   }
{
 *  ICMapTypeCreator()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICMapTypeCreator( inst: ICInstance; fType: OSType; fCreator: OSType; const (*var*) filename: Str255; var entry: ICMapEntry ): OSStatus; external name '_ICMapTypeCreator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b4] 
   * Takes the type and creator (and optionally the name) of an outgoing
   * file and returns the most appropriate mappings database entry.
   * The filename may be either the name of the outgoing file or
   * the empty string.
   * Returns icPrefNotFoundErr if no suitable entry found.
   }
{ ----- Mid Level Routines -----  }
{
 *  ICMapEntriesFilename()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICMapEntriesFilename( inst: ICInstance; entries: Handle; const (*var*) filename: Str255; var entry: ICMapEntry ): OSStatus; external name '_ICMapEntriesFilename';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Takes the name of an incoming file and returns the most appropriate
   * mappings database entry, based on its extension.
   * entries must be a handle to a valid IC mappings database preference.
   * filename must not be the empty string.
   * Returns icPrefNotFoundErr if no suitable entry is found.
   }
{
 *  ICMapEntriesTypeCreator()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICMapEntriesTypeCreator( inst: ICInstance; entries: Handle; fType: OSType; fCreator: OSType; const (*var*) filename: Str255; var entry: ICMapEntry ): OSStatus; external name '_ICMapEntriesTypeCreator';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Takes the type and creator (and optionally the name) of an outgoing
   * file and returns the most appropriate mappings database entry.
   * entries must be a handle to a valid IC mappings database preference.
   * The filename may be either the name of the outgoing file or
   * the empty string.
   * Returns icPrefNotFoundErr if no suitable entry found.
   }
{ ----- Low Level Routines -----  }
{
 *  ICCountMapEntries()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICCountMapEntries( inst: ICInstance; entries: Handle; var count: SIGNEDLONG ): OSStatus; external name '_ICCountMapEntries';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Counts the number of entries in the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * count is set to the number of entries.
   }
{
 *  ICGetIndMapEntry()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetIndMapEntry( inst: ICInstance; entries: Handle; index: SIGNEDLONG; var pos: SIGNEDLONG; var entry: ICMapEntry ): OSStatus; external name '_ICGetIndMapEntry';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Gets the index'th entry in the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * index must be in the range from 1 to the number of entries in the database.
   * The value of pos is ignored on input. pos is set to the position of
   * the index'th entry in the database and is suitable for passing back
   * into ICSetMapEntry.
   * Does not return any user data associated with the entry.
   }
{
 *  ICGetMapEntry()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetMapEntry( inst: ICInstance; entries: Handle; pos: SIGNEDLONG; var entry: ICMapEntry ): OSStatus; external name '_ICGetMapEntry';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Returns the entry located at position pos in the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * pos should be 0 to get the first entry. To get the subsequent entries, add
   * entry.total_size to pos and iterate.
   * Does not return any user data associated with the entry.
   }
{
 *  ICSetMapEntry()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICSetMapEntry( inst: ICInstance; entries: Handle; pos: SIGNEDLONG; const (*var*) entry: ICMapEntry ): OSStatus; external name '_ICSetMapEntry';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Sets the entry located at position pos in the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * pos should be either a value returned from ICGetIndMapEntry or a value
   * calculated using ICGetMapEntry.
   * entry is a var parameter purely for stack space reasons. It is not
   * modified in any way.
   * Any user data associated with the entry is unmodified.
   }
{
 *  ICDeleteMapEntry()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICDeleteMapEntry( inst: ICInstance; entries: Handle; pos: SIGNEDLONG ): OSStatus; external name '_ICDeleteMapEntry';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Deletes the mappings database entry at pos.
   * entries must be a handle to a valid IC mappings database preference.
   * pos should be either a value returned from ICGetIndMapEntry or a value
   * calculated using ICGetMapEntry.
   * Also deletes any user data associated with the entry.
   }
{
 *  ICAddMapEntry()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICAddMapEntry( inst: ICInstance; entries: Handle; const (*var*) entry: ICMapEntry ): OSStatus; external name '_ICAddMapEntry';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r1] [c1] [b3] 
   * Adds an entry to the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * The entry is added to the end of the entries database.
   * No user data is added.
   }
{ ***** Profile Management Routines *****  }
{
 *  ICGetCurrentProfile()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetCurrentProfile( inst: ICInstance; var currentID: ICProfileID ): OSStatus; external name '_ICGetCurrentProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b3] 
   * Returns the profile ID of the current profile.
   }
{
 *  ICSetCurrentProfile()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICSetCurrentProfile( inst: ICInstance; newID: ICProfileID ): OSStatus; external name '_ICSetCurrentProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b3] 
   * Sets the current profile to the profile specified in newProfile.
   }
{
 *  ICCountProfiles()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICCountProfiles( inst: ICInstance; var count: SIGNEDLONG ): OSStatus; external name '_ICCountProfiles';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b1] 
   * Returns the total number of profiles.
   }
{
 *  ICGetIndProfile()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetIndProfile( inst: ICInstance; index: SIGNEDLONG; var thisID: ICProfileID ): OSStatus; external name '_ICGetIndProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b1] 
   * Returns the profile ID of the index'th profile.  index must be positive.
   * Returns icProfileNotFoundErr if index is greater than the total number
   * of profiles.
   }
{
 *  ICGetProfileName()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICGetProfileName( inst: ICInstance; thisID: ICProfileID; var name: Str255 ): OSStatus; external name '_ICGetProfileName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b3] 
   * Returns the name of a profile given its ID.  The name may not uniquely
   * identify the profile.  [That's what the profile ID is for!]  The name
   * is assumed to be in the system script.
   }
{
 *  ICSetProfileName()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICSetProfileName( inst: ICInstance; thisID: ICProfileID; const (*var*) name: Str255 ): OSStatus; external name '_ICSetProfileName';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b3] 
   * This routine sets the name of the specified profile.  Profile names
   * need not be unique.  The name should be in the system script.
   }
{
 *  ICAddProfile()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICAddProfile( inst: ICInstance; prototypeID: ICProfileID; var newID: ICProfileID ): OSStatus; external name '_ICAddProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b2] 
   * If prototypeID = kICNilProfileID, this routine
   * creates a default profile, otherwise it creates a
   * profile by cloning the prototype profile.  The ID
   * of the new profile is returned in newID.
   * The new profile will be give a new, unique, name.
   * This does not switch to the new profile.
   }
{
 *  ICDeleteProfile()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in ApplicationServices.framework but deprecated in 10.7
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 }
function ICDeleteProfile( inst: ICInstance; thisID: ICProfileID ): OSStatus; external name '_ICDeleteProfile';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_7 *)


{ [r3] [c1] [b2] 
   * This routine deletes the profile specified by
   * thisID.  Attempting to delete the current profile
   * or the last profile will return error.
   }

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
