{
     File:       InternetConfig.p
 
     Contains:   Internet Config interfaces
 
     Version:    Technology: based on IC 2.5 alpha
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  � 1999-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{*********************************************************************************************}


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

unit InternetConfig;
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
uses MacTypes,Files,Aliases,Components,AEDataModel;


{$ALIGN MAC68K}

{***********************************************************************************************
  IC error codes
 ***********************************************************************************************}


const
	icPrefNotFoundErr			= -666;							{  preference not found (duh!)   }
	icPermErr					= -667;							{  cannot set preference   }
	icPrefDataErr				= -668;							{  problem with preference data   }
	icInternalErr				= -669;							{  hmm, this is not good   }
	icTruncatedErr				= -670;							{  more data was present than was returned   }
	icNoMoreWritersErr			= -671;							{  you cannot begin a write session because someone else is already doing it   }
	icNothingToOverrideErr		= -672;							{  no component for the override component to capture   }
	icNoURLErr					= -673;							{  no URL found   }
	icConfigNotFoundErr			= -674;							{  no configuration was found   }
	icConfigInappropriateErr	= -675;							{  incorrect manufacturer code   }
	icProfileNotFoundErr		= -676;							{  profile not found   }
	icTooManyProfilesErr		= -677;							{  too many profiles in database   }

	{	***********************************************************************************************
	  IC versions (not necessarily, but historically, from a component)
	 ***********************************************************************************************	}

	kICComponentInterfaceVersion0 = $00000000;					{  IC >= 1.0   }
	kICComponentInterfaceVersion1 = $00010000;					{  IC >= 1.1   }
	kICComponentInterfaceVersion2 = $00020000;					{  IC >= 1.2   }
	kICComponentInterfaceVersion3 = $00030000;					{  IC >= 2.0   }
	kICComponentInterfaceVersion4 = $00040000;					{  IC >= 2.5   }
	kICComponentInterfaceVersion = $00040000;					{  current version number is 4   }

	{	***********************************************************************************************
	  opaque type for preference reference
	 ***********************************************************************************************	}


type
	ICInstance    = ^SInt32; { an opaque 32-bit type }
	ICInstancePtr = ^ICInstance;  { when a var xx:ICInstance parameter can be nil, it is changed to xx: ICInstancePtr }

	{	***********************************************************************************************
	  a record that specifies a folder, an array of such records, and a pointer to such an array
	 ***********************************************************************************************	}
	ICDirSpecPtr = ^ICDirSpec;
	ICDirSpec = record
		vRefNum:				SInt16;
		dirID:					SInt32;
	end;

	ICDirSpecArray						= array [0..3] of ICDirSpec;
	ICDirSpecArrayPtr					= ^ICDirSpecArray;

	{	***********************************************************************************************
	  preference attributes type, bit number constants, and mask constants
	 ***********************************************************************************************	}
	ICAttr								= UInt32;


const
	kICAttrLockedBit			= 0;
	kICAttrVolatileBit			= 1;

	kICAttrNoChange				= $FFFFFFFF;					{  pass this to ICSetPref to tell it not to change the attributes   }
	kICAttrLockedMask			= $00000001;
	kICAttrVolatileMask			= $00000002;

	{	***********************************************************************************************
	  permissions for use with ICBegin
	 ***********************************************************************************************	}


type
	ICPerm								= UInt8;


const
	icNoPerm					= 0;
	icReadOnlyPerm				= 1;
	icReadWritePerm				= 2;

	{	***********************************************************************************************
	  a reference to an instance's current configuration
	 ***********************************************************************************************	}

{$ifc CALL_NOT_IN_CARBON}

type
	ICConfigRefPtr = ^ICConfigRef;
	ICConfigRef = record
		manufacturer:			OSType;
																		{  other private data follows   }
	end;

	ICConfigRefHandle					= ^ICConfigRefPtr;

{$endc}  {CALL_NOT_IN_CARBON}

{***********************************************************************************************
  profile IDs
 ***********************************************************************************************}

type
	ICProfileID							= SInt32;
	ICProfileIDPtr						= ^ICProfileID;


const
	kICNilProfileID				= 0;

	{	***********************************************************************************************
	  other constants
	 ***********************************************************************************************	}

	kICNoUserInteractionBit		= 0;

	kICNoUserInteractionMask	= $00000001;

	kICFileType					= $49434170 (* 'ICAp' *);
	kICCreator					= $49434170 (* 'ICAp' *);

	{	***********************************************************************************************
	  Apple event constants
	 ***********************************************************************************************	}

	kInternetEventClass			= $4755524C (* 'GURL' *);
	kAEGetURL					= $4755524C (* 'GURL' *);
	kAEFetchURL					= $4655524C (* 'FURL' *);
	keyAEAttaching				= $41746368 (* 'Atch' *);

	{	 AERegistry.i defines a compatible keyAEDestination 	}

	kICEditPreferenceEventClass	= $49434170 (* 'ICAp' *);
	kICEditPreferenceEvent		= $49434170 (* 'ICAp' *);
	keyICEditPreferenceDestination = $64657374 (* 'dest' *);

	{	***********************************************************************************************
	  constants for use with ICGetVersion
	 ***********************************************************************************************	}

	kICComponentVersion			= 0;							{  Return a component version, comparable to kICComponentInterfaceVersion   }
	kICNumVersion				= 1;							{  Return a NumVersion structure   }

	{	***********************************************************************************************
	  types and constants for use with kICDocumentFont, et. al.
	 ***********************************************************************************************	}

type
	ICFontRecordPtr = ^ICFontRecord;
	ICFontRecord = record
		size:					SInt16;
		face:					SInt8;
		pad:					SInt8;
		font:					Str255;
	end;

	ICFontRecordHandle					= ^ICFontRecordPtr;

	{	***********************************************************************************************
	  types and constants for use with kICCharacterSet, et. al.
	 ***********************************************************************************************	}
	ICCharTablePtr = ^ICCharTable;
	ICCharTable = record
		netToMac:				packed array [0..255] of UInt8;
		macToNet:				packed array [0..255] of UInt8;
	end;

	ICCharTableHandle					= ^ICCharTablePtr;

	{	***********************************************************************************************
	  types and constants for use with kICHelper, et. al.
	 ***********************************************************************************************	}
	ICAppSpecPtr = ^ICAppSpec;
	ICAppSpec = record
		fCreator:				OSType;
		name:					Str63;
	end;

	ICAppSpecHandle						= ^ICAppSpecPtr;
	ICAppSpecListPtr = ^ICAppSpecList;
	ICAppSpecList = record
		numberOfItems:			SInt16;
		appSpecs:				array [0..0] of ICAppSpec;
	end;

	ICAppSpecListHandle					= ^ICAppSpecListPtr;

	{	***********************************************************************************************
	  types and constants for use with kICDownloadFolder, et. al.
	 ***********************************************************************************************	}

{$ifc NOT OLDROUTINENAMES}
	ICFileSpecPtr = ^ICFileSpec;
	ICFileSpec = record
		volName:				Str31;
		volCreationDate:		SInt32;
		fss:					FSSpec;
		alias:					AliasRecord;
																		{  plus extra data, aliasSize 0 means no alias manager present when }
																		{  ICFileSpecification was created }
	end;

	ICFileSpecHandle					= ^ICFileSpecPtr;
{$elsec}
	ICFileSpecPtr = ^ICFileSpec;
	ICFileSpec = record
		vol_name:				Str31;
		vol_creation_date:		SInt32;
		fss:					FSSpec;
		alias:					AliasRecord;
	end;

	ICFileSpecHandle					= ^ICFileSpecPtr;
{$endc}


const
	kICFileSpecHeaderSize		= 106;

	{	***********************************************************************************************
	  types and constants for use with ICMapFilename, et. al.
	 ***********************************************************************************************	}

type
	ICMapEntryFlags						= SInt32;
	ICFixedLength						= SInt16;

{$ifc NOT OLDROUTINENAMES}
	ICMapEntryPtr = ^ICMapEntry;
	ICMapEntry = record
		totalLength:			SInt16;
		fixedLength:			ICFixedLength;
		version:				SInt16;
		fileType:				OSType;
		fileCreator:			OSType;
		postCreator:			OSType;
		flags:					ICMapEntryFlags;
																		{  variable part starts here }
		extension:				Str255;
		creatorAppName:			Str255;
		postAppName:			Str255;
		MIMEType:				Str255;
		entryName:				Str255;
	end;

	ICMapEntryHandle					= ^ICMapEntryPtr;

{$elsec}
	ICMapEntryPtr = ^ICMapEntry;
	ICMapEntry = record
		total_length:			SInt16;
		fixed_length:			ICFixedLength;
		version:				SInt16;
		file_type:				OSType;
		file_creator:			OSType;
		post_creator:			OSType;
		flags:					ICMapEntryFlags;
		extension:				Str255;
		creator_app_name:		Str255;
		post_app_name:			Str255;
		MIME_type:				Str255;
		entry_name:				Str255;
	end;

	ICMapEntryHandle					= ^ICMapEntryPtr;
{$endc}


const
	kICMapFixedLength			= 22;							{  number in fixedLength field }

	kICMapBinaryBit				= 0;							{  file should be transfered in binary as opposed to text mode }
	kICMapResourceForkBit		= 1;							{  the resource fork of the file is significant }
	kICMapDataForkBit			= 2;							{  the data fork of the file is significant }
	kICMapPostBit				= 3;							{  post process using post fields }
	kICMapNotIncomingBit		= 4;							{  ignore this mapping for incoming files }
	kICMapNotOutgoingBit		= 5;							{  ignore this mapping for outgoing files }

	kICMapBinaryMask			= $00000001;					{  file should be transfered in binary as opposed to text mode }
	kICMapResourceForkMask		= $00000002;					{  the resource fork of the file is significant }
	kICMapDataForkMask			= $00000004;					{  the data fork of the file is significant }
	kICMapPostMask				= $00000008;					{  post process using post fields }
	kICMapNotIncomingMask		= $00000010;					{  ignore this mapping for incoming files }
	kICMapNotOutgoingMask		= $00000020;					{  ignore this mapping for outgoing files }

	{	***********************************************************************************************
	  types and constants for use with kICServices, et. al.
	 ***********************************************************************************************	}

type
	ICServiceEntryFlags					= SInt16;
	ICServiceEntryPtr = ^ICServiceEntry;
	ICServiceEntry = record
		name:					Str255;
		port:					SInt16;
		flags:					ICServiceEntryFlags;
	end;

	ICServiceEntryHandle				= ^ICServiceEntryPtr;


const
	kICServicesTCPBit			= 0;
	kICServicesUDPBit			= 1;							{  both bits can be set, which means the service is both TCP and UDP, eg daytime }

	kICServicesTCPMask			= $00000001;
	kICServicesUDPMask			= $00000002;					{  both bits can be set, which means the service is both TCP and UDP, eg daytime }


type
	ICServicesPtr = ^ICServices;
	ICServices = record
		count:					SInt16;
		services:				array [0..0] of ICServiceEntry;
	end;

	ICServicesHandle					= ^ICServicesPtr;

	{	***********************************************************************************************
	  default file name, for internal use, overridden by a component resource
	 ***********************************************************************************************	}

{$ifc CALL_NOT_IN_CARBON}

const
	kICDefaultFileName			= 'Internet Preferences';
{$endc}  {CALL_NOT_IN_CARBON}

{***********************************************************************************************
  keys
 ***********************************************************************************************}
{ 
    key reserved for use by Internet Config 
}

const
	kICReservedKey				= 'kICReservedKey';
	{	
	    STR# -- formatted, list of Archie servers  
		}
	kICArchieAll				= 'ArchieAll';
	{	
	    PString -- formatted, preferred Archie server   
		}
	kICArchiePreferred			= 'ArchiePreferred';
	{	
	    ICCharTable -- Mac-to-Net and Net-to-Mac character translation   
		}
	kICCharacterSet				= 'CharacterSet';
	{	
	    ICFontRecord -- font used for proportional text   
		}
	kICDocumentFont				= 'DocumentFont';
	{	
	    ICFileSpec -- where to put newly downloaded files   
		}
	kICDownloadFolder			= 'DownloadFolder';
	{	
	    PString -- user@host.domain, email address of user, ie return address   
		}
	kICEmail					= 'Email';
	{	
	    PString -- host.domain, default FTP server   
		}
	kICFTPHost					= 'FTPHost';
	{	
	    PString -- second level FTP proxy authorisation   
		}
	kICFTPProxyAccount			= 'FTPProxyAccount';
	{	
	    PString -- host.domain   
		}
	kICFTPProxyHost				= 'FTPProxyHost';
	{	
	    PString -- scrambled, password for FTPProxyUser   
		}
	kICFTPProxyPassword			= 'FTPProxyPassword';
	{	
	    PString -- first level FTP proxy authorisation   
		}
	kICFTPProxyUser				= 'FTPProxyUser';
	{	
	    PString -- host.domain, default finger server   
		}
	kICFingerHost				= 'FingerHost';
	{	
	    PString -- host.domain, default Gopher server   
		}
	kICGopherHost				= 'GopherHost';
	{	
	    PString -- host.domain, see note in Prog Docs   
		}
	kICGopherProxy				= 'GopherProxy';
	{	
	    PString -- host.domain   
		}
	kICHTTPProxyHost			= 'HTTPProxyHost';
	{	
	    ICAppSpec -- helpers for URL schemes   
		}
	kICHelper					= 'Helper�';
	{	
	    PString -- description for URL scheme   
		}
	kICHelperDesc				= 'HelperDesc�';
	{	
	    ICAppSpecList -- list of common helpers for URL schemes   
		}
	kICHelperList				= 'HelperList�';
	{	
	    PString -- host.domain, Internet Relay Chat server   
		}
	kICIRCHost					= 'IRCHost';
	{	
	    STR# -- formatted, list of Info-Mac servers   
		}
	kICInfoMacAll				= 'InfoMacAll';
	{	
	    PString -- formatted, preferred Info-Mac server   
		}
	kICInfoMacPreferred			= 'InfoMacPreferred';
	{	
	    PString -- string LDAP thing   
		}
	kICLDAPSearchbase			= 'LDAPSearchbase';
	{	
	    PString -- host.domain   
		}
	kICLDAPServer				= 'LDAPServer';
	{	
	    ICFontRecord -- font used for lists of items (eg news article lists)   
		}
	kICListFont					= 'ListFont';
	{	
	    PString -- host for MacSearch queries   
		}
	kICMacSearchHost			= 'MacSearchHost';
	{	
	    PString -- user@host.domain, account from which to fetch mail   
		}
	kICMailAccount				= 'MailAccount';
	{	
	    TEXT -- extra headers for mail messages   
		}
	kICMailHeaders				= 'MailHeaders';
	{	
	    PString -- scrambled, password for MailAccount   
		}
	kICMailPassword				= 'MailPassword';
	{	
	    ICMapEntries -- file type mapping, see documentation   
		}
	kICMapping					= 'Mapping';
	{	
	    PString -- host.domain, NNTP server   
		}
	kICNNTPHost					= 'NNTPHost';
	{	
	    PString -- host.domain, Network Time Protocol (NTP)   
		}
	kICNTPHost					= 'NTPHost';
	{	
	    Boolean   
		}
	kICNewMailDialog			= 'NewMailDialog';
	{	
	    Boolean -- how to announce new mail   
		}
	kICNewMailFlashIcon			= 'NewMailFlashIcon';
	{	
	    Boolean   
		}
	kICNewMailPlaySound			= 'NewMailPlaySound';
	{	
	    PString   
		}
	kICNewMailSoundName			= 'NewMailSoundName';
	{	
	    PString -- scrambled, password for NewsAuthUsername   
		}
	kICNewsAuthPassword			= 'NewsAuthPassword';
	{	
	    PString -- user name for authorised news servers   
		}
	kICNewsAuthUsername			= 'NewsAuthUsername';
	{	
	    TEXT -- extra headers for news messages   
		}
	kICNewsHeaders				= 'NewsHeaders';
	{	
	    STR# -- list of domains not to be proxied   
		}
	kICNoProxyDomains			= 'NoProxyDomains';
	{	
	    PString -- for X-Organization string   
		}
	kICOrganization				= 'Organization';
	{	
	    PString -- host.domain, default Ph server   
		}
	kICPhHost					= 'PhHost';
	{	
	    TEXT -- default response for finger servers   
		}
	kICPlan						= $506C616E (* 'Plan' *);
	{	
	    ICFontRecord -- font used to print ScreenFont   
		}
	kICPrinterFont				= 'PrinterFont';
	{	
	    PString -- used to quote responses in news and mail   
		}
	kICQuotingString			= 'QuotingString';
	{	
	    PString -- real name of user   
		}
	kICRealName					= 'RealName';
	{	
	    PString -- RTSP Proxy Host
		}
	kICRTSPProxyHost			= 'RTSPProxyHost';
	{	
	    PString -- host.domain, SMTP server   
		}
	kICSMTPHost					= 'SMTPHost';
	{	
	    ICFontRecord -- font used for monospaced text (eg news articles)   
		}
	kICScreenFont				= 'ScreenFont';
	{	
	    ICServices -- TCP and IP port-to-name mapping   
		}
	kICServices					= 'Services';
	{	
	    TEXT -- append to news and mail messages   
		}
	kICSignature				= 'Signature';
	{	
	    TEXT -- preferred mailing address   
		}
	kICSnailMailAddress			= 'SnailMailAddress';
	{	
	    PString -- host.domain, remember that host.domain format allows ":port" and " port"  
		}
	kICSocksHost				= 'SocksHost';
	{	
	    PString -- host.domain, default Telnet address   
		}
	kICTelnetHost				= 'TelnetHost';
	{	
	    STR# -- formatted, list of UMich servers   
		}
	kICUMichAll					= 'UMichAll';
	{	
	    PString -- formatted, preferred UMich server   
		}
	kICUMichPreferred			= 'UMichPreferred';
	{	
	    Boolean   
		}
	kICUseFTPProxy				= 'UseFTPProxy';
	{	
	    Boolean   
		}
	kICUseGopherProxy			= 'UseGopherProxy';
	{	
	    Boolean   
		}
	kICUseHTTPProxy				= 'UseHTTPProxy';
	{	
	    Boolean -- use PASV command for FTP transfers   
		}
	kICUsePassiveFTP			= 'UsePassiveFTP';
	{	
	    Boolean
		}
	kICUseRTSPProxy				= 'UseRTSPProxy';
	{	
	    Boolean   
		}
	kICUseSocks					= 'UseSocks';
	{	
	    PString -- no idea   
		}
	kICWAISGateway				= 'WAISGateway';
	{	
	    PString -- URL, users default WWW page   
		}
	kICWWWHomePage				= 'WWWHomePage';
	{	
	    RGBColor -- background colour for web pages   
		}
	kICWebBackgroundColour		= 'WebBackgroundColour';
	{	
	    RGBColor -- colour for read links   
		}
	kICWebReadColor				= '646F6777쩦ebReadColor';
	{	
	    PString -- URL, users default search page   
		}
	kICWebSearchPagePrefs		= 'WebSearchPagePrefs';
	{	
	    RGBColor -- colour for normal text   
		}
	kICWebTextColor				= 'WebTextColor';
	{	
	    Boolean -- whether to underline links   
		}
	kICWebUnderlineLinks		= '646F6777쩦ebUnderlineLinks';
	{	
	    RGBColor -- colour for unread links   
		}
	kICWebUnreadColor			= '646F6777쩦ebUnreadColor';
	{	
	    PString -- host.domain, default whois server   
		}
	kICWhoisHost				= 'WhoisHost';

	{	***********************************************************************************************
	
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
	
	 ***********************************************************************************************	}

	{	 ***** Starting Up and Shutting Down *****  	}
	{
	 *  ICStart()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ICStart(var inst: ICInstance; signature: OSType): OSStatus; external name '_ICStart';

{ Call this at application initialisation. Set signature to a value
   * which has been regsitered with DTS to allow for future expansion
   * of the IC system. Returns inst as a connection to the IC system.
   }
{
 *  ICStop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICStop(inst: ICInstance): OSStatus; external name '_ICStop';

{ [b5] 
   * Call this at application initialisation, after which inst
   * is no longer valid connection to IC.
   }
{
 *  ICGetVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetVersion(inst: ICInstance; whichVersion: SInt32; var version: UInt32): OSStatus; external name '_ICGetVersion';
{ [r4] [c3] [b3] 
   * Returns the version of Internet Config.  Pass kICComponentVersion
   * to get the version as previously returned by GetComponenVerson.
   * Pass kICNumVersion to get a NumVersion structure.
   }
{ ***** Specifying a Configuration *****  }
{$ifc CALL_NOT_IN_CARBON}
{
 *  ICFindConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICFindConfigFile(inst: ICInstance; count: SInt16; folders: ICDirSpecArrayPtr): OSStatus; external name '_ICFindConfigFile';
{ [b5] 
   * Call to configure this connection to IC.
   * Set count as the number of valid elements in folders.
   * Set folders to a pointer to the folders to search.
   * Setting count to 0 and folders to nil is OK.
   * Searches the specified folders and then the Preferences folder
   * in a unspecified manner.
   }
{
 *  ICFindUserConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICFindUserConfigFile(inst: ICInstance; var where: ICDirSpec): OSStatus; external name '_ICFindUserConfigFile';
{ [r1] [b5] 
   * Similar to ICFindConfigFile except that it only searches the folder
   * specified in where.  If the input parameters are valid the routine
   * will always successful configure the instance, creating an
   * empty configuration if necessary
   * For use with double-clickable preference files.
   }
{
 *  ICGeneralFindConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICGeneralFindConfigFile(inst: ICInstance; searchPrefs: boolean; canCreate: boolean; count: SInt16; folders: ICDirSpecArrayPtr): OSStatus; external name '_ICGeneralFindConfigFile';
{ [r2] [b5] 
   * Call to configure this connection to IC.
   * This routine acts as a more general replacement for
   * ICFindConfigFile and ICFindUserConfigFile.
   * Set search_prefs to true if you want it to search the preferences folder.
   * Set can_create to true if you want it to be able to create a new config.
   * Set count as the number of valid elements in folders.
   * Set folders to a pointer to the folders to search.
   * Setting count to 0 and folders to nil is OK.
   * Searches the specified folders and then optionally the Preferences folder
   * in a unspecified manner.
   }
{
 *  ICChooseConfig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICChooseConfig(inst: ICInstance): OSStatus; external name '_ICChooseConfig';
{ [r2] [b5] 
   * Requests the user to choose a configuration, typically using some
   * sort of modal dialog. If the user cancels the dialog the configuration
   * state will be unaffected.
   }
{
 *  ICChooseNewConfig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICChooseNewConfig(inst: ICInstance): OSStatus; external name '_ICChooseNewConfig';
{ [r2] [b5] 
   * Requests the user to create a new configuration, typically using some
   * sort of modal dialog. If the user cancels the dialog the configuration
   * state will be unaffected.
   }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  ICGetConfigName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetConfigName(inst: ICInstance; longname: boolean; var name: Str255): OSStatus; external name '_ICGetConfigName';
{ [r2] [c1] [b3] 
   * Returns a string that describes the current configuration at a user
   * level. Set longname to true if you want a long name, up to 255
   * characters, or false if you want a short name, typically about 32
   * characters.
   * The returned string is for user display only. If you rely on the
   * exact format of it, you will conflict with any future IC
   * implementation that doesn't use explicit preference files.
   }
{$ifc CALL_NOT_IN_CARBON}
{
 *  ICGetConfigReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICGetConfigReference(inst: ICInstance; ref: ICConfigRefHandle): OSStatus; external name '_ICGetConfigReference';
{ [r2] [c1] [b3] 
   * Returns a self-contained reference to the instance's current
   * configuration.
   * ref must be a valid non-nil handle and it will be resized to fit the
   * resulting data.
   }
{
 *  ICSetConfigReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICSetConfigReference(inst: ICInstance; ref: ICConfigRefHandle; flags: SInt32): OSStatus; external name '_ICSetConfigReference';
{ [r2] [b5] 
   * Reconfigures the instance using a configuration reference that was
   * got using ICGetConfigReference reference. Set the
   * icNoUserInteraction_bit in flags if you require that this routine
   * not present a modal dialog. Other flag bits are reserved and should
   * be set to zero.
   * ref must not be nil.
   }
{ ***** Private Routines *****
 * 
 * If you are calling these routines, you are most probably doing something
 * wrong.  Please read the documentation for more details.
  }
{
 *  ICSpecifyConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICSpecifyConfigFile(inst: ICInstance; var config: FSSpec): OSStatus; external name '_ICSpecifyConfigFile';
{ [b5] 
   * For use only by the IC application.
   * If you call this routine yourself, you will conflict with any
   * future IC implementation that doesn't use explicit preference files.
   }
{
 *  ICRefreshCaches()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICRefreshCaches(inst: ICInstance): OSStatus; external name '_ICRefreshCaches';
{ [r3] [c1] [b3] 
   * For use only by the IC application.
   * If you call this routine yourself, you will conflict with any
   * future IC implementation that doesn't use explicit preference files.
   }
{ ***** Getting Information *****  }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  ICGetSeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetSeed(inst: ICInstance; var seed: SInt32): OSStatus; external name '_ICGetSeed';
{ [c3] [b3] 
   * Returns the current seed for the IC prefs database.
   * This seed changes each time a non-volatile preference is changed.
   * You can poll this to determine if any cached preferences change.
   }
{
 *  ICGetPerm()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetPerm(inst: ICInstance; var perm: ICPerm): OSStatus; external name '_ICGetPerm';
{ [c3] [b3] 
   * Returns the access permissions currently associated with this instance.
   * While applications normally know what permissions they have,
   * this routine is designed for use by override components.
   }
{$ifc CALL_NOT_IN_CARBON}
{
 *  ICDefaultFileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICDefaultFileName(inst: ICInstance; var name: Str63): OSStatus; external name '_ICDefaultFileName';
{ [c3] [b3] 
   * Returns the default file name for IC preference files.
   * Applications should never need to call this routine.
   * If you rely on information returned by this routine yourself,
   * you may conflict with any future IC implementation that doesn't use
   * explicit preference files.
   * The component calls this routine to set up the default IC file name.
   * This allows this operation to be intercepted by a component that has
   * captured us. It currently gets it from the component resource file.
   * The glue version is hardwired to "Internet Preferences".
   }
{
 *  ICGetComponentInstance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICGetComponentInstance(inst: ICInstance; var componentInst: ComponentInstance): OSStatus; external name '_ICGetComponentInstance';

{ [c3] [b3] 
   * Returns noErr and the connection to the IC component,
   * if we're using the component.
   * Returns badComponenInstance and nil if we're operating with glue.
   }
{ ***** Reading and Writing Preferences *****  }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  ICBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICBegin(inst: ICInstance; perm: ByteParameter): OSStatus; external name '_ICBegin';
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
 *  ICGetPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetPref(inst: ICInstance; const (*var*) key: Str255; var attr: ICAttr; buf: UnivPtr; var size: SInt32): OSStatus; external name '_ICGetPref';
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
 *  ICSetPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICSetPref(inst: ICInstance; const (*var*) key: Str255; attr: ICAttr; buf: UnivPtr; size: SInt32): OSStatus; external name '_ICSetPref';
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
 *  ICFindPrefHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICFindPrefHandle(inst: ICInstance; const (*var*) key: Str255; var attr: ICAttr; prefh: Handle): OSStatus; external name '_ICFindPrefHandle';
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
 *  ICGetPrefHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetPrefHandle(inst: ICInstance; const (*var*) key: Str255; var attr: ICAttr; var prefh: Handle): OSStatus; external name '_ICGetPrefHandle';
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
 *  ICSetPrefHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICSetPrefHandle(inst: ICInstance; const (*var*) key: Str255; attr: ICAttr; prefh: Handle): OSStatus; external name '_ICSetPrefHandle';
{ [r1] [c1] [b4] 
   * Sets the preference specified by key from the IC database to the
   * value contained in prefh.
   * key must not be the empty string.
   * If prefh is nil then the preference value is not set.
   * If buf is not nil then the preference value is set to the data
   * contained in it.
   * If attr is ICattr_no_change then the preference attributes are not set.
   * Otherwise the preference attributes are set to attr.
   * Returns icPermErr if the previous ICBegin was passed icReadOnlyPerm.
   * Returns icPermErr if current attr is locked, new attr is locked and prefh <> nil.
   }
{
 *  ICCountPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICCountPref(inst: ICInstance; var count: SInt32): OSStatus; external name '_ICCountPref';
{ [c1] [b1] 
   * Counts the total number of preferences.
   * If the routine returns an error, count is set to 0.
   }
{
 *  ICGetIndPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetIndPref(inst: ICInstance; index: SInt32; var key: Str255): OSStatus; external name '_ICGetIndPref';
{ [c1] [b1] 
   * Returns the key of the index'th preference.
   * index must be positive.
   * Returns icPrefNotFoundErr if index is greater than the total number of preferences.
   * If the routine returns an error, key is undefined.
   }
{
 *  ICDeletePref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICDeletePref(inst: ICInstance; const (*var*) key: Str255): OSStatus; external name '_ICDeletePref';
{ [c1] [b2] 
   * Deletes the preference specified by key.
   * key must not be the empty string.
   * Returns icPrefNotFound if the preference specified by key is not present.
   }
{
 *  ICEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICEnd(inst: ICInstance): OSStatus; external name '_ICEnd';
{ [c1] [b1] 
   * Terminates a preference session, as started by ICBegin.
   * You must have called ICBegin before calling this routine.
   }
{
 *  ICGetDefaultPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetDefaultPref(inst: ICInstance; const (*var*) key: Str255; prefH: Handle): OSStatus; external name '_ICGetDefaultPref';
{ [r4] [c3] [b5] 
   * Returns a default preference value for the specified key.  You
   * must pass in a valid prefH, which is resized to fit the data.
   }
{ ***** User Interface Stuff *****  }
{
 *  ICEditPreferences()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICEditPreferences(inst: ICInstance; const (*var*) key: Str255): OSStatus; external name '_ICEditPreferences';
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
 *  ICLaunchURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICLaunchURL(inst: ICInstance; const (*var*) hint: Str255; data: UnivPtr; len: SInt32; var selStart: SInt32; var selEnd: SInt32): OSStatus; external name '_ICLaunchURL';
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
 *  ICParseURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICParseURL(inst: ICInstance; const (*var*) hint: Str255; data: UnivPtr; len: SInt32; var selStart: SInt32; var selEnd: SInt32; url: Handle): OSStatus; external name '_ICParseURL';
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
 *  ICCreateGURLEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICCreateGURLEvent(inst: ICInstance; helperCreator: OSType; urlH: Handle; var theEvent: AppleEvent): OSStatus; external name '_ICCreateGURLEvent';
{ [r4] [c1] [b3] 
   * Creates a GURL Apple event, targetted at the application whose creator
   * code is helperCreator, with a direct object containing the URL text from urlH.
   }
{
 *  ICSendGURLEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICSendGURLEvent(inst: ICInstance; var theEvent: AppleEvent): OSStatus; external name '_ICSendGURLEvent';
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
 *  ICMapFilename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICMapFilename(inst: ICInstance; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICMapFilename';
{ [r1] [c1] [b4] 
   * Takes the name of an incoming file and returns the most appropriate
   * mappings database entry, based on its extension.
   * filename must not be the empty string.
   * Returns icPrefNotFoundErr if no suitable entry is found.
   }
{
 *  ICMapTypeCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICMapTypeCreator(inst: ICInstance; fType: OSType; fCreator: OSType; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICMapTypeCreator';
{ [r1] [c1] [b4] 
   * Takes the type and creator (and optionally the name) of an outgoing
   * file and returns the most appropriate mappings database entry.
   * The filename may be either the name of the outgoing file or
   * the empty string.
   * Returns icPrefNotFoundErr if no suitable entry found.
   }
{ ----- Mid Level Routines -----  }
{
 *  ICMapEntriesFilename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICMapEntriesFilename(inst: ICInstance; entries: Handle; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICMapEntriesFilename';
{ [r1] [c1] [b3] 
   * Takes the name of an incoming file and returns the most appropriate
   * mappings database entry, based on its extension.
   * entries must be a handle to a valid IC mappings database preference.
   * filename must not be the empty string.
   * Returns icPrefNotFoundErr if no suitable entry is found.
   }
{
 *  ICMapEntriesTypeCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICMapEntriesTypeCreator(inst: ICInstance; entries: Handle; fType: OSType; fCreator: OSType; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICMapEntriesTypeCreator';
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
 *  ICCountMapEntries()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICCountMapEntries(inst: ICInstance; entries: Handle; var count: SInt32): OSStatus; external name '_ICCountMapEntries';
{ [r1] [c1] [b3] 
   * Counts the number of entries in the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * count is set to the number of entries.
   }
{
 *  ICGetIndMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetIndMapEntry(inst: ICInstance; entries: Handle; index: SInt32; var pos: SInt32; var entry: ICMapEntry): OSStatus; external name '_ICGetIndMapEntry';
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
 *  ICGetMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetMapEntry(inst: ICInstance; entries: Handle; pos: SInt32; var entry: ICMapEntry): OSStatus; external name '_ICGetMapEntry';
{ [r1] [c1] [b3] 
   * Returns the entry located at position pos in the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * pos should be 0 to get the first entry. To get the subsequent entries, add
   * entry.total_size to pos and iterate.
   * Does not return any user data associated with the entry.
   }
{
 *  ICSetMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICSetMapEntry(inst: ICInstance; entries: Handle; pos: SInt32; const (*var*) entry: ICMapEntry): OSStatus; external name '_ICSetMapEntry';
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
 *  ICDeleteMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICDeleteMapEntry(inst: ICInstance; entries: Handle; pos: SInt32): OSStatus; external name '_ICDeleteMapEntry';
{ [r1] [c1] [b3] 
   * Deletes the mappings database entry at pos.
   * entries must be a handle to a valid IC mappings database preference.
   * pos should be either a value returned from ICGetIndMapEntry or a value
   * calculated using ICGetMapEntry.
   * Also deletes any user data associated with the entry.
   }
{
 *  ICAddMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAddMapEntry(inst: ICInstance; entries: Handle; const (*var*) entry: ICMapEntry): OSStatus; external name '_ICAddMapEntry';
{ [r1] [c1] [b3] 
   * Adds an entry to the mappings database.
   * entries must be a handle to a valid IC mappings database preference.
   * The entry is added to the end of the entries database.
   * No user data is added.
   }
{ ***** Profile Management Routines *****  }
{
 *  ICGetCurrentProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetCurrentProfile(inst: ICInstance; var currentID: ICProfileID): OSStatus; external name '_ICGetCurrentProfile';
{ [r3] [c1] [b3] 
   * Returns the profile ID of the current profile.
   }
{
 *  ICSetCurrentProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICSetCurrentProfile(inst: ICInstance; newID: ICProfileID): OSStatus; external name '_ICSetCurrentProfile';
{ [r3] [c1] [b3] 
   * Sets the current profile to the profile specified in newProfile.
   }
{
 *  ICCountProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICCountProfiles(inst: ICInstance; var count: SInt32): OSStatus; external name '_ICCountProfiles';
{ [r3] [c1] [b1] 
   * Returns the total number of profiles.
   }
{
 *  ICGetIndProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetIndProfile(inst: ICInstance; index: SInt32; var thisID: ICProfileID): OSStatus; external name '_ICGetIndProfile';
{ [r3] [c1] [b1] 
   * Returns the profile ID of the index'th profile.  index must be positive.
   * Returns icProfileNotFoundErr if index is greater than the total number
   * of profiles.
   }
{
 *  ICGetProfileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICGetProfileName(inst: ICInstance; thisID: ICProfileID; var name: Str255): OSStatus; external name '_ICGetProfileName';
{ [r3] [c1] [b3] 
   * Returns the name of a profile given its ID.  The name may not uniquely
   * identify the profile.  [That's what the profile ID is for!]  The name
   * is assumed to be in the system script.
   }
{
 *  ICSetProfileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICSetProfileName(inst: ICInstance; thisID: ICProfileID; const (*var*) name: Str255): OSStatus; external name '_ICSetProfileName';
{ [r3] [c1] [b3] 
   * This routine sets the name of the specified profile.  Profile names
   * need not be unique.  The name should be in the system script.
   }
{
 *  ICAddProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICAddProfile(inst: ICInstance; prototypeID: ICProfileID; var newID: ICProfileID): OSStatus; external name '_ICAddProfile';
{ [r3] [c1] [b2] 
   * If prototypeID = kICNilProfileID, this routine
   * creates a default profile, otherwise it creates a
   * profile by cloning the prototype profile.  The ID
   * of the new profile is returned in newID.
   * The new profile will be give a new, unique, name.
   * This does not switch to the new profile.
   }
{
 *  ICDeleteProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICDeleteProfile(inst: ICInstance; thisID: ICProfileID): OSStatus; external name '_ICDeleteProfile';
{ [r3] [c1] [b2] 
   * This routine deletes the profile specified by
   * thisID.  Attempting to delete the current profile
   * or the last profile will return error.
   }
{***********************************************************************************************
  NOTHING BELOW THIS DIVIDER IS IN CARBON
 ***********************************************************************************************}
{ ***** Interrupt Safe Routines *****  }
{$ifc CALL_NOT_IN_CARBON}
{
 *  ICRequiresInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICRequiresInterruptSafe(inst: ICInstance): OSStatus; external name '_ICRequiresInterruptSafe';
{ [r3] [c2] [b3] 
   * You must call this routine before calling GetMapEntryInterruptSafe
   * to give IC chance to cache the mappings data in memory.  The only
   * way to clear this state is to close the instance.  You can not reconfigure
   * the instance after calling this routine.
   }
{
 *  ICGetMappingInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICGetMappingInterruptSafe(inst: ICInstance; var mappingPref: Ptr; var mappingPrefSize: SInt32): OSStatus; external name '_ICGetMappingInterruptSafe';
{ [r3] [c2] [b3] 
   * Returns the "Mapping" preference in an interrupt safe fashion.
   * The preference returned pointer is valid until the next
   * non-interrupt safe call to IC.  Typically this API is used
   * by software that needs to map extensions to type and creator
   * at interrupt time, eg foreign file systems.
   }
{
 *  ICGetSeedInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICGetSeedInterruptSafe(inst: ICInstance; var seed: SInt32): OSStatus; external name '_ICGetSeedInterruptSafe';
{ [r3] [c2] [b3] 
   * An interrupt safe version of ICGetSeed.
   }
{ ***** Starting Up and Shutting Down *****  }
{
 *  ICCStart()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCStart(var inst: ComponentInstance; creator: OSType): OSStatus; external name '_ICCStart';

{ See comment for ICCStart.  }
{
 *  ICCStop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCStop(inst: ComponentInstance): OSStatus; external name '_ICCStop';

{ See comment for ICCStop.  }
{
 *  ICCGetVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetVersion(inst: ComponentInstance; whichVersion: SInt32; var version: UInt32): OSStatus; external name '_ICCGetVersion';
{ See comment for ICCGetVersion.  }
{ ***** Specifying a Configuration *****  }
{
 *  ICCFindConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCFindConfigFile(inst: ComponentInstance; count: SInt16; folders: ICDirSpecArrayPtr): OSStatus; external name '_ICCFindConfigFile';
{ See comment for ICCFindConfigFile.  }
{
 *  ICCFindUserConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCFindUserConfigFile(inst: ComponentInstance; var where: ICDirSpec): OSStatus; external name '_ICCFindUserConfigFile';
{ See comment for ICCFindUserConfigFile.  }
{
 *  ICCGeneralFindConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGeneralFindConfigFile(inst: ComponentInstance; searchPrefs: boolean; canCreate: boolean; count: SInt16; folders: ICDirSpecArrayPtr): OSStatus; external name '_ICCGeneralFindConfigFile';
{ See comment for ICCGeneralFindConfigFile.  }
{
 *  ICCChooseConfig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCChooseConfig(inst: ComponentInstance): OSStatus; external name '_ICCChooseConfig';
{ See comment for ICCChooseConfig.  }
{
 *  ICCChooseNewConfig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCChooseNewConfig(inst: ComponentInstance): OSStatus; external name '_ICCChooseNewConfig';
{ See comment for ICCChooseNewConfig.  }
{
 *  ICCGetConfigName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetConfigName(inst: ComponentInstance; longname: boolean; var name: Str255): OSStatus; external name '_ICCGetConfigName';
{ See comment for ICCGetConfigName.  }
{
 *  ICCGetConfigReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetConfigReference(inst: ComponentInstance; ref: ICConfigRefHandle): OSStatus; external name '_ICCGetConfigReference';
{ See comment for ICCGetConfigReference.  }
{
 *  ICCSetConfigReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSetConfigReference(inst: ComponentInstance; ref: ICConfigRefHandle; flags: SInt32): OSStatus; external name '_ICCSetConfigReference';
{ See comment for ICCSetConfigReference.  }
{ ***** Private Routines *****
 * 
 * If you are calling these routines, you are most probably doing something
 * wrong.  Please read the documentation for more details.
  }
{
 *  ICCSpecifyConfigFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSpecifyConfigFile(inst: ComponentInstance; var config: FSSpec): OSStatus; external name '_ICCSpecifyConfigFile';
{ See comment for ICCSpecifyConfigFile.  }
{
 *  ICCRefreshCaches()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCRefreshCaches(inst: ComponentInstance): OSStatus; external name '_ICCRefreshCaches';
{ See comment for ICCRefreshCaches.  }
{ ***** Getting Information *****  }
{
 *  ICCGetSeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetSeed(inst: ComponentInstance; var seed: SInt32): OSStatus; external name '_ICCGetSeed';
{ See comment for ICCGetSeed.  }
{
 *  ICCGetPerm()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetPerm(inst: ComponentInstance; var perm: ICPerm): OSStatus; external name '_ICCGetPerm';
{ See comment for ICCGetPerm.  }
{
 *  ICCDefaultFileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCDefaultFileName(inst: ComponentInstance; var name: Str63): OSStatus; external name '_ICCDefaultFileName';
{ See comment for ICCDefaultFileName.  }
{
 *  ICCGetComponentInstance()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetComponentInstance(inst: ComponentInstance; var componentInst: ComponentInstance): OSStatus; external name '_ICCGetComponentInstance';

{ See comment for ICCGetComponentInstance.  }
{ ***** Reading and Writing Preferences *****  }
{
 *  ICCBegin()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCBegin(inst: ComponentInstance; perm: ByteParameter): OSStatus; external name '_ICCBegin';
{ See comment for ICCBegin.  }
{
 *  ICCGetPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetPref(inst: ComponentInstance; const (*var*) key: Str255; var attr: ICAttr; buf: Ptr; var size: SInt32): OSStatus; external name '_ICCGetPref';
{ See comment for ICCGetPref.  }
{
 *  ICCSetPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSetPref(inst: ComponentInstance; const (*var*) key: Str255; attr: ICAttr; buf: Ptr; size: SInt32): OSStatus; external name '_ICCSetPref';
{ See comment for ICCSetPref.  }
{
 *  ICCFindPrefHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCFindPrefHandle(inst: ComponentInstance; const (*var*) key: Str255; var attr: ICAttr; prefh: Handle): OSStatus; external name '_ICCFindPrefHandle';
{ See comment for ICCFindPrefHandle.  }
{
 *  ICCGetPrefHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetPrefHandle(inst: ComponentInstance; const (*var*) key: Str255; var attr: ICAttr; var prefh: Handle): OSStatus; external name '_ICCGetPrefHandle';
{ See comment for ICCGetPrefHandle.  }
{
 *  ICCSetPrefHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSetPrefHandle(inst: ComponentInstance; const (*var*) key: Str255; attr: ICAttr; prefh: Handle): OSStatus; external name '_ICCSetPrefHandle';
{ See comment for ICCSetPrefHandle.  }
{
 *  ICCCountPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCCountPref(inst: ComponentInstance; var count: SInt32): OSStatus; external name '_ICCCountPref';
{ See comment for ICCCountPref.  }
{
 *  ICCGetIndPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetIndPref(inst: ComponentInstance; index: SInt32; var key: Str255): OSStatus; external name '_ICCGetIndPref';
{ See comment for ICCGetIndPref.  }
{
 *  ICCDeletePref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCDeletePref(inst: ComponentInstance; const (*var*) key: Str255): OSStatus; external name '_ICCDeletePref';
{ See comment for ICCDeletePref.  }
{
 *  ICCEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCEnd(inst: ComponentInstance): OSStatus; external name '_ICCEnd';
{ See comment for ICCEnd.  }
{
 *  ICCGetDefaultPref()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetDefaultPref(inst: ComponentInstance; const (*var*) key: Str255; prefH: Handle): OSStatus; external name '_ICCGetDefaultPref';
{ See comment for ICCGetDefaultPref.  }
{ ***** User Interface Stuff *****  }
{
 *  ICCEditPreferences()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCEditPreferences(inst: ComponentInstance; const (*var*) key: Str255): OSStatus; external name '_ICCEditPreferences';
{ See comment for ICCEditPreferences.  }
{ ***** URL Handling *****  }
{
 *  ICCLaunchURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCLaunchURL(inst: ComponentInstance; const (*var*) hint: Str255; data: Ptr; len: SInt32; var selStart: SInt32; var selEnd: SInt32): OSStatus; external name '_ICCLaunchURL';
{ See comment for ICCLaunchURL.  }
{
 *  ICCParseURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCParseURL(inst: ComponentInstance; const (*var*) hint: Str255; data: Ptr; len: SInt32; var selStart: SInt32; var selEnd: SInt32; url: Handle): OSStatus; external name '_ICCParseURL';
{ See comment for ICCParseURL.  }
{
 *  ICCCreateGURLEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCCreateGURLEvent(inst: ComponentInstance; helperCreator: OSType; urlH: Handle; var theEvent: AppleEvent): OSStatus; external name '_ICCCreateGURLEvent';
{ See comment for ICCCreateGURLEvent.  }
{
 *  ICCSendGURLEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSendGURLEvent(inst: ComponentInstance; var theEvent: AppleEvent): OSStatus; external name '_ICCSendGURLEvent';
{ See comment for ICCSendGURLEvent.  }
{ ***** Mappings Routines *****
 * 
 * Routines for interrogating mappings database.
 * 
 * ----- High Level Routines -----
  }
{
 *  ICCMapFilename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCMapFilename(inst: ComponentInstance; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICCMapFilename';
{ See comment for ICCMapFilename.  }
{
 *  ICCMapTypeCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCMapTypeCreator(inst: ComponentInstance; fType: OSType; fCreator: OSType; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICCMapTypeCreator';
{ See comment for ICCMapTypeCreator.  }
{ ----- Mid Level Routines -----  }
{
 *  ICCMapEntriesFilename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCMapEntriesFilename(inst: ComponentInstance; entries: Handle; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICCMapEntriesFilename';
{ See comment for ICCMapEntriesFilename.  }
{
 *  ICCMapEntriesTypeCreator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCMapEntriesTypeCreator(inst: ComponentInstance; entries: Handle; fType: OSType; fCreator: OSType; const (*var*) filename: Str255; var entry: ICMapEntry): OSStatus; external name '_ICCMapEntriesTypeCreator';
{ See comment for ICCMapEntriesTypeCreator.  }
{ ----- Low Level Routines -----  }
{
 *  ICCCountMapEntries()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCCountMapEntries(inst: ComponentInstance; entries: Handle; var count: SInt32): OSStatus; external name '_ICCCountMapEntries';
{ See comment for ICCCountMapEntries.  }
{
 *  ICCGetIndMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetIndMapEntry(inst: ComponentInstance; entries: Handle; index: SInt32; var pos: SInt32; var entry: ICMapEntry): OSStatus; external name '_ICCGetIndMapEntry';
{ See comment for ICCGetIndMapEntry.  }
{
 *  ICCGetMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetMapEntry(inst: ComponentInstance; entries: Handle; pos: SInt32; var entry: ICMapEntry): OSStatus; external name '_ICCGetMapEntry';
{ See comment for ICCGetMapEntry.  }
{
 *  ICCSetMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSetMapEntry(inst: ComponentInstance; entries: Handle; pos: SInt32; var entry: ICMapEntry): OSStatus; external name '_ICCSetMapEntry';
{ See comment for ICCSetMapEntry.  }
{
 *  ICCDeleteMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCDeleteMapEntry(inst: ComponentInstance; entries: Handle; pos: SInt32): OSStatus; external name '_ICCDeleteMapEntry';
{ See comment for ICCDeleteMapEntry.  }
{
 *  ICCAddMapEntry()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCAddMapEntry(inst: ComponentInstance; entries: Handle; var entry: ICMapEntry): OSStatus; external name '_ICCAddMapEntry';
{ See comment for ICCAddMapEntry.  }
{ ***** Profile Management Routines *****  }
{
 *  ICCGetCurrentProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetCurrentProfile(inst: ComponentInstance; var currentID: ICProfileID): OSStatus; external name '_ICCGetCurrentProfile';
{ See comment for ICCGetCurrentProfile.  }
{
 *  ICCSetCurrentProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSetCurrentProfile(inst: ComponentInstance; newID: ICProfileID): OSStatus; external name '_ICCSetCurrentProfile';
{ See comment for ICCSetCurrentProfile.  }
{
 *  ICCCountProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCCountProfiles(inst: ComponentInstance; var count: SInt32): OSStatus; external name '_ICCCountProfiles';
{ See comment for ICCCountProfiles.  }
{
 *  ICCGetIndProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetIndProfile(inst: ComponentInstance; index: SInt32; var thisID: ICProfileID): OSStatus; external name '_ICCGetIndProfile';
{ See comment for ICCGetIndProfile.  }
{
 *  ICCGetProfileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetProfileName(inst: ComponentInstance; thisID: ICProfileID; var name: Str255): OSStatus; external name '_ICCGetProfileName';
{ See comment for ICCGetProfileName.  }
{
 *  ICCSetProfileName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCSetProfileName(inst: ComponentInstance; thisID: ICProfileID; const (*var*) name: Str255): OSStatus; external name '_ICCSetProfileName';
{ See comment for ICCSetProfileName.  }
{
 *  ICCAddProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCAddProfile(inst: ComponentInstance; prototypeID: ICProfileID; var newID: ICProfileID): OSStatus; external name '_ICCAddProfile';
{ See comment for ICCAddProfile.  }
{
 *  ICCDeleteProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCDeleteProfile(inst: ComponentInstance; thisID: ICProfileID): OSStatus; external name '_ICCDeleteProfile';
{ See comment for ICCDeleteProfile.  }
{ ***** Interrupt Safe Routines *****  }
{
 *  ICCRequiresInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCRequiresInterruptSafe(inst: ComponentInstance): OSStatus; external name '_ICCRequiresInterruptSafe';
{ See comment for ICCRequiresInterruptSafe.  }
{
 *  ICCGetMappingInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetMappingInterruptSafe(inst: ComponentInstance; var mappingPref: Ptr; var mappingPrefSize: SInt32): OSStatus; external name '_ICCGetMappingInterruptSafe';
{ See comment for ICCGetMappingInterruptSafe.  }
{
 *  ICCGetSeedInterruptSafe()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InternetConfig 2.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function ICCGetSeedInterruptSafe(inst: ComponentInstance; var seed: SInt32): OSStatus; external name '_ICCGetSeedInterruptSafe';
{ See comment for ICCGetSeedInterruptSafe.  }
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}

{***********************************************************************************************
  component selectors
 ***********************************************************************************************}


const
	kICCStart					= 0;
	kICCStop					= 1;
	kICCGetVersion				= 50;
	kICCFindConfigFile			= 2;
	kICCFindUserConfigFile		= 14;
	kICCGeneralFindConfigFile	= 30;
	kICCChooseConfig			= 33;
	kICCChooseNewConfig			= 34;
	kICCGetConfigName			= 35;
	kICCGetConfigReference		= 31;
	kICCSetConfigReference		= 32;
	kICCSpecifyConfigFile		= 3;
	kICCRefreshCaches			= 47;
	kICCGetSeed					= 4;
	kICCGetPerm					= 13;
	kICCDefaultFileName			= 11;
	kICCBegin					= 5;
	kICCGetPref					= 6;
	kICCSetPref					= 7;
	kICCFindPrefHandle			= 36;
	kICCGetPrefHandle			= 26;
	kICCSetPrefHandle			= 27;
	kICCCountPref				= 8;
	kICCGetIndPref				= 9;
	kICCDeletePref				= 12;
	kICCEnd						= 10;
	kICCGetDefaultPref			= 49;
	kICCEditPreferences			= 15;
	kICCLaunchURL				= 17;
	kICCParseURL				= 16;
	kICCCreateGURLEvent			= 51;
	kICCSendGURLEvent			= 52;
	kICCMapFilename				= 24;
	kICCMapTypeCreator			= 25;
	kICCMapEntriesFilename		= 28;
	kICCMapEntriesTypeCreator	= 29;
	kICCCountMapEntries			= 18;
	kICCGetIndMapEntry			= 19;
	kICCGetMapEntry				= 20;
	kICCSetMapEntry				= 21;
	kICCDeleteMapEntry			= 22;
	kICCAddMapEntry				= 23;
	kICCGetCurrentProfile		= 37;
	kICCSetCurrentProfile		= 38;
	kICCCountProfiles			= 39;
	kICCGetIndProfile			= 40;
	kICCGetProfileName			= 41;
	kICCSetProfileName			= 42;
	kICCAddProfile				= 43;
	kICCDeleteProfile			= 44;
	kICCRequiresInterruptSafe	= 45;
	kICCGetMappingInterruptSafe	= 46;
	kICCGetSeedInterruptSafe	= 48;
	kICCFirstSelector			= 0;
	kICCLastSelector			= 52;

	{	***********************************************************************************************
	  component selector proc infos
	 ***********************************************************************************************	}

	kICCStartProcInfo			= 1008;
	kICCStopProcInfo			= 240;
	kICCGetVersionProcInfo		= 4080;
	kICCFindConfigFileProcInfo	= 3824;
	kICCFindUserConfigFileProcInfo = 1008;
	kICCGeneralFindConfigFileProcInfo = 58864;
	kICCChooseConfigProcInfo	= 240;
	kICCChooseNewConfigProcInfo	= 240;
	kICCGetConfigNameProcInfo	= 3568;
	kICCGetConfigReferenceProcInfo = 1008;
	kICCSetConfigReferenceProcInfo = 4080;
	kICCSpecifyConfigFileProcInfo = 1008;
	kICCRefreshCachesProcInfo	= 240;
	kICCGetSeedProcInfo			= 1008;
	kICCGetPermProcInfo			= 1008;
	kICCDefaultFileNameProcInfo	= 1008;
	kICCGetComponentInstanceProcInfo = 1008;
	kICCBeginProcInfo			= 496;
	kICCGetPrefProcInfo			= 65520;
	kICCSetPrefProcInfo			= 65520;
	kICCFindPrefHandleProcInfo	= 16368;
	kICCGetPrefHandleProcInfo	= 16368;
	kICCSetPrefHandleProcInfo	= 16368;
	kICCCountPrefProcInfo		= 1008;
	kICCGetIndPrefProcInfo		= 4080;
	kICCDeletePrefProcInfo		= 1008;
	kICCEndProcInfo				= 240;
	kICCGetDefaultPrefProcInfo	= 4080;
	kICCEditPreferencesProcInfo	= 1008;
	kICCLaunchURLProcInfo		= 262128;
	kICCParseURLProcInfo		= 1048560;
	kICCCreateGURLEventProcInfo	= 16368;
	kICCSendGURLEventProcInfo	= 1008;
	kICCMapFilenameProcInfo		= 4080;
	kICCMapTypeCreatorProcInfo	= 65520;
	kICCMapEntriesFilenameProcInfo = 16368;
	kICCMapEntriesTypeCreatorProcInfo = 262128;
	kICCCountMapEntriesProcInfo	= 4080;
	kICCGetIndMapEntryProcInfo	= 65520;
	kICCGetMapEntryProcInfo		= 16368;
	kICCSetMapEntryProcInfo		= 16368;
	kICCDeleteMapEntryProcInfo	= 4080;
	kICCAddMapEntryProcInfo		= 4080;
	kICCGetCurrentProfileProcInfo = 1008;
	kICCSetCurrentProfileProcInfo = 1008;
	kICCCountProfilesProcInfo	= 1008;
	kICCGetIndProfileProcInfo	= 4080;
	kICCGetProfileNameProcInfo	= 4080;
	kICCSetProfileNameProcInfo	= 4080;
	kICCAddProfileProcInfo		= 4080;
	kICCDeleteProfileProcInfo	= 1008;
	kICCRequiresInterruptSafeProcInfo = 240;
	kICCGetMappingInterruptSafeProcInfo = 4080;
	kICCGetSeedInterruptSafeProcInfo = 1008;

	{	***********************************************************************************************
	  component identifiers
	 ***********************************************************************************************	}

	kICComponentType			= $50524546 (* 'PREF' *);
	kICComponentSubType			= $49434170 (* 'ICAp' *);
	kICComponentManufacturer	= $4A505145 (* 'JPQE' *);

	{	***********************************************************************************************
	  The following type is now obsolete.
	  If you're using it, please switch to ComponentInstance or ICInstance.
	 ***********************************************************************************************	}

{$ifc OLDROUTINENAMES}

type
	internetConfigurationComponent		= ComponentInstance;

{$endc}  {OLDROUTINENAMES}
{$endc}  {CALL_NOT_IN_CARBON}

{***********************************************************************************************
  old names for stuff declared above
 ***********************************************************************************************}

{$ifc OLDROUTINENAMES}


type
	ICError								= SInt32;


const
	ICattr_no_change			= $FFFFFFFF;
	ICattr_locked_bit			= 0;
	ICattr_locked_mask			= $00000001;
	ICattr_volatile_bit			= 1;
	ICattr_volatile_mask		= $00000002;
	icNoUserInteraction_bit		= 0;
	icNoUserInteraction_mask	= $00000001;
	ICfiletype					= $49434170 (* 'ICAp' *);
	ICcreator					= $49434170 (* 'ICAp' *);

	{	
	    ICFileInfo was originally used to define the format of a key.
	    That key was removed, but we forgot to remove ICFileInfo.
	    I hope to remove it entirely, but for the moment it's available
	    if you define OLDROUTINENAMES.
		}

type
	ICFileInfoPtr = ^ICFileInfo;
	ICFileInfo = record
		fType:					OSType;
		fCreator:				OSType;
		name:					Str63;
	end;

	ICFileInfoHandle					= ^ICFileInfoPtr;


const
	ICfile_spec_header_size		= 106;

	ICmap_binary_bit			= 0;
	ICmap_binary_mask			= $00000001;
	ICmap_resource_fork_bit		= 1;
	ICmap_resource_fork_mask	= $00000002;
	ICmap_data_fork_bit			= 2;
	ICmap_data_fork_mask		= $00000004;
	ICmap_post_bit				= 3;
	ICmap_post_mask				= $00000008;
	ICmap_not_incoming_bit		= 4;
	ICmap_not_incoming_mask		= $00000010;
	ICmap_not_outgoing_bit		= 5;
	ICmap_not_outgoing_mask		= $00000020;
	ICmap_fixed_length			= 22;

	ICservices_tcp_bit			= 0;
	ICservices_tcp_mask			= $00000001;
	ICservices_udp_bit			= 1;
	ICservices_udp_mask			= $00000002;

	{	    This definitions are a) very long, and b) don't conform
	    to Mac OS standards for naming constants, so I've put
	    them in only if you're using OLDROUTINENAMES.  Please switch
	    to the new names given above.
		}
	internetConfigurationComponentType = $50524546 (* 'PREF' *);				{  the component type  }
	internetConfigurationComponentSubType = $49434170 (* 'ICAp' *);				{  the component subtype  }
	internetConfigurationComponentInterfaceVersion0 = $00000000; {  IC >= 1.0  }
	internetConfigurationComponentInterfaceVersion1 = $00010000; {  IC >= 1.1  }
	internetConfigurationComponentInterfaceVersion2 = $00020000; {  IC >= 1.2  }
	internetConfigurationComponentInterfaceVersion3 = $00030000; {  IC >= 2.0  }
																{  current version number is version 3  }
	internetConfigurationComponentInterfaceVersion = $00030000;

{$endc}  {OLDROUTINENAMES}

{$ALIGN MAC68K}


end.
