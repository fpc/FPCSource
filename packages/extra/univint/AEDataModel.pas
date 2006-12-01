{
     File:       AEDataModel.p
 
     Contains:   AppleEvent Data Model Interfaces.
 
     Version:    Technology: Mac OS 9
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1996-2002 by Apple Computer, Inc., all rights reserved
 
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

unit AEDataModel;
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
uses MacTypes,MixedMode;


{$ALIGN MAC68K}

{ Apple event descriptor types }

const
	typeBoolean					= $626F6F6C (* 'bool' *);
	typeChar					= $54455854 (* 'TEXT' *); { Deprecated, use typeUTF8Text instead. }

{ The preferred unicode text types.  In both cases, there is no explicit null termination or length byte. }

	typeUTF16ExternalRepresentation = $75743136 (* 'ut16' *); { big-endian 16 bit unicode with optional byte-order-mark, or little-endian 16 bit unicode with required byte-order-mark. }
	typeUTF8Text                = $75746638 (* 'utf8' *); { 8 bit unicode }

	{	 Preferred numeric Apple event descriptor types 	}
	typeSInt16					= $73686F72 (* 'shor' *);
	typeSInt32					= $6C6F6E67 (* 'long' *);
	typeUInt32					= $6D61676E (* 'magn' *);
	typeSInt64					= $636F6D70 (* 'comp' *);
	typeIEEE32BitFloatingPoint	= $73696E67 (* 'sing' *);
	typeIEEE64BitFloatingPoint	= $646F7562 (* 'doub' *);
	type128BitFloatingPoint		= $6C64626C (* 'ldbl' *);
	typeDecimalStruct			= $6465636D (* 'decm' *);

	{	 Non-preferred Apple event descriptor types 	}
	typeSMInt					= $73686F72 (* 'shor' *);
	typeShortInteger			= $73686F72 (* 'shor' *);
	typeInteger					= $6C6F6E67 (* 'long' *);
	typeLongInteger				= $6C6F6E67 (* 'long' *);
	typeMagnitude				= $6D61676E (* 'magn' *);
	typeComp					= $636F6D70 (* 'comp' *);
	typeSMFloat					= $73696E67 (* 'sing' *);
	typeShortFloat				= $73696E67 (* 'sing' *);
	typeFloat					= $646F7562 (* 'doub' *);
	typeLongFloat				= $646F7562 (* 'doub' *);
	typeExtended				= $65787465 (* 'exte' *);

	{	 More Apple event descriptor types 	}
	typeAEList					= $6C697374 (* 'list' *);
	typeAERecord				= $7265636F (* 'reco' *);
	typeAppleEvent				= $61657674 (* 'aevt' *);
	typeEventRecord				= $65767263 (* 'evrc' *);
	typeTrue					= $74727565 (* 'true' *);
	typeFalse					= $66616C73 (* 'fals' *);
	typeAlias					= $616C6973 (* 'alis' *);
	typeEnumerated				= $656E756D (* 'enum' *);
	typeType					= $74797065 (* 'type' *);
	typeAppParameters			= $61707061 (* 'appa' *);
	typeProperty				= $70726F70 (* 'prop' *);
	typeFSS						= $66737320 (* 'fss ' *);
	typeFSRef					= $66737266 (* 'fsrf' *);
	typeFileURL					= $6675726C (* 'furl' *);
	typeKeyword					= $6B657977 (* 'keyw' *);
	typeSectionH				= $73656374 (* 'sect' *);
	typeWildCard				= $2A2A2A2A (* '****' *);
	typeApplSignature			= $7369676E (* 'sign' *);
	typeQDRectangle				= $71647274 (* 'qdrt' *);
	typeFixed					= $66697864 (* 'fixd' *);
	typeProcessSerialNumber		= $70736E20 (* 'psn ' *);
	typeApplicationURL			= $6170726C (* 'aprl' *);
	typeNull					= $6E756C6C (* 'null' *);						{  null or nonexistent data  }

{$ifc CALL_NOT_IN_CARBON}
	{	 Deprecated addressing modes under Carbon 	}
	typeSessionID				= $73736964 (* 'ssid' *);
	typeTargetID				= $74617267 (* 'targ' *);
	typeDispatcherID			= $64737074 (* 'dspt' *);

{$endc}  {CALL_NOT_IN_CARBON}

	{ New addressing modes for MacOS X }
	typeKernelProcessID         = $6B706964 (* 'kpid' *);
	typeMachPort                = $706F7274 (* 'port' *);

	{ Targeting applications by bundle ID is only available in Mac OS X 10.3 or later. }
	typeApplicationBundleID     = $62756E64 (* 'bund' *);

	{	 Keywords for Apple event attributes 	}
	keyTransactionIDAttr		= $7472616E (* 'tran' *);
	keyReturnIDAttr				= $72746964 (* 'rtid' *);
	keyEventClassAttr			= $6576636C (* 'evcl' *);
	keyEventIDAttr				= $65766964 (* 'evid' *);
	keyAddressAttr				= $61646472 (* 'addr' *);
	keyOptionalKeywordAttr		= $6F70746B (* 'optk' *);
	keyTimeoutAttr				= $74696D6F (* 'timo' *);
	keyInteractLevelAttr		= $696E7465 (* 'inte' *);						{  this attribute is read only - will be set in AESend  }
	keyEventSourceAttr			= $65737263 (* 'esrc' *);						{  this attribute is read only - returned as typeShortInteger  }
	keyMissedKeywordAttr		= $6D697373 (* 'miss' *);						{  this attribute is read only  }
	keyOriginalAddressAttr		= $66726F6D (* 'from' *);						{  new in 1.0.1  }
	keyAcceptTimeoutAttr		= $6163746D (* 'actm' *);						{  new for Mac OS X  }
	keyReplyRequestedAttr       = $72657071 (* 'repq' *);                       { Was a reply requested for this event - returned as typeBoolean }

	{ These bits are specified in the keyXMLDebuggingAttr (an SInt32) }
	kAEDebugPOSTHeader          = 1 shl 0; { headers of the HTTP post we sent - typeChar }
	kAEDebugReplyHeader         = 1 shl 1; { headers returned by the server }
	kAEDebugXMLRequest          = 1 shl 2; { the XML request we sent }
	kAEDebugXMLResponse         = 1 shl 3; { the XML reply from the server }
	kAEDebugXMLDebugAll         = $FFFFFFFF; { everything! }

	{ These values can be added as a parameter to the direct object of a
    SOAP message to specify the serialization schema.  If not
    specified, kSOAP1999Schema is the default. These should be added as
    typeType. }
	kSOAP1999Schema             = $73733939 (* 'ss99' *);
	kSOAP2001Schema             = $73733031 (* 'ss01' *);

	{ outgoing event attributes }
	keyUserNameAttr             = $756E616D (* 'unam' *);
	keyUserPasswordAttr         = $70617373 (* 'pass' *); { not sent with the event }
	keyDisableAuthenticationAttr = $61757468 (* 'auth' *); { When present and with a non zero value (that is, false, or integer 0), }
                                                           { AESend will not authenticate the user.  If not present, or with a non-zero}
                                                           { value, AESend will prompt for authentication information from the user if the interaction level allows. }
	keyXMLDebuggingAttr         = $78646267 (* 'xdbg' *); { a bitfield of specifying which XML debugging data is to be returned with the event }
                                                          { Event class / id }
	kAERPCClass                 = $72706320 (* 'rpc ' *); { for outgoing XML events }
	kAEXMLRPCScheme             = $52504332 (* 'RPC2' *); { event ID: event should be sent to an XMLRPC endpoint }
	kAESOAPScheme               = $534F4150 (* 'SOAP' *); { event ID: event should be sent to a SOAP endpoint }
	kAESharedScriptHandler      = $77736370 (* 'wscp' *); { event ID: handler for incoming XML requests }
                                                          { these parameters exist as part of the direct object of the event for both incoming and outgoing requests }
	keyRPCMethodName            = $6D657468 (* 'meth' *); { name of the method to call }
	keyRPCMethodParam           = $7061726D (* 'parm' *); { the list (or structure) of parameters }
	keyRPCMethodParamOrder      = $2F6F7264 (* '/ord' *); { if a structure, the order of parameters (a list) }
                                                          { when keyXMLDebugginAttr so specifies, these additional parameters will be part of the reply. }
	keyAEPOSTHeaderData         = $70686564 (* 'phed' *); { what we sent to the server }
	keyAEReplyHeaderData        = $72686564 (* 'rhed' *); { what the server sent to us }
	keyAEXMLRequestData         = $78726571 (* 'xreq' *); { what we sent to the server }
	keyAEXMLReplyData           = $78726570 (* 'xrep' *); { what the server sent to us }
                                                          { additional parameters that can be specified in the direct object of the event }
	keyAdditionalHTTPHeaders    = $61686564 (* 'ahed' *); { list of additional HTTP headers (a list of 2 element lists) }
	keySOAPAction               = $73616374 (* 'sact' *); { the SOAPAction header (required for SOAP messages) }
	keySOAPMethodNameSpace      = $6D737063 (* 'mspc' *); { Optional namespace (defaults to m:) }
	keySOAPMethodNameSpaceURI   = $6D737075 (* 'mspu' *); { Required namespace URI }
	keySOAPSchemaVersion        = $73736368 (* 'ssch' *); { Optional XML Schema version, defaults to kSOAP1999Schama }

{ 
   When serializing AERecords as SOAP structures, it is possible
   to specify the namespace and type of the structure.  To do this,
   add a keySOAPStructureMetaData record to the top level of the
   record to be serialized.  If present, this will be used to specify
   the structure namespace.  This will produce a structure elment that
   looks like:

    <myStruct
        xmlns:myNamespace="http://myUri.org/xsd",
        xsi:type="myNamespace:MyStructType">
        ...
    </myStruct>

}
	keySOAPStructureMetaData    = $2F736D64 (* '/smd' *);
	keySOAPSMDNamespace         = $73736E73 (* 'ssns' *); { "myNamespace"}
	keySOAPSMDNamespaceURI      = $73736E75 (* 'ssnu' *); { "http://myUri.org/xsd"}
	keySOAPSMDType              = $73737470 (* 'sstp' *); { "MyStructType"}

{ 
 * Web Services Proxy support.  Available only on Mac OS X 10.2 or later.
 * These constants should be added as attributes on the event that is
 * being sent (not part of the direct object.)
 }
   { Automatically configure the proxy based on System Configuration }
	kAEUseHTTPProxyAttr         = $78757072 (* 'xupr' *); { a typeBoolean.  Defaults to true.}
                                                          { manually specify the proxy host and port. }
	kAEHTTPProxyPortAttr        = $78687470 (* 'xhtp' *); { a typeSInt32}
	kAEHTTPProxyHostAttr        = $78687468 (* 'xhth' *); { a typeChar}

{
 * Web Services SOCKS support.  kAEUseSocksAttr is a boolean that
 * specifies whether to automatically configure SOCKS proxies by
 * querying System Configuration.
 }
	kAESocks4Protocol           = 4;
	kAESocks5Protocol           = 5;

	kAEUseSocksAttr             = $78736373 (* 'xscs' *); { a typeBoolean.  Defaults to true.}
                                                          { This attribute specifies a specific SOCKS protocol to be used }
	kAESocksProxyAttr           = $78736F6B (* 'xsok' *); { a typeSInt32}
                                                     { if version >= 4 }
	kAESocksHostAttr            = $78736873 (* 'xshs' *); { a typeChar}
	kAESocksPortAttr            = $78736870 (* 'xshp' *); { a typeSInt32}
	kAESocksUserAttr            = $78736875 (* 'xshu' *); { a typeChar}
                                                     { if version >= 5 }
	kAESocksPasswordAttr        = $78736877 (* 'xshw' *); { a typeChar}



	{	  Constants used for specifying the factoring of AEDescLists. 	}
	kAEDescListFactorNone		= 0;
	kAEDescListFactorType		= 4;
	kAEDescListFactorTypeAndSize = 8;

	{	 Constants used creating an AppleEvent 	}
																{  Constant for the returnID param of AECreateAppleEvent  }
	kAutoGenerateReturnID		= -1;							{  AECreateAppleEvent will generate a session-unique ID  }
																{  Constant for transaction ID’s  }
	kAnyTransactionID			= 0;							{  no transaction is in use  }

	{	 Apple event manager data types 	}

type
	DescType							= ResType;
	DescTypePtr					= ^DescType;
	AEKeyword							= FourCharCode;
	AEKeywordPtr					= ^AEKeyword;
{$ifc OPAQUE_TOOLBOX_STRUCTS}
	AEDataStorage    = ^SInt32; { an opaque 32-bit type }
	AEDataStoragePtr = ^AEDataStorage;  { when a var xx:AEDataStorage parameter can be nil, it is changed to xx: AEDataStoragePtr }
{$elsec}
	AEDataStorage						= Handle;
{$endc}  {OPAQUE_TOOLBOX_STRUCTS}

	AEDescPtr = ^AEDesc;
	AEDesc = record
		descriptorType:			DescType;
		dataHandle:				AEDataStorage;
	end;

	AEKeyDescPtr = ^AEKeyDesc;
	AEKeyDesc = record
		descKey:				AEKeyword;
		descContent:			AEDesc;
	end;

	{	 a list of AEDesc's is a special kind of AEDesc 	}
	AEDescList							= AEDesc;
	AEDescListPtr 						= ^AEDescList;
	{	 AERecord is a list of keyworded AEDesc's 	}
	AERecord							= AEDescList;
	AERecordPtr 						= ^AERecord;
	{	 an AEDesc which contains address data 	}
	AEAddressDesc						= AEDesc;
	AEAddressDescPtr 					= ^AEAddressDesc;
	{	 an AERecord that contains an AppleEvent, and related data types 	}
	AppleEvent							= AERecord;
	AppleEventPtr 						= ^AppleEvent;
	AEReturnID							= SInt16;
	AETransactionID						= SInt32;
	AEEventClass						= FourCharCode;
	AEEventID							= FourCharCode;
	AEArrayType							= SInt8;

const
	kAEDataArray				= 0;
	kAEPackedArray				= 1;
	kAEDescArray				= 3;
	kAEKeyDescArray				= 4;


	kAEHandleArray				= 2;


type
	AEArrayDataPtr = ^AEArrayData;
	AEArrayData = record
		case SInt16 of
		0: (
			kAEDataArray:		array [0..0] of SInt16;
			);
		1: (
			kAEPackedArray:		SInt8;
			);
		2: (
			kAEHandleArray:		array [0..0] of Handle;
			);
		3: (
			kAEDescArray:		array [0..0] of AEDesc;
			);
		4: (
			kAEKeyDescArray:	array [0..0] of AEKeyDesc;
			);
	end;

	AEArrayDataPointer					= ^AEArrayData;
	AEArrayDataPointerPtr 				= ^AEArrayDataPointer;
	{	*************************************************************************
	  These constants are used by AEMach and AEInteraction APIs.  They are not
	  strictly part of the data format, but are declared here due to layering.
	*************************************************************************	}
	AESendPriority 				= SInt16;
const
	kAENormalPriority			= $00000000;					{  post message at the end of the event queue  }
	kAEHighPriority				= $00000001;					{  post message at the front of the event queue (same as nAttnMsg)  }


type
	AESendMode 					= SInt32;
const
	kAENoReply					= $00000001;					{  sender doesn't want a reply to event  }
	kAEQueueReply				= $00000002;					{  sender wants a reply but won't wait  }
	kAEWaitReply				= $00000003;					{  sender wants a reply and will wait  }
	kAEDontReconnect			= $00000080;					{  don't reconnect if there is a sessClosedErr from PPCToolbox  }
	kAEWantReceipt				= $00000200;					{  (nReturnReceipt) sender wants a receipt of message  }
	kAENeverInteract			= $00000010;					{  server should not interact with user  }
	kAECanInteract				= $00000020;					{  server may try to interact with user  }
	kAEAlwaysInteract			= $00000030;					{  server should always interact with user where appropriate  }
	kAECanSwitchLayer			= $00000040;					{  interaction may switch layer  }
	kAEDontRecord				= $00001000;					{  don't record this event - available only in vers 1.0.1 and greater  }
	kAEDontExecute				= $00002000;					{  don't send the event for recording - available only in vers 1.0.1 and greater  }
	kAEProcessNonReplyEvents	= $00008000;					{  allow processing of non-reply events while awaiting synchronous AppleEvent reply  }


	{	 Constants for timeout durations 	}
	kAEDefaultTimeout			= -1;							{  timeout value determined by AEM  }
	kNoTimeOut					= -2;							{  wait until reply comes back, however long it takes  }


	{	*************************************************************************
	  These calls are used to set up and modify the coercion dispatch table.
	*************************************************************************	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	AECoerceDescProcPtr = function(const (*var*) fromDesc: AEDesc; toType: DescType; handlerRefcon: SInt32; var toDesc: AEDesc): OSErr;
{$elsec}
	AECoerceDescProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	AECoercePtrProcPtr = function(typeCode: DescType; dataPtr: UnivPtr; dataSize: Size; toType: DescType; handlerRefcon: SInt32; var result: AEDesc): OSErr;
{$elsec}
	AECoercePtrProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	AECoerceDescUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AECoerceDescUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	AECoercePtrUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AECoercePtrUPP = UniversalProcPtr;
{$endc}	

const
	uppAECoerceDescProcInfo = $00003FE0;
	uppAECoercePtrProcInfo = $0003FFE0;
	{
	 *  NewAECoerceDescUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewAECoerceDescUPP(userRoutine: AECoerceDescProcPtr): AECoerceDescUPP; external name '_NewAECoerceDescUPP'; { old name was NewAECoerceDescProc }
{
 *  NewAECoercePtrUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewAECoercePtrUPP(userRoutine: AECoercePtrProcPtr): AECoercePtrUPP; external name '_NewAECoercePtrUPP'; { old name was NewAECoercePtrProc }
{
 *  DisposeAECoerceDescUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAECoerceDescUPP(userUPP: AECoerceDescUPP); external name '_DisposeAECoerceDescUPP';
{
 *  DisposeAECoercePtrUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAECoercePtrUPP(userUPP: AECoercePtrUPP); external name '_DisposeAECoercePtrUPP';
{
 *  InvokeAECoerceDescUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAECoerceDescUPP(const (*var*) fromDesc: AEDesc; toType: DescType; handlerRefcon: SInt32; var toDesc: AEDesc; userRoutine: AECoerceDescUPP): OSErr; external name '_InvokeAECoerceDescUPP'; { old name was CallAECoerceDescProc }
{
 *  InvokeAECoercePtrUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAECoercePtrUPP(typeCode: DescType; dataPtr: UnivPtr; dataSize: Size; toType: DescType; handlerRefcon: SInt32; var result: AEDesc; userRoutine: AECoercePtrUPP): OSErr; external name '_InvokeAECoercePtrUPP'; { old name was CallAECoercePtrProc }
{ a AECoercionHandlerUPP is by default a AECoerceDescUPP.  If you are registering a 
    Ptr based coercion handler you will have to add a cast to AECoerceDescUPP from 
    your AECoercePtrUPP type.  A future release of the interfaces will fix this by
    introducing seperate Desc and Ptr coercion handler installation/remove/query routines. }

type
	AECoercionHandlerUPP				= AECoerceDescUPP;
	{
	 *  AEInstallCoercionHandler()
	 *  
	 *  Mac OS X threading:
	 *    Thread safe since version 10.2
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function AEInstallCoercionHandler(fromType: DescType; toType: DescType; handler: AECoercionHandlerUPP; handlerRefcon: SInt32; fromTypeIsDesc: boolean; isSysHandler: boolean): OSErr; external name '_AEInstallCoercionHandler';
{
 *  AERemoveCoercionHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AERemoveCoercionHandler(fromType: DescType; toType: DescType; handler: AECoercionHandlerUPP; isSysHandler: boolean): OSErr; external name '_AERemoveCoercionHandler';
{
 *  AEGetCoercionHandler()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetCoercionHandler(fromType: DescType; toType: DescType; var handler: AECoercionHandlerUPP; var handlerRefcon: SInt32; var fromTypeIsDesc: boolean; isSysHandler: boolean): OSErr; external name '_AEGetCoercionHandler';
{*************************************************************************
  The following calls provide for a coercion interface.
*************************************************************************}
{
 *  AECoercePtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECoercePtr(typeCode: DescType; dataPtr: UnivPtr; dataSize: Size; toType: DescType; var result: AEDesc): OSErr; external name '_AECoercePtr';
{
 *  AECoerceDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECoerceDesc(const (*var*) theAEDesc: AEDesc; toType: DescType; var result: AEDesc): OSErr; external name '_AECoerceDesc';
{*************************************************************************
 The following calls apply to any AEDesc. Every 'result' descriptor is
 created for you, so you will be responsible for memory management
 (including disposing) of the descriptors so created.  
*************************************************************************}
{ because AEDescs are opaque under Carbon, this AEInitializeDesc provides a
   'clean' way of initializating them to be empty. }
{
 *  AEInitializeDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure AEInitializeDesc(var desc: AEDesc); external name '_AEInitializeDesc';


{
 *  AECreateDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECreateDesc(typeCode: DescType; dataPtr: UnivPtr; dataSize: Size; var result: AEDesc): OSErr; external name '_AECreateDesc';
{
 *  AEDisposeDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEDisposeDesc(var theAEDesc: AEDesc): OSErr; external name '_AEDisposeDesc';
{
 *  AEDuplicateDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEDuplicateDesc(const (*var*) theAEDesc: AEDesc; var result: AEDesc): OSErr; external name '_AEDuplicateDesc';

{
 * Create an AEDesc with memory "borrowed" from the application. The
 * data passed in *must* be immutable and not freed until the Dispose
 * callback is made.
 * The dispose callback may be made at any time, including during the
 * creation of the descriptor.
 * If possible, the descriptor will be copied to the address space of
 * any recipient process using virtual memory APIs and avoid an
 * actual memory copy.
 }
type
	AEDisposeExternalProcPtr = procedure( dataPtr: {const} UnivPtr; dataLength: Size; refcon: SInt32 );

{$ifc OPAQUE_UPP_TYPES}
	AEDisposeExternalUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AEDisposeExternalUPP = UniversalProcPtr;
{$endc}	

{
 *  AECreateDescFromExternalPtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function AECreateDescFromExternalPtr( descriptorType: OSType; dataPtr: {const} UnivPtr; dataLength: Size; disposeCallback: AEDisposeExternalUPP; disposeRefcon: SInt32; var theDesc: AEDesc ): OSStatus; external name '_AECreateDescFromExternalPtr';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{*************************************************************************
  The following calls apply to AEDescList. Since AEDescList is a subtype of
  AEDesc, the calls in the previous section can also be used for AEDescList.
  All list and array indices are 1-based. If the data was greater than
  maximumSize in the routines below, then actualSize will be greater than
  maximumSize, but only maximumSize bytes will actually be retrieved.
*************************************************************************}
{
 *  AECreateList()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECreateList(factoringPtr: UnivPtr; factoredSize: Size; isRecord: boolean; var resultList: AEDescList): OSErr; external name '_AECreateList';
{
 *  AECountItems()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECountItems(const (*var*) theAEDescList: AEDescList; var theCount: SInt32): OSErr; external name '_AECountItems';
{
 *  AEPutPtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutPtr(var theAEDescList: AEDescList; index: SInt32; typeCode: DescType; dataPtr: UnivPtr; dataSize: Size): OSErr; external name '_AEPutPtr';
{
 *  AEPutDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutDesc(var theAEDescList: AEDescList; index: SInt32; const (*var*) theAEDesc: AEDesc): OSErr; external name '_AEPutDesc';
{
 *  AEGetNthPtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetNthPtr(const (*var*) theAEDescList: AEDescList; index: SInt32; desiredType: DescType; theAEKeywordPtr: AEKeywordPtr; typeCode: DescTypePtr; dataPtr: UnivPtr; maximumSize: Size; actualSize: SizePtr): OSErr; external name '_AEGetNthPtr';
{
 *  AEGetNthDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetNthDesc(const (*var*) theAEDescList: AEDescList; index: SInt32; desiredType: DescType; theAEKeywordPtr: AEKeywordPtr; var result: AEDesc): OSErr; external name '_AEGetNthDesc';
{
 *  AESizeOfNthItem()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AESizeOfNthItem(const (*var*) theAEDescList: AEDescList; index: SInt32; typeCode: DescTypePtr; dataSize: SizePtr): OSErr; external name '_AESizeOfNthItem';
{
 *  AEGetArray()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetArray(const (*var*) theAEDescList: AEDescList; arrayType: AEArrayType; arrayPtr: AEArrayDataPointer; maximumSize: Size; var itemType: DescType; var itemSize: Size; var itemCount: SInt32): OSErr; external name '_AEGetArray';
{
 *  AEPutArray()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutArray(var theAEDescList: AEDescList; arrayType: AEArrayType; const (*var*) arrayPtr: AEArrayData; itemType: DescType; itemSize: Size; itemCount: SInt32): OSErr; external name '_AEPutArray';
{
 *  AEDeleteItem()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEDeleteItem(var theAEDescList: AEDescList; index: SInt32): OSErr; external name '_AEDeleteItem';
{*************************************************************************
 The following calls apply to AERecord. Since AERecord is a subtype of
 AEDescList, the calls in the previous sections can also be used for
 AERecord an AERecord can be created by using AECreateList with isRecord
 set to true. 
*************************************************************************}
{************************************************************************
 AERecords can have an abitrary descriptorType.  This allows you to
 check if desc is truly an AERecord
***********************************************************************}
{
 *  AECheckIsRecord()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECheckIsRecord(const (*var*) theDesc: AEDesc): boolean; external name '_AECheckIsRecord';

{
  Note: none of the “key” calls were available in the PowerPC 7.x IntefaceLib.
  In C, a #define is used to map “key” calls to “param” calls.  In pascal
  this mapping is done in externally linked glue code.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  AEPutKeyPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AEPutKeyPtr(var theAERecord: AERecord; theAEKeyword: AEKeyword; typeCode: DescType; dataPtr: UnivPtr; dataSize: Size): OSErr; external name '_AEPutKeyPtr';
{
 *  AEPutKeyDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AEPutKeyDesc(var theAERecord: AERecord; theAEKeyword: AEKeyword; const (*var*) theAEDesc: AEDesc): OSErr; external name '_AEPutKeyDesc';
{
 *  AEGetKeyPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AEGetKeyPtr(const (*var*) theAERecord: AERecord; theAEKeyword: AEKeyword; desiredType: DescType; var typeCode: DescType; dataPtr: UnivPtr; maximumSize: Size; var actualSize: Size): OSErr; external name '_AEGetKeyPtr';
{
 *  AEGetKeyDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AEGetKeyDesc(const (*var*) theAERecord: AERecord; theAEKeyword: AEKeyword; desiredType: DescType; var result: AEDesc): OSErr; external name '_AEGetKeyDesc';
{
 *  AESizeOfKeyDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AESizeOfKeyDesc(const (*var*) theAERecord: AERecord; theAEKeyword: AEKeyword; var typeCode: DescType; var dataSize: Size): OSErr; external name '_AESizeOfKeyDesc';
{
 *  AEDeleteKeyDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AEDeleteKeyDesc(var theAERecord: AERecord; theAEKeyword: AEKeyword): OSErr; external name '_AEDeleteKeyDesc';
{$endc}  {CALL_NOT_IN_CARBON}

{*************************************************************************
  The following calls create and manipulate the AppleEvent data type.
*************************************************************************}
{
 *  AECreateAppleEvent()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AECreateAppleEvent(theAEEventClass: AEEventClass; theAEEventID: AEEventID; const (*var*) target: AEAddressDesc; returnID: AEReturnID; transactionID: AETransactionID; var result: AppleEvent): OSErr; external name '_AECreateAppleEvent';
{*************************************************************************
  The following calls are used to pack and unpack parameters from records
  of type AppleEvent. Since AppleEvent is a subtype of AERecord, the calls
  in the previous sections can also be used for variables of type
  AppleEvent. The next six calls are in fact identical to the six calls
  for AERecord.
*************************************************************************}
{
 *  AEPutParamPtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutParamPtr(var theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; typeCode: DescType; dataPtr: UnivPtr; dataSize: Size): OSErr; external name '_AEPutParamPtr';
{
 *  AEPutParamDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutParamDesc(var theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; const (*var*) theAEDesc: AEDesc): OSErr; external name '_AEPutParamDesc';
{
 *  AEGetParamPtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetParamPtr(const (*var*) theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; desiredType: DescType; actualType: DescTypePtr; dataPtr: UnivPtr; maximumSize: Size; actualSize: SizePtr): OSErr; external name '_AEGetParamPtr';
{
 *  AEGetParamDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetParamDesc(const (*var*) theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; desiredType: DescType; var result: AEDesc): OSErr; external name '_AEGetParamDesc';
{
 *  AESizeOfParam()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AESizeOfParam(const (*var*) theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; typeCode: DescTypePtr; dataSize: SizePtr): OSErr; external name '_AESizeOfParam';
{
 *  AEDeleteParam()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEDeleteParam(var theAppleEvent: AppleEvent; theAEKeyword: AEKeyword): OSErr; external name '_AEDeleteParam';
{*************************************************************************
 The following calls also apply to type AppleEvent. Message attributes are
 far more restricted, and can only be accessed through the following 5
 calls. The various list and record routines cannot be used to access the
 attributes of an event. 
*************************************************************************}
{
 *  AEGetAttributePtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetAttributePtr(const (*var*) theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; desiredType: DescType; typeCode: DescTypePtr; dataPtr: UnivPtr; maximumSize: Size; actualSize: SizePtr): OSErr; external name '_AEGetAttributePtr';
{
 *  AEGetAttributeDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetAttributeDesc(const (*var*) theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; desiredType: DescType; var result: AEDesc): OSErr; external name '_AEGetAttributeDesc';
{
 *  AESizeOfAttribute()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AESizeOfAttribute(const (*var*) theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; typeCode: DescTypePtr; dataSize: SizePtr): OSErr; external name '_AESizeOfAttribute';
{
 *  AEPutAttributePtr()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutAttributePtr(var theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; typeCode: DescType; dataPtr: UnivPtr; dataSize: Size): OSErr; external name '_AEPutAttributePtr';
{
 *  AEPutAttributeDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEPutAttributeDesc(var theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; const (*var*) theAEDesc: AEDesc): OSErr; external name '_AEPutAttributeDesc';
{*************************************************************************
 AppleEvent Serialization Support

    AESizeOfFlattenedDesc, AEFlattenDesc, AEUnflattenDesc
    
    These calls will work for all AppleEvent data types and between different
    versions of the OS (including between Mac OS 9 and X)
    
    Basic types, AEDesc, AEList and AERecord are OK, but AppleEvent records
    themselves may not be reliably flattened for storage.
*************************************************************************}
{
   AEFlattenDesc
   Returns the amount of buffer space needed to flatten the
   AEDesc. Call this before AEFlattenDesc to make sure your
   buffer has enough room for the operation.
}

{
 *  AESizeOfFlattenedDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AESizeOfFlattenedDesc(const (*var*) theAEDesc: AEDesc): Size; external name '_AESizeOfFlattenedDesc';

{
   AEFlattenDesc
   Fills a buffer with a flattened representation of the
   AEDesc and returns the amount of buffer used in actualSize.
   If bufferSize was too small it returns errAEBufferTooSmall
   (-1741) and does not fill in any of the buffer. The resulting
   buffer is only useful with an AEUnflattenDesc call.
   
   Note: if you pass a NULL buffer pointer it returns noErr but
   fills in the actualSize field anyway.
}

{
 *  AEFlattenDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEFlattenDesc(const (*var*) theAEDesc: AEDesc; buffer: Ptr; bufferSize: Size; var actualSize: Size): OSStatus; external name '_AEFlattenDesc';

{
   AEUnflattenDesc
   Allocates an AEDesc (given a Null Desc) given a flattened
   data buffer. It assumes it was given a good buffer filled
   in by AEFlattenDesc. It returns paramErr if it discovers
   something fishy about the buffer.
}

{
 *  AEUnflattenDesc()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.4 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEUnflattenDesc(buffer: Ptr; var result: AEDesc): OSStatus; external name '_AEUnflattenDesc';

{*************************************************************************
 The following calls are necessary to deal with opaque data in AEDescs, because the
 traditional way of dealing with a basic AEDesc has been to dereference the dataHandle
 directly.  This is not supported under Carbon.
*************************************************************************}
{$ifc ACCESSOR_CALLS_ARE_FUNCTIONS}
{
        AEGetDescData no longer supports automatic coercion. If you'd like to
        coerce the descriptor use AECoerceDesc.
    }
{
 *  AEGetDescData()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetDescData(const (*var*) theAEDesc: AEDesc; dataPtr: UnivPtr; maximumSize: Size): OSErr; external name '_AEGetDescData';

{
 *  AEGetDescDataSize()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEGetDescDataSize(const (*var*) theAEDesc: AEDesc): Size; external name '_AEGetDescDataSize';

{
 *  AEReplaceDescData()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AEReplaceDescData(typeCode: DescType; dataPtr: UnivPtr; dataSize: Size; var theAEDesc: AEDesc): OSErr; external name '_AEReplaceDescData';

{$endc}  {ACCESSOR_CALLS_ARE_FUNCTIONS}

{
 * Retrieve a range of bytes from an AEDesc.  This obviates the need
 * to retrieve the entire data from the event using AEGetDescData.
 * This is only valid for data type AEDescs.  If the requested length
 * and offset are such that they do not fit entirely with the data of the
 * desc, errAEBufferTooSmall is returned.
 }
{
 *  AEGetDescDataRange()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function AEGetDescDataRange( const (*var*) dataDesc: AEDesc; buffer: UnivPtr; offset: Size; length: Size ): OSStatus; external name '_AEGetDescDataRange';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)


{*************************************************************************
  A AEEventHandler is installed to process an AppleEvent 
*************************************************************************}

type
{$ifc TYPED_FUNCTION_POINTERS}
	AEEventHandlerProcPtr = function(const (*var*) theAppleEvent: AppleEvent; var reply: AppleEvent; handlerRefcon: SInt32): OSErr;
{$elsec}
	AEEventHandlerProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	AEEventHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AEEventHandlerUPP = UniversalProcPtr;
{$endc}	

const
	uppAEEventHandlerProcInfo = $00000FE0;

{
 *  NewAEDisposeExternalUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewAEDisposeExternalUPP( userRoutine: AEDisposeExternalProcPtr ): AEDisposeExternalUPP; external name '_NewAEDisposeExternalUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

	{
	 *  NewAEEventHandlerUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewAEEventHandlerUPP(userRoutine: AEEventHandlerProcPtr): AEEventHandlerUPP; external name '_NewAEEventHandlerUPP'; { old name was NewAEEventHandlerProc }

{
 *  DisposeAEDisposeExternalUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeAEDisposeExternalUPP( userUPP: AEDisposeExternalUPP ); external name '_DisposeAEDisposeExternalUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{
 *  DisposeAEEventHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAEEventHandlerUPP(userUPP: AEEventHandlerUPP); external name '_DisposeAEEventHandlerUPP';

{
 *  InvokeAEDisposeExternalUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeAEDisposeExternalUPP( dataPtr: {const} UnivPtr; dataLength: Size; refcon: SInt32; userUPP: AEDisposeExternalUPP ); external name '_InvokeAEDisposeExternalUPP';
(* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{
 *  InvokeAEEventHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAEEventHandlerUPP(const (*var*) theAppleEvent: AppleEvent; var reply: AppleEvent; handlerRefcon: SInt32; userRoutine: AEEventHandlerUPP): OSErr; external name '_InvokeAEEventHandlerUPP'; { old name was CallAEEventHandlerProc }
{$ALIGN MAC68K}


end.
