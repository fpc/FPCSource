{ Parsed from Foundation.framework NSNetServices.h }


{$ifdef TYPES}
type
  NSNetServicePtr = ^NSNetService;
  NSNetServiceBrowserPtr = ^NSNetServiceBrowser;
  NSNetServiceDelegateProtocolPtr = ^NSNetServiceDelegateProtocol;
  NSNetServiceBrowserDelegateProtocolPtr = ^NSNetServiceBrowserDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSNetServicesErrorCode: NSString; cvar; external;
  NSNetServicesErrorDomain: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSNetServicesError = NSInteger;
  NSNetServicesErrorPtr = ^NSNetServicesError;

const
  NSNetServicesUnknownError = -72000;
  NSNetServicesCollisionError = -72001;
  NSNetServicesNotFoundError = -72002;
  NSNetServicesActivityInProgress = -72003;
  NSNetServicesBadArgumentError = -72004;
  NSNetServicesCancelledError = -72005;
  NSNetServicesInvalidError = -72006;
  NSNetServicesTimeoutError = -72007;

type
  NSNetServiceOptions = NSUInteger;
  NSNetServiceOptionsPtr = ^NSNetServiceOptions;

const
  NSNetServiceNoAutoRename = 1 shl 0;
  NSNetServiceListenForConnections = 1 shl 1 { available in 10_9, 7_0 };
{$endif}

{$ifdef CLASSES}

type
  NSNetService = objcclass external (NSObject)
  private
    _netService: id;
    _delegate: id;
    _reserved: id;
  public
    function initWithDomain_type_name_port (domain: NSString; type_: NSString; name: NSString; port: cint): instancetype; message 'initWithDomain:type:name:port:';
    function initWithDomain_type_name (domain: NSString; type_: NSString; name: NSString): instancetype; message 'initWithDomain:type:name:';
    procedure scheduleInRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    procedure setDelegate(newValue: NSNetServiceDelegateProtocol); message 'setDelegate:';
    function delegate: NSNetServiceDelegateProtocol; message 'delegate';
    procedure setIncludesPeerToPeer(newValue: ObjCBOOL); message 'setIncludesPeerToPeer:';
    function includesPeerToPeer: ObjCBOOL; message 'includesPeerToPeer';
    function name: NSString; message 'name';
    function type_: NSString; message 'type';
    function domain: NSString; message 'domain';
    function hostName: NSString; message 'hostName';
    function addresses: NSArray; message 'addresses';
    function port: NSInteger; message 'port';
    procedure publish; message 'publish';
    procedure publishWithOptions (options: NSNetServiceOptions); message 'publishWithOptions:'; { available in 10_5, 2_0 }
    procedure resolve; message 'resolve'; deprecated 'in 10_2, 10_4, 2_0, 2_0';
    procedure stop; message 'stop';
    class function dictionaryFromTXTRecordData (txtData: NSData): NSDictionary; message 'dictionaryFromTXTRecordData:';
    class function dataFromTXTRecordDictionary (txtDictionary: NSDictionary): NSData; message 'dataFromTXTRecordDictionary:';
    procedure resolveWithTimeout (timeout: NSTimeInterval); message 'resolveWithTimeout:';
    function getInputStream_outputStream (inputStream: NSInputStreamPtr; outputStream: NSOutputStreamPtr): ObjCBOOL; message 'getInputStream:outputStream:';
    function setTXTRecordData (recordData: NSData): ObjCBOOL; message 'setTXTRecordData:';
    function TXTRecordData: NSData; message 'TXTRecordData';
    procedure startMonitoring; message 'startMonitoring';
    procedure stopMonitoring; message 'stopMonitoring';
  end;

type
  NSNetServiceBrowser = objcclass external (NSObject)
  private
    _netServiceBrowser: id;
    _delegate: id;
    _reserved: pointer;
  public
    function init: instancetype; message 'init';
    procedure setDelegate(newValue: NSNetServiceBrowserDelegateProtocol); message 'setDelegate:';
    function delegate: NSNetServiceBrowserDelegateProtocol; message 'delegate';
    procedure setIncludesPeerToPeer(newValue: ObjCBOOL); message 'setIncludesPeerToPeer:';
    function includesPeerToPeer: ObjCBOOL; message 'includesPeerToPeer';
    procedure scheduleInRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode (aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    procedure searchForBrowsableDomains; message 'searchForBrowsableDomains';
    procedure searchForRegistrationDomains; message 'searchForRegistrationDomains';
    procedure searchForServicesOfType_inDomain (type_: NSString; domainString: NSString); message 'searchForServicesOfType:inDomain:';
    procedure stop; message 'stop';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSNetServiceDelegateProtocol = objcprotocol external name 'NSNetServiceDelegate' (NSObjectProtocol)
  optional
    procedure netServiceWillPublish (sender: NSNetService); message 'netServiceWillPublish:';
    procedure netServiceDidPublish (sender: NSNetService); message 'netServiceDidPublish:';
    procedure netService_didNotPublish (sender: NSNetService; errorDict: NSDictionary); message 'netService:didNotPublish:';
    procedure netServiceWillResolve (sender: NSNetService); message 'netServiceWillResolve:';
    procedure netServiceDidResolveAddress (sender: NSNetService); message 'netServiceDidResolveAddress:';
    procedure netService_didNotResolve (sender: NSNetService; errorDict: NSDictionary); message 'netService:didNotResolve:';
    procedure netServiceDidStop (sender: NSNetService); message 'netServiceDidStop:';
    procedure netService_didUpdateTXTRecordData (sender: NSNetService; data: NSData); message 'netService:didUpdateTXTRecordData:';
    procedure netService_didAcceptConnectionWithInputStream_outputStream (sender: NSNetService; inputStream: NSInputStream; outputStream: NSOutputStream); message 'netService:didAcceptConnectionWithInputStream:outputStream:'; { available in 10_9, 7_0 }
  end;

type
  NSNetServiceBrowserDelegateProtocol = objcprotocol external name 'NSNetServiceBrowserDelegate' (NSObjectProtocol)
  optional
    procedure netServiceBrowserWillSearch (aNetServiceBrowser: NSNetServiceBrowser); message 'netServiceBrowserWillSearch:';
    procedure netServiceBrowserDidStopSearch (aNetServiceBrowser: NSNetServiceBrowser); message 'netServiceBrowserDidStopSearch:';
    procedure netServiceBrowser_didNotSearch (aNetServiceBrowser: NSNetServiceBrowser; errorDict: NSDictionary); message 'netServiceBrowser:didNotSearch:';
    procedure netServiceBrowser_didFindDomain_moreComing (aNetServiceBrowser: NSNetServiceBrowser; domainString: NSString; moreComing: ObjCBOOL); message 'netServiceBrowser:didFindDomain:moreComing:';
    procedure netServiceBrowser_didFindService_moreComing (aNetServiceBrowser: NSNetServiceBrowser; aNetService: NSNetService; moreComing: ObjCBOOL); message 'netServiceBrowser:didFindService:moreComing:';
    procedure netServiceBrowser_didRemoveDomain_moreComing (aNetServiceBrowser: NSNetServiceBrowser; domainString: NSString; moreComing: ObjCBOOL); message 'netServiceBrowser:didRemoveDomain:moreComing:';
    procedure netServiceBrowser_didRemoveService_moreComing (aNetServiceBrowser: NSNetServiceBrowser; aNetService: NSNetService; moreComing: ObjCBOOL); message 'netServiceBrowser:didRemoveService:moreComing:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  NSNetService_NSDeprecated = objccategory external name 'NSDeprecated' (NSNetService)
    function protocolSpecificInformation: NSString; message 'protocolSpecificInformation'; deprecated 'in 10_2, 10_4, 2_0, 2_0';
    procedure setProtocolSpecificInformation (specificInformation: NSString); message 'setProtocolSpecificInformation:'; deprecated 'in 10_2, 10_4, 2_0, 2_0';
  end;

type
  NSNetServiceBrowser_NSDeprecated = objccategory external name 'NSDeprecated' (NSNetServiceBrowser)
    procedure searchForAllDomains; message 'searchForAllDomains'; deprecated 'in 10_2, 10_4, 2_0, 2_0';
  end;
{$endif}

{$endif}
