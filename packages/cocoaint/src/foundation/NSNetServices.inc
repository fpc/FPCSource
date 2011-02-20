{ Parsed from Foundation.framework NSNetServices.h }

{$ifdef TYPES}
{$ifndef NSNETSERVICES_PAS_T}
{$define NSNETSERVICES_PAS_T}

{ Constants }

const
  NSNetServicesUnknownError = -72000;
  NSNetServicesCollisionError = -72001;
  NSNetServicesNotFoundError = -72002;
  NSNetServicesActivityInProgress = -72003;
  NSNetServicesBadArgumentError = -72004;
  NSNetServicesCancelledError = -72005;
  NSNetServicesInvalidError = -72006;
  NSNetServicesTimeoutError = -72007;

const
  NSNetServiceNoAutoRename = 1 shl 0;

{ Types }
type
  NSNetServicesError = NSInteger;
  NSNetServicesErrorPtr = ^NSNetServicesError;
  NSNetServiceOptions = NSUInteger;
  NSNetServiceOptionsPtr = ^NSNetServiceOptions;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSNETSERVICES_PAS_R}
{$define NSNETSERVICES_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSNETSERVICES_PAS_F}
{$define NSNETSERVICES_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSNETSERVICES_PAS_S}
{$define NSNETSERVICES_PAS_S}

{ External string constants }
var
  NSNetServicesErrorCode: NSString; cvar; external;
  NSNetServicesErrorDomain: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNetServiceDelegateProtocol = objcprotocol;
  NSNetServiceBrowserDelegateProtocol = objcprotocol;
  NSNetService = objcclass;
  NSNetServicePointer = ^NSNetService;
  NSNetServicePtr = NSNetServicePointer;
  NSNetServiceBrowser = objcclass;
  NSNetServiceBrowserPointer = ^NSNetServiceBrowser;
  NSNetServiceBrowserPtr = NSNetServiceBrowserPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNETSERVICES_PAS_C}
{$define NSNETSERVICES_PAS_C}

{ NSNetService }
  NSNetService = objcclass external (NSObject)
  private
    _netService: id;
    _delegate: id;
    _reserved: id;
    
  public
    function initWithDomain_type_name_port(domain_: NSString; type__: NSString; name_: NSString; port_: cint): id; message 'initWithDomain:type:name:port:';
    function initWithDomain_type_name(domain_: NSString; type__: NSString; name_: NSString): id; message 'initWithDomain:type:name:';
    function delegate: NSNetServiceDelegateProtocol; message 'delegate';
    procedure setDelegate(delegate_: NSNetServiceDelegateProtocol); message 'setDelegate:';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function domain: NSString; message 'domain';
    function type_: NSString; message 'type';
    function name: NSString; message 'name';
    function addresses: NSArray; message 'addresses';
    function port: NSInteger; message 'port';
    procedure publish; message 'publish';
    procedure publishWithOptions(options: NSNetServiceOptions); message 'publishWithOptions:';
    procedure resolve; message 'resolve'; deprecated 'in Mac OS X 10.4 and later';
    procedure stop; message 'stop';
    class function dictionaryFromTXTRecordData(txtData: NSData): NSDictionary; message 'dictionaryFromTXTRecordData:';
    class function dataFromTXTRecordDictionary(txtDictionary: NSDictionary): NSData; message 'dataFromTXTRecordDictionary:';
    function hostName: NSString; message 'hostName';
    procedure resolveWithTimeout(timeout: NSTimeInterval); message 'resolveWithTimeout:';
    function getInputStream_outputStream(inputStream: NSInputStreamPointer; outputStream: NSOutputStreamPointer): Boolean; message 'getInputStream:outputStream:';
    function setTXTRecordData(recordData: NSData): Boolean; message 'setTXTRecordData:';
    function TXTRecordData: NSData; message 'TXTRecordData';
    procedure startMonitoring; message 'startMonitoring';
    procedure stopMonitoring; message 'stopMonitoring';
  end;

{ NSNetServiceBrowser }
  NSNetServiceBrowser = objcclass external (NSObject)
  private
    _netServiceBrowser: id;
    _delegate: id;
    _reserved: Pointer;
    
  public
    function init: id; message 'init';
    function delegate: NSNetServiceBrowserDelegateProtocol; message 'delegate';
    procedure setDelegate(delegate_: NSNetServiceBrowserDelegateProtocol); message 'setDelegate:';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    procedure searchForBrowsableDomains; message 'searchForBrowsableDomains';
    procedure searchForRegistrationDomains; message 'searchForRegistrationDomains';
    procedure searchForServicesOfType_inDomain(type_: NSString; domainString: NSString); message 'searchForServicesOfType:inDomain:';
    procedure stop; message 'stop';
  end;

{ NSDeprecated_NSNetServiceCategory }
  NSDeprecated_NSNetServiceCategory = objccategory external name 'NSDeprecated' (NSNetService)
    function protocolSpecificInformation: NSString; message 'protocolSpecificInformation'; deprecated 'in Mac OS X 10.4 and later';
    procedure setProtocolSpecificInformation(specificInformation: NSString); message 'setProtocolSpecificInformation:'; deprecated 'in Mac OS X 10.4 and later';
  end;

{ NSDeprecated_NSNetServiceBrowserCategory }
  NSDeprecated_NSNetServiceBrowserCategory = objccategory external name 'NSDeprecated' (NSNetServiceBrowser)
    procedure searchForAllDomains; message 'searchForAllDomains'; deprecated 'in Mac OS X 10.4 and later';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSNETSERVICES_PAS_P}
{$define NSNETSERVICES_PAS_P}
  
{ NSNetServiceDelegate Protocol }
  NSNetServiceDelegateProtocol = objcprotocol external name 'NSNetServiceDelegate'
  optional
    procedure netServiceWillPublish(sender: NSNetService); message 'netServiceWillPublish:';
    procedure netServiceDidPublish(sender: NSNetService); message 'netServiceDidPublish:';
    procedure netService_didNotPublish(sender: NSNetService; errorDict: NSDictionary); message 'netService:didNotPublish:';
    procedure netServiceWillResolve(sender: NSNetService); message 'netServiceWillResolve:';
    procedure netServiceDidResolveAddress(sender: NSNetService); message 'netServiceDidResolveAddress:';
    procedure netService_didNotResolve(sender: NSNetService; errorDict: NSDictionary); message 'netService:didNotResolve:';
    procedure netServiceDidStop(sender: NSNetService); message 'netServiceDidStop:';
    procedure netService_didUpdateTXTRecordData(sender: NSNetService; data: NSData); message 'netService:didUpdateTXTRecordData:';
  end;
  
{ NSNetServiceBrowserDelegate Protocol }
  NSNetServiceBrowserDelegateProtocol = objcprotocol external name 'NSNetServiceBrowserDelegate'
  optional
    procedure netServiceBrowserWillSearch(aNetServiceBrowser: NSNetServiceBrowser); message 'netServiceBrowserWillSearch:';
    procedure netServiceBrowserDidStopSearch(aNetServiceBrowser: NSNetServiceBrowser); message 'netServiceBrowserDidStopSearch:';
    procedure netServiceBrowser_didNotSearch(aNetServiceBrowser: NSNetServiceBrowser; errorDict: NSDictionary); message 'netServiceBrowser:didNotSearch:';
    procedure netServiceBrowser_didFindDomain_moreComing(aNetServiceBrowser: NSNetServiceBrowser; domainString: NSString; moreComing: Boolean); message 'netServiceBrowser:didFindDomain:moreComing:';
    procedure netServiceBrowser_didFindService_moreComing(aNetServiceBrowser: NSNetServiceBrowser; aNetService: NSNetService; moreComing: Boolean); message 'netServiceBrowser:didFindService:moreComing:';
    procedure netServiceBrowser_didRemoveDomain_moreComing(aNetServiceBrowser: NSNetServiceBrowser; domainString: NSString; moreComing: Boolean); message 'netServiceBrowser:didRemoveDomain:moreComing:';
    procedure netServiceBrowser_didRemoveService_moreComing(aNetServiceBrowser: NSNetServiceBrowser; aNetService: NSNetService; moreComing: Boolean); message 'netServiceBrowser:didRemoveService:moreComing:';
  end;
{$endif}
{$endif}
