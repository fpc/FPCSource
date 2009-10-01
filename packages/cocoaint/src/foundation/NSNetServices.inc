{ Parsed from Foundation.framework NSNetServices.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
  NSNetServiceNoAutoRename = 1  shl  0;

{ Types }
type
  NSNetServicesError = NSInteger;
  NSNetServiceOptions = NSUInteger;

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
  NSNetServicesErrorCode: NSString; external name '_NSNetServicesErrorCode';
  NSNetServicesErrorDomain: NSString; external name '_NSNetServicesErrorDomain';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNetService = objcclass;
  NSNetServicePointer = ^NSNetService;
  NSNetServiceBrowser = objcclass;
  NSNetServiceBrowserPointer = ^NSNetServiceBrowser;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNETSERVICES_PAS_C}
{$define NSNETSERVICES_PAS_C}

{ NSNetService }
  NSNetService = objcclass(NSObject)
  private
    __netService: id;
    __delegate: id;
    __reserved: id;
    
  public
    class function alloc: NSNetService; message 'alloc';

    function initWithDomain_type_name_port(domain_: NSString; type__: NSString; name_: NSString; port_: cint): id; message 'initWithDomain:type:name:port:';
    function initWithDomain_type_name(domain_: NSString; type__: NSString; name_: NSString): id; message 'initWithDomain:type:name:';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    function domain: NSString; message 'domain';
    function type_: NSString; message 'type';
    function name: NSString; message 'name';
    function addresses: NSArray; message 'addresses';
    function port: NSInteger; message 'port';
    procedure publish; message 'publish';
    procedure publishWithOptions(options: NSNetServiceOptions); message 'publishWithOptions:';
    procedure resolve; message 'resolve';
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

    { Category: NSDeprecated }
    function protocolSpecificInformation: NSString; message 'protocolSpecificInformation';
    procedure setProtocolSpecificInformation(specificInformation: NSString); message 'setProtocolSpecificInformation:';
  end; external;

{ NSNetServiceBrowser }
  NSNetServiceBrowser = objcclass(NSObject)
  private
    __netServiceBrowser: id;
    __delegate: id;
    __reserved: Pointer;
    
  public
    class function alloc: NSNetServiceBrowser; message 'alloc';

    function init: id; message 'init';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    procedure scheduleInRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'scheduleInRunLoop:forMode:';
    procedure removeFromRunLoop_forMode(aRunLoop: NSRunLoop; mode: NSString); message 'removeFromRunLoop:forMode:';
    procedure searchForBrowsableDomains; message 'searchForBrowsableDomains';
    procedure searchForRegistrationDomains; message 'searchForRegistrationDomains';
    procedure searchForServicesOfType_inDomain(type_: NSString; domainString: NSString); message 'searchForServicesOfType:inDomain:';
    procedure stop; message 'stop';

    { Category: NSDeprecated }
    procedure searchForAllDomains; message 'searchForAllDomains';
  end; external;

{$endif}
{$endif}
