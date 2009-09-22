{ Parsed from Foundation.framework NSConnection.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCONNECTION_PAS_H}
{$define NSCONNECTION_PAS_H}
type
  NSConnectionPointer = Pointer;
  NSDistantObjectRequestPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCONNECTION_PAS_T}
{$define NSCONNECTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCONNECTION_PAS_R}
{$define NSCONNECTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCONNECTION_PAS_F}
{$define NSCONNECTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCONNECTION_PAS_T}
{$define NSCONNECTION_PAS_T}

{ External string constants }
var
  NSConnectionReplyMode: NSString; external name '_NSConnectionReplyMode';
  NSConnectionDidDieNotification: NSString; external name '_NSConnectionDidDieNotification';
  NSFailedAuthenticationException: NSString; external name '_NSFailedAuthenticationException';
  NSConnectionDidInitializeNotification: NSString; external name '_NSConnectionDidInitializeNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSConnection = objcclass;
  NSDistantObjectRequest = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCONNECTION_PAS_C}
{$define NSCONNECTION_PAS_C}

{ NSConnection }
  NSConnection = objcclass(NSObject)
  private
    _receivePort: id;
    _sendPort: id;
    _delegate: id;
    _busy: longint;
    _localProxyCount: longint;
    _waitCount: longint;
    _delayedRL: id;
    _statistics: id;
    _isDead: char;
    _isValid: char;
    _wantsInvalid: char;
    _authGen: 0..1;
    _authCheck: 0..1;
    _encryptFlag: 0..1;
    _decryptFlag: 0..1;
    _doRequest: 0..1;
    _isQueueing: 0..1;
    _isMulti: 0..1;
    _invalidateRP: 0..1;
    ____1: id;
    ____2: id;
    _runLoops: id;
    _requestModes: id;
    _rootObject: id;
    _registerInfo: Pointer;
    _replMode: id;
    _classInfoImported: id;
    _releasedProxies: id;
    _reserved: Pointer;
    
  public
    class function alloc: NSConnection; message 'alloc';

    function statistics: NSDictionary; message 'statistics';
    class function allConnections: NSArray; message 'allConnections';
    class function defaultConnection: NSConnection; message 'defaultConnection';
    class function connectionWithRegisteredName_host(name: NSString; hostName: NSString): id; message 'connectionWithRegisteredName:host:';
    class function connectionWithRegisteredName_host_usingNameServer(name: NSString; hostName: NSString; server: NSPortNameServer): id; message 'connectionWithRegisteredName:host:usingNameServer:';
    class function rootProxyForConnectionWithRegisteredName_host(name: NSString; hostName: NSString): NSDistantObject; message 'rootProxyForConnectionWithRegisteredName:host:';
    class function rootProxyForConnectionWithRegisteredName_host_usingNameServer(name: NSString; hostName: NSString; server: NSPortNameServer): NSDistantObject; message 'rootProxyForConnectionWithRegisteredName:host:usingNameServer:';
    class function serviceConnectionWithName_rootObject_usingNameServer(name: NSString; root: id; server: NSPortNameServer): id; message 'serviceConnectionWithName:rootObject:usingNameServer:';
    class function serviceConnectionWithName_rootObject(name: NSString; root: id): id; message 'serviceConnectionWithName:rootObject:';
    procedure setRequestTimeout(ti: NSTimeInterval); message 'setRequestTimeout:';
    function requestTimeout: NSTimeInterval; message 'requestTimeout';
    procedure setReplyTimeout(ti: NSTimeInterval); message 'setReplyTimeout:';
    function replyTimeout: NSTimeInterval; message 'replyTimeout';
    procedure setRootObject(anObject: id); message 'setRootObject:';
    function rootObject: id; message 'rootObject';
    function rootProxy: NSDistantObject; message 'rootProxy';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setIndependentConversationQueueing(yorn: Boolean); message 'setIndependentConversationQueueing:';
    function independentConversationQueueing: Boolean; message 'independentConversationQueueing';
    function isValid: Boolean; message 'isValid';
    procedure invalidate; message 'invalidate';
    procedure addRequestMode(rmode: NSString); message 'addRequestMode:';
    procedure removeRequestMode(rmode: NSString); message 'removeRequestMode:';
    function requestModes: NSArray; message 'requestModes';
    function registerName(name: NSString): Boolean; message 'registerName:';
    function registerName_withNameServer(name: NSString; server: NSPortNameServer): Boolean; message 'registerName:withNameServer:';
    class function connectionWithReceivePort_sendPort(receivePort_: NSPort; sendPort_: NSPort): id; message 'connectionWithReceivePort:sendPort:';
    class function currentConversation: id; message 'currentConversation';
    function initWithReceivePort_sendPort(receivePort_: NSPort; sendPort_: NSPort): id; message 'initWithReceivePort:sendPort:';
    function sendPort: NSPort; message 'sendPort';
    function receivePort: NSPort; message 'receivePort';
    procedure enableMultipleThreads; message 'enableMultipleThreads';
    function multipleThreadsEnabled: Boolean; message 'multipleThreadsEnabled';
    procedure addRunLoop(runloop: NSRunLoop); message 'addRunLoop:';
    procedure removeRunLoop(runloop: NSRunLoop); message 'removeRunLoop:';
    procedure runInNewThread; message 'runInNewThread';
    function remoteObjects: NSArray; message 'remoteObjects';
    function localObjects: NSArray; message 'localObjects';
  end; external;

{ NSDistantObjectRequest }
  NSDistantObjectRequest = objcclass(NSObject)
    
  public
    class function alloc: NSDistantObjectRequest; message 'alloc';

    function invocation: NSInvocation; message 'invocation';
    function connection: NSConnection; message 'connection';
    function conversation: id; message 'conversation';
    procedure replyWithException(exception: NSException); message 'replyWithException:';
  end; external;

{$endif}
{$endif}
