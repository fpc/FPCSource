{ Parsed from Foundation.framework NSConnection.h }


{$ifdef TYPES}
type
  NSConnectionPtr = ^NSConnection;
  NSDistantObjectRequestPtr = ^NSDistantObjectRequest;
  NSConnectionDelegateProtocolPtr = ^NSConnectionDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSConnection = objcclass external (NSObject)
  private
    receivePort: id;
    sendPort: id;
    delegate: id;
    busy: cint32;
    localProxyCount: cint32;
    waitCount: cint32;
    delayedRL: id;
    statistics: id;
    isDead: char;
    isValid: char;
    wantsInvalid: char;
    _anonStruct_NSConnection0: record
      case byte of
        0: (anonBitField_NSConnection0: cuint);
        1: (data: bitpacked record
          authGen: 0..1;
          authCheck: 0..1;
          _reserved1: 0..1;
          _reserved2: 0..1;
          doRequest: 0..1;
          isQueueing: 0..1;
          isMulti: 0..1;
          invalidateRP: 0..1;
        end;
      );
    end;
    ___1: id;
    ___2: id;
    runLoops: id;
    requestModes: id;
    rootObject: id;
    registerInfo: pointer;
    replMode: id;
    classInfoImported: id;
    releasedProxies: id;
    reserved: id;
  public
    function statistics_: NSDictionary; message 'statistics';
    class function allConnections: NSArray; message 'allConnections';
    class function defaultConnection: NSConnection; message 'defaultConnection'; deprecated 'in 10_0, 10_6, NA, NA';
    class function connectionWithRegisteredName_host (name: NSString; hostName: NSString): instancetype; message 'connectionWithRegisteredName:host:';
    class function connectionWithRegisteredName_host_usingNameServer (name: NSString; hostName: NSString; server: NSPortNameServer): instancetype; message 'connectionWithRegisteredName:host:usingNameServer:';
    class function rootProxyForConnectionWithRegisteredName_host (name: NSString; hostName: NSString): NSDistantObject; message 'rootProxyForConnectionWithRegisteredName:host:';
    class function rootProxyForConnectionWithRegisteredName_host_usingNameServer (name: NSString; hostName: NSString; server: NSPortNameServer): NSDistantObject; message 'rootProxyForConnectionWithRegisteredName:host:usingNameServer:';
    class function serviceConnectionWithName_rootObject_usingNameServer (name: NSString; root: id; server: NSPortNameServer): instancetype; message 'serviceConnectionWithName:rootObject:usingNameServer:'; { available in 10_5, 2_0 }
    class function serviceConnectionWithName_rootObject (name: NSString; root: id): instancetype; message 'serviceConnectionWithName:rootObject:'; { available in 10_5, 2_0 }
    procedure setRequestTimeout(newValue: NSTimeInterval); message 'setRequestTimeout:';
    function requestTimeout: NSTimeInterval; message 'requestTimeout';
    procedure setReplyTimeout(newValue: NSTimeInterval); message 'setReplyTimeout:';
    function replyTimeout: NSTimeInterval; message 'replyTimeout';
    procedure setRootObject(newValue: id); message 'setRootObject:';
    function rootObject_: id; message 'rootObject';
    procedure setDelegate(newValue: NSConnectionDelegateProtocol); message 'setDelegate:';
    function delegate_: NSConnectionDelegateProtocol; message 'delegate';
    procedure setIndependentConversationQueueing(newValue: ObjCBOOL); message 'setIndependentConversationQueueing:';
    function independentConversationQueueing: ObjCBOOL; message 'independentConversationQueueing';
    function isValid_: ObjCBOOL; message 'isValid';
    function rootProxy: NSDistantObject; message 'rootProxy';
    procedure invalidate; message 'invalidate';
    procedure addRequestMode (rmode: NSString); message 'addRequestMode:';
    procedure removeRequestMode (rmode: NSString); message 'removeRequestMode:';
    function requestModes_: NSArray; message 'requestModes';
    function registerName (name: NSString): ObjCBOOL; message 'registerName:';
    function registerName_withNameServer (name: NSString; server: NSPortNameServer): ObjCBOOL; message 'registerName:withNameServer:';
    class function connectionWithReceivePort_sendPort (receivePort_: NSPort; sendPort_: NSPort): instancetype; message 'connectionWithReceivePort:sendPort:';
    class function currentConversation: id; message 'currentConversation';
    function initWithReceivePort_sendPort (receivePort_: NSPort; sendPort_: NSPort): instancetype; message 'initWithReceivePort:sendPort:';
    function sendPort_: NSPort; message 'sendPort';
    function receivePort_: NSPort; message 'receivePort';
    procedure enableMultipleThreads; message 'enableMultipleThreads';
    function multipleThreadsEnabled: ObjCBOOL; message 'multipleThreadsEnabled';
    procedure addRunLoop (runloop: NSRunLoop); message 'addRunLoop:';
    procedure removeRunLoop (runloop: NSRunLoop); message 'removeRunLoop:';
    procedure runInNewThread; message 'runInNewThread';
    function remoteObjects: NSArray; message 'remoteObjects';
    function localObjects: NSArray; message 'localObjects';
    procedure dispatchWithComponents (components: NSArray); message 'dispatchWithComponents:'; { available in 10_7, 5_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSConnectionReplyMode: NSString; cvar; external;
  NSConnectionDidDieNotification: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSConnectionDelegateProtocol = objcprotocol external name 'NSConnectionDelegate' (NSObjectProtocol)
  optional
    function makeNewConnection_sender (conn: NSConnection; ancestor: NSConnection): ObjCBOOL; message 'makeNewConnection:sender:';
    function connection_shouldMakeNewConnection (ancestor: NSConnection; conn: NSConnection): ObjCBOOL; message 'connection:shouldMakeNewConnection:';
    function authenticationDataForComponents (components: NSArray): NSData; message 'authenticationDataForComponents:';
    function authenticateComponents_withData (components: NSArray; signature: NSData): ObjCBOOL; message 'authenticateComponents:withData:';
    function createConversationForConnection (conn: NSConnection): id; message 'createConversationForConnection:';
    function connection_handleRequest (connection: NSConnection; doreq: NSDistantObjectRequest): ObjCBOOL; message 'connection:handleRequest:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFailedAuthenticationException: NSString; cvar; external;
  NSConnectionDidInitializeNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSDistantObjectRequest = objcclass external (NSObject)
  public
    function invocation: NSInvocation; message 'invocation';
    function connection: NSConnection; message 'connection';
    function conversation: id; message 'conversation';
    procedure replyWithException (exception: NSException); message 'replyWithException:';
  end;
{$endif}

