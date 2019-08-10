{ Parsed from MultipeerConnectivity.framework MCSession.h }


{$ifdef TYPES}
type
  MCSessionPtr = ^MCSession;
  MCSessionDelegateProtocolPtr = ^MCSessionDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  MCSessionSendDataMode = NSInteger;
  MCSessionSendDataModePtr = ^MCSessionSendDataMode;

const
  MCSessionSendDataReliable = 0;
  MCSessionSendDataUnreliable = 1;

type
  MCSessionState = NSInteger;
  MCSessionStatePtr = ^MCSessionState;

const
  MCSessionStateNotConnected = 0;
  MCSessionStateConnecting = 1;
  MCSessionStateConnected = 2;

type
  MCEncryptionPreference = NSInteger;
  MCEncryptionPreferencePtr = ^MCEncryptionPreference;

const
  MCEncryptionOptional = 0;
  MCEncryptionRequired = 1;
  MCEncryptionNone = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kMCSessionMinimumNumberOfPeers: NSUInteger { available in 10_10,7_0 }; cvar; external;
  kMCSessionMaximumNumberOfPeers: NSUInteger { available in 10_10,7_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  MCSession = objcclass external (NSObject)
  public
    function initWithPeer (myPeerID: MCPeerID): instancetype; message 'initWithPeer:';
    function initWithPeer_securityIdentity_encryptionPreference (myPeerID: MCPeerID; identity: NSArray; encryptionPreference: MCEncryptionPreference): instancetype; message 'initWithPeer:securityIdentity:encryptionPreference:'; { NS_DESIGNATED_INITIALIZER }
    function sendData_toPeers_withMode_error (data: NSData; peerIDs: NSArray; mode: MCSessionSendDataMode; error: NSErrorPtr): ObjCBOOL; message 'sendData:toPeers:withMode:error:';
    procedure disconnect; message 'disconnect';
    function sendResourceAtURL_withName_toPeer_withCompletionHandler (resourceURL: NSURL; resourceName: NSString; peerID: MCPeerID; completionHandler: OpaqueCBlock): NSProgress; message 'sendResourceAtURL:withName:toPeer:withCompletionHandler:';
    function startStreamWithName_toPeer_error (streamName: NSString; peerID: MCPeerID; error: NSErrorPtr): NSOutputStream; message 'startStreamWithName:toPeer:error:';
    procedure setDelegate(newValue: MCSessionDelegateProtocol); message 'setDelegate:';
    function delegate: MCSessionDelegateProtocol; message 'delegate';
    function myPeerID: MCPeerID; message 'myPeerID';
    function securityIdentity: NSArray; message 'securityIdentity';
    function encryptionPreference: MCEncryptionPreference; message 'encryptionPreference';
    function connectedPeers: NSArray; message 'connectedPeers';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  MCSessionDelegateProtocol = objcprotocol external name 'MCSessionDelegate' (NSObjectProtocol)
    procedure session_peer_didChangeState (session: MCSession; peerID: MCPeerID; state: MCSessionState); message 'session:peer:didChangeState:';
    procedure session_didReceiveData_fromPeer (session: MCSession; data: NSData; peerID: MCPeerID); message 'session:didReceiveData:fromPeer:';
    procedure session_didReceiveStream_withName_fromPeer (session: MCSession; stream: NSInputStream; streamName: NSString; peerID: MCPeerID); message 'session:didReceiveStream:withName:fromPeer:';
    procedure session_didStartReceivingResourceWithName_fromPeer_withProgress (session: MCSession; resourceName: NSString; peerID: MCPeerID; progress: NSProgress); message 'session:didStartReceivingResourceWithName:fromPeer:withProgress:';
    procedure session_didFinishReceivingResourceWithName_fromPeer_atURL_withError (session: MCSession; resourceName: NSString; peerID: MCPeerID; localURL: NSURL; error: NSError); message 'session:didFinishReceivingResourceWithName:fromPeer:atURL:withError:';
  optional
    procedure session_didReceiveCertificate_fromPeer_certificateHandler (session: MCSession; certificate: NSArray; peerID: MCPeerID; certificateHandler: OpaqueCBlock); message 'session:didReceiveCertificate:fromPeer:certificateHandler:';
  end;
{$endif}

{$ifdef CLASSES}

type
  MCSessionCustomDiscovery = objccategory external (MCSession)
    procedure nearbyConnectionDataForPeer_withCompletionHandler (peerID: MCPeerID; completionHandler: OpaqueCBlock); message 'nearbyConnectionDataForPeer:withCompletionHandler:';
    procedure connectPeer_withNearbyConnectionData (peerID: MCPeerID; data: NSData); message 'connectPeer:withNearbyConnectionData:';
    procedure cancelConnectPeer (peerID: MCPeerID); message 'cancelConnectPeer:';
  end;
{$endif}

