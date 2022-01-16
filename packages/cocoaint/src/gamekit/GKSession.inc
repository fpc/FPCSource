{ Parsed from GameKit.framework GKSession.h }


{$ifdef TYPES}
type
  GKSessionPtr = ^GKSession;
{$endif}

{$ifdef CLASSES}

type
  GKSession = objcclass external (NSObject)
  private
    _session: id;
  public
    function initWithSessionID_displayName_sessionMode (sessionID: NSString; name: NSString; mode: GKSessionMode): id; message 'initWithSessionID:displayName:sessionMode:'; deprecated 'in 10_8, 10_10, 3_0, 7_0';
    procedure setDelegate(newValue: GKSessionDelegateProtocol); message 'setDelegate:';
    function delegate: GKSessionDelegateProtocol; message 'delegate';
    function sessionID: NSString; message 'sessionID';
    function displayName: NSString; message 'displayName';
    function sessionMode: GKSessionMode; message 'sessionMode';
    function peerID: NSString; message 'peerID';
    procedure setAvailable(newValue: ObjCBOOL); message 'setAvailable:';
    function isAvailable: ObjCBOOL; message 'isAvailable';
    procedure setDisconnectTimeout(newValue: NSTimeInterval); message 'setDisconnectTimeout:';
    function disconnectTimeout: NSTimeInterval; message 'disconnectTimeout';
    function displayNameForPeer (peerID_: NSString): NSString; message 'displayNameForPeer:';
    function sendData_toPeers_withDataMode_error (data: NSData; peers: NSArray; mode: GKSendDataMode; error: NSErrorPtr): ObjCBOOL; message 'sendData:toPeers:withDataMode:error:'; deprecated 'in 10_8, 10_10, 3_0, 7_0';
    function sendDataToAllPeers_withDataMode_error (data: NSData; mode: GKSendDataMode; error: NSErrorPtr): ObjCBOOL; message 'sendDataToAllPeers:withDataMode:error:'; deprecated 'in 10_8, 10_10, 3_0, 7_0';
    procedure setDataReceiveHandler_withContext (handler: id; context: pointer); message 'setDataReceiveHandler:withContext:';
    procedure connectToPeer_withTimeout (peerID_: NSString; timeout: NSTimeInterval); message 'connectToPeer:withTimeout:';
    procedure cancelConnectToPeer (peerID_: NSString); message 'cancelConnectToPeer:';
    function acceptConnectionFromPeer_error (peerID_: NSString; error: NSErrorPtr): ObjCBOOL; message 'acceptConnectionFromPeer:error:';
    procedure denyConnectionFromPeer (peerID_: NSString); message 'denyConnectionFromPeer:';
    procedure disconnectPeerFromAllPeers (peerID_: NSString); message 'disconnectPeerFromAllPeers:';
    procedure disconnectFromAllPeers; message 'disconnectFromAllPeers';
    function peersWithConnectionState (state: GKPeerConnectionState): NSArray; message 'peersWithConnectionState:'; deprecated 'in 10_8, 10_10, 3_0, 7_0';
  end;
{$endif}

