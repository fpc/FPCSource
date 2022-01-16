{ Parsed from MultipeerConnectivity.framework MCNearbyServiceBrowser.h }


{$ifdef TYPES}
type
  MCNearbyServiceBrowserPtr = ^MCNearbyServiceBrowser;
  MCNearbyServiceBrowserDelegateProtocolPtr = ^MCNearbyServiceBrowserDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  MCNearbyServiceBrowser = objcclass external (NSObject)
  public
    function initWithPeer_serviceType (myPeerID: MCPeerID; serviceType: NSString): instancetype; message 'initWithPeer:serviceType:'; { NS_DESIGNATED_INITIALIZER }
    procedure startBrowsingForPeers; message 'startBrowsingForPeers';
    procedure stopBrowsingForPeers; message 'stopBrowsingForPeers';
    procedure invitePeer_toSession_withContext_timeout (peerID: MCPeerID; session: MCSession; context: NSData; timeout: NSTimeInterval); message 'invitePeer:toSession:withContext:timeout:';
    procedure setDelegate(newValue: MCNearbyServiceBrowserDelegateProtocol); message 'setDelegate:';
    function delegate: MCNearbyServiceBrowserDelegateProtocol; message 'delegate';
    function myPeerID: MCPeerID; message 'myPeerID';
    function serviceType: NSString; message 'serviceType';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  MCNearbyServiceBrowserDelegateProtocol = objcprotocol external name 'MCNearbyServiceBrowserDelegate' (NSObjectProtocol)
    procedure browser_foundPeer_withDiscoveryInfo (browser: MCNearbyServiceBrowser; peerID: MCPeerID; info: NSDictionary); message 'browser:foundPeer:withDiscoveryInfo:';
    procedure browser_lostPeer (browser: MCNearbyServiceBrowser; peerID: MCPeerID); message 'browser:lostPeer:';
  optional
    procedure browser_didNotStartBrowsingForPeers (browser: MCNearbyServiceBrowser; error: NSError); message 'browser:didNotStartBrowsingForPeers:';
  end;
{$endif}

