{ Parsed from MultipeerConnectivity.framework MCNearbyServiceAdvertiser.h }


{$ifdef TYPES}
type
  MCNearbyServiceAdvertiserPtr = ^MCNearbyServiceAdvertiser;
  MCNearbyServiceAdvertiserDelegateProtocolPtr = ^MCNearbyServiceAdvertiserDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  MCNearbyServiceAdvertiser = objcclass external (NSObject)
  public
    function initWithPeer_discoveryInfo_serviceType (myPeerID: MCPeerID; info: NSDictionary; serviceType: NSString): instancetype; message 'initWithPeer:discoveryInfo:serviceType:'; { NS_DESIGNATED_INITIALIZER }
    procedure startAdvertisingPeer; message 'startAdvertisingPeer';
    procedure stopAdvertisingPeer; message 'stopAdvertisingPeer';
    procedure setDelegate(newValue: MCNearbyServiceAdvertiserDelegateProtocol); message 'setDelegate:';
    function delegate: MCNearbyServiceAdvertiserDelegateProtocol; message 'delegate';
    function myPeerID: MCPeerID; message 'myPeerID';
    function discoveryInfo: NSDictionary; message 'discoveryInfo';
    function serviceType: NSString; message 'serviceType';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  MCNearbyServiceAdvertiserDelegateProtocol = objcprotocol external name 'MCNearbyServiceAdvertiserDelegate' (NSObjectProtocol)
    procedure advertiser_didReceiveInvitationFromPeer_withContext_invitationHandler (advertiser: MCNearbyServiceAdvertiser; peerID: MCPeerID; context: NSData; invitationHandler: OpaqueCBlock); message 'advertiser:didReceiveInvitationFromPeer:withContext:invitationHandler:';
  optional
    procedure advertiser_didNotStartAdvertisingPeer (advertiser: MCNearbyServiceAdvertiser; error: NSError); message 'advertiser:didNotStartAdvertisingPeer:';
  end;
{$endif}

