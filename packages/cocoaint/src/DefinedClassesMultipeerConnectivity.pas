{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesMultipeerConnectivity;
interface

type
  MCAdvertiserAssistant = objcclass external;
  MCBrowserViewController = objcclass external;
  MCNearbyServiceAdvertiser = objcclass external;
  MCNearbyServiceBrowser = objcclass external;
  MCPeerID = objcclass external;
  MCSession = objcclass external;
  MCAdvertiserAssistantDelegateProtocol = objcprotocol external name 'MCAdvertiserAssistantDelegate';
  MCBrowserViewControllerDelegateProtocol = objcprotocol external name 'MCBrowserViewControllerDelegate';
  MCNearbyServiceAdvertiserDelegateProtocol = objcprotocol external name 'MCNearbyServiceAdvertiserDelegate';
  MCNearbyServiceBrowserDelegateProtocol = objcprotocol external name 'MCNearbyServiceBrowserDelegate';
  MCSessionDelegateProtocol = objcprotocol external name 'MCSessionDelegate';

type
  NSProgress = objcclass external;

implementation
end.
