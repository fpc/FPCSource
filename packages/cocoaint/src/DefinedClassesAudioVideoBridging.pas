{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}

unit DefinedClassesAudioVideoBridging;
interface

type
  AVB17221ACMPInterface = objcclass external;
  AVB17221ACMPMessage = objcclass external;
  AVB17221AECPAEMMessage = objcclass external;
  AVB17221AECPAVCMessage = objcclass external;
  AVB17221AECPAddressAccessMessage = objcclass external;
  AVB17221AECPAddressAccessTLV = objcclass external;
  AVB17221AECPInterface = objcclass external;
  AVB17221AECPMessage = objcclass external;
  AVB17221AECPVendorMessage = objcclass external;
  AVB17221Entity = objcclass external;
  AVB17221EntityDiscovery = objcclass external;
  AVB1722ControlInterface = objcclass external;
  AVBCentralManager = objcclass external;
  AVBEthernetInterface = objcclass external;
  AVBInterface = objcclass external;
  AVBMACAddress = objcclass external;
  AVB17221ACMPClientProtocol = objcprotocol external name 'AVB17221ACMPClient';
  AVB17221AECPClientProtocol = objcprotocol external name 'AVB17221AECPClient';
  AVB17221EntityDiscoveryDelegateProtocol = objcprotocol external name 'AVB17221EntityDiscoveryDelegate';

type
  AVB1722MAAP = objcclass external;
  AVB8021ASTimeSync = objcclass external;
  AVBMSRPDomain = objcclass external;
  AVBMSRPListener = objcclass external;
  AVBMSRPTalker = objcclass external;
  AVBMVRP = objcclass external;
  AVBNetworkClient = objcclass external;
  AVB17221EntityPublisherProtocol = objcprotocol external name 'AVB17221EntityPublisher';
  AVBInterfaceDelegateProtocol = objcprotocol external name 'AVBInterfaceDelegate';

implementation
end.
