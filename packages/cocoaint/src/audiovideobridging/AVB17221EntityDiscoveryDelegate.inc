{ Parsed from AudioVideoBridging.framework AVB17221EntityDiscoveryDelegate.h }


{$ifdef TYPES}
type
  AVB17221EntityDiscoveryDelegateProtocolPtr = ^AVB17221EntityDiscoveryDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  AVB17221EntityPropertyChanged = NSUInteger;
  AVB17221EntityPropertyChangedPtr = ^AVB17221EntityPropertyChanged;

const
  AVB17221EntityPropertyChangedTimeToLive = $00000001;
  AVB17221EntityPropertyChangedGUID = $00000002 deprecated 'in 10_8, 10_9, NA, NA';
  AVB17221EntityPropertyChangedEntityID = $00000002 { available in 10_9, NA };
  AVB17221EntityPropertyChangedVendorID = $00000004;
  AVB17221EntityPropertyChangedModelID = $00000008;
  AVB17221EntityPropertyChangedEntityCapabilities = $00000010;
  AVB17221EntityPropertyChangedTalkerStreamSources = $00000020;
  AVB17221EntityPropertyChangedTalkerCapabilities = $00000040;
  AVB17221EntityPropertyChangedListenerStreamSinks = $00000080;
  AVB17221EntityPropertyChangedListenerCapabilities = $00000100;
  AVB17221EntityPropertyChangedControllerCapabilities = $00000200;
  AVB17221EntityPropertyChangedAvailableIndex = $00000400;
  AVB17221EntityPropertyChangedASGrandmasterID = $00000800 deprecated 'in 10_8, 10_9, NA, NA';
  AVB17221EntityPropertyChangedGPTPGrandmasterID = $00000800 { available in 10_9, NA };
  AVB17221EntityPropertyChangedMACAddress = $00001000;
  AVB17221EntityPropertyChangedAssociationID = $00008000;
  AVB17221EntityPropertyChangedEntityType = $00010000;
  AVB17221EntityPropertyChangedIdentifyControlIndex = $00020000 { available in 10_9, NA };
  AVB17221EntityPropertyChangedInterfaceIndex = $00040000 { available in 10_9, NA };
  AVB17221EntityPropertyChangedGPTPDomainNumber = $00080000 { available in 10_9, NA };
{$endif}

{$ifdef PROTOCOLS}

type
  AVB17221EntityDiscoveryDelegateProtocol = objcprotocol external name 'AVB17221EntityDiscoveryDelegate'
  required
    procedure didAddRemoteEntity_on17221EntityDiscovery (newEntity: AVB17221Entity; entityDiscovery: AVB17221EntityDiscovery); message 'didAddRemoteEntity:on17221EntityDiscovery:';
    procedure didRemoveRemoteEntity_on17221EntityDiscovery (oldEntity: AVB17221Entity; entityDiscovery: AVB17221EntityDiscovery); message 'didRemoveRemoteEntity:on17221EntityDiscovery:';
    procedure didRediscoverRemoteEntity_on17221EntityDiscovery (entity: AVB17221Entity; entityDiscovery: AVB17221EntityDiscovery); message 'didRediscoverRemoteEntity:on17221EntityDiscovery:';
    procedure didUpdateRemoteEntity_changedProperties_on17221EntityDiscovery (entity: AVB17221Entity; changedProperties: AVB17221EntityPropertyChanged; entityDiscovery: AVB17221EntityDiscovery); message 'didUpdateRemoteEntity:changedProperties:on17221EntityDiscovery:';
    procedure didAddLocalEntity_on17221EntityDiscovery (newEntity: AVB17221Entity; entityDiscovery: AVB17221EntityDiscovery); message 'didAddLocalEntity:on17221EntityDiscovery:';
    procedure didRemoveLocalEntity_on17221EntityDiscovery (oldEntity: AVB17221Entity; entityDiscovery: AVB17221EntityDiscovery); message 'didRemoveLocalEntity:on17221EntityDiscovery:';
    procedure didRediscoverLocalEntity_on17221EntityDiscovery (entity: AVB17221Entity; entityDiscovery: AVB17221EntityDiscovery); message 'didRediscoverLocalEntity:on17221EntityDiscovery:';
    procedure didUpdateLocalEntity_changedProperties_on17221EntityDiscovery (entity: AVB17221Entity; changedProperties: AVB17221EntityPropertyChanged; entityDiscovery: AVB17221EntityDiscovery); message 'didUpdateLocalEntity:changedProperties:on17221EntityDiscovery:';
  end;
{$endif}

