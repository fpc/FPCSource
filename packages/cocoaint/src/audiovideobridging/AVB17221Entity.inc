{ Parsed from AudioVideoBridging.framework AVB17221Entity.h }


{$ifdef TYPES}
type
  AVB17221EntityPtr = ^AVB17221Entity;
{$endif}

{$ifdef CLASSES}

type
  AVB17221Entity = objcclass external (NSObject)
  private
    {$ifndef _LP64}
    localEntity: ObjCBOOL;
    timeToLive: cuint8;
    entityID: cuint64;
    entityModelID: cuint64;
    entityCapabilities: AVB17221ADPEntityCapabilities;
    talkerStreamSources: cuint16;
    talkerCapabilities: AVB17221ADPTalkerCapabilities;
    listenerStreamSinks: cuint16;
    listenerCapabilities: AVB17221ADPListenerCapabilities;
    controllerCapabilities: AVB17221ADPControllerCapabilities;
    availableIndex: cuint32;
    gPTPGrandmasterID: cuint64;
    associationID: cuint64;
    macAddresses: NSArray;
    entityDiscovery: AVB17221EntityDiscovery;
    entityPublisher: AVB17221EntityPublisherProtocol;
    lastAvailableIndex: cuint32;
    identifyControlIndex: cuint16;
    gPTPDomainNumber: cuint8;
    interfaceIndex: cuint16;
    {$endif}
    service: io_service_t;
    interestNotficiation: io_object_t;
    notificationPort: IONotificationPortRef;
  public
    procedure setLocalEntity(newValue: ObjCBOOL); message 'setLocalEntity:';
    function isLocalEntity: ObjCBOOL; message 'isLocalEntity';
    procedure setTimeToLive(newValue: cuint8); message 'setTimeToLive:';
    function timeToLive_: cuint8; message 'timeToLive';
    procedure setGuid(newValue: cuint64); message 'setGuid:';
    function guid: cuint64; message 'guid';
    procedure setEntityID(newValue: cuint64); message 'setEntityID:';
    function entityID_: cuint64; message 'entityID';
    procedure setVendorID(newValue: cuint32); message 'setVendorID:';
    function vendorID: cuint32; message 'vendorID';
    procedure setModelID(newValue: cuint32); message 'setModelID:';
    function modelID: cuint32; message 'modelID';
    procedure setEntityModelID(newValue: cuint64); message 'setEntityModelID:';
    function entityModelID_: cuint64; message 'entityModelID';
    procedure setEntityCapabilities(newValue: AVB17221ADPEntityCapabilities); message 'setEntityCapabilities:';
    function entityCapabilities_: AVB17221ADPEntityCapabilities; message 'entityCapabilities';
    procedure setTalkerStreamSources(newValue: cuint16); message 'setTalkerStreamSources:';
    function talkerStreamSources_: cuint16; message 'talkerStreamSources';
    procedure setTalkerCapabilities(newValue: AVB17221ADPTalkerCapabilities); message 'setTalkerCapabilities:';
    function talkerCapabilities_: AVB17221ADPTalkerCapabilities; message 'talkerCapabilities';
    procedure setListenerStreamSinks(newValue: cuint16); message 'setListenerStreamSinks:';
    function listenerStreamSinks_: cuint16; message 'listenerStreamSinks';
    procedure setListenerCapabilities(newValue: AVB17221ADPListenerCapabilities); message 'setListenerCapabilities:';
    function listenerCapabilities_: AVB17221ADPListenerCapabilities; message 'listenerCapabilities';
    procedure setControllerCapabilities(newValue: AVB17221ADPControllerCapabilities); message 'setControllerCapabilities:';
    function controllerCapabilities_: AVB17221ADPControllerCapabilities; message 'controllerCapabilities';
    procedure setAvailableIndex(newValue: cuint32); message 'setAvailableIndex:';
    function availableIndex_: cuint32; message 'availableIndex';
    procedure setAsGrandmasterID(newValue: cuint64); message 'setAsGrandmasterID:';
    function asGrandmasterID: cuint64; message 'asGrandmasterID';
    procedure setGPTPGrandmasterID(newValue: cuint64); message 'setGPTPGrandmasterID:';
    function gPTPGrandmasterID_: cuint64; message 'gPTPGrandmasterID';
    procedure setGPTPDomainNumber(newValue: cuint8); message 'setGPTPDomainNumber:';
    function gPTPDomainNumber_: cuint8; message 'gPTPDomainNumber';
    procedure setIdentifyControlIndex(newValue: cuint16); message 'setIdentifyControlIndex:';
    function identifyControlIndex_: cuint16; message 'identifyControlIndex';
    procedure setInterfaceIndex(newValue: cuint16); message 'setInterfaceIndex:';
    function interfaceIndex_: cuint16; message 'interfaceIndex';
    procedure setAssociationID(newValue: cuint64); message 'setAssociationID:';
    function associationID_: cuint64; message 'associationID';
    procedure setMacAddresses(newValue: NSArray); message 'setMacAddresses:';
    function macAddresses_: NSArray; message 'macAddresses';
    procedure setEntityDiscovery(newValue: AVB17221EntityDiscovery); message 'setEntityDiscovery:';
    function entityDiscovery_: AVB17221EntityDiscovery; message 'entityDiscovery';
  end;
{$endif}

