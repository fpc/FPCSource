{ Parsed from AudioVideoBridging.framework AVB17221EntityDiscovery.h }


{$ifdef TYPES}
type
  AVB17221EntityDiscoveryPtr = ^AVB17221EntityDiscovery;
{$endif}

{$ifdef CLASSES}

type
  AVB17221EntityDiscovery = objcclass external (NSObject)
  private
    connection: io_connect_t;
    {$ifndef _LP64}
    interfaceName: NSString;
    interfacePrivate: AVBInterface;
    discoveryDelegate: AVB17221EntityDiscoveryDelegateProtocol;
    {$endif}
    notificationPort: IONotificationPortRef;
    callbackQueue: dispatch_queue_t;
    localArrivalIterator: io_iterator_t;
    localDepartureIterator: io_iterator_t;
    remoteArrivalIterator: io_iterator_t;
    remoteDepartureIterator: io_iterator_t;
    localEntities: NSMutableDictionary;
    remoteEntities: NSMutableDictionary;
    notification: io_object_t;
  public
    procedure setInterfaceName(newValue: NSString); message 'setInterfaceName:';
    function interfaceName_: NSString; message 'interfaceName';
    function interface_: AVBInterface; message 'interface';
    procedure setDiscoveryDelegate(newValue: AVB17221EntityDiscoveryDelegateProtocol); message 'setDiscoveryDelegate:';
    function discoveryDelegate_: AVB17221EntityDiscoveryDelegateProtocol; message 'discoveryDelegate';
    function initWithInterfaceName (anInterfaceName: NSString): instancetype; message 'initWithInterfaceName:';
    procedure primeIterators; message 'primeIterators';
    function discoverEntities: ObjCBOOL; message 'discoverEntities';
    function discoverEntity (entityID: cuint64): ObjCBOOL; message 'discoverEntity:';
    function addLocalEntity_error (anEntity: AVB17221Entity; error: NSErrorPtr): ObjCBOOL; message 'addLocalEntity:error:';
    function removeLocalEntity_error (guid: cuint64; error: NSErrorPtr): ObjCBOOL; message 'removeLocalEntity:error:';
    function changeEntityWithGUID_toNewASGrandmasterID_error (entityGUID: cuint64; asGrandmasterID: cuint64; error: NSErrorPtr): ObjCBOOL; message 'changeEntityWithGUID:toNewASGrandmasterID:error:'; deprecated 'in 10_8, 10_9, NA, NA';
    function changeEntityWithEntityID_toNewGPTPGrandmasterID_error (entityID: cuint64; gPTPGrandmasterID: cuint64; error: NSErrorPtr): ObjCBOOL; message 'changeEntityWithEntityID:toNewGPTPGrandmasterID:error:'; { available in 10_9, NA }
  end;
{$endif}

