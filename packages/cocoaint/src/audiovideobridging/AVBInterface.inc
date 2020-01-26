{ Parsed from AudioVideoBridging.framework AVBInterface.h }


{$ifdef TYPES}
type
  AVBInterfacePtr = ^AVBInterface;
{$endif}

{$ifdef CLASSES}

type
  AVBInterface = objcclass external (NSObject)
  private
    _networkClient: AVBNetworkClient;
    _interfaceName: NSString;
    _mvrp: AVBMVRP;
    _msrpDomain: AVBMSRPDomain;
    _msrpListener: AVBMSRPListener;
    _msrpTalker: AVBMSRPTalker;
    _entityDiscovery: AVB17221EntityDiscovery;
    _timeSync: AVB8021ASTimeSync;
    _aecp: AVB17221AECPInterface;
    _acmp: AVB17221ACMPInterface;
    _maap: AVB1722MAAP;
    _macAddress: AVBMACAddress;
    _interfaceDelegate: AVBInterfaceDelegateProtocol;
    _networkClientCreationQueue: dispatch_queue_t;
    _mvrpCreationQueue: dispatch_queue_t;
    _msrpDomainCreationQueue: dispatch_queue_t;
    _msrpListenerCreationQueue: dispatch_queue_t;
    _msrpTalkerCreationQueue: dispatch_queue_t;
    _entityDiscoveryCreationQueue: dispatch_queue_t;
    _timeSyncCreationQueue: dispatch_queue_t;
    _aecpCreationQueue: dispatch_queue_t;
    _acmpCreationQueue: dispatch_queue_t;
    _maapCreationQueue: dispatch_queue_t;
    _notificationQueue: dispatch_queue_t;
    _notificationPort: IONotificationPortRef;
    _networkClientIterator: io_iterator_t;
    _mvrpIterator: io_iterator_t;
    _msrpDomainIterator: io_iterator_t;
    _msrpListenerIterator: io_iterator_t;
    _msrpTalkerIterator: io_iterator_t;
    _entityDiscoveryIterator: io_iterator_t;
    _timeSyncIterator: io_iterator_t;
    _aecpIterator: io_iterator_t;
    _acmpIterator: io_iterator_t;
    _maapIterator: io_iterator_t;
    _virtualEntities: NSMutableArray;
  public
    function interfaceName: NSString; message 'interfaceName';
    function entityDiscovery: AVB17221EntityDiscovery; message 'entityDiscovery';
    function aecp: AVB17221AECPInterface; message 'aecp';
    function acmp: AVB17221ACMPInterface; message 'acmp';
    class function macAddressForInterfaceNamed (anInterfaceName: NSString): AVBMACAddress; message 'macAddressForInterfaceNamed:';
    class function supportedInterfaces: NSArray; message 'supportedInterfaces';
    class function isAVBEnabledOnInterfaceNamed (anInterfaceName: NSString): ObjCBOOL; message 'isAVBEnabledOnInterfaceNamed:';
    class function isAVBCapableInterfaceNamed (anInterfaceName: NSString): ObjCBOOL; message 'isAVBCapableInterfaceNamed:';
    function initWithInterfaceName (anInterfaceName: NSString): instancetype; message 'initWithInterfaceName:';
    class function myGUID: cuint64; message 'myGUID';
    class function myEntityID: cuint64; message 'myEntityID';
  end;
{$endif}

