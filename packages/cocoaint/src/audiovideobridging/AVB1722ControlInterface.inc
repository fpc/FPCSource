{ Parsed from AudioVideoBridging.framework AVB1722ControlInterface.h }


{$ifdef TYPES}
type
  AVB1722ControlInterfacePtr = ^AVB1722ControlInterface;
{$endif}

{$ifdef CLASSES}

type
  AVB1722ControlInterface = objcclass external (NSObject)
  private
    callbackQueue: dispatch_queue_t;
    commandsQueue: dispatch_queue_t;
    transportQueue: dispatch_queue_t;
    connection: io_connect_t;
    notificationPort: IONotificationPortRef;
    receiveAddress: mach_vm_address_t;
    receiveSize: mach_vm_size_t;
    transmitAddress: mach_vm_address_t;
    transmitSize: mach_vm_size_t;
    transmitWriteHead: cuint32;
    notification: io_object_t;
    shouldRemove: ObjCBOOL;
    {$ifndef _LP64}
    interfaceName: NSString;
    interfacePrivate: AVBInterface;
    {$endif}
  public
    procedure setInterfaceName(newValue: NSString); message 'setInterfaceName:';
    function interfaceName_: NSString; message 'interfaceName';
    function interface_: AVBInterface; message 'interface';
    function initWithService_onInterface (aService: io_object_t; anInterface: AVBInterface): id; message 'initWithService:onInterface:'; deprecated 'in 10_8, 10_9, NA, NA';
    function initWithService_onInterfaceNamed (aService: io_object_t; anInterfaceName: NSString): id; message 'initWithService:onInterfaceNamed:'; deprecated 'in 10_8, 10_9, NA, NA';
    function initWithInterfaceName (anInterfaceName: NSString): instancetype; message 'initWithInterfaceName:'; { available in 10_9, NA }
  end;
{$endif}

