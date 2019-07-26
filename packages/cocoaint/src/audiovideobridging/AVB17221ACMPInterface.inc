{ Parsed from AudioVideoBridging.framework AVB17221ACMPInterface.h }


{$ifdef TYPES}
type
  AVB17221ACMPInterfacePtr = ^AVB17221ACMPInterface;
  AVB17221ACMPClientProtocolPtr = ^AVB17221ACMPClientProtocol;
{$endif}

{$ifdef TYPES}
type
  AVB17221ACMPInterfaceCompletion = OpaqueCBlock;
{$endif}

{$ifdef PROTOCOLS}

type
  AVB17221ACMPClientProtocol = objcprotocol external name 'AVB17221ACMPClient'
  required
    function ACMPDidReceiveCommand_onInterface (message_: AVB17221ACMPMessage; anInterface: AVB17221ACMPInterface): ObjCBOOL; message 'ACMPDidReceiveCommand:onInterface:';
    function ACMPDidReceiveResponse_onInterface (message_: AVB17221ACMPMessage; anInterface: AVB17221ACMPInterface): ObjCBOOL; message 'ACMPDidReceiveResponse:onInterface:';
  end;
{$endif}

{$ifdef CLASSES}

type
  AVB17221ACMPInterface = objcclass external (AVB1722ControlInterface)
  private
    commandHandlers: NSMutableDictionary;
    nextSequenceID: cuint16;
    multicastDestinationAddress: AVBMACAddress;
    _monitorMode: ObjCBOOL;
    _monitorModeDelegate: AVB17221ACMPClientProtocol;
    pendingResponses: NSMutableArray;
  public
    procedure setMulticastDestinationAddress(newValue: AVBMACAddress); message 'setMulticastDestinationAddress:';
    function multicastDestinationAddress_: AVBMACAddress; message 'multicastDestinationAddress';
    class function ACMPInterfaceWithInterface (anInterface: AVBInterface): AVB17221ACMPInterface; message 'ACMPInterfaceWithInterface:';
    class function ACMPInterfaceWithInterfaceNamed (anInterfaceName: NSString): AVB17221ACMPInterface; message 'ACMPInterfaceWithInterfaceNamed:';
    function setHandler_forGUID (handler: AVB17221ACMPClientProtocol; targetGUID: cuint64): ObjCBOOL; message 'setHandler:forGUID:'; deprecated 'in 10_8, 10_9, NA, NA';
    function setHandler_forEntityID (handler: AVB17221ACMPClientProtocol; targetEntityID: cuint64): ObjCBOOL; message 'setHandler:forEntityID:'; { available in 10_9, NA }
    procedure removeHandlerForGUID (targetGUID: cuint64); message 'removeHandlerForGUID:'; deprecated 'in 10_8, 10_9, NA, NA';
    procedure removeHandlerForEntityID (targetEntityID: cuint64); message 'removeHandlerForEntityID:'; { available in 10_9, NA }
    function sendACMPResponseMessage_error (message_: AVB17221ACMPMessage; error: NSErrorPtr): ObjCBOOL; message 'sendACMPResponseMessage:error:';
    function sendACMPCommandMessage_completionHandler (message_: AVB17221ACMPMessage; completionHandler: AVB17221ACMPInterfaceCompletion): ObjCBOOL; message 'sendACMPCommandMessage:completionHandler:';
  end;
{$endif}

