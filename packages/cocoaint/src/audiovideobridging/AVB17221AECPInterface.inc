{ Parsed from AudioVideoBridging.framework AVB17221AECPInterface.h }


{$ifdef TYPES}
type
  AVB17221AECPInterfacePtr = ^AVB17221AECPInterface;
  AVB17221AECPClientProtocolPtr = ^AVB17221AECPClientProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  AVB17221AECPClientProtocol = objcprotocol external name 'AVB17221AECPClient'
  required
    function AECPDidReceiveCommand_onInterface (message_: AVB17221AECPMessage; anInterface: AVB17221AECPInterface): ObjCBOOL; message 'AECPDidReceiveCommand:onInterface:';
    function AECPDidReceiveResponse_onInterface (message_: AVB17221AECPMessage; anInterface: AVB17221AECPInterface): ObjCBOOL; message 'AECPDidReceiveResponse:onInterface:';
  end;
{$endif}

{$ifdef TYPES}
type
  AVB17221AECPInterfaceCompletion = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  AVB17221AECPInterface = objcclass external (AVB1722ControlInterface)
  private
    commandHandlers: NSMutableDictionary;
    nextSequenceID: cuint16;
    inflightControllerEntityID: cuint64;
    inflightMessageType: cuint8;
    inflightSequenceID: cuint16;
    lastReceivedSequenceID: cuint16;
    _monitorMode: ObjCBOOL;
    _monitorModeDelegate: AVB17221AECPClientProtocol;
    pendingAEMResponses: NSMutableArray;
    pendingAAResponses: NSMutableArray;
  public
    class function AECPInterfaceWithInterface (anInterface: AVBInterface): AVB17221AECPInterface; message 'AECPInterfaceWithInterface:';
    class function AECPInterfaceWithInterfaceNamed (anInterfaceName: NSString): AVB17221AECPInterface; message 'AECPInterfaceWithInterfaceNamed:';
    function setHandler_forGUID (handler: AVB17221AECPClientProtocol; targetGUID: cuint64): ObjCBOOL; message 'setHandler:forGUID:'; deprecated 'in 10_8, 10_9, NA, NA';
    function setHandler_forEntityID (handler: AVB17221AECPClientProtocol; targetEntityID: cuint64): ObjCBOOL; message 'setHandler:forEntityID:'; { available in 10_9, NA }
    procedure removeHandlerForGUID (targetGUID: cuint64); message 'removeHandlerForGUID:'; deprecated 'in 10_8, 10_9, NA, NA';
    procedure removeHandlerForEntityID (targetEntityID: cuint64); message 'removeHandlerForEntityID:'; { available in 10_9, NA }
    function sendCommand_toMACAddress_completionHandler (message_: AVB17221AECPMessage; destMAC: AVBMACAddress; completionHandler: AVB17221AECPInterfaceCompletion): ObjCBOOL; message 'sendCommand:toMACAddress:completionHandler:';
    function sendResponse_toMACAddress_error (message_: AVB17221AECPMessage; destMAC: AVBMACAddress; error: NSErrorPtr): ObjCBOOL; message 'sendResponse:toMACAddress:error:';
  end;
{$endif}

