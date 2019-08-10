{ Parsed from AudioVideoBridging.framework AVB17221AECPMessage.h }


{$ifdef TYPES}
type
  AVB17221AECPMessagePtr = ^AVB17221AECPMessage;
  AVB17221AECPAEMMessagePtr = ^AVB17221AECPAEMMessage;
  AVB17221AECPAddressAccessMessagePtr = ^AVB17221AECPAddressAccessMessage;
  AVB17221AECPAddressAccessTLVPtr = ^AVB17221AECPAddressAccessTLV;
  AVB17221AECPAVCMessagePtr = ^AVB17221AECPAVCMessage;
  AVB17221AECPVendorMessagePtr = ^AVB17221AECPVendorMessage;
{$endif}

{$ifdef CLASSES}

type
  AVB17221AECPMessage = objcclass external (NSObject, NSCopyingProtocol)
  private
    messageType: AVB17221AECPMessageType;
    status: AVB17221AECPStatusCode;
    targetEntityID: cuint64;
    controllerEntityID: cuint64;
    sequenceID: cuint16;
    sourceMAC: AVBMACAddress;
  public
    procedure setMessageType(newValue: AVB17221AECPMessageType); message 'setMessageType:';
    function messageType_: AVB17221AECPMessageType; message 'messageType';
    procedure setStatus(newValue: AVB17221AECPStatusCode); message 'setStatus:';
    function status_: AVB17221AECPStatusCode; message 'status';
    procedure setTargetGUID(newValue: cuint64); message 'setTargetGUID:';
    function targetGUID: cuint64; message 'targetGUID';
    procedure setTargetEntityID(newValue: cuint64); message 'setTargetEntityID:';
    function targetEntityID_: cuint64; message 'targetEntityID';
    procedure setControllerGUID(newValue: cuint64); message 'setControllerGUID:';
    function controllerGUID: cuint64; message 'controllerGUID';
    procedure setControllerEntityID(newValue: cuint64); message 'setControllerEntityID:';
    function controllerEntityID_: cuint64; message 'controllerEntityID';
    procedure setSequenceID(newValue: cuint16); message 'setSequenceID:';
    function sequenceID_: cuint16; message 'sequenceID';
    procedure setSourceMAC(newValue: AVBMACAddress); message 'setSourceMAC:';
    function sourceMAC_: AVBMACAddress; message 'sourceMAC';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;

type
  AVB17221AECPAEMMessage = objcclass external (AVB17221AECPMessage)
  private
    rawCommandType: cuint16;
    {$ifndef _LP64}
    commandSpecificData: NSData;
    {$endif}
  public
    procedure setCommandType(newValue: AVB17221AEMCommandType); message 'setCommandType:';
    function commandType: AVB17221AEMCommandType; message 'commandType';
    procedure setUnsolicited(newValue: ObjCBOOL); message 'setUnsolicited:';
    function isUnsolicited: ObjCBOOL; message 'isUnsolicited';
    procedure setControllerRequest(newValue: ObjCBOOL); message 'setControllerRequest:';
    function isControllerRequest: ObjCBOOL; message 'isControllerRequest';
    procedure setCommandSpecificData(newValue: NSData); message 'setCommandSpecificData:';
    function commandSpecificData_: NSData; message 'commandSpecificData';
    class function commandMessage: AVB17221AECPAEMMessage; message 'commandMessage';
    class function responseMessage: AVB17221AECPAEMMessage; message 'responseMessage';
  end;

type
  AVB17221AECPAddressAccessMessage = objcclass external (AVB17221AECPMessage)
  private
    tlvs: NSArray;
  public
    procedure setTlvs(newValue: NSArray); message 'setTlvs:';
    function tlvs_: NSArray; message 'tlvs';
    class function commandMessage: AVB17221AECPAddressAccessMessage; message 'commandMessage';
    class function responseMessage: AVB17221AECPAddressAccessMessage; message 'responseMessage';
  end;

type
  AVB17221AECPAddressAccessTLV = objcclass external (NSObject)
  private
    mode: AVB17221AECPAddressAccessTLVMode;
    address: cuint64;
    memoryData: NSData;
  public
    procedure setMode(newValue: AVB17221AECPAddressAccessTLVMode); message 'setMode:';
    function mode_: AVB17221AECPAddressAccessTLVMode; message 'mode';
    procedure setAddress(newValue: cuint64); message 'setAddress:';
    function address_: cuint64; message 'address';
    procedure setMemoryData(newValue: NSData); message 'setMemoryData:';
    function memoryData_: NSData; message 'memoryData';
  end;

type
  AVB17221AECPAVCMessage = objcclass external (AVB17221AECPMessage)
  private
    commandResponse: NSData;
  public
    procedure setCommandResponse(newValue: NSData); message 'setCommandResponse:';
    function commandResponse_: NSData; message 'commandResponse';
  end;

type
  AVB17221AECPVendorMessage = objcclass external (AVB17221AECPMessage)
  private
    protocolID: cuint64;
    protocolSpecificData: NSData;
  public
    procedure setProtocolID(newValue: cuint64); message 'setProtocolID:';
    function protocolID_: cuint64; message 'protocolID';
    procedure setProtocolSpecificData(newValue: NSData); message 'setProtocolSpecificData:';
    function protocolSpecificData_: NSData; message 'protocolSpecificData';
  end;
{$endif}

