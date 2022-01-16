{ Parsed from AudioVideoBridging.framework AVB17221ACMPMessage.h }


{$ifdef TYPES}
type
  AVB17221ACMPMessagePtr = ^AVB17221ACMPMessage;
{$endif}

{$ifdef CLASSES}

type
  AVB17221ACMPMessage = objcclass external (NSObject, NSCopyingProtocol)
  private
    messageType: AVB17221ACMPMessageType;
    status: AVB17221ACMPStatusCode;
    streamID: cuint64;
    controllerEntityID: cuint64;
    talkerEntityID: cuint64;
    listenerEntityID: cuint64;
    talkerUniqueID: cuint16;
    listenerUniqueID: cuint16;
    connectionCount: cuint16;
    sequenceID: cuint16;
    flags: AVB17221ACMPFlags;
    vlanID: cuint16;
    sourceMAC: AVBMACAddress;
    destinationMAC: AVBMACAddress;
  public
    procedure setMessageType(newValue: AVB17221ACMPMessageType); message 'setMessageType:';
    function messageType_: AVB17221ACMPMessageType; message 'messageType';
    procedure setStatus(newValue: AVB17221ACMPStatusCode); message 'setStatus:';
    function status_: AVB17221ACMPStatusCode; message 'status';
    procedure setStreamID(newValue: cuint64); message 'setStreamID:';
    function streamID_: cuint64; message 'streamID';
    procedure setControllerGUID(newValue: cuint64); message 'setControllerGUID:';
    function controllerGUID: cuint64; message 'controllerGUID';
    procedure setControllerEntityID(newValue: cuint64); message 'setControllerEntityID:';
    function controllerEntityID_: cuint64; message 'controllerEntityID';
    procedure setTalkerGUID(newValue: cuint64); message 'setTalkerGUID:';
    function talkerGUID: cuint64; message 'talkerGUID';
    procedure setTalkerEntityID(newValue: cuint64); message 'setTalkerEntityID:';
    function talkerEntityID_: cuint64; message 'talkerEntityID';
    procedure setListenerGUID(newValue: cuint64); message 'setListenerGUID:';
    function listenerGUID: cuint64; message 'listenerGUID';
    procedure setListenerEntityID(newValue: cuint64); message 'setListenerEntityID:';
    function listenerEntityID_: cuint64; message 'listenerEntityID';
    procedure setTalkerUniqueID(newValue: cuint16); message 'setTalkerUniqueID:';
    function talkerUniqueID_: cuint16; message 'talkerUniqueID';
    procedure setListenerUniqueID(newValue: cuint16); message 'setListenerUniqueID:';
    function listenerUniqueID_: cuint16; message 'listenerUniqueID';
    procedure setDestinationMAC(newValue: AVBMACAddress); message 'setDestinationMAC:';
    function destinationMAC_: AVBMACAddress; message 'destinationMAC';
    procedure setConnectionCount(newValue: cuint16); message 'setConnectionCount:';
    function connectionCount_: cuint16; message 'connectionCount';
    procedure setSequenceID(newValue: cuint16); message 'setSequenceID:';
    function sequenceID_: cuint16; message 'sequenceID';
    procedure setFlags(newValue: AVB17221ACMPFlags); message 'setFlags:';
    function flags_: AVB17221ACMPFlags; message 'flags';
    procedure setVlanID(newValue: cuint16); message 'setVlanID:';
    function vlanID_: cuint16; message 'vlanID';
    procedure setSourceMAC(newValue: AVBMACAddress); message 'setSourceMAC:';
    function sourceMAC_: AVBMACAddress; message 'sourceMAC';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

