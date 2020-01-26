{ Parsed from CryptoTokenKit.framework TKSmartCard.h }


{$ifdef TYPES}
type
  TKSmartCardSlotManagerPtr = ^TKSmartCardSlotManager;
  TKSmartCardSlotPtr = ^TKSmartCardSlot;
  TKSmartCardPtr = ^TKSmartCard;
{$endif}

{$ifdef CLASSES}

type
  TKSmartCardSlotManager = objcclass external (NSObject)
  public
    class function defaultManager: instancetype; message 'defaultManager';
    function slotNames: NSArray; message 'slotNames';
    procedure getSlotWithName_reply (name: NSString; reply: OpaqueCBlock); message 'getSlotWithName:reply:';
  end;
{$endif}

{$ifdef TYPES}
type
  TKSmartCardSlotState = NSInteger;
  TKSmartCardSlotStatePtr = ^TKSmartCardSlotState;

const
  TKSmartCardSlotStateMissing = 0;
  TKSmartCardSlotStateEmpty = 1;
  TKSmartCardSlotStateProbing = 2;
  TKSmartCardSlotStateMuteCard = 3;
  TKSmartCardSlotStateValidCard = 4;
  TKSmartCardNoSlot = TKSmartCardSlotStateMissing deprecated 'in 10_10, 10_10';
  TKSmartCardSlotEmpty = TKSmartCardSlotStateEmpty deprecated 'in 10_10, 10_10';
  TKSmartCardSlotProbing = TKSmartCardSlotStateProbing deprecated 'in 10_10, 10_10';
  TKSmartCardSlotMuteCard = TKSmartCardSlotStateMuteCard deprecated 'in 10_10, 10_10';
  TKSmartCardSlotValidCard = TKSmartCardSlotStateValidCard deprecated 'in 10_10, 10_10';
{$endif}

{$ifdef CLASSES}

type
  TKSmartCardSlot = objcclass external (NSObject)
  public
    function state: TKSmartCardSlotState; message 'state';
    function ATR: TKSmartCardATR; message 'ATR';
    function name: NSString; message 'name';
    function maxInputLength: NSInteger; message 'maxInputLength';
    function maxOutputLength: NSInteger; message 'maxOutputLength';
    function makeSmartCard: TKSmartCard; message 'makeSmartCard';
  end;

type
  TKSmartCard = objcclass external (NSObject)
  public
    function slot: TKSmartCardSlot; message 'slot';
    function valid: ObjCBOOL; message 'valid';
    procedure setAllowedProtocols(newValue: TKSmartCardProtocol); message 'setAllowedProtocols:';
    function allowedProtocols: TKSmartCardProtocol; message 'allowedProtocols';
    function currentProtocol: TKSmartCardProtocol; message 'currentProtocol';
    procedure setSensitive(newValue: ObjCBOOL); message 'setSensitive:';
    function sensitive: ObjCBOOL; message 'sensitive';
    procedure setContext(newValue: id); message 'setContext:';
    function context: id; message 'context';
    procedure beginSessionWithReply (reply: OpaqueCBlock); message 'beginSessionWithReply:';
    procedure transmitRequest_reply (request: NSData; reply: OpaqueCBlock); message 'transmitRequest:reply:';
    procedure endSession; message 'endSession';
  end;


type
  APDULevelTransmit = objccategory external (TKSmartCard)
    procedure setCla(newValue: UInt8); message 'setCla:';
    function cla: UInt8; message 'cla';
    procedure setUseExtendedLength(newValue: ObjCBOOL); message 'setUseExtendedLength:';
    function useExtendedLength: ObjCBOOL; message 'useExtendedLength';
    procedure sendIns_p1_p2_data_le_reply (ins: UInt8; p1: UInt8; p2: UInt8; requestData: NSData; le: NSNumber; reply: OpaqueCBlock); message 'sendIns:p1:p2:data:le:reply:';
  end;
{$endif}

