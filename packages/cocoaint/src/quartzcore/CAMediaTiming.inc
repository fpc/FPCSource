{ Parsed from QuartzCore.framework CAMediaTiming.h }


{$ifdef TYPES}
type
  CAMediaTimingProtocolPtr = ^CAMediaTimingProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  CAMediaTimingProtocol = objcprotocol external name 'CAMediaTiming'
    procedure setBeginTime(newValue: CFTimeInterval); message 'setBeginTime:';
    function beginTime: CFTimeInterval; message 'beginTime';
    procedure setDuration(newValue: CFTimeInterval); message 'setDuration:';
    function duration: CFTimeInterval; message 'duration';
    procedure setSpeed(newValue: single); message 'setSpeed:';
    function speed: single; message 'speed';
    procedure setTimeOffset(newValue: CFTimeInterval); message 'setTimeOffset:';
    function timeOffset: CFTimeInterval; message 'timeOffset';
    procedure setRepeatCount(newValue: single); message 'setRepeatCount:';
    function repeatCount: single; message 'repeatCount';
    procedure setRepeatDuration(newValue: CFTimeInterval); message 'setRepeatDuration:';
    function repeatDuration: CFTimeInterval; message 'repeatDuration';
    procedure setAutoreverses(newValue: ObjCBOOL); message 'setAutoreverses:';
    function autoreverses: ObjCBOOL; message 'autoreverses';
    procedure setFillMode(newValue: NSString); message 'setFillMode:';
    function fillMode: NSString; message 'fillMode';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAFillModeForwards: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFillModeBackwards: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFillModeBoth: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFillModeRemoved: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAFillModeFrozen: NSString; cvar; external;
{$endif}

