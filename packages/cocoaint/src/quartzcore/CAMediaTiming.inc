{ Parsed from Quartzcore.framework CAMediaTiming.h }

{$ifdef TYPES}
{$ifndef CAMEDIATIMING_PAS_T}
{$define CAMEDIATIMING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAMEDIATIMING_PAS_R}
{$define CAMEDIATIMING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAMEDIATIMING_PAS_F}
{$define CAMEDIATIMING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAMEDIATIMING_PAS_S}
{$define CAMEDIATIMING_PAS_S}

{ External string constants }
var
  kCAFillModeForwards: NSString; cvar; external;
  kCAFillModeBackwards: NSString; cvar; external;
  kCAFillModeBoth: NSString; cvar; external;
  kCAFillModeRemoved: NSString; cvar; external;
  kCAFillModeFrozen: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAMediaTimingProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef CAMEDIATIMING_PAS_P}
{$define CAMEDIATIMING_PAS_P}
  
{ CAMediaTiming Protocol }
  CAMediaTimingProtocol = objcprotocol external name 'CAMediaTiming'
    procedure setBeginTime (newValue: CFTimeInterval); message 'setBeginTime:';
    function beginTime: CFTimeInterval; message 'beginTime';
    procedure setDuration (newValue: CFTimeInterval); message 'setDuration:';
    function duration: CFTimeInterval; message 'duration';
    procedure setSpeed (newValue: single); message 'setSpeed:';
    function speed: single; message 'speed';
    procedure setTimeOffset (newValue: CFTimeInterval); message 'setTimeOffset:';
    function timeOffset: CFTimeInterval; message 'timeOffset';
    procedure setRepeatCount (newValue: single); message 'setRepeatCount:';
    function repeatCount: single; message 'repeatCount';
    procedure setRepeatDuration (newValue: CFTimeInterval); message 'setRepeatDuration:';
    function repeatDuration: CFTimeInterval; message 'repeatDuration';
    procedure setAutoreverses (newValue: Boolean); message 'setAutoreverses:';
    function autoreverses: Boolean; message 'autoreverses';
    procedure setFillMode (newValue: NSString); message 'setFillMode:';
    function fillMode: NSString; message 'fillMode';
  end;
{$endif}
{$endif}
