{ Parsed from Appkit.framework NSTouch.h }

{$ifdef TYPES}
{$ifndef NSTOUCH_PAS_T}
{$define NSTOUCH_PAS_T}

{ Constants }

const
  NSTouchPhaseBegan = 1 shl 0;
  NSTouchPhaseMoved = 1 shl 1;
  NSTouchPhaseStationary = 1 shl 2;
  NSTouchPhaseEnded = 1 shl 3;
  NSTouchPhaseCancelled = 1 shl 4;
  NSTouchPhaseAny = NSUIntegerMax;

{ Types }
type
  NSTouchPhase = NSUInteger;
  NSTouchPhasePtr = ^NSTouchPhase;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOUCH_PAS_R}
{$define NSTOUCH_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOUCH_PAS_F}
{$define NSTOUCH_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOUCH_PAS_S}
{$define NSTOUCH_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTouch = objcclass;
  NSTouchPointer = ^NSTouch;
  NSTouchPtr = NSTouchPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOUCH_PAS_C}
{$define NSTOUCH_PAS_C}

{ NSTouch }
  NSTouch = objcclass external (NSObject, NSCopyingProtocol)
  private
    _index: NSInteger;
    _identity: id;
    _phase: NSTouchPhase;
    _normalizedPosition: NSPoint;
    _privateFlags: NSInteger;
    _view: NSView;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
    _reserved4: id;
    _device: id;
    _deviceSize: NSSize;
    _reserved5: id;
    _reserved6: id;
    _reserved7: id;
    _reserved8: id;
    
  public
    function nSObject: id; message 'nSObject';
    function identity: id; message 'identity';
    function phase: NSTouchPhase; message 'phase';
    function normalizedPosition: NSPoint; message 'normalizedPosition';
    function isResting: Boolean; message 'isResting';
    function device: id; message 'device';
    function deviceSize: NSSize; message 'deviceSize';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
