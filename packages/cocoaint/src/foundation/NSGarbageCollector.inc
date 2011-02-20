{ Parsed from Foundation.framework NSGarbageCollector.h }

{$ifdef TYPES}
{$ifndef NSGARBAGECOLLECTOR_PAS_T}
{$define NSGARBAGECOLLECTOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGARBAGECOLLECTOR_PAS_R}
{$define NSGARBAGECOLLECTOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGARBAGECOLLECTOR_PAS_F}
{$define NSGARBAGECOLLECTOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGARBAGECOLLECTOR_PAS_S}
{$define NSGARBAGECOLLECTOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGarbageCollector = objcclass;
  NSGarbageCollectorPointer = ^NSGarbageCollector;
  NSGarbageCollectorPtr = NSGarbageCollectorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGARBAGECOLLECTOR_PAS_C}
{$define NSGARBAGECOLLECTOR_PAS_C}

{ NSGarbageCollector }
  NSGarbageCollector = objcclass external (NSObject)
    
  public
    class function defaultCollector: id; message 'defaultCollector';
    function isCollecting: Boolean; message 'isCollecting'; deprecated 'in Mac OS X 10.6 and later';
    procedure disable; message 'disable';
    procedure enable; message 'enable';
    function isEnabled: Boolean; message 'isEnabled';
    procedure collectIfNeeded; message 'collectIfNeeded';
    procedure collectExhaustively; message 'collectExhaustively';
    procedure disableCollectorForPointer(ptr: Pointer); message 'disableCollectorForPointer:';
    procedure enableCollectorForPointer(ptr: Pointer); message 'enableCollectorForPointer:';
    function zone: NSZonePtr; message 'zone';
  end;

{$endif}
{$endif}
