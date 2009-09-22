{ Parsed from Foundation.framework NSGarbageCollector.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:45 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSGARBAGECOLLECTOR_PAS_H}
{$define NSGARBAGECOLLECTOR_PAS_H}
type
  NSGarbageCollectorPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSGARBAGECOLLECTOR_PAS_T}
{$define NSGARBAGECOLLECTOR_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSGarbageCollector = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGARBAGECOLLECTOR_PAS_C}
{$define NSGARBAGECOLLECTOR_PAS_C}

{ NSGarbageCollector }
  NSGarbageCollector = objcclass(NSObject)
    
  public
    class function alloc: NSGarbageCollector; message 'alloc';

    class function defaultCollector: id; message 'defaultCollector';
    function isCollecting: Boolean; message 'isCollecting';
    procedure disable; message 'disable';
    procedure enable; message 'enable';
    function isEnabled: Boolean; message 'isEnabled';
    procedure collectIfNeeded; message 'collectIfNeeded';
    procedure collectExhaustively; message 'collectExhaustively';
    procedure disableCollectorForPointer(ptr: Pointer); message 'disableCollectorForPointer:';
    procedure enableCollectorForPointer(ptr: Pointer); message 'enableCollectorForPointer:';
    function zone_: NSZone; message 'zone';
  end; external;

{$endif}
{$endif}
