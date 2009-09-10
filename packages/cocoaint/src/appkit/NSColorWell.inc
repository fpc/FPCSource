{ Parsed from Appkit.framework NSColorWell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOLORWELL_PAS_H}
{$define NSCOLORWELL_PAS_H}
type
  NSColorWellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOLORWELL_PAS_T}
{$define NSCOLORWELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLORWELL_PAS_R}
{$define NSCOLORWELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLORWELL_PAS_F}
{$define NSCOLORWELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLORWELL_PAS_T}
{$define NSCOLORWELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorWell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORWELL_PAS_C}
{$define NSCOLORWELL_PAS_C}

{ NSColorWell }
  NSColorWell = objcclass(NSControl)
  private
    __color: NSColor;
    __target: id;
    __action: SEL;
    __cwFlags: bitpacked record
        isActive: 0..1;
        isBordered: 0..1;
        cantDraw: 0..1;
        isNotContinuous: 0..1;
        reservedColorWell: 0..((1 shl 28)-1);
      end;
    
  public
    class function alloc: NSColorWell; message 'alloc';

    procedure deactivate; message 'deactivate';
    procedure activate(exclusive: Boolean); message 'activate:';
    function isActive: Boolean; message 'isActive';
    procedure drawWellInside(insideRect: NSRect); message 'drawWellInside:';
    function isBordered: Boolean; message 'isBordered';
    procedure setBordered(flag: Boolean); message 'setBordered:';
    procedure takeColorFrom(sender: id); message 'takeColorFrom:';
    procedure setColor(color_: NSColor); message 'setColor:';
    function color: NSColor; message 'color';
  end; external;

{$endif}
{$endif}
