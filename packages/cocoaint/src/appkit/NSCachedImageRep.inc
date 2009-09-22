{ Parsed from Appkit.framework NSCachedImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCACHEDIMAGEREP_PAS_H}
{$define NSCACHEDIMAGEREP_PAS_H}
type
  NSCachedImageRepPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCACHEDIMAGEREP_PAS_T}
{$define NSCACHEDIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCACHEDIMAGEREP_PAS_R}
{$define NSCACHEDIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCACHEDIMAGEREP_PAS_F}
{$define NSCACHEDIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCACHEDIMAGEREP_PAS_T}
{$define NSCACHEDIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCachedImageRep = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCACHEDIMAGEREP_PAS_C}
{$define NSCACHEDIMAGEREP_PAS_C}

{ NSCachedImageRep }
  NSCachedImageRep = objcclass(NSImageRep)
  private
    __origin: NSPoint;
    __window: NSWindow;
    __cache: Pointer; {garbage collector: __strong }
    
  public
    class function alloc: NSCachedImageRep; message 'alloc';

    function initWithWindow_rect(win: NSWindow; rect_: NSRect): id; message 'initWithWindow:rect:';
    function initWithSize_depth_separate_alpha(size_: NSSize; depth: NSWindowDepth; flag: Boolean; alpha: Boolean): id; message 'initWithSize:depth:separate:alpha:';
    function window: NSWindow; message 'window';
    function rect: NSRect; message 'rect';
  end; external;

{$endif}
{$endif}
