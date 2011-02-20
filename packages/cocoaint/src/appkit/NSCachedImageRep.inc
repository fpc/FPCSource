{ Parsed from Appkit.framework NSCachedImageRep.h }

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
{$ifndef NSCACHEDIMAGEREP_PAS_S}
{$define NSCACHEDIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCachedImageRep = objcclass;
  NSCachedImageRepPointer = ^NSCachedImageRep;
  NSCachedImageRepPtr = NSCachedImageRepPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCACHEDIMAGEREP_PAS_C}
{$define NSCACHEDIMAGEREP_PAS_C}

{ NSCachedImageRep }
  NSCachedImageRep = objcclass external (NSImageRep)
  private
    _origin: NSPoint;
    _window: NSWindow;
    _cache: Pointer; {garbage collector: __strong }
    
  public
    function initWithWindow_rect(win: NSWindow; rect_: NSRect): id; message 'initWithWindow:rect:'; deprecated 'in Mac OS X 10.6 and later';
    function initWithSize_depth_separate_alpha(size_: NSSize; depth: NSWindowDepth; flag: Boolean; alpha: Boolean): id; message 'initWithSize:depth:separate:alpha:'; deprecated 'in Mac OS X 10.6 and later';
    function window: NSWindow; message 'window'; deprecated 'in Mac OS X 10.6 and later';
    function rect: NSRect; message 'rect'; deprecated 'in Mac OS X 10.6 and later';
  end;

{$endif}
{$endif}
