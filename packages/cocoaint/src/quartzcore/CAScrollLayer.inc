{ Parsed from Quartzcore.framework CAScrollLayer.h }

{$ifdef TYPES}
{$ifndef CASCROLLLAYER_PAS_T}
{$define CASCROLLLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CASCROLLLAYER_PAS_R}
{$define CASCROLLLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CASCROLLLAYER_PAS_F}
{$define CASCROLLLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CASCROLLLAYER_PAS_S}
{$define CASCROLLLAYER_PAS_S}

{ External string constants }
var
  kCAScrollNone: NSString; cvar; external;
  kCAScrollVertically: NSString; cvar; external;
  kCAScrollHorizontally: NSString; cvar; external;
  kCAScrollBoth: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAScrollLayer = objcclass;
  CAScrollLayerPointer = ^CAScrollLayer;
  CAScrollLayerPtr = CAScrollLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CASCROLLLAYER_PAS_C}
{$define CASCROLLLAYER_PAS_C}

{ CAScrollLayer }
  CAScrollLayer = objcclass external (CALayer)
    
  public
    procedure scrollToPoint(p: CGPoint); message 'scrollToPoint:';
    procedure scrollToRect(r: CGRect); message 'scrollToRect:';
    procedure setScrollMode (newValue: NSString); message 'setScrollMode:';
    function scrollMode: NSString; message 'scrollMode';
  end;

{ CALayerScrollingCategory }
  CALayerScrollingCategory = objccategory external (CALayer)
    procedure scrollPoint(p: CGPoint); message 'scrollPoint:';
    procedure scrollRectToVisible(r: CGRect); message 'scrollRectToVisible:';
    function visibleRect: CGRect; message 'visibleRect';
  end;

{$endif}
{$endif}
