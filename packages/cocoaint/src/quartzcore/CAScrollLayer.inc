{ Parsed from Quartzcore.framework CAScrollLayer.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


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
  CAScrollLayer = objcclass(CALayer)
    
  public
    class function alloc: CAScrollLayer; message 'alloc';

    procedure scrollToPoint(p: CGPoint); message 'scrollToPoint:';
    procedure scrollToRect(r: CGRect); message 'scrollToRect:';
    procedure setScrollMode (newValue: NSString); message 'setScrollMode:';
    function scrollMode: NSString; message 'scrollMode';
  end; external;

{ CALayerScrollingCategory }
  CALayerScrollingCategory = objccategory(CALayer)
    procedure scrollPoint(p: CGPoint); message 'scrollPoint:';
    procedure scrollRectToVisible(r: CGRect); message 'scrollRectToVisible:';
    function visibleRect: CGRect; message 'visibleRect';
  end; external;

{$endif}
{$endif}
