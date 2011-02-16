{ Parsed from Webkit.framework DOMUIEvent.h }

{$ifdef TYPES}
{$ifndef DOMUIEVENT_PAS_T}
{$define DOMUIEVENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMUIEVENT_PAS_R}
{$define DOMUIEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMUIEVENT_PAS_F}
{$define DOMUIEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMUIEVENT_PAS_S}
{$define DOMUIEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMUIEvent = objcclass;
  DOMUIEventPointer = ^DOMUIEvent;
  DOMUIEventPtr = DOMUIEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMUIEVENT_PAS_C}
{$define DOMUIEVENT_PAS_C}

{ DOMUIEvent }
  DOMUIEvent = objcclass external (DOMEvent)
    
  public
    function view: DOMAbstractView; message 'view';
    function detail: cint; message 'detail';
    function keyCode: cint; message 'keyCode';
    function charCode: cint; message 'charCode';
    function layerX: cint; message 'layerX';
    function layerY: cint; message 'layerY';
    function pageX: cint; message 'pageX';
    function pageY: cint; message 'pageY';
    function which: cint; message 'which';
    procedure initUIEvent_canBubble_cancelable_view_detail(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint); message 'initUIEvent:canBubble:cancelable:view:detail:';
  end;

{ DOMUIEventDeprecatedCategory }
  DOMUIEventDeprecatedCategory = objccategory external (DOMUIEvent)
    procedure initUIEvent(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint); message 'initUIEvent:type:canBubble:cancelable:view:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
