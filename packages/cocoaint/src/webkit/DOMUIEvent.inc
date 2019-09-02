{ Parsed from WebKit.framework DOMUIEvent.h }


{$ifdef TYPES}
type
  DOMUIEventPtr = ^DOMUIEvent;
{$endif}

{$ifdef CLASSES}

type
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
    procedure initUIEvent_canBubble_cancelable_view_detail (type_: NSString; canBubble: ObjCBOOL; cancelable_: ObjCBOOL; view_: DOMAbstractView; detail_: cint); message 'initUIEvent:canBubble:cancelable:view:detail:'; { available in 10_5 }
  end;


type
  DOMUIEventDeprecated = objccategory external (DOMUIEvent)
    procedure initUIEvent_canBubble_view (type_: NSString; cancelable_: ObjCBOOL; detail_: cint); message 'initUIEvent:canBubble:view:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

