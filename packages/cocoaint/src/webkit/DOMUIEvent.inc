{ Parsed from Webkit.framework DOMUIEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMUIEVENT_PAS_C}
{$define DOMUIEVENT_PAS_C}

{ DOMUIEvent }
  DOMUIEvent = objcclass(DOMEvent)
    
  public
    class function alloc: DOMUIEvent; message 'alloc';

    function view: DOMAbstractView; message 'view';
    function detail: cint; message 'detail';
    procedure initUIEvent_canBubble_cancelable_view_detail(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint); message 'initUIEvent:canBubble:cancelable:view:detail:';

    { Category: DOMUIEventDeprecated }
    procedure initUIEvent(type__: NSString; canBubble: Boolean; cancelable_: Boolean; view_: DOMAbstractView; detail_: cint); message 'initUIEvent:type:canBubble:cancelable:view:';
  end; external;

{$endif}
{$endif}
