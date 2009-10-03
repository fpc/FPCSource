{ Parsed from Webkit.framework DOMWheelEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMWHEELEVENT_PAS_T}
{$define DOMWHEELEVENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMWHEELEVENT_PAS_R}
{$define DOMWHEELEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMWHEELEVENT_PAS_F}
{$define DOMWHEELEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMWHEELEVENT_PAS_S}
{$define DOMWHEELEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMWheelEvent = objcclass;
  DOMWheelEventPointer = ^DOMWheelEvent;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMWHEELEVENT_PAS_C}
{$define DOMWHEELEVENT_PAS_C}

{ DOMWheelEvent }
  DOMWheelEvent = objcclass(DOMUIEvent)
    
  public
    class function alloc: DOMWheelEvent; message 'alloc';

    function screenX: cint; message 'screenX';
    function screenY: cint; message 'screenY';
    function clientX: cint; message 'clientX';
    function clientY: cint; message 'clientY';
    function ctrlKey: Boolean; message 'ctrlKey';
    function shiftKey: Boolean; message 'shiftKey';
    function altKey: Boolean; message 'altKey';
    function metaKey: Boolean; message 'metaKey';
    function wheelDelta: cint; message 'wheelDelta';
    function isHorizontal: Boolean; message 'isHorizontal';
  end; external;

{$endif}
{$endif}
