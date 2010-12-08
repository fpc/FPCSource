{ Parsed from Webkit.framework DOMOverflowEvent.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMOVERFLOWEVENT_PAS_T}
{$define DOMOVERFLOWEVENT_PAS_T}

{ Constants }

const
  DOM_HORIZONTAL = 0;
  DOM_VERTICAL = 1;
  DOM_BOTH = 2;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMOVERFLOWEVENT_PAS_R}
{$define DOMOVERFLOWEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMOVERFLOWEVENT_PAS_F}
{$define DOMOVERFLOWEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMOVERFLOWEVENT_PAS_S}
{$define DOMOVERFLOWEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMOverflowEvent = objcclass;
  DOMOverflowEventPointer = ^DOMOverflowEvent;
  DOMOverflowEventPtr = DOMOverflowEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMOVERFLOWEVENT_PAS_C}
{$define DOMOVERFLOWEVENT_PAS_C}

{ DOMOverflowEvent }
  DOMOverflowEvent = objcclass(DOMEvent)
    
  public
    class function alloc: DOMOverflowEvent; message 'alloc';

    function orient: cuint; message 'orient';
    function horizontalOverflow: Boolean; message 'horizontalOverflow';
    function verticalOverflow: Boolean; message 'verticalOverflow';
    procedure initOverflowEvent_horizontalOverflow_verticalOverflow(orient_: cushort; horizontalOverflow_: Boolean; verticalOverflow_: Boolean); message 'initOverflowEvent:horizontalOverflow:verticalOverflow:';
  end; external;

{$endif}
{$endif}
