{ Parsed from Webkit.framework DOMMutationEvent.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMMUTATIONEVENT_PAS_T}
{$define DOMMUTATIONEVENT_PAS_T}

{ Constants }

const
  DOM_MODIFICATION = 1;
  DOM_ADDITION = 2;
  DOM_REMOVAL = 3;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMMUTATIONEVENT_PAS_R}
{$define DOMMUTATIONEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMMUTATIONEVENT_PAS_F}
{$define DOMMUTATIONEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMMUTATIONEVENT_PAS_S}
{$define DOMMUTATIONEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMMutationEvent = objcclass;
  DOMMutationEventPointer = ^DOMMutationEvent;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMMUTATIONEVENT_PAS_C}
{$define DOMMUTATIONEVENT_PAS_C}

{ DOMMutationEvent }
  DOMMutationEvent = objcclass(DOMEvent)
    
  public
    class function alloc: DOMMutationEvent; message 'alloc';

    function relatedNode: DOMNode; message 'relatedNode';
    function prevValue: NSString; message 'prevValue';
    function newValue: NSString; message 'newValue';
    function attrName: NSString; message 'attrName';
    function attrChange: cushort; message 'attrChange';
    procedure initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange(type__: NSStringPointer; canBubble: Boolean; cancelable_: Boolean; relatedNode_: DOMNode; prevValue_: NSStringPointer; newValue_: NSStringPointer; attrName_: NSStringPointer; attrChange_: cushort); message 'initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:';

    { Category: DOMMutationEventDeprecated }
    procedure initMutationEvent(type__: NSStringPointer; canBubble: Boolean; cancelable_: Boolean; relatedNode_: DOMNode; prevValue_: NSStringPointer; newValue_: NSStringPointer; attrName_: NSStringPointer; attrChange_: cushort); message 'initMutationEvent:type:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:';
  end; external;

{$endif}
{$endif}
