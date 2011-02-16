{ Parsed from Webkit.framework DOMMutationEvent.h }

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
  DOMMutationEventPtr = DOMMutationEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMMUTATIONEVENT_PAS_C}
{$define DOMMUTATIONEVENT_PAS_C}

{ DOMMutationEvent }
  DOMMutationEvent = objcclass external (DOMEvent)
    
  public
    function relatedNode: DOMNode; message 'relatedNode';
    function prevValue: NSString; message 'prevValue';
    function newValue: NSString; message 'newValue';
    function attrName: NSString; message 'attrName';
    function attrChange: cuint; message 'attrChange';
    procedure initMutationEvent_canBubble_cancelable_relatedNode_prevValue_newValue_attrName_attrChange(type__: NSString; canBubble: Boolean; cancelable_: Boolean; relatedNode_: DOMNode; prevValue_: NSString; newValue_: NSString; attrName_: NSString; attrChange_: cushort); message 'initMutationEvent:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:attrChange:';
  end;

{ DOMMutationEventDeprecatedCategory }
  DOMMutationEventDeprecatedCategory = objccategory external (DOMMutationEvent)
    procedure initMutationEvent(type__: NSString; canBubble: Boolean; cancelable_: Boolean; relatedNode_: DOMNode; prevValue_: NSString; newValue_: NSString; attrName_: NSString; attrChange_: cushort); message 'initMutationEvent:type:canBubble:cancelable:relatedNode:prevValue:newValue:attrName:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
