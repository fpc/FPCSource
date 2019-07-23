{ Parsed from Webkit.framework DOMProgressEvent.h }

{$ifdef TYPES}
{$ifndef DOMPROGRESSEVENT_PAS_T}
{$define DOMPROGRESSEVENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMPROGRESSEVENT_PAS_R}
{$define DOMPROGRESSEVENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMPROGRESSEVENT_PAS_F}
{$define DOMPROGRESSEVENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMPROGRESSEVENT_PAS_S}
{$define DOMPROGRESSEVENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMProgressEvent = objcclass;
  DOMProgressEventPointer = ^DOMProgressEvent;
  DOMProgressEventPtr = DOMProgressEventPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMPROGRESSEVENT_PAS_C}
{$define DOMPROGRESSEVENT_PAS_C}

{ DOMProgressEvent }
  DOMProgressEvent = objcclass external (DOMEvent)
    
  public
    function lengthComputable: ObjCBOOL; message 'lengthComputable';
    function loaded: cuint; message 'loaded';
    function total: cuint; message 'total';
    procedure initProgressEvent_canBubbleArg_cancelableArg_lengthComputableArg_loadedArg_totalArg(typeArg: NSString; canBubbleArg: ObjCBOOL; cancelableArg: ObjCBOOL; lengthComputableArg: ObjCBOOL; loadedArg: cuint; totalArg: cuint); message 'initProgressEvent:canBubbleArg:cancelableArg:lengthComputableArg:loadedArg:totalArg:';
  end;

{$endif}
{$endif}
