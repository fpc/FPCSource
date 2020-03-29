{ Parsed from WebKit.framework DOMProgressEvent.h }


{$ifdef TYPES}
type
  DOMProgressEventPtr = ^DOMProgressEvent;
{$endif}

{$ifdef CLASSES}

type
  DOMProgressEvent = objcclass external (DOMEvent)
  public
    function lengthComputable: ObjCBOOL; message 'lengthComputable';
    function loaded: culonglong; message 'loaded';
    function total: culonglong; message 'total';
  end;
{$endif}

