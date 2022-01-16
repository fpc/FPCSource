{ Parsed from WebKit.framework DOMCounter.h }


{$ifdef TYPES}
type
  DOMCounterPtr = ^DOMCounter;
{$endif}

{$ifdef CLASSES}

type
  DOMCounter = objcclass external (DOMObject)
  public
    function identifier: NSString; message 'identifier';
    function listStyle: NSString; message 'listStyle';
    function separator: NSString; message 'separator';
  end;
{$endif}

