{ Parsed from WebKit.framework DOMText.h }


{$ifdef TYPES}
type
  DOMTextPtr = ^DOMText;
{$endif}

{$ifdef CLASSES}

type
  DOMText = objcclass external (DOMCharacterData)
  public
    function wholeText: NSString; message 'wholeText';
    function splitText (offset: cuint): DOMText; message 'splitText:';
    function replaceWholeText (content: NSString): DOMText; message 'replaceWholeText:'; { available in 10_6 }
  end;
{$endif}

