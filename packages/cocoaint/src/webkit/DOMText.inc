{ Parsed from Webkit.framework DOMText.h }

{$ifdef TYPES}
{$ifndef DOMTEXT_PAS_T}
{$define DOMTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMTEXT_PAS_R}
{$define DOMTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMTEXT_PAS_F}
{$define DOMTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMTEXT_PAS_S}
{$define DOMTEXT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMText = objcclass;
  DOMTextPointer = ^DOMText;
  DOMTextPtr = DOMTextPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMTEXT_PAS_C}
{$define DOMTEXT_PAS_C}

{ DOMText }
  DOMText = objcclass external (DOMCharacterData)
    
  public
    function wholeText: NSString; message 'wholeText';
    function splitText(offset: cuint): DOMText; message 'splitText:';
    function replaceWholeText(content: NSString): DOMText; message 'replaceWholeText:';
  end;

{$endif}
{$endif}
