{ Parsed from WebKit.framework DOMHTMLFieldSetElement.h }


{$ifdef TYPES}
type
  DOMHTMLFieldSetElementPtr = ^DOMHTMLFieldSetElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLFieldSetElement = objcclass external (DOMHTMLElement)
  public
    function form: DOMHTMLFormElement; message 'form';
  end;
{$endif}

