{ Parsed from WebKit.framework DOMAbstractView.h }


{$ifdef TYPES}
type
  DOMAbstractViewPtr = ^DOMAbstractView;
{$endif}

{$ifdef CLASSES}

type
  DOMAbstractView = objcclass external (DOMObject)
  public
    function document: DOMDocument; message 'document';
  end;
{$endif}

