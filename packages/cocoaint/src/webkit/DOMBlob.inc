{ Parsed from WebKit.framework DOMBlob.h }


{$ifdef TYPES}
type
  DOMBlobPtr = ^DOMBlob;
{$endif}

{$ifdef CLASSES}

type
  DOMBlob = objcclass external (DOMObject)
  public
    function size: culonglong; message 'size';
  end;
{$endif}

