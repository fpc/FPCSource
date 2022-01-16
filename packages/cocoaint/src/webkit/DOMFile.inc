{ Parsed from WebKit.framework DOMFile.h }


{$ifdef TYPES}
type
  DOMFilePtr = ^DOMFile;
{$endif}

{$ifdef CLASSES}

type
  DOMFile = objcclass external (DOMBlob)
  public
    function name: NSString; message 'name';
  end;
{$endif}

