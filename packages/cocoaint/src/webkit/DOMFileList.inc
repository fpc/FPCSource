{ Parsed from WebKit.framework DOMFileList.h }


{$ifdef TYPES}
type
  DOMFileListPtr = ^DOMFileList;
{$endif}

{$ifdef CLASSES}

type
  DOMFileList = objcclass external (DOMObject)
  public
    function length: cuint; message 'length';
    function item (index: cuint): DOMFile; message 'item:';
  end;
{$endif}

