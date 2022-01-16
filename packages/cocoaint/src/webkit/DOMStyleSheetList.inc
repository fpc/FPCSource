{ Parsed from WebKit.framework DOMStyleSheetList.h }


{$ifdef TYPES}
type
  DOMStyleSheetListPtr = ^DOMStyleSheetList;
{$endif}

{$ifdef CLASSES}

type
  DOMStyleSheetList = objcclass external (DOMObject)
  public
    function length: cuint; message 'length';
    function item (index: cuint): DOMStyleSheet; message 'item:';
  end;
{$endif}

