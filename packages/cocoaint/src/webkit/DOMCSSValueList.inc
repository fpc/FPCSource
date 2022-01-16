{ Parsed from WebKit.framework DOMCSSValueList.h }


{$ifdef TYPES}
type
  DOMCSSValueListPtr = ^DOMCSSValueList;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSValueList = objcclass external (DOMCSSValue)
  public
    function length: cuint; message 'length';
    function item (index: cuint): DOMCSSValue; message 'item:';
  end;
{$endif}

