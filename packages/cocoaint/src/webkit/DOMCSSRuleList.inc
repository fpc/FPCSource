{ Parsed from WebKit.framework DOMCSSRuleList.h }


{$ifdef TYPES}
type
  DOMCSSRuleListPtr = ^DOMCSSRuleList;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSRuleList = objcclass external (DOMObject)
  public
    function length: cuint; message 'length';
    function item (index: cuint): DOMCSSRule; message 'item:';
  end;
{$endif}

