{ Parsed from WebKit.framework DOMCSSStyleDeclaration.h }


{$ifdef TYPES}
type
  DOMCSSStyleDeclarationPtr = ^DOMCSSStyleDeclaration;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSStyleDeclaration = objcclass external (DOMObject)
  public
    procedure setCssText(newValue: NSString); message 'setCssText:';
    function cssText: NSString; message 'cssText';
    function length: cuint; message 'length';
    function parentRule: DOMCSSRule; message 'parentRule';
    function getPropertyValue (propertyName: NSString): NSString; message 'getPropertyValue:';
    function getPropertyCSSValue (propertyName: NSString): DOMCSSValue; message 'getPropertyCSSValue:';
    function removeProperty (propertyName: NSString): NSString; message 'removeProperty:';
    function getPropertyPriority (propertyName: NSString): NSString; message 'getPropertyPriority:';
    procedure setProperty_value_priority (propertyName: NSString; value: NSString; priority: NSString); message 'setProperty:value:priority:'; { available in 10_5 }
    function item (index: cuint): NSString; message 'item:';
    function getPropertyShorthand (propertyName: NSString): NSString; message 'getPropertyShorthand:'; deprecated 'in 10_5, 10_5';
    function isPropertyImplicit (propertyName: NSString): ObjCBOOL; message 'isPropertyImplicit:'; { available in 10_5 }
  end;


type
  DOMCSSStyleDeclarationDeprecated = objccategory external (DOMCSSStyleDeclaration)
    procedure setProperty_value (propertyName: NSString; priority: NSString); message 'setProperty:value:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

