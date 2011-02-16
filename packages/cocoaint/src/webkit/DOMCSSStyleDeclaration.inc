{ Parsed from Webkit.framework DOMCSSStyleDeclaration.h }

{$ifdef TYPES}
{$ifndef DOMCSSSTYLEDECLARATION_PAS_T}
{$define DOMCSSSTYLEDECLARATION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSSTYLEDECLARATION_PAS_R}
{$define DOMCSSSTYLEDECLARATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSSTYLEDECLARATION_PAS_F}
{$define DOMCSSSTYLEDECLARATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSSTYLEDECLARATION_PAS_S}
{$define DOMCSSSTYLEDECLARATION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSStyleDeclaration = objcclass;
  DOMCSSStyleDeclarationPointer = ^DOMCSSStyleDeclaration;
  DOMCSSStyleDeclarationPtr = DOMCSSStyleDeclarationPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSSTYLEDECLARATION_PAS_C}
{$define DOMCSSSTYLEDECLARATION_PAS_C}

{ DOMCSSStyleDeclaration }
  DOMCSSStyleDeclaration = objcclass external (DOMObject)
    
  public
    procedure setCssText (newValue: NSString); message 'setCssText:';
    function cssText: NSString; message 'cssText';
    function length: cuint; message 'length';
    function parentRule: DOMCSSRule; message 'parentRule';
    function getPropertyValue(propertyName: NSString): NSString; message 'getPropertyValue:';
    function getPropertyCSSValue(propertyName: NSString): DOMCSSValue; message 'getPropertyCSSValue:';
    function removeProperty(propertyName: NSString): NSString; message 'removeProperty:';
    function getPropertyPriority(propertyName: NSString): NSString; message 'getPropertyPriority:';
    procedure setProperty_value_priority(propertyName: NSString; value: NSString; priority: NSString); message 'setProperty:value:priority:';
    function item(index: cuint): NSString; message 'item:';
    function getPropertyShorthand(propertyName: NSString): NSString; message 'getPropertyShorthand:';
    function isPropertyImplicit(propertyName: NSString): Boolean; message 'isPropertyImplicit:';
  end;

{ DOMCSSStyleDeclarationDeprecatedCategory }
  DOMCSSStyleDeclarationDeprecatedCategory = objccategory external (DOMCSSStyleDeclaration)
    procedure setProperty(propertyName: NSString; value: NSString; priority: NSString); message 'setProperty:propertyName:value:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
