{ Parsed from Webkit.framework DOMCSSStyleDeclaration.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSSTYLEDECLARATION_PAS_C}
{$define DOMCSSSTYLEDECLARATION_PAS_C}

{ DOMCSSStyleDeclaration }
  DOMCSSStyleDeclaration = objcclass(DOMObject)
    
  public
    class function alloc: DOMCSSStyleDeclaration; message 'alloc';

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

    { Category: DOMCSSStyleDeclarationDeprecated }
    procedure setProperty(propertyName: NSString; value: NSString; priority: NSString); message 'setProperty:propertyName:value:';
  end; external;

{$endif}
{$endif}
