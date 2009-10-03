{ Parsed from Webkit.framework DOMCSSStyleDeclaration.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
    function getPropertyValue(propertyName: NSStringPointer): NSString; message 'getPropertyValue:';
    function getPropertyCSSValue(propertyName: NSStringPointer): DOMCSSValue; message 'getPropertyCSSValue:';
    function removeProperty(propertyName: NSStringPointer): NSString; message 'removeProperty:';
    function getPropertyPriority(propertyName: NSStringPointer): NSString; message 'getPropertyPriority:';
    procedure setProperty_value_priority(propertyName: NSStringPointer; value: NSStringPointer; priority: NSStringPointer); message 'setProperty:value:priority:';
    function item(index: cuint): NSString; message 'item:';
    function getPropertyShorthand(propertyName: NSStringPointer): NSString; message 'getPropertyShorthand:';
    function isPropertyImplicit(propertyName: NSStringPointer): Boolean; message 'isPropertyImplicit:';

    { Category: DOMCSSStyleDeclarationDeprecated }
    procedure setProperty(propertyName: NSStringPointer; value: NSStringPointer; priority: NSStringPointer); message 'setProperty:propertyName:value:';
  end; external;

{$endif}
{$endif}
