{ Parsed from Webkit.framework DOMCSSStyleRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMCSSSTYLERULE_PAS_T}
{$define DOMCSSSTYLERULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSSTYLERULE_PAS_R}
{$define DOMCSSSTYLERULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSSTYLERULE_PAS_F}
{$define DOMCSSSTYLERULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSSTYLERULE_PAS_S}
{$define DOMCSSSTYLERULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSStyleRule = objcclass;
  DOMCSSStyleRulePointer = ^DOMCSSStyleRule;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSSTYLERULE_PAS_C}
{$define DOMCSSSTYLERULE_PAS_C}

{ DOMCSSStyleRule }
  DOMCSSStyleRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSStyleRule; message 'alloc';

    procedure setSelectorText (newValue: NSString); message 'setSelectorText:';
    function selectorText: NSString; message 'selectorText';
    function style: DOMCSSStyleDeclaration; message 'style';
  end; external;

{$endif}
{$endif}
