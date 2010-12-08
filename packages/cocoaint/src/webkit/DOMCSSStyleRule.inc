{ Parsed from Webkit.framework DOMCSSStyleRule.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMCSSStyleRulePtr = DOMCSSStyleRulePointer;

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
