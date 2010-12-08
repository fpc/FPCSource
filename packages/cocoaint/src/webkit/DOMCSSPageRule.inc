{ Parsed from Webkit.framework DOMCSSPageRule.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMCSSPAGERULE_PAS_T}
{$define DOMCSSPAGERULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSPAGERULE_PAS_R}
{$define DOMCSSPAGERULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSPAGERULE_PAS_F}
{$define DOMCSSPAGERULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSPAGERULE_PAS_S}
{$define DOMCSSPAGERULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSPageRule = objcclass;
  DOMCSSPageRulePointer = ^DOMCSSPageRule;
  DOMCSSPageRulePtr = DOMCSSPageRulePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSPAGERULE_PAS_C}
{$define DOMCSSPAGERULE_PAS_C}

{ DOMCSSPageRule }
  DOMCSSPageRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSPageRule; message 'alloc';

    procedure setSelectorText (newValue: NSString); message 'setSelectorText:';
    function selectorText: NSString; message 'selectorText';
    function style: DOMCSSStyleDeclaration; message 'style';
  end; external;

{$endif}
{$endif}
