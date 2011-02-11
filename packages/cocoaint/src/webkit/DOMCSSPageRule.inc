{ Parsed from Webkit.framework DOMCSSPageRule.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:19 CET 2011 }


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
  DOMCSSPageRule = objcclass external (DOMCSSRule)
    
  public
    procedure setSelectorText (newValue: NSString); message 'setSelectorText:';
    function selectorText: NSString; message 'selectorText';
    function style: DOMCSSStyleDeclaration; message 'style';
  end;

{$endif}
{$endif}
