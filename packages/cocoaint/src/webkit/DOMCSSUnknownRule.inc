{ Parsed from Webkit.framework DOMCSSUnknownRule.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMCSSUNKNOWNRULE_PAS_T}
{$define DOMCSSUNKNOWNRULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSUNKNOWNRULE_PAS_R}
{$define DOMCSSUNKNOWNRULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSUNKNOWNRULE_PAS_F}
{$define DOMCSSUNKNOWNRULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSUNKNOWNRULE_PAS_S}
{$define DOMCSSUNKNOWNRULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSUnknownRule = objcclass;
  DOMCSSUnknownRulePointer = ^DOMCSSUnknownRule;
  DOMCSSUnknownRulePtr = DOMCSSUnknownRulePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSUNKNOWNRULE_PAS_C}
{$define DOMCSSUNKNOWNRULE_PAS_C}

{ DOMCSSUnknownRule }
  DOMCSSUnknownRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSUnknownRule; message 'alloc';
  end; external;

{$endif}
{$endif}
