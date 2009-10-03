{ Parsed from Webkit.framework DOMCSSUnknownRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
