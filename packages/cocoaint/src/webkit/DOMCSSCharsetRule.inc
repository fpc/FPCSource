{ Parsed from Webkit.framework DOMCSSCharsetRule.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMCSSCHARSETRULE_PAS_T}
{$define DOMCSSCHARSETRULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSCHARSETRULE_PAS_R}
{$define DOMCSSCHARSETRULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSCHARSETRULE_PAS_F}
{$define DOMCSSCHARSETRULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSCHARSETRULE_PAS_S}
{$define DOMCSSCHARSETRULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSCharsetRule = objcclass;
  DOMCSSCharsetRulePointer = ^DOMCSSCharsetRule;
  DOMCSSCharsetRulePtr = DOMCSSCharsetRulePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSCHARSETRULE_PAS_C}
{$define DOMCSSCHARSETRULE_PAS_C}

{ DOMCSSCharsetRule }
  DOMCSSCharsetRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSCharsetRule; message 'alloc';

    function encoding: NSString; message 'encoding';
  end; external;

{$endif}
{$endif}
