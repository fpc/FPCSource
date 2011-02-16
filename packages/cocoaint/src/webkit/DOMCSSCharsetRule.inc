{ Parsed from Webkit.framework DOMCSSCharsetRule.h }

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
  DOMCSSCharsetRule = objcclass external (DOMCSSRule)
    
  public
    function encoding: NSString; message 'encoding';
  end;

{$endif}
{$endif}
