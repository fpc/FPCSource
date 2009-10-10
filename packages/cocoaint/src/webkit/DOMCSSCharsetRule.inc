{ Parsed from Webkit.framework DOMCSSCharsetRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
