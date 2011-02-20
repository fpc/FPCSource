{ Parsed from Webkit.framework DOMCSSImportRule.h }

{$ifdef TYPES}
{$ifndef DOMCSSIMPORTRULE_PAS_T}
{$define DOMCSSIMPORTRULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSIMPORTRULE_PAS_R}
{$define DOMCSSIMPORTRULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSIMPORTRULE_PAS_F}
{$define DOMCSSIMPORTRULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSIMPORTRULE_PAS_S}
{$define DOMCSSIMPORTRULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSImportRule = objcclass;
  DOMCSSImportRulePointer = ^DOMCSSImportRule;
  DOMCSSImportRulePtr = DOMCSSImportRulePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSIMPORTRULE_PAS_C}
{$define DOMCSSIMPORTRULE_PAS_C}

{ DOMCSSImportRule }
  DOMCSSImportRule = objcclass external (DOMCSSRule)
    
  public
    function href: NSString; message 'href';
    function media: DOMMediaList; message 'media';
    function styleSheet: DOMCSSStyleSheet; message 'styleSheet';
  end;

{$endif}
{$endif}
