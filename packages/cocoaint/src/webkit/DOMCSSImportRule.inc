{ Parsed from Webkit.framework DOMCSSImportRule.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMCSSImportRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSImportRule; message 'alloc';

    function href: NSString; message 'href';
    function media: DOMMediaList; message 'media';
    function styleSheet: DOMCSSStyleSheet; message 'styleSheet';
  end; external;

{$endif}
{$endif}
