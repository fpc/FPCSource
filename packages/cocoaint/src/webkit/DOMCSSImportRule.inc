{ Parsed from Webkit.framework DOMCSSImportRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
