{ Parsed from Webkit.framework DOMCSSRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMCSSRULE_PAS_T}
{$define DOMCSSRULE_PAS_T}

{ Constants }

const
  DOM_UNKNOWN_RULE = 0;
  DOM_STYLE_RULE = 1;
  DOM_CHARSET_RULE = 2;
  DOM_IMPORT_RULE = 3;
  DOM_MEDIA_RULE = 4;
  DOM_FONT_FACE_RULE = 5;
  DOM_PAGE_RULE = 6;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSRULE_PAS_R}
{$define DOMCSSRULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSRULE_PAS_F}
{$define DOMCSSRULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSRULE_PAS_S}
{$define DOMCSSRULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSRule = objcclass;
  DOMCSSRulePointer = ^DOMCSSRule;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSRULE_PAS_C}
{$define DOMCSSRULE_PAS_C}

{ DOMCSSRule }
  DOMCSSRule = objcclass(DOMObject)
    
  public
    class function alloc: DOMCSSRule; message 'alloc';

    function type_: cushort; message 'type_';
    procedure setCssText (newValue: NSString); message 'setCssText:';
    function cssText: NSString; message 'cssText';
    function parentStyleSheet: DOMCSSStyleSheet; message 'parentStyleSheet';
    function parentRule: DOMCSSRule; message 'parentRule';
  end; external;

{$endif}
{$endif}
