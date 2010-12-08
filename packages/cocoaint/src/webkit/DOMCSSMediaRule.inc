{ Parsed from Webkit.framework DOMCSSMediaRule.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMCSSMEDIARULE_PAS_T}
{$define DOMCSSMEDIARULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSMEDIARULE_PAS_R}
{$define DOMCSSMEDIARULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSMEDIARULE_PAS_F}
{$define DOMCSSMEDIARULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSMEDIARULE_PAS_S}
{$define DOMCSSMEDIARULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSMediaRule = objcclass;
  DOMCSSMediaRulePointer = ^DOMCSSMediaRule;
  DOMCSSMediaRulePtr = DOMCSSMediaRulePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSMEDIARULE_PAS_C}
{$define DOMCSSMEDIARULE_PAS_C}

{ DOMCSSMediaRule }
  DOMCSSMediaRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSMediaRule; message 'alloc';

    function media: DOMMediaList; message 'media';
    function cssRules: DOMCSSRuleList; message 'cssRules';
    function insertRule_index(rule: NSString; index: cuint): cuint; message 'insertRule:index:';
    procedure deleteRule(index: cuint); message 'deleteRule:';
  end; external;

{ DOMCSSMediaRuleDeprecatedCategory }
  DOMCSSMediaRuleDeprecatedCategory = objccategory(DOMCSSMediaRule)
    function insertRule(rule: NSString; index: cuint): cuint; message 'insertRule:rule:';
  end; external;

{$endif}
{$endif}
