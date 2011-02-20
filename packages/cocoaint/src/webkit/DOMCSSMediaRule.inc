{ Parsed from Webkit.framework DOMCSSMediaRule.h }

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
  DOMCSSMediaRule = objcclass external (DOMCSSRule)
    
  public
    function media: DOMMediaList; message 'media';
    function cssRules: DOMCSSRuleList; message 'cssRules';
    function insertRule_index(rule: NSString; index: cuint): cuint; message 'insertRule:index:';
    procedure deleteRule(index: cuint); message 'deleteRule:';
  end;

{ DOMCSSMediaRuleDeprecatedCategory }
  DOMCSSMediaRuleDeprecatedCategory = objccategory external (DOMCSSMediaRule)
    function insertRule(rule: NSString; index: cuint): cuint; message 'insertRule:rule:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
