{ Parsed from Webkit.framework DOMCSSMediaRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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

    { Category: DOMCSSMediaRuleDeprecated }
    function insertRule(rule: NSString; index: cuint): cuint; message 'insertRule:rule:';
  end; external;

{$endif}
{$endif}
