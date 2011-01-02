{ Parsed from Webkit.framework DOMCSSStyleSheet.h }
{ Version: 2.1.4 - Sun Jan 2 15:08:41 CET 2011 }


{$ifdef TYPES}
{$ifndef DOMCSSSTYLESHEET_PAS_T}
{$define DOMCSSSTYLESHEET_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSSTYLESHEET_PAS_R}
{$define DOMCSSSTYLESHEET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSSTYLESHEET_PAS_F}
{$define DOMCSSSTYLESHEET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSSTYLESHEET_PAS_S}
{$define DOMCSSSTYLESHEET_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSStyleSheet = objcclass;
  DOMCSSStyleSheetPointer = ^DOMCSSStyleSheet;
  DOMCSSStyleSheetPtr = DOMCSSStyleSheetPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSSTYLESHEET_PAS_C}
{$define DOMCSSSTYLESHEET_PAS_C}

{ DOMCSSStyleSheet }
  DOMCSSStyleSheet = objcclass external (DOMStyleSheet)
    
  public
    function ownerRule: DOMCSSRule; message 'ownerRule';
    function cssRules: DOMCSSRuleList; message 'cssRules';
    function rules: DOMCSSRuleList; message 'rules';
    function insertRule_index(rule: NSString; index: cuint): cuint; message 'insertRule:index:';
    procedure deleteRule(index: cuint); message 'deleteRule:';
    function addRule_style_index(selector: NSString; style: NSString; index: cuint): cint; message 'addRule:style:index:';
    procedure removeRule(index: cuint); message 'removeRule:';
  end;

{ DOMCSSStyleSheetDeprecatedCategory }
  DOMCSSStyleSheetDeprecatedCategory = objccategory external (DOMCSSStyleSheet)
    function insertRule(rule: NSString; index: cuint): cuint; message 'insertRule:rule:';
  end;

{$endif}
{$endif}
