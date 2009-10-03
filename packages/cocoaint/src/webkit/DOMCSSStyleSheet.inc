{ Parsed from Webkit.framework DOMCSSStyleSheet.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSSTYLESHEET_PAS_C}
{$define DOMCSSSTYLESHEET_PAS_C}

{ DOMCSSStyleSheet }
  DOMCSSStyleSheet = objcclass(DOMStyleSheet)
    
  public
    class function alloc: DOMCSSStyleSheet; message 'alloc';

    function ownerRule: DOMCSSRule; message 'ownerRule';
    function cssRules: DOMCSSRuleList; message 'cssRules';
    function insertRule_index(rule: NSStringPointer; index: cuint): cuint; message 'insertRule:index:';
    procedure deleteRule(index: cuint); message 'deleteRule:';

    { Category: DOMCSSStyleSheetDeprecated }
    function insertRule(rule: NSStringPointer; index: cuint): cuint; message 'insertRule:rule:';
  end; external;

{$endif}
{$endif}
