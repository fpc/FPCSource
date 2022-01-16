{ Parsed from WebKit.framework DOMCSSStyleSheet.h }


{$ifdef TYPES}
type
  DOMCSSStyleSheetPtr = ^DOMCSSStyleSheet;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSStyleSheet = objcclass external (DOMStyleSheet)
  public
    function ownerRule: DOMCSSRule; message 'ownerRule';
    function cssRules: DOMCSSRuleList; message 'cssRules';
    function rules: DOMCSSRuleList; message 'rules';
    function insertRule_index (rule: NSString; index: cuint): cuint; message 'insertRule:index:'; { available in 10_5 }
    procedure deleteRule (index: cuint); message 'deleteRule:';
    function addRule_style_index (selector: NSString; style: NSString; index: cuint): cint; message 'addRule:style:index:'; { available in 10_6 }
    procedure removeRule (index: cuint); message 'removeRule:'; { available in 10_6 }
  end;


type
  DOMCSSStyleSheetDeprecated = objccategory external (DOMCSSStyleSheet)
    function insertRule (rule: NSString): cuint; message 'insertRule:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

