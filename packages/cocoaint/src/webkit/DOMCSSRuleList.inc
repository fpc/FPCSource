{ Parsed from Webkit.framework DOMCSSRuleList.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:19 CET 2011 }


{$ifdef TYPES}
{$ifndef DOMCSSRULELIST_PAS_T}
{$define DOMCSSRULELIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSRULELIST_PAS_R}
{$define DOMCSSRULELIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSRULELIST_PAS_F}
{$define DOMCSSRULELIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSRULELIST_PAS_S}
{$define DOMCSSRULELIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSRuleList = objcclass;
  DOMCSSRuleListPointer = ^DOMCSSRuleList;
  DOMCSSRuleListPtr = DOMCSSRuleListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSRULELIST_PAS_C}
{$define DOMCSSRULELIST_PAS_C}

{ DOMCSSRuleList }
  DOMCSSRuleList = objcclass external (DOMObject)
    
  public
    function length: cuint; message 'length';
    function item(index: cuint): DOMCSSRule; message 'item:';
  end;

{$endif}
{$endif}
