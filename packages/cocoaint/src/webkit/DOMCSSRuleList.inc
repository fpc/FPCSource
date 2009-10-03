{ Parsed from Webkit.framework DOMCSSRuleList.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSRULELIST_PAS_C}
{$define DOMCSSRULELIST_PAS_C}

{ DOMCSSRuleList }
  DOMCSSRuleList = objcclass(DOMObject)
    
  public
    class function alloc: DOMCSSRuleList; message 'alloc';

    function length: cuint; message 'length';
    function item(index: cuint): DOMCSSRule; message 'item:';
  end; external;

{$endif}
{$endif}
