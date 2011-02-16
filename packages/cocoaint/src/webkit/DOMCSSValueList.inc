{ Parsed from Webkit.framework DOMCSSValueList.h }

{$ifdef TYPES}
{$ifndef DOMCSSVALUELIST_PAS_T}
{$define DOMCSSVALUELIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSVALUELIST_PAS_R}
{$define DOMCSSVALUELIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSVALUELIST_PAS_F}
{$define DOMCSSVALUELIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSVALUELIST_PAS_S}
{$define DOMCSSVALUELIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSValueList = objcclass;
  DOMCSSValueListPointer = ^DOMCSSValueList;
  DOMCSSValueListPtr = DOMCSSValueListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSVALUELIST_PAS_C}
{$define DOMCSSVALUELIST_PAS_C}

{ DOMCSSValueList }
  DOMCSSValueList = objcclass external (DOMCSSValue)
    
  public
    function length: cuint; message 'length';
    function item(index: cuint): DOMCSSValue; message 'item:';
  end;

{$endif}
{$endif}
