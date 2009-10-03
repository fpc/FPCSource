{ Parsed from Webkit.framework DOMCSSValueList.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSVALUELIST_PAS_C}
{$define DOMCSSVALUELIST_PAS_C}

{ DOMCSSValueList }
  DOMCSSValueList = objcclass(DOMCSSValue)
    
  public
    class function alloc: DOMCSSValueList; message 'alloc';

    function length: cuint; message 'length';
    function item(index: cuint): DOMCSSValue; message 'item:';
  end; external;

{$endif}
{$endif}
