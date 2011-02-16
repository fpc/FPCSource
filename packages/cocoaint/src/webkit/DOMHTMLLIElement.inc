{ Parsed from Webkit.framework DOMHTMLLIElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLLIELEMENT_PAS_T}
{$define DOMHTMLLIELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLLIELEMENT_PAS_R}
{$define DOMHTMLLIELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLLIELEMENT_PAS_F}
{$define DOMHTMLLIELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLLIELEMENT_PAS_S}
{$define DOMHTMLLIELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLLIElement = objcclass;
  DOMHTMLLIElementPointer = ^DOMHTMLLIElement;
  DOMHTMLLIElementPtr = DOMHTMLLIElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLLIELEMENT_PAS_C}
{$define DOMHTMLLIELEMENT_PAS_C}

{ DOMHTMLLIElement }
  DOMHTMLLIElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: cint); message 'setValue:';
    function value: cint; message 'value';
  end;

{$endif}
{$endif}
