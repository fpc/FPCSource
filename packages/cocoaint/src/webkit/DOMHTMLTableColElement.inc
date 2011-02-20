{ Parsed from Webkit.framework DOMHTMLTableColElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLTABLECOLELEMENT_PAS_T}
{$define DOMHTMLTABLECOLELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTABLECOLELEMENT_PAS_R}
{$define DOMHTMLTABLECOLELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTABLECOLELEMENT_PAS_F}
{$define DOMHTMLTABLECOLELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTABLECOLELEMENT_PAS_S}
{$define DOMHTMLTABLECOLELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTableColElement = objcclass;
  DOMHTMLTableColElementPointer = ^DOMHTMLTableColElement;
  DOMHTMLTableColElementPtr = DOMHTMLTableColElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLECOLELEMENT_PAS_C}
{$define DOMHTMLTABLECOLELEMENT_PAS_C}

{ DOMHTMLTableColElement }
  DOMHTMLTableColElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setCh (newValue: NSString); message 'setCh:';
    function ch: NSString; message 'ch';
    procedure setChOff (newValue: NSString); message 'setChOff:';
    function chOff: NSString; message 'chOff';
    procedure setSpan (newValue: cint); message 'setSpan:';
    function span: cint; message 'span';
    procedure setVAlign (newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end;

{$endif}
{$endif}
