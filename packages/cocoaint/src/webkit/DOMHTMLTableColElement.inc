{ Parsed from Webkit.framework DOMHTMLTableColElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLECOLELEMENT_PAS_C}
{$define DOMHTMLTABLECOLELEMENT_PAS_C}

{ DOMHTMLTableColElement }
  DOMHTMLTableColElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLTableColElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setCh (newValue: NSString); message 'setCh:';
    function ch: NSString; message 'ch';
    procedure setChOff (newValue: NSString); message 'setChOff:';
    function chOff: NSString; message 'chOff';
    procedure setVAlign (newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end; external;

{$endif}
{$endif}
