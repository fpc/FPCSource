{ Parsed from Webkit.framework DOMHTMLTableCellElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLTABLECELLELEMENT_PAS_T}
{$define DOMHTMLTABLECELLELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTABLECELLELEMENT_PAS_R}
{$define DOMHTMLTABLECELLELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTABLECELLELEMENT_PAS_F}
{$define DOMHTMLTABLECELLELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTABLECELLELEMENT_PAS_S}
{$define DOMHTMLTABLECELLELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTableCellElement = objcclass;
  DOMHTMLTableCellElementPointer = ^DOMHTMLTableCellElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLECELLELEMENT_PAS_C}
{$define DOMHTMLTABLECELLELEMENT_PAS_C}

{ DOMHTMLTableCellElement }
  DOMHTMLTableCellElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLTableCellElement; message 'alloc';

    function cellIndex: cint; message 'cellIndex';
    procedure setAbbr (newValue: NSString); message 'setAbbr:';
    function abbr: NSString; message 'abbr';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAxis (newValue: NSString); message 'setAxis:';
    function axis: NSString; message 'axis';
    procedure setBgColor (newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setCh (newValue: NSString); message 'setCh:';
    function ch: NSString; message 'ch';
    procedure setChOff (newValue: NSString); message 'setChOff:';
    function chOff: NSString; message 'chOff';
    procedure setHeaders (newValue: NSString); message 'setHeaders:';
    function headers: NSString; message 'headers';
    procedure setHeight (newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setScope (newValue: NSString); message 'setScope:';
    function scope: NSString; message 'scope';
    procedure setVAlign (newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end; external;

{$endif}
{$endif}
