{ Parsed from Webkit.framework DOMHTMLTableCellElement.h }

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
  DOMHTMLTableCellElementPtr = DOMHTMLTableCellElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLECELLELEMENT_PAS_C}
{$define DOMHTMLTABLECELLELEMENT_PAS_C}

{ DOMHTMLTableCellElement }
  DOMHTMLTableCellElement = objcclass external (DOMHTMLElement)
    
  public
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
    procedure setColSpan (newValue: cint); message 'setColSpan:';
    function colSpan: cint; message 'colSpan';
    procedure setHeaders (newValue: NSString); message 'setHeaders:';
    function headers: NSString; message 'headers';
    procedure setHeight (newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setNoWrap (newValue: Boolean); message 'setNoWrap:';
    function noWrap: Boolean; message 'noWrap';
    procedure setRowSpan (newValue: cint); message 'setRowSpan:';
    function rowSpan: cint; message 'rowSpan';
    procedure setScope (newValue: NSString); message 'setScope:';
    function scope: NSString; message 'scope';
    procedure setVAlign (newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end;

{$endif}
{$endif}
