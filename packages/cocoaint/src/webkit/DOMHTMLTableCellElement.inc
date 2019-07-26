{ Parsed from WebKit.framework DOMHTMLTableCellElement.h }


{$ifdef TYPES}
type
  DOMHTMLTableCellElementPtr = ^DOMHTMLTableCellElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTableCellElement = objcclass external (DOMHTMLElement)
  public
    function cellIndex: cint; message 'cellIndex';
    procedure setAbbr(newValue: NSString); message 'setAbbr:';
    function abbr: NSString; message 'abbr';
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAxis(newValue: NSString); message 'setAxis:';
    function axis: NSString; message 'axis';
    procedure setBgColor(newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setCh(newValue: NSString); message 'setCh:';
    function ch: NSString; message 'ch';
    procedure setChOff(newValue: NSString); message 'setChOff:';
    function chOff: NSString; message 'chOff';
    procedure setColSpan(newValue: cint); message 'setColSpan:';
    function colSpan: cint; message 'colSpan';
    procedure setHeaders(newValue: NSString); message 'setHeaders:';
    function headers: NSString; message 'headers';
    procedure setHeight(newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setNoWrap(newValue: ObjCBOOL); message 'setNoWrap:';
    function noWrap: ObjCBOOL; message 'noWrap';
    procedure setRowSpan(newValue: cint); message 'setRowSpan:';
    function rowSpan: cint; message 'rowSpan';
    procedure setScope(newValue: NSString); message 'setScope:';
    function scope: NSString; message 'scope';
    procedure setVAlign(newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    procedure setWidth(newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end;
{$endif}

