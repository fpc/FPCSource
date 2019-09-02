{ Parsed from WebKit.framework DOMHTMLTableRowElement.h }


{$ifdef TYPES}
type
  DOMHTMLTableRowElementPtr = ^DOMHTMLTableRowElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTableRowElement = objcclass external (DOMHTMLElement)
  public
    function rowIndex: cint; message 'rowIndex';
    function sectionRowIndex: cint; message 'sectionRowIndex';
    function cells: DOMHTMLCollection; message 'cells';
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setBgColor(newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setCh(newValue: NSString); message 'setCh:';
    function ch: NSString; message 'ch';
    procedure setChOff(newValue: NSString); message 'setChOff:';
    function chOff: NSString; message 'chOff';
    procedure setVAlign(newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    function insertCell (index: cint): DOMHTMLElement; message 'insertCell:';
    procedure deleteCell (index: cint); message 'deleteCell:';
  end;
{$endif}

