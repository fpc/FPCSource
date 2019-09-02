{ Parsed from WebKit.framework DOMHTMLTableSectionElement.h }


{$ifdef TYPES}
type
  DOMHTMLTableSectionElementPtr = ^DOMHTMLTableSectionElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTableSectionElement = objcclass external (DOMHTMLElement)
  public
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setCh(newValue: NSString); message 'setCh:';
    function ch: NSString; message 'ch';
    procedure setChOff(newValue: NSString); message 'setChOff:';
    function chOff: NSString; message 'chOff';
    procedure setVAlign(newValue: NSString); message 'setVAlign:';
    function vAlign: NSString; message 'vAlign';
    function rows: DOMHTMLCollection; message 'rows';
    function insertRow (index: cint): DOMHTMLElement; message 'insertRow:';
    procedure deleteRow (index: cint); message 'deleteRow:';
  end;
{$endif}

