{ Parsed from WebKit.framework DOMHTMLTableElement.h }


{$ifdef TYPES}
type
  DOMHTMLTableElementPtr = ^DOMHTMLTableElement;
{$endif}

{$ifdef CLASSES}

type
  DOMHTMLTableElement = objcclass external (DOMHTMLElement)
  public
    procedure setCaption(newValue: DOMHTMLTableCaptionElement); message 'setCaption:';
    function caption: DOMHTMLTableCaptionElement; message 'caption';
    procedure setTHead(newValue: DOMHTMLTableSectionElement); message 'setTHead:';
    function tHead: DOMHTMLTableSectionElement; message 'tHead';
    procedure setTFoot(newValue: DOMHTMLTableSectionElement); message 'setTFoot:';
    function tFoot: DOMHTMLTableSectionElement; message 'tFoot';
    function rows: DOMHTMLCollection; message 'rows';
    function tBodies: DOMHTMLCollection; message 'tBodies';
    procedure setAlign(newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setBgColor(newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setBorder(newValue: NSString); message 'setBorder:';
    function border: NSString; message 'border';
    procedure setCellPadding(newValue: NSString); message 'setCellPadding:';
    function cellPadding: NSString; message 'cellPadding';
    procedure setCellSpacing(newValue: NSString); message 'setCellSpacing:';
    function cellSpacing: NSString; message 'cellSpacing';
    procedure setFrameBorders(newValue: NSString); message 'setFrameBorders:';
    function frameBorders: NSString; message 'frameBorders';
    procedure setRules(newValue: NSString); message 'setRules:';
    function rules: NSString; message 'rules';
    procedure setSummary(newValue: NSString); message 'setSummary:';
    function summary: NSString; message 'summary';
    procedure setWidth(newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
    function createTHead: DOMHTMLElement; message 'createTHead';
    procedure deleteTHead; message 'deleteTHead';
    function createTFoot: DOMHTMLElement; message 'createTFoot';
    procedure deleteTFoot; message 'deleteTFoot';
    function createCaption: DOMHTMLElement; message 'createCaption';
    procedure deleteCaption; message 'deleteCaption';
    function insertRow (index: cint): DOMHTMLElement; message 'insertRow:';
    procedure deleteRow (index: cint); message 'deleteRow:';
  end;
{$endif}

