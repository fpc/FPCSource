{ Parsed from Webkit.framework DOMHTMLTableElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLTABLEELEMENT_PAS_T}
{$define DOMHTMLTABLEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTABLEELEMENT_PAS_R}
{$define DOMHTMLTABLEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTABLEELEMENT_PAS_F}
{$define DOMHTMLTABLEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTABLEELEMENT_PAS_S}
{$define DOMHTMLTABLEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTableElement = objcclass;
  DOMHTMLTableElementPointer = ^DOMHTMLTableElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTABLEELEMENT_PAS_C}
{$define DOMHTMLTABLEELEMENT_PAS_C}

{ DOMHTMLTableElement }
  DOMHTMLTableElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLTableElement; message 'alloc';

    procedure setCaption (newValue: DOMHTMLTableCaptionElement); message 'setCaption:';
    function caption: DOMHTMLTableCaptionElement; message 'caption';
    procedure setTHead (newValue: DOMHTMLTableSectionElement); message 'setTHead:';
    function tHead: DOMHTMLTableSectionElement; message 'tHead';
    procedure setTFoot (newValue: DOMHTMLTableSectionElement); message 'setTFoot:';
    function tFoot: DOMHTMLTableSectionElement; message 'tFoot';
    function rows: DOMHTMLCollection; message 'rows';
    function tBodies: DOMHTMLCollection; message 'tBodies';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setBgColor (newValue: NSString); message 'setBgColor:';
    function bgColor: NSString; message 'bgColor';
    procedure setBorder (newValue: NSString); message 'setBorder:';
    function border: NSString; message 'border';
    procedure setCellPadding (newValue: NSString); message 'setCellPadding:';
    function cellPadding: NSString; message 'cellPadding';
    procedure setCellSpacing (newValue: NSString); message 'setCellSpacing:';
    function cellSpacing: NSString; message 'cellSpacing';
    procedure setFrameBorders (newValue: NSString); message 'setFrameBorders:';
    function frameBorders: NSString; message 'frameBorders';
    procedure setRules (newValue: NSString); message 'setRules:';
    function rules: NSString; message 'rules';
    procedure setSummary (newValue: NSString); message 'setSummary:';
    function summary: NSString; message 'summary';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
    function createTHead: DOMHTMLElement; message 'createTHead';
    procedure deleteTHead; message 'deleteTHead';
    function createTFoot: DOMHTMLElement; message 'createTFoot';
    procedure deleteTFoot; message 'deleteTFoot';
    function createCaption: DOMHTMLElement; message 'createCaption';
    procedure deleteCaption; message 'deleteCaption';
    function insertRow(index: cint): DOMHTMLElement; message 'insertRow:';
    procedure deleteRow(index: cint); message 'deleteRow:';
  end; external;

{$endif}
{$endif}
