{ Parsed from Webkit.framework DOMHTMLObjectElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLOBJECTELEMENT_PAS_T}
{$define DOMHTMLOBJECTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLOBJECTELEMENT_PAS_R}
{$define DOMHTMLOBJECTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLOBJECTELEMENT_PAS_F}
{$define DOMHTMLOBJECTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLOBJECTELEMENT_PAS_S}
{$define DOMHTMLOBJECTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLObjectElement = objcclass;
  DOMHTMLObjectElementPointer = ^DOMHTMLObjectElement;
  DOMHTMLObjectElementPtr = DOMHTMLObjectElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOBJECTELEMENT_PAS_C}
{$define DOMHTMLOBJECTELEMENT_PAS_C}

{ DOMHTMLObjectElement }
  DOMHTMLObjectElement = objcclass external (DOMHTMLElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setCode (newValue: NSString); message 'setCode:';
    function code: NSString; message 'code';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setArchive (newValue: NSString); message 'setArchive:';
    function archive: NSString; message 'archive';
    procedure setBorder (newValue: NSString); message 'setBorder:';
    function border: NSString; message 'border';
    procedure setCodeBase (newValue: NSString); message 'setCodeBase:';
    function codeBase: NSString; message 'codeBase';
    procedure setCodeType (newValue: NSString); message 'setCodeType:';
    function codeType: NSString; message 'codeType';
    procedure setData (newValue: NSString); message 'setData:';
    function data: NSString; message 'data';
    procedure setDeclare (newValue: Boolean); message 'setDeclare:';
    function declare: Boolean; message 'declare';
    procedure setHeight (newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setHspace (newValue: cint); message 'setHspace:';
    function hspace: cint; message 'hspace';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setStandby (newValue: NSString); message 'setStandby:';
    function standby: NSString; message 'standby';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setUseMap (newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    procedure setVspace (newValue: cint); message 'setVspace:';
    function vspace: cint; message 'vspace';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
    function contentDocument: DOMDocument; message 'contentDocument';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
  end;

{$endif}
{$endif}
