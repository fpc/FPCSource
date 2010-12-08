{ Parsed from Webkit.framework DOMHTMLAppletElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMHTMLAPPLETELEMENT_PAS_T}
{$define DOMHTMLAPPLETELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLAPPLETELEMENT_PAS_R}
{$define DOMHTMLAPPLETELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLAPPLETELEMENT_PAS_F}
{$define DOMHTMLAPPLETELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLAPPLETELEMENT_PAS_S}
{$define DOMHTMLAPPLETELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLAppletElement = objcclass;
  DOMHTMLAppletElementPointer = ^DOMHTMLAppletElement;
  DOMHTMLAppletElementPtr = DOMHTMLAppletElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLAPPLETELEMENT_PAS_C}
{$define DOMHTMLAPPLETELEMENT_PAS_C}

{ DOMHTMLAppletElement }
  DOMHTMLAppletElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLAppletElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setArchive (newValue: NSString); message 'setArchive:';
    function archive: NSString; message 'archive';
    procedure setCode (newValue: NSString); message 'setCode:';
    function code: NSString; message 'code';
    procedure setCodeBase (newValue: NSString); message 'setCodeBase:';
    function codeBase: NSString; message 'codeBase';
    procedure setHeight (newValue: NSString); message 'setHeight:';
    function height: NSString; message 'height';
    procedure setHspace (newValue: cint); message 'setHspace:';
    function hspace: cint; message 'hspace';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setObject (newValue: NSString); message 'setObject:';
    function object_: NSString; message 'object_';
    procedure setVspace (newValue: cint); message 'setVspace:';
    function vspace: cint; message 'vspace';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end; external;

{$endif}
{$endif}
