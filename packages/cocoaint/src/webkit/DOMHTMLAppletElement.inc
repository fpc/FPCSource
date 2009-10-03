{ Parsed from Webkit.framework DOMHTMLAppletElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setObject (newValue: NSString); message 'setObject:';
    function object_: NSString; message 'object_';
    procedure setWidth (newValue: NSString); message 'setWidth:';
    function width: NSString; message 'width';
  end; external;

{$endif}
{$endif}
