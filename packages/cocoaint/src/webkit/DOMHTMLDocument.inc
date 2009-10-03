{ Parsed from Webkit.framework DOMHTMLDocument.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLDOCUMENT_PAS_T}
{$define DOMHTMLDOCUMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLDOCUMENT_PAS_R}
{$define DOMHTMLDOCUMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLDOCUMENT_PAS_F}
{$define DOMHTMLDOCUMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLDOCUMENT_PAS_S}
{$define DOMHTMLDOCUMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLDocument = objcclass;
  DOMHTMLDocumentPointer = ^DOMHTMLDocument;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDOCUMENT_PAS_C}
{$define DOMHTMLDOCUMENT_PAS_C}

{ DOMHTMLDocument }
  DOMHTMLDocument = objcclass(DOMDocument)
    
  public
    class function alloc: DOMHTMLDocument; message 'alloc';

    procedure setTitle (newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function referrer: NSString; message 'referrer';
    function domain: NSString; message 'domain';
    function uRL: NSString; message 'uRL';
    procedure setBody (newValue: DOMHTMLElement); message 'setBody:';
    function body: DOMHTMLElement; message 'body';
    function images: DOMHTMLCollection; message 'images';
    function applets: DOMHTMLCollection; message 'applets';
    function links: DOMHTMLCollection; message 'links';
    function forms: DOMHTMLCollection; message 'forms';
    function anchors: DOMHTMLCollection; message 'anchors';
    procedure setCookie (newValue: NSString); message 'setCookie:';
    function cookie: NSString; message 'cookie';
    procedure open; message 'open';
    procedure close; message 'close';
    procedure write(text: NSStringPointer); message 'write:';
    procedure writeln(text: NSStringPointer); message 'writeln:';
    function getElementsByName(elementName: NSStringPointer): DOMNodeList; message 'getElementsByName:';
    function getElementById(elementId: NSStringPointer): DOMElement; message 'getElementById:';

    { Category: DOMHTMLDocumentExtensions }
    function createDocumentFragmentWithMarkupString_baseURL(markupString: NSStringPointer; baseURL: NSURLPointer): DOMDocumentFragment; message 'createDocumentFragmentWithMarkupString:baseURL:';
    function createDocumentFragmentWithText(text: NSStringPointer): DOMDocumentFragment; message 'createDocumentFragmentWithText:';
  end; external;

{$endif}
{$endif}
