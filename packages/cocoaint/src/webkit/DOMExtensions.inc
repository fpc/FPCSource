{ Parsed from Webkit.framework DOMExtensions.h }

{$ifdef TYPES}
{$ifndef DOMEXTENSIONS_PAS_T}
{$define DOMEXTENSIONS_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMEXTENSIONS_PAS_R}
{$define DOMEXTENSIONS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMEXTENSIONS_PAS_F}
{$define DOMEXTENSIONS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMEXTENSIONS_PAS_S}
{$define DOMEXTENSIONS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef DOMEXTENSIONS_PAS_C}
{$define DOMEXTENSIONS_PAS_C}

{ DOMNodeExtensionsCategory }
  DOMNodeExtensionsCategory = objccategory external (DOMNode)
    function boundingBox: NSRect; message 'boundingBox';
    function lineBoxRects: NSArray; message 'lineBoxRects';
  end;

{ DOMElementAppKitExtensionsCategory }
  DOMElementAppKitExtensionsCategory = objccategory external (DOMElement)
    function image: NSImage; message 'image';
  end;

{ DOMHTMLDocumentExtensionsCategory }
  DOMHTMLDocumentExtensionsCategory = objccategory external (DOMHTMLDocument)
    function createDocumentFragmentWithMarkupString_baseURL(markupString: NSString; baseURL: NSURL): DOMDocumentFragment; message 'createDocumentFragmentWithMarkupString:baseURL:';
    function createDocumentFragmentWithText(text: NSString): DOMDocumentFragment; message 'createDocumentFragmentWithText:';
  end;

{$endif}
{$endif}
