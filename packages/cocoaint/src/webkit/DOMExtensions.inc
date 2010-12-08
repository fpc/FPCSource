{ Parsed from Webkit.framework DOMExtensions.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMNodeExtensionsCategory = objccategory(DOMNode)
  end; external;

{ DOMElementAppKitExtensionsCategory }
  DOMElementAppKitExtensionsCategory = objccategory(DOMElement)
  end; external;

{ DOMHTMLDocumentExtensionsCategory }
  DOMHTMLDocumentExtensionsCategory = objccategory(DOMHTMLDocument)
    function createDocumentFragmentWithMarkupString_baseURL(markupString: NSString; baseURL: NSURL): DOMDocumentFragment; message 'createDocumentFragmentWithMarkupString:baseURL:';
    function createDocumentFragmentWithText(text: NSString): DOMDocumentFragment; message 'createDocumentFragmentWithText:';
  end; external;

{$endif}
{$endif}
