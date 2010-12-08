{ Parsed from Webkit.framework WebDOMOperations.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef WEBDOMOPERATIONS_PAS_T}
{$define WEBDOMOPERATIONS_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBDOMOPERATIONS_PAS_R}
{$define WEBDOMOPERATIONS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBDOMOPERATIONS_PAS_F}
{$define WEBDOMOPERATIONS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBDOMOPERATIONS_PAS_S}
{$define WEBDOMOPERATIONS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef WEBDOMOPERATIONS_PAS_C}
{$define WEBDOMOPERATIONS_PAS_C}

{ WebDOMNodeOperationsCategory }
  WebDOMNodeOperationsCategory = objccategory(DOMNode)
    function webArchive: WebArchive; message 'webArchive';
  end; external;

{ WebDOMDocumentOperationsCategory }
  WebDOMDocumentOperationsCategory = objccategory(DOMDocument)
    function webFrame: WebFrame; message 'webFrame';
    function URLWithAttributeString(string_: NSString): NSURL; message 'URLWithAttributeString:';
  end; external;

{ WebDOMRangeOperationsCategory }
  WebDOMRangeOperationsCategory = objccategory(DOMRange)
    function webArchive: WebArchive; message 'webArchive';
    function markupString: NSString; message 'markupString';
  end; external;

{ WebDOMHTMLFrameElementOperationsCategory }
  WebDOMHTMLFrameElementOperationsCategory = objccategory(DOMHTMLFrameElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end; external;

{ WebDOMHTMLIFrameElementOperationsCategory }
  WebDOMHTMLIFrameElementOperationsCategory = objccategory(DOMHTMLIFrameElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end; external;

{ WebDOMHTMLObjectElementOperationsCategory }
  WebDOMHTMLObjectElementOperationsCategory = objccategory(DOMHTMLObjectElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end; external;

{$endif}
{$endif}
