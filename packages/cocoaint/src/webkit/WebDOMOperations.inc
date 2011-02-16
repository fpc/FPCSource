{ Parsed from Webkit.framework WebDOMOperations.h }

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
  WebDOMNodeOperationsCategory = objccategory external (DOMNode)
    function webArchive: WebArchive; message 'webArchive';
  end;

{ WebDOMDocumentOperationsCategory }
  WebDOMDocumentOperationsCategory = objccategory external (DOMDocument)
    function webFrame: WebFrame; message 'webFrame';
    function URLWithAttributeString(string_: NSString): NSURL; message 'URLWithAttributeString:';
  end;

{ WebDOMRangeOperationsCategory }
  WebDOMRangeOperationsCategory = objccategory external (DOMRange)
    function webArchive: WebArchive; message 'webArchive';
    function markupString: NSString; message 'markupString';
  end;

{ WebDOMHTMLFrameElementOperationsCategory }
  WebDOMHTMLFrameElementOperationsCategory = objccategory external (DOMHTMLFrameElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end;

{ WebDOMHTMLIFrameElementOperationsCategory }
  WebDOMHTMLIFrameElementOperationsCategory = objccategory external (DOMHTMLIFrameElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end;

{ WebDOMHTMLObjectElementOperationsCategory }
  WebDOMHTMLObjectElementOperationsCategory = objccategory external (DOMHTMLObjectElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end;

{$endif}
{$endif}
