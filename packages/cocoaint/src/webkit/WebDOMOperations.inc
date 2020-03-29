{ Parsed from WebKit.framework WebDOMOperations.h }

{$ifdef CLASSES}

type
  WebDOMNodeOperations = objccategory external (DOMNode)
    function webArchive: WebArchive; message 'webArchive';
  end;

type
  WebDOMDocumentOperations = objccategory external (DOMDocument)
    function webFrame: WebFrame; message 'webFrame';
    function URLWithAttributeString (string_: NSString): NSURL; message 'URLWithAttributeString:';
  end;

type
  WebDOMRangeOperations = objccategory external (DOMRange)
    function webArchive: WebArchive; message 'webArchive';
    function markupString: NSString; message 'markupString';
  end;

type
  WebDOMHTMLFrameElementOperations = objccategory external (DOMHTMLFrameElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end;

type
  WebDOMHTMLIFrameElementOperations = objccategory external (DOMHTMLIFrameElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end;

type
  WebDOMHTMLObjectElementOperations = objccategory external (DOMHTMLObjectElement)
    function contentFrame: WebFrame; message 'contentFrame';
  end;
{$endif}

