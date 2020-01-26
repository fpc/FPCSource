{ Parsed from WebKit.framework DOMDocument.h }


{$ifdef TYPES}
type
  DOMDocumentPtr = ^DOMDocument;
{$endif}

{$ifdef CLASSES}

type
  DOMDocument = objcclass external (DOMNode)
  public
    function doctype: DOMDocumentType; message 'doctype';
    function implementation_: DOMImplementation; message 'implementation';
    function documentElement: DOMElement; message 'documentElement';
    function inputEncoding: NSString; message 'inputEncoding';
    function xmlEncoding: NSString; message 'xmlEncoding';
    procedure setXmlVersion(newValue: NSString); message 'setXmlVersion:';
    function xmlVersion: NSString; message 'xmlVersion';
    procedure setXmlStandalone(newValue: ObjCBOOL); message 'setXmlStandalone:';
    function xmlStandalone: ObjCBOOL; message 'xmlStandalone';
    procedure setDocumentURI(newValue: NSString); message 'setDocumentURI:';
    function documentURI: NSString; message 'documentURI';
    function defaultView: DOMAbstractView; message 'defaultView';
    function styleSheets: DOMStyleSheetList; message 'styleSheets';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function referrer: NSString; message 'referrer';
    function domain: NSString; message 'domain';
    function URL: NSString; message 'URL';
    procedure setCookie(newValue: NSString); message 'setCookie:';
    function cookie: NSString; message 'cookie';
    procedure setBody(newValue: DOMHTMLElement); message 'setBody:';
    function body: DOMHTMLElement; message 'body';
    function images: DOMHTMLCollection; message 'images';
    function applets: DOMHTMLCollection; message 'applets';
    function links: DOMHTMLCollection; message 'links';
    function forms: DOMHTMLCollection; message 'forms';
    function anchors: DOMHTMLCollection; message 'anchors';
    function lastModified: NSString; message 'lastModified';
    procedure setCharset(newValue: NSString); message 'setCharset:';
    function charset: NSString; message 'charset';
    function defaultCharset: NSString; message 'defaultCharset';
    function readyState: NSString; message 'readyState';
    function characterSet: NSString; message 'characterSet';
    function preferredStylesheetSet: NSString; message 'preferredStylesheetSet';
    procedure setSelectedStylesheetSet(newValue: NSString); message 'setSelectedStylesheetSet:';
    function selectedStylesheetSet: NSString; message 'selectedStylesheetSet';
    function activeElement: DOMElement; message 'activeElement';
    function createElement (tagName: NSString): DOMElement; message 'createElement:';
    function createDocumentFragment: DOMDocumentFragment; message 'createDocumentFragment';
    function createTextNode (data: NSString): DOMText; message 'createTextNode:';
    function createComment (data: NSString): DOMComment; message 'createComment:';
    function createCDATASection (data: NSString): DOMCDATASection; message 'createCDATASection:';
    function createProcessingInstruction_data (target: NSString; data: NSString): DOMProcessingInstruction; message 'createProcessingInstruction:data:'; { available in 10_5 }
    function createAttribute (name: NSString): DOMAttr; message 'createAttribute:';
    function createEntityReference (name: NSString): DOMEntityReference; message 'createEntityReference:';
    function getElementsByTagName (tagname: NSString): DOMNodeList; message 'getElementsByTagName:';
    function importNode_deep (importedNode: DOMNode; deep: ObjCBOOL): DOMNode; message 'importNode:deep:'; { available in 10_5 }
    function createElementNS_qualifiedName (namespaceURI_: NSString; qualifiedName: NSString): DOMElement; message 'createElementNS:qualifiedName:'; { available in 10_5 }
    function createAttributeNS_qualifiedName (namespaceURI_: NSString; qualifiedName: NSString): DOMAttr; message 'createAttributeNS:qualifiedName:'; { available in 10_5 }
    function getElementsByTagNameNS_localName (namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:localName:'; { available in 10_5 }
    function getElementById (elementId: NSString): DOMElement; message 'getElementById:';
    function adoptNode (source: DOMNode): DOMNode; message 'adoptNode:'; { available in 10_5 }
    function createEvent (eventType: NSString): DOMEvent; message 'createEvent:';
    function createRange: DOMRange; message 'createRange';
    function createNodeIterator_whatToShow_filter_expandEntityReferences (root: DOMNode; whatToShow: cuint; filter: DOMNodeFilterProtocol; expandEntityReferences: ObjCBOOL): DOMNodeIterator; message 'createNodeIterator:whatToShow:filter:expandEntityReferences:'; { available in 10_5 }
    function createTreeWalker_whatToShow_filter_expandEntityReferences (root: DOMNode; whatToShow: cuint; filter: DOMNodeFilterProtocol; expandEntityReferences: ObjCBOOL): DOMTreeWalker; message 'createTreeWalker:whatToShow:filter:expandEntityReferences:'; { available in 10_5 }
    function getOverrideStyle_pseudoElement (element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getOverrideStyle:pseudoElement:'; { available in 10_5 }
    function createExpression_resolver (expression: NSString; resolver: DOMXPathNSResolverProtocol): DOMXPathExpression; message 'createExpression:resolver:'; { available in 10_5 }
    function createNSResolver (nodeResolver: DOMNode): DOMXPathNSResolverProtocol; message 'createNSResolver:'; { available in 10_5 }
    function evaluate_contextNode_resolver_type_inResult (expression: NSString; contextNode: DOMNode; resolver: DOMXPathNSResolverProtocol; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:contextNode:resolver:type:inResult:'; { available in 10_5 }
    function execCommand_userInterface_value (command: NSString; userInterface: ObjCBOOL; value: NSString): ObjCBOOL; message 'execCommand:userInterface:value:'; { available in 10_5 }
    function execCommand_userInterface (command: NSString; userInterface: ObjCBOOL): ObjCBOOL; message 'execCommand:userInterface:'; { available in 10_5 }
    function execCommand (command: NSString): ObjCBOOL; message 'execCommand:'; { available in 10_5 }
    function queryCommandEnabled (command: NSString): ObjCBOOL; message 'queryCommandEnabled:'; { available in 10_5 }
    function queryCommandIndeterm (command: NSString): ObjCBOOL; message 'queryCommandIndeterm:'; { available in 10_5 }
    function queryCommandState (command: NSString): ObjCBOOL; message 'queryCommandState:'; { available in 10_5 }
    function queryCommandSupported (command: NSString): ObjCBOOL; message 'queryCommandSupported:'; { available in 10_5 }
    function queryCommandValue (command: NSString): NSString; message 'queryCommandValue:'; { available in 10_5 }
    function getElementsByName (elementName: NSString): DOMNodeList; message 'getElementsByName:';
    function elementFromPoint_y (x: cint; y: cint): DOMElement; message 'elementFromPoint:y:'; { available in 10_5 }
    function createCSSStyleDeclaration: DOMCSSStyleDeclaration; message 'createCSSStyleDeclaration'; { available in 10_5 }
    function getComputedStyle_pseudoElement (element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getComputedStyle:pseudoElement:'; { available in 10_5 }
    function getMatchedCSSRules_pseudoElement (element: DOMElement; pseudoElement: NSString): DOMCSSRuleList; message 'getMatchedCSSRules:pseudoElement:'; { available in 10_5 }
    function getMatchedCSSRules_pseudoElement_authorOnly (element: DOMElement; pseudoElement: NSString; authorOnly: ObjCBOOL): DOMCSSRuleList; message 'getMatchedCSSRules:pseudoElement:authorOnly:'; { available in 10_5 }
    function getElementsByClassName (tagname: NSString): DOMNodeList; message 'getElementsByClassName:'; { available in 10_6 }
    function hasFocus: ObjCBOOL; message 'hasFocus'; { available in 10_6 }
    function querySelector (selectors: NSString): DOMElement; message 'querySelector:'; { available in 10_6 }
    function querySelectorAll (selectors: NSString): DOMNodeList; message 'querySelectorAll:'; { available in 10_6 }
    procedure webkitCancelFullScreen; message 'webkitCancelFullScreen'; { available in 10_6 }
  end;


type
  DOMDocumentDeprecated = objccategory external (DOMDocument)
    function createProcessingInstruction (target: NSString): DOMProcessingInstruction; message 'createProcessingInstruction:'; deprecated 'in 10_4, 10_5';
    function importNode (importedNode: DOMNode): DOMNode; message 'importNode:'; deprecated 'in 10_4, 10_5';
    function createElementNS (namespaceURI_: NSString): DOMElement; message 'createElementNS:'; deprecated 'in 10_4, 10_5';
    function createAttributeNS (namespaceURI_: NSString): DOMAttr; message 'createAttributeNS:'; deprecated 'in 10_4, 10_5';
    function getElementsByTagNameNS (namespaceURI_: NSString): DOMNodeList; message 'getElementsByTagNameNS:'; deprecated 'in 10_4, 10_5';
    function createNodeIterator_whatToShow (root: DOMNode; filter: DOMNodeFilterProtocol): DOMNodeIterator; message 'createNodeIterator:whatToShow:'; deprecated 'in 10_4, 10_5';
    function createTreeWalker_whatToShow (root: DOMNode; filter: DOMNodeFilterProtocol): DOMTreeWalker; message 'createTreeWalker:whatToShow:'; deprecated 'in 10_4, 10_5';
    function getOverrideStyle (element: DOMElement): DOMCSSStyleDeclaration; message 'getOverrideStyle:'; deprecated 'in 10_4, 10_5';
    function createExpression (expression: NSString): DOMXPathExpression; message 'createExpression:'; deprecated 'in 10_5, 10_5';
    function evaluate_contextNode_type (expression: NSString; resolver: DOMXPathNSResolverProtocol; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:contextNode:type:'; deprecated 'in 10_5, 10_5';
    function getComputedStyle (element: DOMElement): DOMCSSStyleDeclaration; message 'getComputedStyle:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

