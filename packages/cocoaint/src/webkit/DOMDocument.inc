{ Parsed from Webkit.framework DOMDocument.h }

{$ifdef TYPES}
{$ifndef DOMDOCUMENT_PAS_T}
{$define DOMDOCUMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMDOCUMENT_PAS_R}
{$define DOMDOCUMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMDOCUMENT_PAS_F}
{$define DOMDOCUMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMDOCUMENT_PAS_S}
{$define DOMDOCUMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMDocument = objcclass;
  DOMDocumentPointer = ^DOMDocument;
  DOMDocumentPtr = DOMDocumentPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMDOCUMENT_PAS_C}
{$define DOMDOCUMENT_PAS_C}

{ DOMDocument }
  DOMDocument = objcclass external (DOMNode)
    
  public
    function doctype: DOMDocumentType; message 'doctype';
    function implementation_: DOMImplementation; message 'implementation_';
    function documentElement: DOMElement; message 'documentElement';
    function inputEncoding: NSString; message 'inputEncoding';
    function xmlEncoding: NSString; message 'xmlEncoding';
    procedure setXmlVersion (newValue: NSString); message 'setXmlVersion:';
    function xmlVersion: NSString; message 'xmlVersion';
    procedure setXmlStandalone (newValue: Boolean); message 'setXmlStandalone:';
    function xmlStandalone: Boolean; message 'xmlStandalone';
    procedure setDocumentURI (newValue: NSString); message 'setDocumentURI:';
    function documentURI: NSString; message 'documentURI';
    function defaultView: DOMAbstractView; message 'defaultView';
    function styleSheets: DOMStyleSheetList; message 'styleSheets';
    procedure setTitle (newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    function referrer: NSString; message 'referrer';
    function domain: NSString; message 'domain';
    function uRL: NSString; message 'uRL';
    procedure setCookie (newValue: NSString); message 'setCookie:';
    function cookie: NSString; message 'cookie';
    procedure setBody (newValue: DOMHTMLElement); message 'setBody:';
    function body: DOMHTMLElement; message 'body';
    function images: DOMHTMLCollection; message 'images';
    function applets: DOMHTMLCollection; message 'applets';
    function links: DOMHTMLCollection; message 'links';
    function forms: DOMHTMLCollection; message 'forms';
    function anchors: DOMHTMLCollection; message 'anchors';
    function lastModified: NSString; message 'lastModified';
    procedure setCharset (newValue: NSString); message 'setCharset:';
    function charset: NSString; message 'charset';
    function defaultCharset: NSString; message 'defaultCharset';
    function readyState: NSString; message 'readyState';
    function characterSet: NSString; message 'characterSet';
    function preferredStylesheetSet: NSString; message 'preferredStylesheetSet';
    procedure setSelectedStylesheetSet (newValue: NSString); message 'setSelectedStylesheetSet:';
    function selectedStylesheetSet: NSString; message 'selectedStylesheetSet';
    function createElement(tagName: NSString): DOMElement; message 'createElement:';
    function createDocumentFragment: DOMDocumentFragment; message 'createDocumentFragment';
    function createTextNode(data: NSString): DOMText; message 'createTextNode:';
    function createComment(data: NSString): DOMComment; message 'createComment:';
    function createCDATASection(data: NSString): DOMCDATASection; message 'createCDATASection:';
    function createProcessingInstruction_data(target: NSString; data: NSString): DOMProcessingInstruction; message 'createProcessingInstruction:data:';
    function createAttribute(name: NSString): DOMAttr; message 'createAttribute:';
    function createEntityReference(name: NSString): DOMEntityReference; message 'createEntityReference:';
    function getElementsByTagName(tagname: NSString): DOMNodeList; message 'getElementsByTagName:';
    function importNode_deep(importedNode: DOMNode; deep: Boolean): DOMNode; message 'importNode:deep:';
    function createElementNS_qualifiedName(namespaceURI_: NSString; qualifiedName: NSString): DOMElement; message 'createElementNS:qualifiedName:';
    function createAttributeNS_qualifiedName(namespaceURI_: NSString; qualifiedName: NSString): DOMAttr; message 'createAttributeNS:qualifiedName:';
    function getElementsByTagNameNS_localName(namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:localName:';
    function getElementById(elementId: NSString): DOMElement; message 'getElementById:';
    function adoptNode(source: DOMNode): DOMNode; message 'adoptNode:';
    function createEvent(eventType: NSString): DOMEvent; message 'createEvent:';
    function createRange: DOMRange; message 'createRange';
    function createNodeIterator_whatToShow_filter_expandEntityReferences(root: DOMNode; whatToShow: cuint; filter: DOMNodeFilterProtocol; expandEntityReferences: Boolean): DOMNodeIterator; message 'createNodeIterator:whatToShow:filter:expandEntityReferences:';
    function createTreeWalker_whatToShow_filter_expandEntityReferences(root: DOMNode; whatToShow: cuint; filter: DOMNodeFilterProtocol; expandEntityReferences: Boolean): DOMTreeWalker; message 'createTreeWalker:whatToShow:filter:expandEntityReferences:';
    function getOverrideStyle_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getOverrideStyle:pseudoElement:';
    function createExpression_resolver(expression: NSString; resolver: DOMXPathNSResolverProtocol): DOMXPathExpression; message 'createExpression:resolver:';
    function createNSResolver(nodeResolver: DOMNode): DOMXPathNSResolverProtocol; message 'createNSResolver:';
    function evaluate_contextNode_resolver_type_inResult(expression: NSString; contextNode: DOMNode; resolver: DOMXPathNSResolverProtocol; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:contextNode:resolver:type:inResult:';
    function execCommand_userInterface_value(command: NSString; userInterface: Boolean; value: NSString): Boolean; message 'execCommand:userInterface:value:';
    function execCommand_userInterface(command: NSString; userInterface: Boolean): Boolean; message 'execCommand:userInterface:';
    function execCommand(command: NSString): Boolean; message 'execCommand:';
    function queryCommandEnabled(command: NSString): Boolean; message 'queryCommandEnabled:';
    function queryCommandIndeterm(command: NSString): Boolean; message 'queryCommandIndeterm:';
    function queryCommandState(command: NSString): Boolean; message 'queryCommandState:';
    function queryCommandSupported(command: NSString): Boolean; message 'queryCommandSupported:';
    function queryCommandValue(command: NSString): NSString; message 'queryCommandValue:';
    function getElementsByName(elementName: NSString): DOMNodeList; message 'getElementsByName:';
    function elementFromPoint_y(x: cint; y: cint): DOMElement; message 'elementFromPoint:y:';
    function createCSSStyleDeclaration: DOMCSSStyleDeclaration; message 'createCSSStyleDeclaration';
    function getComputedStyle_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getComputedStyle:pseudoElement:';
    function getMatchedCSSRules_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSRuleList; message 'getMatchedCSSRules:pseudoElement:';
    function getMatchedCSSRules_pseudoElement_authorOnly(element: DOMElement; pseudoElement: NSString; authorOnly: Boolean): DOMCSSRuleList; message 'getMatchedCSSRules:pseudoElement:authorOnly:';
    function getElementsByClassName(tagname: NSString): DOMNodeList; message 'getElementsByClassName:';
    function querySelector(selectors: NSString): DOMElement; message 'querySelector:';
    function querySelectorAll(selectors: NSString): DOMNodeList; message 'querySelectorAll:';
  end;

{ DOMDocumentDeprecatedCategory }
  DOMDocumentDeprecatedCategory = objccategory external (DOMDocument)
    function createProcessingInstruction(target: NSString; data: NSString): DOMProcessingInstruction; message 'createProcessingInstruction:target:'; deprecated 'in_webkit_version_3 and later';
    function importNode(importedNode: DOMNode; deep: Boolean): DOMNode; message 'importNode:importedNode:'; deprecated 'in_webkit_version_3 and later';
    function createElementNS(namespaceURI_: NSString; qualifiedName: NSString): DOMElement; message 'createElementNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function createAttributeNS(namespaceURI_: NSString; qualifiedName: NSString): DOMAttr; message 'createAttributeNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function getElementsByTagNameNS(namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:namespaceURI:'; deprecated 'in_webkit_version_3 and later';
    function createNodeIterator(root: DOMNode; whatToShow: cuint; filter: DOMNodeFilterProtocol; expandEntityReferences: Boolean): DOMNodeIterator; message 'createNodeIterator:root:whatToShow:filter:'; deprecated 'in_webkit_version_3 and later';
    function createTreeWalker(root: DOMNode; whatToShow: cuint; filter: DOMNodeFilterProtocol; expandEntityReferences: Boolean): DOMTreeWalker; message 'createTreeWalker:root:whatToShow:filter:'; deprecated 'in_webkit_version_3 and later';
    function getOverrideStyle(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getOverrideStyle:element:'; deprecated 'in_webkit_version_3 and later';
    function createExpression(expression: NSString; resolver: DOMXPathNSResolverProtocol): DOMXPathExpression; message 'createExpression:expression:';
    function evaluate(expression: NSString; contextNode: DOMNode; resolver: DOMXPathNSResolverProtocol; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:expression:contextNode:resolver:type:';
    function getComputedStyle(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getComputedStyle:element:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
