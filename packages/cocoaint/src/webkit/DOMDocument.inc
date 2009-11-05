{ Parsed from Webkit.framework DOMDocument.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMDOCUMENT_PAS_C}
{$define DOMDOCUMENT_PAS_C}

{ DOMDocument }
  DOMDocument = objcclass(DOMNode)
    
  public
    class function alloc: DOMDocument; message 'alloc';

    function doctype: DOMDocumentType; message 'doctype';
    function implementation_: DOMImplementation; message 'implementation_';
    function documentElement: DOMElement; message 'documentElement';
    function defaultView: DOMAbstractView; message 'defaultView';
    function styleSheets: DOMStyleSheetList; message 'styleSheets';
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
    function getOverrideStyle_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getOverrideStyle:pseudoElement:';
    function createExpression_resolver(expression: NSString; resolver: id): DOMXPathExpression; message 'createExpression:resolver:';
    function createNSResolver(nodeResolver: DOMNode): id; message 'createNSResolver:';
    function evaluate_contextNode_resolver_type_inResult(expression: NSString; contextNode: DOMNode; resolver: id; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:contextNode:resolver:type:inResult:';
    function createCSSStyleDeclaration: DOMCSSStyleDeclaration; message 'createCSSStyleDeclaration';
    function getComputedStyle_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getComputedStyle:pseudoElement:';
    function getMatchedCSSRules_pseudoElement(element: DOMElement; pseudoElement: NSString): DOMCSSRuleList; message 'getMatchedCSSRules:pseudoElement:';
    function getMatchedCSSRules_pseudoElement_authorOnly(element: DOMElement; pseudoElement: NSString; authorOnly: Boolean): DOMCSSRuleList; message 'getMatchedCSSRules:pseudoElement:authorOnly:';

    { Category: DOMDocumentDeprecated }
    function createProcessingInstruction(target: NSString; data: NSString): DOMProcessingInstruction; message 'createProcessingInstruction:target:';
    function importNode(importedNode: DOMNode; deep: Boolean): DOMNode; message 'importNode:importedNode:';
    function createElementNS(namespaceURI_: NSString; qualifiedName: NSString): DOMElement; message 'createElementNS:namespaceURI:';
    function createAttributeNS(namespaceURI_: NSString; qualifiedName: NSString): DOMAttr; message 'createAttributeNS:namespaceURI:';
    function getElementsByTagNameNS(namespaceURI_: NSString; localName_: NSString): DOMNodeList; message 'getElementsByTagNameNS:namespaceURI:';
    function getOverrideStyle(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getOverrideStyle:element:';
    function createExpression(expression: NSString; resolver: id): DOMXPathExpression; message 'createExpression:expression:';
    function evaluate(expression: NSString; contextNode: DOMNode; resolver: id; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:expression:contextNode:resolver:type:';
    function getComputedStyle(element: DOMElement; pseudoElement: NSString): DOMCSSStyleDeclaration; message 'getComputedStyle:element:';

    { Category: DOMDocumentTraversal }
    function createNodeIterator_whatToShow_filter_expandEntityReferences(root: DOMNode; whatToShow: cuint; filter: id; expandEntityReferences: Boolean): DOMNodeIterator; message 'createNodeIterator:whatToShow:filter:expandEntityReferences:';
    function createTreeWalker_whatToShow_filter_expandEntityReferences(root: DOMNode; whatToShow: cuint; filter: id; expandEntityReferences: Boolean): DOMTreeWalker; message 'createTreeWalker:whatToShow:filter:expandEntityReferences:';

    { Category: DOMDocumentTraversalDeprecated }
    function createNodeIterator(root: DOMNode; whatToShow: cuint; filter: id; expandEntityReferences: Boolean): DOMNodeIterator; message 'createNodeIterator:root:whatToShow:filter:';
    function createTreeWalker(root: DOMNode; whatToShow: cuint; filter: id; expandEntityReferences: Boolean): DOMTreeWalker; message 'createTreeWalker:root:whatToShow:filter:';
  end; external;

{$endif}
{$endif}
