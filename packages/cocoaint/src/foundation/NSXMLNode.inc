{ Parsed from Foundation.framework NSXMLNode.h }

{$ifdef TYPES}
{$ifndef NSXMLNODE_PAS_T}
{$define NSXMLNODE_PAS_T}

{ Constants }

const
  NSXMLInvalidKind = 0;
  NSXMLDocumentKind = 1;
  NSXMLElementKind = 2;
  NSXMLAttributeKind = 3;
  NSXMLNamespaceKind = 4;
  NSXMLProcessingInstructionKind = 5;
  NSXMLCommentKind = 6;
  NSXMLTextKind = 7;
  NSXMLDTDKind = 8;
  NSXMLEntityDeclarationKind = 9;
  NSXMLAttributeDeclarationKind = 10;
  NSXMLElementDeclarationKind = 11;
  NSXMLNotationDeclarationKind = 12;

{ Types }
type
  NSXMLNodeKind = NSUInteger;
  NSXMLNodeKindPtr = ^NSXMLNodeKind;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSXMLNODE_PAS_R}
{$define NSXMLNODE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSXMLNODE_PAS_F}
{$define NSXMLNODE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSXMLNODE_PAS_S}
{$define NSXMLNODE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLNode = objcclass;
  NSXMLNodePointer = ^NSXMLNode;
  NSXMLNodePtr = NSXMLNodePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLNODE_PAS_C}
{$define NSXMLNODE_PAS_C}

{ NSXMLNode }
  NSXMLNode = objcclass external (NSObject, NSCopyingProtocol)
  private
    _kind: NSXMLNodeKind;
    _parent: NSXMLNode;
    _index: NSUInteger;
    _objectValue: id;
    
  public
    function initWithKind(kind_: NSXMLNodeKind): id; message 'initWithKind:';
    function initWithKind_options(kind_: NSXMLNodeKind; options: NSUInteger): id; message 'initWithKind:options:';
    class function document: id; message 'document';
    class function documentWithRootElement(element: NSXMLElement): id; message 'documentWithRootElement:';
    class function elementWithName(name_: NSString): id; message 'elementWithName:';
    class function elementWithName_URI(name_: NSString; URI_: NSString): id; message 'elementWithName:URI:';
    class function elementWithName_stringValue(name_: NSString; string_: NSString): id; message 'elementWithName:stringValue:';
    class function elementWithName_children_attributes(name_: NSString; children_: NSArray; attributes: NSArray): id; message 'elementWithName:children:attributes:';
    class function attributeWithName_stringValue(name_: NSString; stringValue_: NSString): id; message 'attributeWithName:stringValue:';
    class function attributeWithName_URI_stringValue(name_: NSString; URI_: NSString; stringValue_: NSString): id; message 'attributeWithName:URI:stringValue:';
    class function namespaceWithName_stringValue(name_: NSString; stringValue_: NSString): id; message 'namespaceWithName:stringValue:';
    class function processingInstructionWithName_stringValue(name_: NSString; stringValue_: NSString): id; message 'processingInstructionWithName:stringValue:';
    class function commentWithStringValue(stringValue_: NSString): id; message 'commentWithStringValue:';
    class function textWithStringValue(stringValue_: NSString): id; message 'textWithStringValue:';
    class function DTDNodeWithXMLString(string_: NSString): id; message 'DTDNodeWithXMLString:';
    function kind: NSXMLNodeKind; message 'kind';
    procedure setName(name_: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setObjectValue(value: id); message 'setObjectValue:';
    function objectValue: id; message 'objectValue';
    procedure setStringValue(string_: NSString); message 'setStringValue:';
    procedure setStringValue_resolvingEntities(string_: NSString; resolve: Boolean); message 'setStringValue:resolvingEntities:';
    function stringValue: NSString; message 'stringValue';
    function index: NSUInteger; message 'index';
    function level: NSUInteger; message 'level';
    function rootDocument: NSXMLDocument; message 'rootDocument';
    function parent: NSXMLNode; message 'parent';
    function childCount: NSUInteger; message 'childCount';
    function children: NSArray; message 'children';
    function childAtIndex(index_: NSUInteger): NSXMLNode; message 'childAtIndex:';
    function previousSibling: NSXMLNode; message 'previousSibling';
    function nextSibling: NSXMLNode; message 'nextSibling';
    function previousNode: NSXMLNode; message 'previousNode';
    function nextNode: NSXMLNode; message 'nextNode';
    procedure detach; message 'detach';
    function XPath: NSString; message 'XPath';
    function localName: NSString; message 'localName';
    function prefix: NSString; message 'prefix';
    procedure setURI(URI_: NSString); message 'setURI:';
    function URI: NSString; message 'URI';
    class function localNameForName(name_: NSString): NSString; message 'localNameForName:';
    class function prefixForName(name_: NSString): NSString; message 'prefixForName:';
    class function predefinedNamespaceForPrefix(name_: NSString): NSXMLNode; message 'predefinedNamespaceForPrefix:';
    function description: NSString; message 'description';
    function XMLString: NSString; message 'XMLString';
    function XMLStringWithOptions(options: NSUInteger): NSString; message 'XMLStringWithOptions:';
    function canonicalXMLStringPreservingComments(comments: Boolean): NSString; message 'canonicalXMLStringPreservingComments:';
    function nodesForXPath_error(xpath_: NSString; error: NSErrorPointer): NSArray; message 'nodesForXPath:error:';
    function objectsForXQuery_constants_error(xquery: NSString; constants: NSDictionary; error: NSErrorPointer): NSArray; message 'objectsForXQuery:constants:error:';
    function objectsForXQuery_error(xquery: NSString; error: NSErrorPointer): NSArray; message 'objectsForXQuery:error:';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
