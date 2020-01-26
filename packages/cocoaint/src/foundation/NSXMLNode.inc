{ Parsed from Foundation.framework NSXMLNode.h }


{ Types from NSXMLNode }
{$ifdef TYPES}





{$endif}


{$ifdef TYPES}
type
  NSXMLNodePtr = ^NSXMLNode;
{$endif}

{$ifdef TYPES}
type
  NSXMLNodeKind = NSUInteger;
  NSXMLNodeKindPtr = ^NSXMLNodeKind;

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
{$endif}

{$ifdef CLASSES}

type
  NSXMLNode = objcclass external (NSObject, NSCopyingProtocol)
  private
    {$if defined(__LP64__)}
    _parent: NSXMLNode;
    _objectValue: id;
    _anonStruct_NSXMLNode0: record
      case byte of
        0: (anonBitField_NSXMLNode0: cuint);
        1: (data: bitpacked record
          _kind: 0..((1 shl 4)-1);
          _index: 0..((1 shl 28)-1);
        end;
      );
    end;
    _private: cint32;
    {$else}
    _kind_: NSXMLNodeKind;
    _parent_: NSXMLNode;
    _index_: cuint32;
    _objectValue_: id;
    {$endif}
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithKind (kind: NSXMLNodeKind): instancetype; message 'initWithKind:';
    function initWithKind_options (kind: NSXMLNodeKind; options: NSUInteger): instancetype; message 'initWithKind:options:'; { NS_DESIGNATED_INITIALIZER }
    class function document: id; message 'document';
    class function documentWithRootElement (element: NSXMLElement): id; message 'documentWithRootElement:';
    class function elementWithName (name: NSString): id; message 'elementWithName:';
    class function elementWithName_URI (name: NSString; URI: NSString): id; message 'elementWithName:URI:';
    class function elementWithName_stringValue (name: NSString; string_: NSString): id; message 'elementWithName:stringValue:';
    class function elementWithName_children_attributes (name: NSString; children: NSArray; attributes: NSArray): id; message 'elementWithName:children:attributes:';
    class function attributeWithName_stringValue (name: NSString; stringValue: NSString): id; message 'attributeWithName:stringValue:';
    class function attributeWithName_URI_stringValue (name: NSString; URI: NSString; stringValue: NSString): id; message 'attributeWithName:URI:stringValue:';
    class function namespaceWithName_stringValue (name: NSString; stringValue: NSString): id; message 'namespaceWithName:stringValue:';
    class function processingInstructionWithName_stringValue (name: NSString; stringValue: NSString): id; message 'processingInstructionWithName:stringValue:';
    class function commentWithStringValue (stringValue: NSString): id; message 'commentWithStringValue:';
    class function textWithStringValue (stringValue: NSString): id; message 'textWithStringValue:';
    class function DTDNodeWithXMLString (string_: NSString): id; message 'DTDNodeWithXMLString:';
    function kind: NSXMLNodeKind; message 'kind';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setObjectValue(newValue: id); message 'setObjectValue:';
    function objectValue: id; message 'objectValue';
    procedure setStringValue(newValue: NSString); message 'setStringValue:';
    function stringValue: NSString; message 'stringValue';
    procedure setStringValue_resolvingEntities (string_: NSString; resolve: ObjCBOOL); message 'setStringValue:resolvingEntities:';
    function index: NSUInteger; message 'index';
    function level: NSUInteger; message 'level';
    function rootDocument: NSXMLDocument; message 'rootDocument';
    function parent: NSXMLNode; message 'parent';
    function childCount: NSUInteger; message 'childCount';
    function children: NSArray; message 'children';
    function childAtIndex (index_: NSUInteger): NSXMLNode; message 'childAtIndex:';
    function previousSibling: NSXMLNode; message 'previousSibling';
    function nextSibling: NSXMLNode; message 'nextSibling';
    function previousNode: NSXMLNode; message 'previousNode';
    function nextNode: NSXMLNode; message 'nextNode';
    procedure detach; message 'detach';
    function XPath: NSString; message 'XPath';
    function localName: NSString; message 'localName';
    function prefix: NSString; message 'prefix';
    procedure setURI(newValue: NSString); message 'setURI:';
    function URI: NSString; message 'URI';
    class function localNameForName (name_: NSString): NSString; message 'localNameForName:';
    class function prefixForName (name_: NSString): NSString; message 'prefixForName:';
    class function predefinedNamespaceForPrefix (name_: NSString): NSXMLNode; message 'predefinedNamespaceForPrefix:';
    function description: NSString; message 'description';
    function XMLString: NSString; message 'XMLString';
    function XMLStringWithOptions (options: NSUInteger): NSString; message 'XMLStringWithOptions:';
    function canonicalXMLStringPreservingComments (comments: ObjCBOOL): NSString; message 'canonicalXMLStringPreservingComments:';
    function nodesForXPath_error (xpath_: NSString; error: NSErrorPtr): NSArray; message 'nodesForXPath:error:';
    function objectsForXQuery_constants_error (xquery: NSString; constants: NSDictionary; error: NSErrorPtr): NSArray; message 'objectsForXQuery:constants:error:';
    function objectsForXQuery_error (xquery: NSString; error: NSErrorPtr): NSArray; message 'objectsForXQuery:error:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

