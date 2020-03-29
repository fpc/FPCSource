{ Parsed from Foundation.framework NSXMLElement.h }


{ Types from NSXMLElement }
{$ifdef TYPES}




{$endif}


{$ifdef TYPES}
type
  NSXMLElementPtr = ^NSXMLElement;
{$endif}

{$ifdef CLASSES}

type
  NSXMLElement = objcclass external (NSXMLNode)
  private
    _name: NSString;
    _attributes: id;
    _namespaces: id;
    _children: NSArray;
    _childrenHaveMutated: ObjCBOOL;
    _zeroOrOneAttributes: ObjCBOOL;
    _zeroOrOneNamespaces: ObjCBOOL;
    _padding: cuint8;
    _URI: NSString;
    _prefixIndex: NSInteger;
  public
    function initWithName (name_: NSString): instancetype; message 'initWithName:';
    function initWithName_URI (name_: NSString; URI_: NSString): instancetype; message 'initWithName:URI:'; { NS_DESIGNATED_INITIALIZER }
    function initWithName_stringValue (name_: NSString; string_: NSString): instancetype; message 'initWithName:stringValue:';
    function initWithXMLString_error (string_: NSString; error: NSErrorPtr): instancetype; message 'initWithXMLString:error:'; { NS_DESIGNATED_INITIALIZER }
    function initWithKind_options (kind_: NSXMLNodeKind; options: NSUInteger): instancetype; message 'initWithKind:options:';
    function elementsForName (name_: NSString): NSArray; message 'elementsForName:';
    function elementsForLocalName_URI (localName_: NSString; URI_: NSString): NSArray; message 'elementsForLocalName:URI:';
    procedure addAttribute (attribute: NSXMLNode); message 'addAttribute:';
    procedure removeAttributeForName (name_: NSString); message 'removeAttributeForName:';
    procedure setAttributes(newValue: NSArray); message 'setAttributes:';
    function attributes: NSArray; message 'attributes';
    procedure setAttributesWithDictionary (attributes_: NSDictionary); message 'setAttributesWithDictionary:';
    function attributeForName (name_: NSString): NSXMLNode; message 'attributeForName:';
    function attributeForLocalName_URI (localName_: NSString; URI_: NSString): NSXMLNode; message 'attributeForLocalName:URI:';
    procedure addNamespace (aNamespace: NSXMLNode); message 'addNamespace:';
    procedure removeNamespaceForPrefix (name_: NSString); message 'removeNamespaceForPrefix:';
    procedure setNamespaces(newValue: NSArray); message 'setNamespaces:';
    function namespaces: NSArray; message 'namespaces';
    function namespaceForPrefix (name_: NSString): NSXMLNode; message 'namespaceForPrefix:';
    function resolveNamespaceForName (name_: NSString): NSXMLNode; message 'resolveNamespaceForName:';
    function resolvePrefixForNamespaceURI (namespaceURI: NSString): NSString; message 'resolvePrefixForNamespaceURI:';
    procedure insertChild_atIndex (child: NSXMLNode; index_: NSUInteger); message 'insertChild:atIndex:';
    procedure insertChildren_atIndex (children_: NSArray; index_: NSUInteger); message 'insertChildren:atIndex:';
    procedure removeChildAtIndex (index_: NSUInteger); message 'removeChildAtIndex:';
    procedure setChildren (children_: NSArray); message 'setChildren:';
    procedure addChild (child: NSXMLNode); message 'addChild:';
    procedure replaceChildAtIndex_withNode (index_: NSUInteger; node: NSXMLNode); message 'replaceChildAtIndex:withNode:';
    procedure normalizeAdjacentTextNodesPreservingCDATA (preserve: ObjCBOOL); message 'normalizeAdjacentTextNodesPreservingCDATA:';
  end;


type
  NSXMLElement_NSDeprecated = objccategory external name 'NSDeprecated' (NSXMLElement)
    procedure setAttributesAsDictionary (attributes_: NSDictionary); message 'setAttributesAsDictionary:';
  end;
{$endif}

