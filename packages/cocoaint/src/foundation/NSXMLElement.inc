{ Parsed from Foundation.framework NSXMLElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSXMLELEMENT_PAS_T}
{$define NSXMLELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSXMLELEMENT_PAS_R}
{$define NSXMLELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSXMLELEMENT_PAS_F}
{$define NSXMLELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSXMLELEMENT_PAS_S}
{$define NSXMLELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLElement = objcclass;
  NSXMLElementPointer = ^NSXMLElement;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLELEMENT_PAS_C}
{$define NSXMLELEMENT_PAS_C}

{ NSXMLElement }
  NSXMLElement = objcclass(NSXMLNode)
  private
    __name: NSString;
    __attributes: NSMutableArray;
    __namespaces: NSMutableArray;
    __children: NSArray;
    __childrenHaveMutated: Boolean;
        _padding3: array[0..2] of byte;
    __URI: NSString;
    __prefixIndex: NSInteger;
    
  public
    class function alloc: NSXMLElement; message 'alloc';

    function initWithName(name_: NSString): id; message 'initWithName:';
    function initWithName_URI(name_: NSString; URI_: NSString): id; message 'initWithName:URI:';
    function initWithName_stringValue(name_: NSString; string_: NSString): id; message 'initWithName:stringValue:';
    function initWithXMLString_error(string_: NSString; error: NSErrorPointer): id; message 'initWithXMLString:error:';
    function elementsForName(name_: NSString): NSArray; message 'elementsForName:';
    function elementsForLocalName_URI(localName_: NSString; URI_: NSString): NSArray; message 'elementsForLocalName:URI:';
    procedure addAttribute(attribute: NSXMLNode); message 'addAttribute:';
    procedure removeAttributeForName(name_: NSString); message 'removeAttributeForName:';
    procedure setAttributes(attributes_: NSArray); message 'setAttributes:';
    procedure setAttributesAsDictionary(attributes_: NSDictionary); message 'setAttributesAsDictionary:';
    function attributes: NSArray; message 'attributes';
    function attributeForName(name_: NSString): NSXMLNode; message 'attributeForName:';
    function attributeForLocalName_URI(localName_: NSString; URI_: NSString): NSXMLNode; message 'attributeForLocalName:URI:';
    procedure addNamespace(aNamespace: NSXMLNode); message 'addNamespace:';
    procedure removeNamespaceForPrefix(name_: NSString); message 'removeNamespaceForPrefix:';
    procedure setNamespaces(namespaces_: NSArray); message 'setNamespaces:';
    function namespaces: NSArray; message 'namespaces';
    function namespaceForPrefix(name_: NSString): NSXMLNode; message 'namespaceForPrefix:';
    function resolveNamespaceForName(name_: NSString): NSXMLNode; message 'resolveNamespaceForName:';
    function resolvePrefixForNamespaceURI(namespaceURI: NSString): NSString; message 'resolvePrefixForNamespaceURI:';
    procedure insertChild_atIndex(child: NSXMLNode; index_: NSUInteger); message 'insertChild:atIndex:';
    procedure insertChildren_atIndex(children_: NSArray; index_: NSUInteger); message 'insertChildren:atIndex:';
    procedure removeChildAtIndex(index_: NSUInteger); message 'removeChildAtIndex:';
    procedure setChildren(children_: NSArray); message 'setChildren:';
    procedure addChild(child: NSXMLNode); message 'addChild:';
    procedure replaceChildAtIndex_withNode(index_: NSUInteger; node: NSXMLNode); message 'replaceChildAtIndex:withNode:';
    procedure normalizeAdjacentTextNodesPreservingCDATA(preserve: Boolean); message 'normalizeAdjacentTextNodesPreservingCDATA:';
  end; external;

{$endif}
{$endif}
