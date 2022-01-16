{ Parsed from Foundation.framework NSXMLDTD.h }


{ Types from NSXMLDTD }
{$ifdef TYPES}



{$endif}


{$ifdef TYPES}
type
  NSXMLDTDPtr = ^NSXMLDTD;
{$endif}

{$ifdef CLASSES}

type
  NSXMLDTD = objcclass external (NSXMLNode)
  private
    _name: NSString;
    _publicID: NSString;
    _systemID: NSString;
    _children: NSArray;
    _childrenHaveMutated: ObjCBOOL;
    _padding3:array[0..2] of cuint8;
    _entities: NSMutableDictionary;
    _elements: NSMutableDictionary;
    _notations: NSMutableDictionary;
    _attributes: NSMutableDictionary;
    _original: NSString;
    _modified: ObjCBOOL;
    _padding2:array[0..2] of cuint8;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithKind_options (kind_: NSXMLNodeKind; options: NSUInteger): instancetype; message 'initWithKind:options:';
    function initWithContentsOfURL_options_error (url: NSURL; mask: NSUInteger; error: NSErrorPtr): instancetype; message 'initWithContentsOfURL:options:error:';
    function initWithData_options_error (data: NSData; mask: NSUInteger; error: NSErrorPtr): instancetype; message 'initWithData:options:error:'; { NS_DESIGNATED_INITIALIZER }
    procedure setPublicID(newValue: NSString); message 'setPublicID:';
    function publicID: NSString; message 'publicID';
    procedure setSystemID(newValue: NSString); message 'setSystemID:';
    function systemID: NSString; message 'systemID';
    procedure insertChild_atIndex (child: NSXMLNode; index_: NSUInteger); message 'insertChild:atIndex:';
    procedure insertChildren_atIndex (children_: NSArray; index_: NSUInteger); message 'insertChildren:atIndex:';
    procedure removeChildAtIndex (index_: NSUInteger); message 'removeChildAtIndex:';
    procedure setChildren (children_: NSArray); message 'setChildren:';
    procedure addChild (child: NSXMLNode); message 'addChild:';
    procedure replaceChildAtIndex_withNode (index_: NSUInteger; node: NSXMLNode); message 'replaceChildAtIndex:withNode:';
    function entityDeclarationForName (name_: NSString): NSXMLDTDNode; message 'entityDeclarationForName:';
    function notationDeclarationForName (name_: NSString): NSXMLDTDNode; message 'notationDeclarationForName:';
    function elementDeclarationForName (name_: NSString): NSXMLDTDNode; message 'elementDeclarationForName:';
    function attributeDeclarationForName_elementName (name_: NSString; elementName: NSString): NSXMLDTDNode; message 'attributeDeclarationForName:elementName:';
    class function predefinedEntityDeclarationForName (name_: NSString): NSXMLDTDNode; message 'predefinedEntityDeclarationForName:';
  end;
{$endif}

