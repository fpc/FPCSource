{ Parsed from Foundation.framework NSXMLDTD.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSXMLDTD_PAS_H}
{$define NSXMLDTD_PAS_H}
type
  NSXMLDTDPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSXMLDTD_PAS_T}
{$define NSXMLDTD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSXMLDTD_PAS_R}
{$define NSXMLDTD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSXMLDTD_PAS_F}
{$define NSXMLDTD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSXMLDTD_PAS_T}
{$define NSXMLDTD_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLDTD = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLDTD_PAS_C}
{$define NSXMLDTD_PAS_C}

{ NSXMLDTD }
  NSXMLDTD = objcclass(NSXMLNode)
  private
    __name: NSString;
    __publicID: NSString;
    __systemID: NSString;
    __children: NSArray;
    __childrenHaveMutated: Boolean;
        _padding3: array[0..2] of byte;
    __entities: NSMutableDictionary;
    __elements: NSMutableDictionary;
    __notations: NSMutableDictionary;
    __attributes: NSMutableDictionary;
    __original: NSString;
    __modified: Boolean;
        _padding2: array[0..2] of byte;
    
  public
    class function alloc: NSXMLDTD; message 'alloc';

    function initWithContentsOfURL_options_error(url: NSURL; mask: NSUInteger; error: NSErrorPointer): id; message 'initWithContentsOfURL:options:error:';
    function initWithData_options_error(data: NSData; mask: NSUInteger; error: NSErrorPointer): id; message 'initWithData:options:error:';
    procedure setPublicID(publicID_: NSString); message 'setPublicID:';
    function publicID: NSString; message 'publicID';
    procedure setSystemID(systemID_: NSString); message 'setSystemID:';
    function systemID: NSString; message 'systemID';
    procedure insertChild_atIndex(child: NSXMLNode; index_: NSUInteger); message 'insertChild:atIndex:';
    procedure insertChildren_atIndex(children_: NSArray; index_: NSUInteger); message 'insertChildren:atIndex:';
    procedure removeChildAtIndex(index_: NSUInteger); message 'removeChildAtIndex:';
    procedure setChildren(children_: NSArray); message 'setChildren:';
    procedure addChild(child: NSXMLNode); message 'addChild:';
    procedure replaceChildAtIndex_withNode(index_: NSUInteger; node: NSXMLNode); message 'replaceChildAtIndex:withNode:';
    function entityDeclarationForName(name_: NSString): NSXMLDTDNode; message 'entityDeclarationForName:';
    function notationDeclarationForName(name_: NSString): NSXMLDTDNode; message 'notationDeclarationForName:';
    function elementDeclarationForName(name_: NSString): NSXMLDTDNode; message 'elementDeclarationForName:';
    function attributeDeclarationForName_elementName(name_: NSString; elementName: NSString): NSXMLDTDNode; message 'attributeDeclarationForName:elementName:';
    class function predefinedEntityDeclarationForName(name_: NSString): NSXMLDTDNode; message 'predefinedEntityDeclarationForName:';
  end; external;

{$endif}
{$endif}
