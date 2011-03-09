{ Parsed from Foundation.framework NSXMLDTDNode.h }

{$ifdef TYPES}
{$ifndef NSXMLDTDNODE_PAS_T}
{$define NSXMLDTDNODE_PAS_T}

{ Constants }

const
  NSXMLEntityGeneralKind = 1;
  NSXMLEntityParsedKind = 2;
  NSXMLEntityUnparsedKind = 3;
  NSXMLEntityParameterKind = 4;
  NSXMLEntityPredefined = 5;
  NSXMLAttributeCDATAKind = 6;
  NSXMLAttributeIDKind = 7;
  NSXMLAttributeIDRefKind = 8;
  NSXMLAttributeIDRefsKind = 9;
  NSXMLAttributeEntityKind = 10;
  NSXMLAttributeEntitiesKind = 11;
  NSXMLAttributeNMTokenKind = 12;
  NSXMLAttributeNMTokensKind = 13;
  NSXMLAttributeEnumerationKind = 14;
  NSXMLAttributeNotationKind = 15;
  NSXMLElementDeclarationUndefinedKind = 16;
  NSXMLElementDeclarationEmptyKind = 17;
  NSXMLElementDeclarationAnyKind = 18;
  NSXMLElementDeclarationMixedKind = 19;
  NSXMLElementDeclarationElementKind = 20;

{ Types }
type
  NSXMLDTDNodeKind = NSUInteger;
  NSXMLDTDNodeKindPtr = ^NSXMLDTDNodeKind;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSXMLDTDNODE_PAS_R}
{$define NSXMLDTDNODE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSXMLDTDNODE_PAS_F}
{$define NSXMLDTDNODE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSXMLDTDNODE_PAS_S}
{$define NSXMLDTDNODE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLDTDNode = objcclass;
  NSXMLDTDNodePointer = ^NSXMLDTDNode;
  NSXMLDTDNodePtr = NSXMLDTDNodePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLDTDNODE_PAS_C}
{$define NSXMLDTDNODE_PAS_C}

{ NSXMLDTDNode }
  NSXMLDTDNode = objcclass external (NSXMLNode)
  private
    _DTDKind: NSXMLDTDNodeKind;
    _name: NSString;
    _notationName: NSString;
    _publicID: NSString;
    _systemID: NSString;
    
  public
    function initWithXMLString(string_: NSString): id; message 'initWithXMLString:';
    procedure setDTDKind(kind_: NSXMLDTDNodeKind); message 'setDTDKind:';
    function DTDKind: NSXMLDTDNodeKind; message 'DTDKind';
    function isExternal: Boolean; message 'isExternal';
    procedure setPublicID(publicID_: NSString); message 'setPublicID:';
    function publicID: NSString; message 'publicID';
    procedure setSystemID(systemID_: NSString); message 'setSystemID:';
    function systemID: NSString; message 'systemID';
    procedure setNotationName(notationName_: NSString); message 'setNotationName:';
    function notationName: NSString; message 'notationName';
  end;

{$endif}
{$endif}
