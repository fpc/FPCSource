{ Parsed from Foundation.framework NSXMLDTDNode.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSXMLDTDNODE_PAS_T}
{$define NSXMLDTDNODE_PAS_T}

{ Constants }

const
  NSXMLEntityGeneralKind = 1;
  NSXMLEntityParsedKind = 0;
  NSXMLEntityUnparsedKind = 1;
  NSXMLEntityParameterKind = 2;
  NSXMLEntityPredefined = 3;
  NSXMLAttributeCDATAKind = 4;
  NSXMLAttributeIDKind = 5;
  NSXMLAttributeIDRefKind = 6;
  NSXMLAttributeIDRefsKind = 7;
  NSXMLAttributeEntityKind = 8;
  NSXMLAttributeEntitiesKind = 9;
  NSXMLAttributeNMTokenKind = 10;
  NSXMLAttributeNMTokensKind = 11;
  NSXMLAttributeEnumerationKind = 12;
  NSXMLAttributeNotationKind = 13;
  NSXMLElementDeclarationUndefinedKind = 14;
  NSXMLElementDeclarationEmptyKind = 15;
  NSXMLElementDeclarationAnyKind = 16;
  NSXMLElementDeclarationMixedKind = 17;
  NSXMLElementDeclarationElementKind = 18;

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
  NSXMLDTDNode = objcclass(NSXMLNode)
  private
    _DTDKind: NSXMLDTDNodeKind;
    _name: NSString;
    _notationName: NSString;
    _publicID: NSString;
    _systemID: NSString;
    
  public
    class function alloc: NSXMLDTDNode; message 'alloc';

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
  end; external;

{$endif}
{$endif}
