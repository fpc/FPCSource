{ Parsed from Foundation.framework NSXMLDTDNode.h }


{$ifdef TYPES}
type
  NSXMLDTDNodePtr = ^NSXMLDTDNode;
{$endif}

{$ifdef TYPES}
type
  NSXMLDTDNodeKind = NSUInteger;
  NSXMLDTDNodeKindPtr = ^NSXMLDTDNodeKind;

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
{$endif}

{$ifdef CLASSES}

type
  NSXMLDTDNode = objcclass external (NSXMLNode)
  private
    _DTDKind: NSXMLDTDNodeKind;
    _name: NSString;
    _notationName: NSString;
    _publicID: NSString;
    _systemID: NSString;
  public
    function initWithXMLString (string_: NSString): instancetype; message 'initWithXMLString:'; { NS_DESIGNATED_INITIALIZER }
    function initWithKind_options (kind_: NSXMLNodeKind; options: NSUInteger): instancetype; message 'initWithKind:options:'; { NS_DESIGNATED_INITIALIZER }
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    procedure setDTDKind(newValue: NSXMLDTDNodeKind); message 'setDTDKind:';
    function DTDKind: NSXMLDTDNodeKind; message 'DTDKind';
    function isExternal: ObjCBOOL; message 'isExternal';
    procedure setPublicID(newValue: NSString); message 'setPublicID:';
    function publicID: NSString; message 'publicID';
    procedure setSystemID(newValue: NSString); message 'setSystemID:';
    function systemID: NSString; message 'systemID';
    procedure setNotationName(newValue: NSString); message 'setNotationName:';
    function notationName: NSString; message 'notationName';
  end;
{$endif}

