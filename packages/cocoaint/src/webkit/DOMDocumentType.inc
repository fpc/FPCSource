{ Parsed from WebKit.framework DOMDocumentType.h }


{$ifdef TYPES}
type
  DOMDocumentTypePtr = ^DOMDocumentType;
{$endif}

{$ifdef CLASSES}

type
  DOMDocumentType = objcclass external (DOMNode)
  public
    function name: NSString; message 'name';
    function entities: DOMNamedNodeMap; message 'entities';
    function notations: DOMNamedNodeMap; message 'notations';
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
    function internalSubset: NSString; message 'internalSubset';
  end;
{$endif}

