{ Parsed from WebKit.framework DOMImplementation.h }


{$ifdef TYPES}
type
  DOMImplementationPtr = ^DOMImplementation;
{$endif}

{$ifdef CLASSES}

type
  DOMImplementation = objcclass external (DOMObject)
  public
    function hasFeature_version (feature: NSString; version: NSString): ObjCBOOL; message 'hasFeature:version:'; { available in 10_5 }
    function createDocumentType_publicId_systemId (qualifiedName: NSString; publicId: NSString; systemId: NSString): DOMDocumentType; message 'createDocumentType:publicId:systemId:'; { available in 10_5 }
    function createDocument_qualifiedName_doctype (namespaceURI: NSString; qualifiedName: NSString; doctype: DOMDocumentType): DOMDocument; message 'createDocument:qualifiedName:doctype:'; { available in 10_5 }
    function createCSSStyleSheet_media (title: NSString; media: NSString): DOMCSSStyleSheet; message 'createCSSStyleSheet:media:'; { available in 10_5 }
    function createHTMLDocument (title: NSString): DOMHTMLDocument; message 'createHTMLDocument:'; { available in 10_5 }
  end;


type
  DOMImplementationDeprecated = objccategory external (DOMImplementation)
    function hasFeature (feature: NSString): ObjCBOOL; message 'hasFeature:'; deprecated 'in 10_4, 10_5';
    function createDocumentType_publicId (qualifiedName: NSString; systemId: NSString): DOMDocumentType; message 'createDocumentType:publicId:'; deprecated 'in 10_4, 10_5';
    function createDocument_qualifiedName (namespaceURI: NSString; doctype: DOMDocumentType): DOMDocument; message 'createDocument:qualifiedName:'; deprecated 'in 10_4, 10_5';
    function createCSSStyleSheet (title: NSString): DOMCSSStyleSheet; message 'createCSSStyleSheet:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

