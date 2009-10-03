{ Parsed from Webkit.framework DOMDocumentType.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMDOCUMENTTYPE_PAS_T}
{$define DOMDOCUMENTTYPE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMDOCUMENTTYPE_PAS_R}
{$define DOMDOCUMENTTYPE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMDOCUMENTTYPE_PAS_F}
{$define DOMDOCUMENTTYPE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMDOCUMENTTYPE_PAS_S}
{$define DOMDOCUMENTTYPE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMDocumentType = objcclass;
  DOMDocumentTypePointer = ^DOMDocumentType;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMDOCUMENTTYPE_PAS_C}
{$define DOMDOCUMENTTYPE_PAS_C}

{ DOMDocumentType }
  DOMDocumentType = objcclass(DOMNode)
    
  public
    class function alloc: DOMDocumentType; message 'alloc';

    function name: NSString; message 'name';
    function entities: DOMNamedNodeMap; message 'entities';
    function notations: DOMNamedNodeMap; message 'notations';
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
    function internalSubset: NSString; message 'internalSubset';
  end; external;

{$endif}
{$endif}
