{ Parsed from Foundation.framework NSXMLDocument.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSXMLDOCUMENT_PAS_H}
{$define NSXMLDOCUMENT_PAS_H}
type
  NSXMLDocumentPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSXMLDOCUMENT_PAS_T}
{$define NSXMLDOCUMENT_PAS_T}

{ Constants }

const
  NSXMLDocumentXMLKind = 0;
  NSXMLDocumentXHTMLKind = 0;
  NSXMLDocumentHTMLKind = 1;
  NSXMLDocumentTextKind = 2;

{ Types }
type
  NSXMLDocumentContentKind = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSXMLDOCUMENT_PAS_R}
{$define NSXMLDOCUMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSXMLDOCUMENT_PAS_F}
{$define NSXMLDOCUMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSXMLDOCUMENT_PAS_T}
{$define NSXMLDOCUMENT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLDocument = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLDOCUMENT_PAS_C}
{$define NSXMLDOCUMENT_PAS_C}

{ NSXMLDocument }
  NSXMLDocument = objcclass(NSXMLNode)
  private
    __encoding: NSString;
    __version: NSString;
    __docType: NSXMLDTD;
    __children: NSArray;
    __childrenHaveMutated: Boolean;
    __standalone: Boolean;
    __padding3: byte;
    __rootElement: NSXMLElement;
    __URI: NSString;
    __MIMEType: NSString;
    __fidelityMask: culong;
    __contentKind: NSXMLDocumentContentKind;
    
  public
    class function alloc: NSXMLDocument; message 'alloc';

    function initWithXMLString_options_error(string_: NSString; mask: culong; var error: NSError): id; message 'initWithXMLString:options:error:';
    function initWithContentsOfURL_options_error(url: NSURL; mask: culong; var error: NSError): id; message 'initWithContentsOfURL:options:error:';
    function initWithData_options_error(data: NSData; mask: culong; var error: NSError): id; message 'initWithData:options:error:';
    function initWithRootElement(element: NSXMLElement): id; message 'initWithRootElement:';
    class function replacementClassForClass(cls: Pobjc_class): Pobjc_class; message 'replacementClassForClass:';
    procedure setCharacterEncoding(encoding: NSString); message 'setCharacterEncoding:';
    function characterEncoding: NSString; message 'characterEncoding';
    procedure setVersion(version_: NSString); message 'setVersion:';
    function version: NSString; message 'version';
    procedure setStandalone(standalone: Boolean); message 'setStandalone:';
    function isStandalone: Boolean; message 'isStandalone';
    procedure setDocumentContentKind(kind_: NSXMLDocumentContentKind); message 'setDocumentContentKind:';
    function documentContentKind: NSXMLDocumentContentKind; message 'documentContentKind';
    procedure setMIMEType(MIMEType_: NSString); message 'setMIMEType:';
    function MIMEType: NSString; message 'MIMEType';
    procedure setDTD(documentTypeDeclaration: NSXMLDTD); message 'setDTD:';
    function DTD: NSXMLDTD; message 'DTD';
    procedure setRootElement(root: NSXMLNode); message 'setRootElement:';
    function rootElement: NSXMLElement; message 'rootElement';
    procedure insertChild_atIndex(child: NSXMLNode; index_: culong); message 'insertChild:atIndex:';
    procedure insertChildren_atIndex(children_: NSArray; index_: culong); message 'insertChildren:atIndex:';
    procedure removeChildAtIndex(index_: culong); message 'removeChildAtIndex:';
    procedure setChildren(children_: NSArray); message 'setChildren:';
    procedure addChild(child: NSXMLNode); message 'addChild:';
    procedure replaceChildAtIndex_withNode(index_: culong; node: NSXMLNode); message 'replaceChildAtIndex:withNode:';
    function XMLData: NSData; message 'XMLData';
    function XMLDataWithOptions(options: culong): NSData; message 'XMLDataWithOptions:';
    function objectByApplyingXSLT_arguments_error(xslt: NSData; arguments: NSDictionary; var error: NSError): id; message 'objectByApplyingXSLT:arguments:error:';
    function objectByApplyingXSLTString_arguments_error(xslt: NSString; arguments: NSDictionary; var error: NSError): id; message 'objectByApplyingXSLTString:arguments:error:';
    function objectByApplyingXSLTAtURL_arguments_error(xsltURL: NSURL; argument: NSDictionary; var error: NSError): id; message 'objectByApplyingXSLTAtURL:arguments:error:';
    function validateAndReturnError(var error: NSError): Boolean; message 'validateAndReturnError:';
  end; external;

{$endif}
{$endif}
