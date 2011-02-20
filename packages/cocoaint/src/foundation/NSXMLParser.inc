{ Parsed from Foundation.framework NSXMLParser.h }

{$ifdef TYPES}
{$ifndef NSXMLPARSER_PAS_T}
{$define NSXMLPARSER_PAS_T}

{ Constants }

const
  NSXMLParserInternalError = 1;
  NSXMLParserOutOfMemoryError = 2;
  NSXMLParserDocumentStartError = 3;
  NSXMLParserEmptyDocumentError = 4;
  NSXMLParserPrematureDocumentEndError = 5;
  NSXMLParserInvalidHexCharacterRefError = 6;
  NSXMLParserInvalidDecimalCharacterRefError = 7;
  NSXMLParserInvalidCharacterRefError = 8;
  NSXMLParserInvalidCharacterError = 9;
  NSXMLParserCharacterRefAtEOFError = 10;
  NSXMLParserCharacterRefInPrologError = 11;
  NSXMLParserCharacterRefInEpilogError = 12;
  NSXMLParserCharacterRefInDTDError = 13;
  NSXMLParserEntityRefAtEOFError = 14;
  NSXMLParserEntityRefInPrologError = 15;
  NSXMLParserEntityRefInEpilogError = 16;
  NSXMLParserEntityRefInDTDError = 17;
  NSXMLParserParsedEntityRefAtEOFError = 18;
  NSXMLParserParsedEntityRefInPrologError = 19;
  NSXMLParserParsedEntityRefInEpilogError = 20;
  NSXMLParserParsedEntityRefInInternalSubsetError = 21;
  NSXMLParserEntityReferenceWithoutNameError = 22;
  NSXMLParserEntityReferenceMissingSemiError = 23;
  NSXMLParserParsedEntityRefNoNameError = 24;
  NSXMLParserParsedEntityRefMissingSemiError = 25;
  NSXMLParserUndeclaredEntityError = 26;
  NSXMLParserUnparsedEntityError = 28;
  NSXMLParserEntityIsExternalError = 29;
  NSXMLParserEntityIsParameterError = 30;
  NSXMLParserUnknownEncodingError = 31;
  NSXMLParserEncodingNotSupportedError = 32;
  NSXMLParserStringNotStartedError = 33;
  NSXMLParserStringNotClosedError = 34;
  NSXMLParserNamespaceDeclarationError = 35;
  NSXMLParserEntityNotStartedError = 36;
  NSXMLParserEntityNotFinishedError = 37;
  NSXMLParserLessThanSymbolInAttributeError = 38;
  NSXMLParserAttributeNotStartedError = 39;
  NSXMLParserAttributeNotFinishedError = 40;
  NSXMLParserAttributeHasNoValueError = 41;
  NSXMLParserAttributeRedefinedError = 42;
  NSXMLParserLiteralNotStartedError = 43;
  NSXMLParserLiteralNotFinishedError = 44;
  NSXMLParserCommentNotFinishedError = 45;
  NSXMLParserProcessingInstructionNotStartedError = 46;
  NSXMLParserProcessingInstructionNotFinishedError = 47;
  NSXMLParserNotationNotStartedError = 48;
  NSXMLParserNotationNotFinishedError = 49;
  NSXMLParserAttributeListNotStartedError = 50;
  NSXMLParserAttributeListNotFinishedError = 51;
  NSXMLParserMixedContentDeclNotStartedError = 52;
  NSXMLParserMixedContentDeclNotFinishedError = 53;
  NSXMLParserElementContentDeclNotStartedError = 54;
  NSXMLParserElementContentDeclNotFinishedError = 55;
  NSXMLParserXMLDeclNotStartedError = 56;
  NSXMLParserXMLDeclNotFinishedError = 57;
  NSXMLParserConditionalSectionNotStartedError = 58;
  NSXMLParserConditionalSectionNotFinishedError = 59;
  NSXMLParserExternalSubsetNotFinishedError = 60;
  NSXMLParserDOCTYPEDeclNotFinishedError = 61;
  NSXMLParserMisplacedCDATAEndStringError = 62;
  NSXMLParserCDATANotFinishedError = 63;
  NSXMLParserMisplacedXMLDeclarationError = 64;
  NSXMLParserSpaceRequiredError = 65;
  NSXMLParserSeparatorRequiredError = 66;
  NSXMLParserNMTOKENRequiredError = 67;
  NSXMLParserNAMERequiredError = 68;
  NSXMLParserPCDATARequiredError = 69;
  NSXMLParserURIRequiredError = 70;
  NSXMLParserPublicIdentifierRequiredError = 71;
  NSXMLParserLTRequiredError = 72;
  NSXMLParserGTRequiredError = 73;
  NSXMLParserLTSlashRequiredError = 74;
  NSXMLParserEqualExpectedError = 75;
  NSXMLParserTagNameMismatchError = 76;
  NSXMLParserUnfinishedTagError = 77;
  NSXMLParserStandaloneValueError = 78;
  NSXMLParserInvalidEncodingNameError = 79;
  NSXMLParserCommentContainsDoubleHyphenError = 80;
  NSXMLParserInvalidEncodingError = 81;
  NSXMLParserExternalStandaloneEntityError = 82;
  NSXMLParserInvalidConditionalSectionError = 83;
  NSXMLParserEntityValueRequiredError = 84;
  NSXMLParserNotWellBalancedError = 85;
  NSXMLParserExtraContentError = 86;
  NSXMLParserInvalidCharacterInEntityError = 87;
  NSXMLParserParsedEntityRefInInternalError = 88;
  NSXMLParserEntityRefLoopError = 89;
  NSXMLParserEntityBoundaryError = 90;
  NSXMLParserInvalidURIError = 91;
  NSXMLParserURIFragmentError = 92;
  NSXMLParserNoDTDError = 94;
  NSXMLParserDelegateAbortedParseError = 512;

{ Types }
type
  NSXMLParserError = NSInteger;
  NSXMLParserErrorPtr = ^NSXMLParserError;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSXMLPARSER_PAS_R}
{$define NSXMLPARSER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSXMLPARSER_PAS_F}
{$define NSXMLPARSER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSXMLPARSER_PAS_S}
{$define NSXMLPARSER_PAS_S}

{ External string constants }
var
  NSXMLParserErrorDomain: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLParserDelegateProtocol = objcprotocol;
  NSXMLParser = objcclass;
  NSXMLParserPointer = ^NSXMLParser;
  NSXMLParserPtr = NSXMLParserPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLPARSER_PAS_C}
{$define NSXMLPARSER_PAS_C}

{ NSXMLParser }
  NSXMLParser = objcclass external (NSObject)
  private
    _parser: Pointer;
    _delegate: id;
    _reserved1: id;
    _reserved2: id;
    _reserved3: id;
    
  public
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function delegate: NSXMLParserDelegateProtocol; message 'delegate';
    procedure setDelegate(delegate_: NSXMLParserDelegateProtocol); message 'setDelegate:';
    procedure setShouldProcessNamespaces(shouldProcessNamespaces_: Boolean); message 'setShouldProcessNamespaces:';
    procedure setShouldReportNamespacePrefixes(shouldReportNamespacePrefixes_: Boolean); message 'setShouldReportNamespacePrefixes:';
    procedure setShouldResolveExternalEntities(shouldResolveExternalEntities_: Boolean); message 'setShouldResolveExternalEntities:';
    function shouldProcessNamespaces: Boolean; message 'shouldProcessNamespaces';
    function shouldReportNamespacePrefixes: Boolean; message 'shouldReportNamespacePrefixes';
    function shouldResolveExternalEntities: Boolean; message 'shouldResolveExternalEntities';
    function parse: Boolean; message 'parse';
    procedure abortParsing; message 'abortParsing';
    function parserError: NSError; message 'parserError';
  end;

{ NSXMLParserLocatorAdditionsCategory }
  NSXMLParserLocatorAdditionsCategory = objccategory external (NSXMLParser)
    function publicID: NSString; message 'publicID';
    function systemID: NSString; message 'systemID';
    function lineNumber: NSInteger; message 'lineNumber';
    function columnNumber: NSInteger; message 'columnNumber';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSXMLPARSER_PAS_P}
{$define NSXMLPARSER_PAS_P}
  
{ NSXMLParserDelegate Protocol }
  NSXMLParserDelegateProtocol = objcprotocol external name 'NSXMLParserDelegate'
  optional
    procedure parserDidStartDocument(parser: NSXMLParser); message 'parserDidStartDocument:';
    procedure parserDidEndDocument(parser: NSXMLParser); message 'parserDidEndDocument:';
    procedure parser_foundNotationDeclarationWithName_publicID_systemID(parser: NSXMLParser; name: NSString; publicID: NSString; systemID: NSString); message 'parser:foundNotationDeclarationWithName:publicID:systemID:';
    procedure parser_foundUnparsedEntityDeclarationWithName_publicID_systemID_notationName(parser: NSXMLParser; name: NSString; publicID: NSString; systemID: NSString; notationName: NSString); message 'parser:foundUnparsedEntityDeclarationWithName:publicID:systemID:notationName:';
    procedure parser_foundAttributeDeclarationWithName_forElement_type_defaultValue(parser: NSXMLParser; attributeName: NSString; elementName: NSString; type_: NSString; defaultValue: NSString); message 'parser:foundAttributeDeclarationWithName:forElement:type:defaultValue:';
    procedure parser_foundElementDeclarationWithName_model(parser: NSXMLParser; elementName: NSString; model: NSString); message 'parser:foundElementDeclarationWithName:model:';
    procedure parser_foundInternalEntityDeclarationWithName_value(parser: NSXMLParser; name: NSString; value: NSString); message 'parser:foundInternalEntityDeclarationWithName:value:';
    procedure parser_foundExternalEntityDeclarationWithName_publicID_systemID(parser: NSXMLParser; name: NSString; publicID: NSString; systemID: NSString); message 'parser:foundExternalEntityDeclarationWithName:publicID:systemID:';
    procedure parser_didStartElement_namespaceURI_qualifiedName_attributes(parser: NSXMLParser; elementName: NSString; namespaceURI: NSString; qName: NSString; attributeDict: NSDictionary); message 'parser:didStartElement:namespaceURI:qualifiedName:attributes:';
    procedure parser_didEndElement_namespaceURI_qualifiedName(parser: NSXMLParser; elementName: NSString; namespaceURI: NSString; qName: NSString); message 'parser:didEndElement:namespaceURI:qualifiedName:';
    procedure parser_didStartMappingPrefix_toURI(parser: NSXMLParser; prefix: NSString; namespaceURI: NSString); message 'parser:didStartMappingPrefix:toURI:';
    procedure parser_didEndMappingPrefix(parser: NSXMLParser; prefix: NSString); message 'parser:didEndMappingPrefix:';
    procedure parser_foundCharacters(parser: NSXMLParser; string_: NSString); message 'parser:foundCharacters:';
    procedure parser_foundIgnorableWhitespace(parser: NSXMLParser; whitespaceString: NSString); message 'parser:foundIgnorableWhitespace:';
    procedure parser_foundProcessingInstructionWithTarget_data(parser: NSXMLParser; target: NSString; data: NSString); message 'parser:foundProcessingInstructionWithTarget:data:';
    procedure parser_foundComment(parser: NSXMLParser; comment: NSString); message 'parser:foundComment:';
    procedure parser_foundCDATA(parser: NSXMLParser; CDATABlock: NSData); message 'parser:foundCDATA:';
    function parser_resolveExternalEntityName_systemID(parser: NSXMLParser; name: NSString; systemID: NSString): NSData; message 'parser:resolveExternalEntityName:systemID:';
    procedure parser_parseErrorOccurred(parser: NSXMLParser; parseError: NSError); message 'parser:parseErrorOccurred:';
    procedure parser_validationErrorOccurred(parser: NSXMLParser; validationError: NSError); message 'parser:validationErrorOccurred:';
  end;
{$endif}
{$endif}
