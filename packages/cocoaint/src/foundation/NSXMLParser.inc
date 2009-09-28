{ Parsed from Foundation.framework NSXMLParser.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSXMLPARSER_PAS_H}
{$define NSXMLPARSER_PAS_H}
type
  NSXMLParserPointer = Pointer;

{$endif}
{$endif}

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
  NSXMLParserErrorDomain: NSString; external name '_NSXMLParserErrorDomain';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSXMLParser = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSXMLPARSER_PAS_C}
{$define NSXMLPARSER_PAS_C}

{ NSXMLParser }
  NSXMLParser = objcclass(NSObject)
  private
    __parser: Pointer;
    __delegate: id;
    __reserved1: id;
    __reserved2: id;
    __reserved3: id;
    
  public
    class function alloc: NSXMLParser; message 'alloc';

    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    procedure setShouldProcessNamespaces(shouldProcessNamespaces_: Boolean); message 'setShouldProcessNamespaces:';
    procedure setShouldReportNamespacePrefixes(shouldReportNamespacePrefixes_: Boolean); message 'setShouldReportNamespacePrefixes:';
    procedure setShouldResolveExternalEntities(shouldResolveExternalEntities_: Boolean); message 'setShouldResolveExternalEntities:';
    function shouldProcessNamespaces: Boolean; message 'shouldProcessNamespaces';
    function shouldReportNamespacePrefixes: Boolean; message 'shouldReportNamespacePrefixes';
    function shouldResolveExternalEntities: Boolean; message 'shouldResolveExternalEntities';
    function parse: Boolean; message 'parse';
    procedure abortParsing; message 'abortParsing';
    function parserError: NSError; message 'parserError';

    { Category: NSXMLParserLocatorAdditions }
    function publicID: NSString; message 'publicID';
    function systemID: NSString; message 'systemID';
    function lineNumber: NSInteger; message 'lineNumber';
    function columnNumber: NSInteger; message 'columnNumber';
  end; external;

{$endif}
{$endif}
