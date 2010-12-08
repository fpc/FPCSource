{ Parsed from Webkit.framework WebDocument.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef WEBDOCUMENT_PAS_T}
{$define WEBDOCUMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBDOCUMENT_PAS_R}
{$define WEBDOCUMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBDOCUMENT_PAS_F}
{$define WEBDOCUMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBDOCUMENT_PAS_S}
{$define WEBDOCUMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebDocumentViewProtocol = objcprotocol;
  WebDocumentSearchingProtocol = objcprotocol;
  WebDocumentTextProtocol = objcprotocol;
  WebDocumentRepresentationProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef WEBDOCUMENT_PAS_P}
{$define WEBDOCUMENT_PAS_P}
  
{ WebDocumentView Protocol }
  WebDocumentViewProtocol = objcprotocol
    procedure setDataSource(dataSource: WebDataSource); message 'setDataSource:';
    procedure dataSourceUpdated(dataSource: WebDataSource); message 'dataSourceUpdated:';
    procedure setNeedsLayout(flag: Boolean); message 'setNeedsLayout:';
    procedure layout; message 'layout';
    procedure viewWillMoveToHostWindow(hostWindow: NSWindowPtr); message 'viewWillMoveToHostWindow:';
    procedure viewDidMoveToHostWindow; message 'viewDidMoveToHostWindow';
  end; external name 'WebDocumentView';
  
{ WebDocumentSearching Protocol }
  WebDocumentSearchingProtocol = objcprotocol
    function searchFor_direction_caseSensitive_wrap(string_: NSString; forward: Boolean; caseFlag: Boolean; wrapFlag: Boolean): Boolean; message 'searchFor:direction:caseSensitive:wrap:';
  end; external name 'WebDocumentSearching';
  
{ WebDocumentText Protocol }
  WebDocumentTextProtocol = objcprotocol
    function supportsTextEncoding: Boolean; message 'supportsTextEncoding';
    function string_: NSString; message 'string';
    function attributedString: NSAttributedString; message 'attributedString';
    function selectedString: NSString; message 'selectedString';
    function selectedAttributedString: NSAttributedString; message 'selectedAttributedString';
    procedure selectAll; message 'selectAll';
    procedure deselectAll; message 'deselectAll';
  end; external name 'WebDocumentText';
  
{ WebDocumentRepresentation Protocol }
  WebDocumentRepresentationProtocol = objcprotocol
    procedure setDataSource(dataSource: WebDataSource); message 'setDataSource:';
    procedure receivedData_withDataSource(data: NSData; dataSource: WebDataSource); message 'receivedData:withDataSource:';
    procedure receivedError_withDataSource(error: NSError; dataSource: WebDataSource); message 'receivedError:withDataSource:';
    procedure finishedLoadingWithDataSource(dataSource: WebDataSource); message 'finishedLoadingWithDataSource:';
    function canProvideDocumentSource: Boolean; message 'canProvideDocumentSource';
    function documentSource: NSString; message 'documentSource';
    function title: NSString; message 'title';
  end; external name 'WebDocumentRepresentation';
{$endif}
{$endif}
