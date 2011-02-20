{ Parsed from Webkit.framework WebDocument.h }

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
  WebDocumentViewProtocol = objcprotocol external name 'WebDocumentView'
    procedure setDataSource(dataSource: WebDataSource); message 'setDataSource:';
    procedure dataSourceUpdated(dataSource: WebDataSource); message 'dataSourceUpdated:';
    procedure setNeedsLayout(flag: Boolean); message 'setNeedsLayout:';
    procedure layout; message 'layout';
    procedure viewWillMoveToHostWindow(hostWindow: NSWindowPtr); message 'viewWillMoveToHostWindow:';
    procedure viewDidMoveToHostWindow; message 'viewDidMoveToHostWindow';
  end;
  
{ WebDocumentSearching Protocol }
  WebDocumentSearchingProtocol = objcprotocol external name 'WebDocumentSearching'
    function searchFor_direction_caseSensitive_wrap(string_: NSString; forward: Boolean; caseFlag: Boolean; wrapFlag: Boolean): Boolean; message 'searchFor:direction:caseSensitive:wrap:';
  end;
  
{ WebDocumentText Protocol }
  WebDocumentTextProtocol = objcprotocol external name 'WebDocumentText'
    function supportsTextEncoding: Boolean; message 'supportsTextEncoding';
    function string_: NSString; message 'string';
    function attributedString: NSAttributedString; message 'attributedString';
    function selectedString: NSString; message 'selectedString';
    function selectedAttributedString: NSAttributedString; message 'selectedAttributedString';
    procedure selectAll; message 'selectAll';
    procedure deselectAll; message 'deselectAll';
  end;
  
{ WebDocumentRepresentation Protocol }
  WebDocumentRepresentationProtocol = objcprotocol external name 'WebDocumentRepresentation'
    procedure setDataSource(dataSource: WebDataSource); message 'setDataSource:';
    procedure receivedData_withDataSource(data: NSData; dataSource: WebDataSource); message 'receivedData:withDataSource:';
    procedure receivedError_withDataSource(error: NSError; dataSource: WebDataSource); message 'receivedError:withDataSource:';
    procedure finishedLoadingWithDataSource(dataSource: WebDataSource); message 'finishedLoadingWithDataSource:';
    function canProvideDocumentSource: Boolean; message 'canProvideDocumentSource';
    function documentSource: NSString; message 'documentSource';
    function title: NSString; message 'title';
  end;
{$endif}
{$endif}
