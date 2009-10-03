{ Parsed from Webkit.framework WebDocument.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
    procedure viewWillMoveToHostWindow(hostWindow: NSWindowPointer); message 'viewWillMoveToHostWindow:';
    procedure viewDidMoveToHostWindow; message 'viewDidMoveToHostWindow';
  end; external name 'WebDocumentView';
  
{ WebDocumentSearching Protocol }
  WebDocumentSearchingProtocol = objcprotocol
    function searchFor_direction_caseSensitive_wrap(string_: NSStringPointer; forward: Boolean; caseFlag: Boolean; wrapFlag: Boolean): Boolean; message 'searchFor:direction:caseSensitive:wrap:';
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
    procedure receivedData_withDataSource(data: NSDataPointer; dataSource: WebDataSource); message 'receivedData:withDataSource:';
    procedure receivedError_withDataSource(error: NSErrorPointer; dataSource: WebDataSource); message 'receivedError:withDataSource:';
    procedure finishedLoadingWithDataSource(dataSource: WebDataSource); message 'finishedLoadingWithDataSource:';
    function canProvideDocumentSource: Boolean; message 'canProvideDocumentSource';
    function documentSource: NSString; message 'documentSource';
    function title: NSString; message 'title';
  end; external name 'WebDocumentRepresentation';
{$endif}
{$endif}
