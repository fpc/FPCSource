{ Parsed from WebKit.framework WebDocument.h }


{$ifdef TYPES}
type
  WebDocumentViewProtocolPtr = ^WebDocumentViewProtocol;
  WebDocumentSearchingProtocolPtr = ^WebDocumentSearchingProtocol;
  WebDocumentTextProtocolPtr = ^WebDocumentTextProtocol;
  WebDocumentRepresentationProtocolPtr = ^WebDocumentRepresentationProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  WebDocumentViewProtocol = objcprotocol external name 'WebDocumentView' (NSObjectProtocol)
    procedure setDataSource (dataSource: WebDataSource); message 'setDataSource:';
    procedure dataSourceUpdated (dataSource: WebDataSource); message 'dataSourceUpdated:';
    procedure setNeedsLayout (flag: ObjCBOOL); message 'setNeedsLayout:';
    procedure layout; message 'layout';
    procedure viewWillMoveToHostWindow (hostWindow: NSWindow); message 'viewWillMoveToHostWindow:';
    procedure viewDidMoveToHostWindow; message 'viewDidMoveToHostWindow';
  end;

type
  WebDocumentSearchingProtocol = objcprotocol external name 'WebDocumentSearching' (NSObjectProtocol)
    function searchFor_direction_caseSensitive_wrap (string_: NSString; forward: ObjCBOOL; caseFlag: ObjCBOOL; wrapFlag: ObjCBOOL): ObjCBOOL; message 'searchFor:direction:caseSensitive:wrap:';
  end;

type
  WebDocumentTextProtocol = objcprotocol external name 'WebDocumentText' (NSObjectProtocol)
    function supportsTextEncoding: ObjCBOOL; message 'supportsTextEncoding';
    function string_: NSString; message 'string';
    function attributedString: NSAttributedString; message 'attributedString';
    function selectedString: NSString; message 'selectedString';
    function selectedAttributedString: NSAttributedString; message 'selectedAttributedString';
    procedure selectAll; message 'selectAll';
    procedure deselectAll; message 'deselectAll';
  end;

type
  WebDocumentRepresentationProtocol = objcprotocol external name 'WebDocumentRepresentation' (NSObjectProtocol)
    procedure setDataSource (dataSource: WebDataSource); message 'setDataSource:';
    procedure receivedData_withDataSource (data: NSData; dataSource: WebDataSource); message 'receivedData:withDataSource:';
    procedure receivedError_withDataSource (error: NSError; dataSource: WebDataSource); message 'receivedError:withDataSource:';
    procedure finishedLoadingWithDataSource (dataSource: WebDataSource); message 'finishedLoadingWithDataSource:';
    function canProvideDocumentSource: ObjCBOOL; message 'canProvideDocumentSource';
    function documentSource: NSString; message 'documentSource';
    function title: NSString; message 'title';
  end;
{$endif}

