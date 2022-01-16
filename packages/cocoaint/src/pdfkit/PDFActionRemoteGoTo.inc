{ Parsed from PDFKit.framework PDFActionRemoteGoTo.h }


{$ifdef TYPES}
type
  PDFActionRemoteGoToPtr = ^PDFActionRemoteGoTo;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFActionRemoteGoTo = objcclass external (PDFAction, NSCopyingProtocol)
  private
    _pdfPriv2: PDFActionRemoteGoToPrivateVars;
  public
    function initWithPageIndex_atPoint_fileURL (pageIndex: NSUInteger; point: NSPoint; url: NSURL): id; message 'initWithPageIndex:atPoint:fileURL:';
    function pageIndex: NSUInteger; message 'pageIndex';
    procedure setPageIndex (pageIndex_: NSUInteger); message 'setPageIndex:';
    function point: NSPoint; message 'point';
    procedure setPoint (point_: NSPoint); message 'setPoint:';
    function URL: NSURL; message 'URL';
    procedure setURL (url_: NSURL); message 'setURL:';
  end;
{$endif}

{$endif}
