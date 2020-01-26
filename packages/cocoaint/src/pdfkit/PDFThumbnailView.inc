{ Parsed from PDFKit.framework PDFThumbnailView.h }


{$ifdef TYPES}
type
  PDFThumbnailViewPtr = ^PDFThumbnailView;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFThumbnailView = objcclass external (NSView, NSCodingProtocol)
  private
    _pdfPriv: PDFThumbnailViewPrivateVars;
  public
    procedure setPDFView (view: PDFView); message 'setPDFView:';
    function PDFView_: PDFView; message 'PDFView';
    procedure setThumbnailSize (size: NSSize); message 'setThumbnailSize:';
    function thumbnailSize: NSSize; message 'thumbnailSize';
    procedure setMaximumNumberOfColumns (maxColumns: NSUInteger); message 'setMaximumNumberOfColumns:';
    function maximumNumberOfColumns: NSUInteger; message 'maximumNumberOfColumns';
    procedure setLabelFont (font: NSFont); message 'setLabelFont:';
    function labelFont: NSFont; message 'labelFont';
    procedure setBackgroundColor (color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setAllowsDragging (allow: ObjCBOOL); message 'setAllowsDragging:';
    function allowsDragging: ObjCBOOL; message 'allowsDragging';
    procedure setAllowsMultipleSelection (flag: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    function selectedPages: NSArray; message 'selectedPages';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;
{$endif}

{$endif}
