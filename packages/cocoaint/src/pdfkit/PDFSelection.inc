{ Parsed from PDFKit.framework PDFSelection.h }


{ Types from PDFSelection }
{$ifdef TYPES}





{$endif}


{$ifdef TYPES}
type
  PDFSelectionPtr = ^PDFSelection;
{$endif}

{$ifdef CLASSES}

type
  PDFSelection = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pdfPriv: PDFSelectionPrivateVars;
  public
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function initWithDocument (document: PDFDocument): id; message 'initWithDocument:';
    {$endif}
    function pages: NSArray; message 'pages';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function color: NSColor; message 'color';
    procedure setColor (color_: NSColor); message 'setColor:';
    {$endif}
    function string_: NSString; message 'string';
    function attributedString: NSAttributedString; message 'attributedString';
    function boundsForPage (page: PDFPage): NSRect; message 'boundsForPage:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7)}
    function numberOfTextRangesOnPage (page: PDFPage): NSUInteger; message 'numberOfTextRangesOnPage:';
    function rangeAtIndex_onPage (index: NSUInteger; page: PDFPage): NSRange; message 'rangeAtIndex:onPage:';
    {$endif}
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function selectionsByLine: NSArray; message 'selectionsByLine';
    {$endif}
    procedure addSelection (selection: PDFSelection); message 'addSelection:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure addSelections (selections: NSArray); message 'addSelections:';
    {$endif}
    procedure extendSelectionAtEnd (succeed: NSInteger); message 'extendSelectionAtEnd:';
    procedure extendSelectionAtStart (precede: NSInteger); message 'extendSelectionAtStart:';
    procedure drawForPage_active (page: PDFPage; active: ObjCBOOL); message 'drawForPage:active:';
    procedure drawForPage_withBox_active (page: PDFPage; box: PDFDisplayBox; active: ObjCBOOL); message 'drawForPage:withBox:active:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

