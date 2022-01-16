{ Parsed from PDFKit.framework PDFOutline.h }


{ Types from PDFOutline }
{$ifdef TYPES}





{$endif}


{$ifdef TYPES}
type
  PDFOutlinePtr = ^PDFOutline;
{$endif}

{$ifdef CLASSES}

type
  PDFOutline = objcclass external (NSObject)
  private
    _pdfPriv: PDFOutlinePrivateVars;
  public
    function init: id; message 'init';
    function document: PDFDocument; message 'document';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function parent: PDFOutline; message 'parent';
    {$endif}
    function numberOfChildren: NSUInteger; message 'numberOfChildren';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function index: NSUInteger; message 'index';
    {$endif}
    function childAtIndex (index_: NSUInteger): PDFOutline; message 'childAtIndex:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure insertChild_atIndex (child: PDFOutline; index_: NSUInteger); message 'insertChild:atIndex:';
    procedure removeFromParent; message 'removeFromParent';
    {$endif}
    function label_: NSString; message 'label';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure setLabel (label__: NSString); message 'setLabel:';
    function isOpen: ObjCBOOL; message 'isOpen';
    procedure setIsOpen (open: ObjCBOOL); message 'setIsOpen:';
    {$endif}
    function destination: PDFDestination; message 'destination';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure setDestination (destination_: PDFDestination); message 'setDestination:';
    function action: PDFAction; message 'action';
    procedure setAction (action_: PDFAction); message 'setAction:';
    {$endif}
  end;
{$endif}

