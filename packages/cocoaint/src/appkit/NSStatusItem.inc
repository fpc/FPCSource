{ Parsed from AppKit.framework NSStatusItem.h }


{$ifdef TYPES}
type
  NSStatusItemPtr = ^NSStatusItem;
{$endif}

{$ifdef CLASSES}

type
  NSStatusItem = objcclass external (NSObject)
  private
    _fStatusBar: NSStatusBar;
    _fLength: CGFloat;
    _fWindow: NSWindow;
    _fView: NSView;
    _fPriority: cint;
    _fFlags: bitpacked record
      case byte of
        0: (_anonBitField__fFlags0: cuint);
        1: (
          customView: 0..1;
          highlightMode: 0..1;
          hasAlternateImage: 0..1;
          hidden: 0..1;
          backgroundStyle: 0..((1 shl 4)-1);
          inAdjustLength: 0..1;
          pendingReplicantDisplay: 0..1;
          disableImageReplicationCount: 0..((1 shl 4)-1);
          updatingReplicant: 0..1;
          didInactiveTemplateStyling: 0..1;
          reserved: 0..((1 shl 16)-1);
        );
      end;
    _statusItemMenu: id;
    _replicants: NSMutableDictionary;
    _subitemOffsets: NSData;
    _displayIdentifier: NSString;
  public
    function statusBar: NSStatusBar; message 'statusBar';
    procedure setLength(newValue: CGFloat); message 'setLength:';
    function length: CGFloat; message 'length';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    function button: NSStatusBarButton; message 'button';
  end;


type
  NSStatusItemDeprecated = objccategory external (NSStatusItem)
    procedure setAction(newValue: SEL); message 'setAction:';
    function action: SEL; message 'action';
    procedure setDoubleAction(newValue: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setTarget(newValue: id); message 'setTarget:';
    function target: id; message 'target';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAttributedTitle(newValue: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setAlternateImage(newValue: NSImage); message 'setAlternateImage:';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setHighlightMode(newValue: ObjCBOOL); message 'setHighlightMode:';
    function highlightMode: ObjCBOOL; message 'highlightMode';
    procedure setToolTip(newValue: NSString); message 'setToolTip:';
    function toolTip: NSString; message 'toolTip';
    function sendActionOn (mask: NSInteger): NSInteger; message 'sendActionOn:';
    procedure setView(newValue: NSView); message 'setView:';
    function view: NSView; message 'view';
    procedure drawStatusBarBackgroundInRect_withHighlight (rect: NSRect; highlight: ObjCBOOL); message 'drawStatusBarBackgroundInRect:withHighlight:';
    procedure popUpStatusItemMenu (menu_: NSMenu); message 'popUpStatusItemMenu:';
  end;
{$endif}

