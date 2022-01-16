{ Parsed from AppKit.framework NSTitlebarAccessoryViewController.h }


{$ifdef TYPES}
type
  NSTitlebarAccessoryViewControllerPtr = ^NSTitlebarAccessoryViewController;
{$endif}

{$ifdef CLASSES}

type
  NSTitlebarAccessoryViewController = objcclass external (NSViewController)
  private
    _layoutAttribute: NSLayoutAttribute;
    _fullScreenMinHeight: CGFloat;
    _containingClipView: NSClipView;
    _revealAmount: CGFloat;
    _inFullScreen: ObjCBOOL;
    _isToolbarAccessoryView: ObjCBOOL;
    _updateCount: NSInteger;
    _anonStruct_NSTitlebarAccessoryViewController0: record
      case byte of
        0: (anonBitField_NSTitlebarAccessoryViewController0: cuint);
        1: (data: bitpacked record
          _isInspectorBarView: 0..1;
          _forceVisible: 0..1;
          _updatingFrame: 0..1;
          _registered: 0..1;
          _reservedTVC: 0..((1 shl 28)-1);
          {$if  not defined(__OBJC2__)}
          _reservedAVC: id;
          _reservedAVC2: id;
          {$endif}
        end;
      );
          end;
  public
    procedure setLayoutAttribute(newValue: NSLayoutAttribute); message 'setLayoutAttribute:';
    function layoutAttribute: NSLayoutAttribute; message 'layoutAttribute';
    procedure setFullScreenMinHeight(newValue: CGFloat); message 'setFullScreenMinHeight:';
    function fullScreenMinHeight: CGFloat; message 'fullScreenMinHeight';
    procedure viewWillAppear; message 'viewWillAppear'; { NS_REQUIRES_SUPER }
    procedure viewDidAppear; message 'viewDidAppear'; { NS_REQUIRES_SUPER }
    procedure viewDidDisappear; message 'viewDidDisappear'; { NS_REQUIRES_SUPER }
  end;
{$endif}

