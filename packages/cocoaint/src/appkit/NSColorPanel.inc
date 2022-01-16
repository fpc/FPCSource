{ Parsed from AppKit.framework NSColorPanel.h }


{$ifdef TYPES}
type
  NSColorPanelPtr = ^NSColorPanel;
{$endif}

{$ifdef TYPES}
type
  NSColorPanelMode = NSInteger;
  NSColorPanelModePtr = ^NSColorPanelMode;

const
  NSNoModeColorPanel = -1 { available in 10_5 };
  NSGrayModeColorPanel = 0;
  NSRGBModeColorPanel = 1;
  NSCMYKModeColorPanel = 2;
  NSHSBModeColorPanel = 3;
  NSCustomPaletteModeColorPanel = 4;
  NSColorListModeColorPanel = 5;
  NSWheelModeColorPanel = 6;
  NSCrayonModeColorPanel = 7;

type
  NSColorPanelOptions = NSUInteger;
  NSColorPanelOptionsPtr = ^NSColorPanelOptions;

const
  NSColorPanelGrayModeMask = $00000001;
  NSColorPanelRGBModeMask = $00000002;
  NSColorPanelCMYKModeMask = $00000004;
  NSColorPanelHSBModeMask = $00000008;
  NSColorPanelCustomPaletteModeMask = $00000010;
  NSColorPanelColorListModeMask = $00000020;
  NSColorPanelWheelModeMask = $00000040;
  NSColorPanelCrayonModeMask = $00000080;
  NSColorPanelAllModesMask = $0000ffff;
{$endif}

{$ifdef CLASSES}

type
  NSColorPanel = objcclass external (NSPanel)
  private
    _colorSwatch: id;
    _accessoryContainerView: id;
    _colorWell: id;
    _pickersWithLoadedViews: NSMutableArray;
    _magnifyButton: id;
    _middleView: id;
    _opacitySlider: id;
    _opacityText: id;
    _opacityView: id;
    _modalButtons: id;
    _pickerView: id;
    _customViewsList: id;
    _customPickerList: id;
    _currViewObject: id;
    _bottomConstraint: id;
    _target: id;
    _accessoryView: id;
    _action: SEL;
    _minColorPanelSize: NSSize;
    _maxColorPanelSize: NSSize;
    _accessoryContainerViewHeight: id;
    _opacityViewHeight: id;
    _reserved1: id;
    _reserved2: id;
    _resizeDimple: id;
    _reserved3: ObjCBOOL;
    _reserved4: ObjCBOOL;
    _handlingOpacityMoveAction: ObjCBOOL;
    _ignoreConstraints: ObjCBOOL;
    _continuous: ObjCBOOL;
    _allowColorSetting: ObjCBOOL;
    _stillInitializing: ObjCBOOL;
    _reserved5: ObjCBOOL;
    _opacityTextController: id;
  public
    class function sharedColorPanel: NSColorPanel; message 'sharedColorPanel';
    class function sharedColorPanelExists: ObjCBOOL; message 'sharedColorPanelExists';
    class function dragColor_withEvent_fromView (color: NSColor; theEvent: NSEvent; sourceView: NSView): ObjCBOOL; message 'dragColor:withEvent:fromView:';
    class procedure setPickerMask (mask: NSColorPanelOptions); message 'setPickerMask:';
    class procedure setPickerMode (mode: NSColorPanelMode); message 'setPickerMode:';
    procedure setAccessoryView(newValue: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure setContinuous(newValue: ObjCBOOL); message 'setContinuous:';
    function isContinuous: ObjCBOOL; message 'isContinuous';
    procedure setShowsAlpha(newValue: ObjCBOOL); message 'setShowsAlpha:';
    function showsAlpha: ObjCBOOL; message 'showsAlpha';
    procedure setMode(newValue: NSColorPanelMode); message 'setMode:';
    function mode: NSColorPanelMode; message 'mode';
    procedure setColor(newValue: NSColor); message 'setColor:';
    function color: NSColor; message 'color';
    function alpha: CGFloat; message 'alpha';
    procedure setAction (aSelector: SEL); message 'setAction:';
    procedure setTarget (anObject: id); message 'setTarget:';
    procedure attachColorList (colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList (colorList: NSColorList); message 'detachColorList:';
  end;


type
  NSApplication_NSColorPanel = objccategory external name 'NSColorPanel' (NSApplication)
    procedure orderFrontColorPanel (sender: id); message 'orderFrontColorPanel:';
  end;

type
  NSColorPanelResponderMethod = objccategory external (NSObject)
    procedure changeColor (sender: id); message 'changeColor:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSColorPanelColorDidChangeNotification: NSString; cvar; external;
{$endif}

