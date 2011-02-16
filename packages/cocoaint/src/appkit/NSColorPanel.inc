{ Parsed from Appkit.framework NSColorPanel.h }

{$ifdef TYPES}
{$ifndef NSCOLORPANEL_PAS_T}
{$define NSCOLORPANEL_PAS_T}

{ Types }
type
  NSColorPanelMode = NSInteger;
  NSColorPanelModePtr = ^NSColorPanelMode;

{ Constants }

const
  NSNoModeColorPanel = -1;
  NSGrayModeColorPanel = 0;
  NSRGBModeColorPanel = 1;
  NSCMYKModeColorPanel = 2;
  NSHSBModeColorPanel = 3;
  NSCustomPaletteModeColorPanel = 4;
  NSColorListModeColorPanel = 5;
  NSWheelModeColorPanel = 6;
  NSCrayonModeColorPanel = 7;

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
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLORPANEL_PAS_R}
{$define NSCOLORPANEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLORPANEL_PAS_F}
{$define NSCOLORPANEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLORPANEL_PAS_S}
{$define NSCOLORPANEL_PAS_S}

{ External string constants }
var
  NSColorPanelColorDidChangeNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorPanel = objcclass;
  NSColorPanelPointer = ^NSColorPanel;
  NSColorPanelPtr = NSColorPanelPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORPANEL_PAS_C}
{$define NSCOLORPANEL_PAS_C}

{ NSColorPanel }
  NSColorPanel = objcclass external (NSPanel)
  private
    _colorSwatch: id;
    _reserved1: id;
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
    _boxAboveSwatch: id;
    _target: id;
    _accessoryView: id;
    _action: SEL;
    _minColorPanelSize: NSSize;
    _maxColorPanelSize: NSSize;
    _reserved2: NSSize;
    _reserved3: NSSize;
    _resizeDimple: id;
    _reserved5: Boolean;
    _reserved6: Boolean;
    _handlingOpacityMoveAction: Boolean;
    _ignoreConstraints: Boolean;
    _continuous: Boolean;
    _allowColorSetting: Boolean;
    _stillInitializing: Boolean;
    _opacityTextController: id;
    
  public
    class function sharedColorPanel: NSColorPanel; message 'sharedColorPanel';
    class function sharedColorPanelExists: Boolean; message 'sharedColorPanelExists';
    class function dragColor_withEvent_fromView(color_: NSColor; theEvent: NSEvent; sourceView: NSView): Boolean; message 'dragColor:withEvent:fromView:';
    class procedure setPickerMask(mask: NSUInteger); message 'setPickerMask:';
    class procedure setPickerMode(mode_: NSColorPanelMode); message 'setPickerMode:';
    procedure setAccessoryView(aView: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    procedure setContinuous(flag: Boolean); message 'setContinuous:';
    function isContinuous: Boolean; message 'isContinuous';
    procedure setShowsAlpha(flag: Boolean); message 'setShowsAlpha:';
    function showsAlpha: Boolean; message 'showsAlpha';
    procedure setMode(mode_: NSColorPanelMode); message 'setMode:';
    function mode: NSColorPanelMode; message 'mode';
    procedure setColor(color_: NSColor); message 'setColor:';
    function color: NSColor; message 'color';
    function alpha: CGFloat; message 'alpha';
    procedure setAction(aSelector: SEL); message 'setAction:';
    procedure setTarget(anObject: id); message 'setTarget:';
    procedure attachColorList(colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList(colorList: NSColorList); message 'detachColorList:';
  end;

{ NSColorPanel_NSApplicationCategory }
  NSColorPanel_NSApplicationCategory = objccategory external name 'NSColorPanel' (NSApplication)
    procedure orderFrontColorPanel(sender: id); message 'orderFrontColorPanel:';
  end;

{ NSColorPanelResponderMethodCategory }
  NSColorPanelResponderMethodCategory = objccategory external (NSObject)
    procedure changeColor(sender: id); message 'changeColor:';
  end;

{$endif}
{$endif}
