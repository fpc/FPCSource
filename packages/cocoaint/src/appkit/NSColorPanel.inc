{ Parsed from Appkit.framework NSColorPanel.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOLORPANEL_PAS_H}
{$define NSCOLORPANEL_PAS_H}
type
  NSColorPanelPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOLORPANEL_PAS_T}
{$define NSCOLORPANEL_PAS_T}

{ Types }
type
  NSColorPanelMode = clong;

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

{ CFString constants }
var
  NSColorPanelColorDidChangeNotification: CFStringRef; external name '_NSColorPanelColorDidChangeNotification';

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
{$ifndef NSCOLORPANEL_PAS_T}
{$define NSCOLORPANEL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorPanel = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORPANEL_PAS_C}
{$define NSCOLORPANEL_PAS_C}

{ NSColorPanel }
  NSColorPanel = objcclass(NSPanel)
  private
    __colorSwatch: id;
    __reserved1: id;
    __colorWell: id;
    __pickersWithLoadedViews: NSMutableArray;
    __magnifyButton: id;
    __middleView: id;
    __opacitySlider: id;
    __opacityText: id;
    __opacityView: id;
    __modalButtons: id;
    __pickerView: id;
    __customViewsList: id;
    __customPickerList: id;
    __currViewObject: id;
    __boxAboveSwatch: id;
    __target: id;
    __accessoryView: id;
    __action: SEL;
    __minColorPanelSize: NSSize;
    __maxColorPanelSize: NSSize;
    __reserved2: NSSize;
    __reserved3: NSSize;
    __resizeDimple: id;
    __reserved5: Boolean;
    __reserved6: Boolean;
    __handlingOpacityMoveAction: Boolean;
    __ignoreConstraints: Boolean;
    __continuous: Boolean;
    __allowColorSetting: Boolean;
    __stillInitializing: Boolean;
    __opacityTextController: id;
    
  public
    class function alloc: NSColorPanel; message 'alloc';

    class function sharedColorPanel: NSColorPanel; message 'sharedColorPanel';
    class function sharedColorPanelExists: Boolean; message 'sharedColorPanelExists';
    class function dragColor_withEvent_fromView(color_: NSColor; theEvent: NSEvent; sourceView: NSView): Boolean; message 'dragColor:withEvent:fromView:';
    class procedure setPickerMask(mask: culong); message 'setPickerMask:';
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
  end; external;

{$endif}
{$endif}
