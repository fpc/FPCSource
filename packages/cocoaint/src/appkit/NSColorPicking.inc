{ Parsed from Appkit.framework NSColorPicking.h }

{$ifdef TYPES}
{$ifndef NSCOLORPICKING_PAS_T}
{$define NSCOLORPICKING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLORPICKING_PAS_R}
{$define NSCOLORPICKING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLORPICKING_PAS_F}
{$define NSCOLORPICKING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLORPICKING_PAS_S}
{$define NSCOLORPICKING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorPickingDefaultProtocol = objcprotocol;
  NSColorPickingCustomProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSCOLORPICKING_PAS_P}
{$define NSCOLORPICKING_PAS_P}
  
{ NSColorPickingDefault Protocol }
  NSColorPickingDefaultProtocol = objcprotocol external name 'NSColorPickingDefault'
    function initWithPickerMask_colorPanel(mask: NSUInteger; owningColorPanel: NSColorPanel): id; message 'initWithPickerMask:colorPanel:';
    function provideNewButtonImage: NSImage; message 'provideNewButtonImage';
    procedure insertNewButtonImage_in(newButtonImage: NSImage; buttonCell: NSButtonCell); message 'insertNewButtonImage:in:';
    procedure viewSizeChanged(sender: id); message 'viewSizeChanged:';
    procedure alphaControlAddedOrRemoved(sender: id); message 'alphaControlAddedOrRemoved:';
    procedure attachColorList(colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList(colorList: NSColorList); message 'detachColorList:';
    procedure setMode(mode: NSColorPanelMode); message 'setMode:';
    function buttonToolTip: NSString; message 'buttonToolTip';
    function minContentSize: NSSize; message 'minContentSize';
  end;
  
{ NSColorPickingCustom Protocol }
  NSColorPickingCustomProtocol = objcprotocol external name 'NSColorPickingCustom'
    function supportsMode(mode: NSColorPanelMode): Boolean; message 'supportsMode:';
    function currentMode: NSColorPanelMode; message 'currentMode';
    function provideNewView(initialRequest: Boolean): NSView; message 'provideNewView:';
    procedure setColor(newColor: NSColor); message 'setColor:';
  end;
{$endif}
{$endif}
