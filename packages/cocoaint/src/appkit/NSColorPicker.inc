{ Parsed from Appkit.framework NSColorPicker.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSCOLORPICKER_PAS_T}
{$define NSCOLORPICKER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLORPICKER_PAS_R}
{$define NSCOLORPICKER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLORPICKER_PAS_F}
{$define NSCOLORPICKER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLORPICKER_PAS_S}
{$define NSCOLORPICKER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorPicker = objcclass;
  NSColorPickerPointer = ^NSColorPicker;
  NSColorPickerPtr = NSColorPickerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORPICKER_PAS_C}
{$define NSCOLORPICKER_PAS_C}

{ NSColorPicker }
  NSColorPicker = objcclass(NSObject, NSColorPickingDefaultProtocol)
  private
    _imageObject: id;
    _colorPanel: NSColorPanel;
    _buttonToolTip: NSString;
    
  public
    class function alloc: NSColorPicker; message 'alloc';

    function initWithPickerMask_colorPanel(mask: NSUInteger; owningColorPanel: NSColorPanel): id; message 'initWithPickerMask:colorPanel:';
    function colorPanel: NSColorPanel; message 'colorPanel';
    function provideNewButtonImage: NSImage; message 'provideNewButtonImage';
    procedure insertNewButtonImage_in(newButtonImage: NSImage; buttonCell: NSButtonCell); message 'insertNewButtonImage:in:';
    procedure viewSizeChanged(sender: id); message 'viewSizeChanged:';
    procedure attachColorList(colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList(colorList: NSColorList); message 'detachColorList:';
    procedure setMode(mode: NSColorPanelMode); message 'setMode:';
    function buttonToolTip: NSString; message 'buttonToolTip';
    function minContentSize: NSSize; message 'minContentSize';

    { Adopted Protocols }
    procedure alphaControlAddedOrRemoved(sender: id);
  end; external;

{$endif}
{$endif}
