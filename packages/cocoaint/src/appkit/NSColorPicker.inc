{ Parsed from Appkit.framework NSColorPicker.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOLORPICKER_PAS_H}
{$define NSCOLORPICKER_PAS_H}
type
  NSColorPickerPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSCOLORPICKER_PAS_T}
{$define NSCOLORPICKER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorPicker = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORPICKER_PAS_C}
{$define NSCOLORPICKER_PAS_C}

{ NSColorPicker }
  NSColorPicker = objcclass(NSObject, NSColorPickingDefaultProtocol)
  private
    __imageObject: id;
    __colorPanel: NSColorPanel;
    __buttonToolTip: NSString;
    
  public
    class function alloc: NSColorPicker; message 'alloc';

    function initWithPickerMask_colorPanel(mask: culong; owningColorPanel: NSColorPanel): id; message 'initWithPickerMask:colorPanel:';
    function colorPanel: NSColorPanel; message 'colorPanel';
    function provideNewButtonImage: NSImage; message 'provideNewButtonImage';
    procedure insertNewButtonImage_in(newButtonImage: NSImage; buttonCell: NSButtonCell); message 'insertNewButtonImage:in:';
    procedure viewSizeChanged(sender: id); message 'viewSizeChanged:';
    procedure attachColorList(colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList(colorList: NSColorList); message 'detachColorList:';
    procedure setMode(mode: NSColorPanelMode); message 'setMode:';
    function buttonToolTip: NSString; message 'buttonToolTip';
    function minContentSize: NSSize; message 'minContentSize';
  end; external;

{$endif}
{$endif}
