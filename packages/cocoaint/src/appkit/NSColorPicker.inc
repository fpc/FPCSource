{ Parsed from AppKit.framework NSColorPicker.h }


{$ifdef TYPES}
type
  NSColorPickerPtr = ^NSColorPicker;
{$endif}

{$ifdef CLASSES}

type
  NSColorPicker = objcclass external (NSObject, NSColorPickingDefaultProtocol)
  private
    _imageObject: id;
    _colorPanel: NSColorPanel;
    _buttonToolTip: NSString;
  public
    function initWithPickerMask_colorPanel (mask: NSUInteger; owningColorPanel: NSColorPanel): instancetype; message 'initWithPickerMask:colorPanel:';
    function colorPanel: NSColorPanel; message 'colorPanel';
    function provideNewButtonImage: NSImage; message 'provideNewButtonImage';
    procedure insertNewButtonImage_in (newButtonImage: NSImage; buttonCell: NSButtonCell); message 'insertNewButtonImage:in:';
    procedure viewSizeChanged (sender: id); message 'viewSizeChanged:';
    procedure attachColorList (colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList (colorList: NSColorList); message 'detachColorList:';
    procedure setMode (mode: NSColorPanelMode); message 'setMode:';
    function buttonToolTip: NSString; message 'buttonToolTip';
    function minContentSize: NSSize; message 'minContentSize';

    { Adopted protocols }
    procedure alphaControlAddedOrRemoved (sender: id); message 'alphaControlAddedOrRemoved:';
  end;
{$endif}

