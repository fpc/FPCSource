{ Parsed from AppKit.framework NSColorPicking.h }


{$ifdef TYPES}
type
  NSColorPickingDefaultProtocolPtr = ^NSColorPickingDefaultProtocol;
  NSColorPickingCustomProtocolPtr = ^NSColorPickingCustomProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSColorPickingDefaultProtocol = objcprotocol external name 'NSColorPickingDefault'
    function initWithPickerMask_colorPanel (mask: NSUInteger; owningColorPanel: NSColorPanel): instancetype; message 'initWithPickerMask:colorPanel:';
    function provideNewButtonImage: NSImage; message 'provideNewButtonImage';
    procedure insertNewButtonImage_in (newButtonImage: NSImage; buttonCell: NSButtonCell); message 'insertNewButtonImage:in:';
    procedure viewSizeChanged (sender: id); message 'viewSizeChanged:';
    procedure alphaControlAddedOrRemoved (sender: id); message 'alphaControlAddedOrRemoved:';
    procedure attachColorList (colorList: NSColorList); message 'attachColorList:';
    procedure detachColorList (colorList: NSColorList); message 'detachColorList:';
    procedure setMode (mode: NSColorPanelMode); message 'setMode:';
    function buttonToolTip: NSString; message 'buttonToolTip'; { available in 10_5 }
    function minContentSize: NSSize; message 'minContentSize'; { available in 10_5 }
  end;

type
  NSColorPickingCustomProtocol = objcprotocol external name 'NSColorPickingCustom' (NSColorPickingDefaultProtocol)
    function supportsMode (mode: NSColorPanelMode): ObjCBOOL; message 'supportsMode:';
    function currentMode: NSColorPanelMode; message 'currentMode';
    function provideNewView (initialRequest: ObjCBOOL): NSView; message 'provideNewView:';
    procedure setColor (newColor: NSColor); message 'setColor:';
  end;
{$endif}

