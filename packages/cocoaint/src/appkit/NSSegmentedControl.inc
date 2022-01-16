{ Parsed from AppKit.framework NSSegmentedControl.h }


{$ifdef TYPES}
type
  NSSegmentedControlPtr = ^NSSegmentedControl;
{$endif}

{$ifdef TYPES}
type
  NSSegmentStyle = NSInteger;
  NSSegmentStylePtr = ^NSSegmentStyle;

const
  NSSegmentStyleAutomatic = 0;
  NSSegmentStyleRounded = 1;
  NSSegmentStyleRoundRect = 3;
  NSSegmentStyleTexturedSquare = 4;
  NSSegmentStyleSmallSquare = 6;
  NSSegmentStyleSeparated = 8 { available in 10_10 };
  NSSegmentStyleTexturedRounded = 2 { available in 10_5 };
  NSSegmentStyleCapsule = 5 { available in 10_5 };
{$endif}

{$ifdef CLASSES}

type
  NSSegmentedControl = objcclass external (NSControl)
  private
    _reserved1: NSInteger;
    _reserved2: NSInteger;
    _reserved3: NSInteger;
    _reserved4: NSInteger;
  public
    procedure setSegmentCount(newValue: NSInteger); message 'setSegmentCount:';
    function segmentCount: NSInteger; message 'segmentCount';
    procedure setSelectedSegment(newValue: NSInteger); message 'setSelectedSegment:';
    function selectedSegment: NSInteger; message 'selectedSegment';
    function selectSegmentWithTag (tag_: NSInteger): ObjCBOOL; message 'selectSegmentWithTag:';
    procedure setWidth_forSegment (width: CGFloat; segment: NSInteger); message 'setWidth:forSegment:';
    function widthForSegment (segment: NSInteger): CGFloat; message 'widthForSegment:';
    procedure setImage_forSegment (image: NSImage; segment: NSInteger); message 'setImage:forSegment:';
    function imageForSegment (segment: NSInteger): NSImage; message 'imageForSegment:';
    procedure setImageScaling_forSegment (scaling: NSImageScaling; segment: NSInteger); message 'setImageScaling:forSegment:'; { available in 10_5 }
    function imageScalingForSegment (segment: NSInteger): NSImageScaling; message 'imageScalingForSegment:'; { available in 10_5 }
    procedure setLabel_forSegment (label_: NSString; segment: NSInteger); message 'setLabel:forSegment:';
    function labelForSegment (segment: NSInteger): NSString; message 'labelForSegment:';
    procedure setMenu_forSegment (menu_: NSMenu; segment: NSInteger); message 'setMenu:forSegment:';
    function menuForSegment (segment: NSInteger): NSMenu; message 'menuForSegment:';
    procedure setSelected_forSegment (selected: ObjCBOOL; segment: NSInteger); message 'setSelected:forSegment:';
    function isSelectedForSegment (segment: NSInteger): ObjCBOOL; message 'isSelectedForSegment:';
    procedure setEnabled_forSegment (enabled: ObjCBOOL; segment: NSInteger); message 'setEnabled:forSegment:';
    function isEnabledForSegment (segment: NSInteger): ObjCBOOL; message 'isEnabledForSegment:';
    procedure setSegmentStyle(newValue: NSSegmentStyle); message 'setSegmentStyle:';
    function segmentStyle: NSSegmentStyle; message 'segmentStyle';
  end;
{$endif}

