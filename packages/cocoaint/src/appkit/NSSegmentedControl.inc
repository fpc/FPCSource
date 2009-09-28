{ Parsed from Appkit.framework NSSegmentedControl.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSEGMENTEDCONTROL_PAS_H}
{$define NSSEGMENTEDCONTROL_PAS_H}
type
  NSSegmentedControlPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSEGMENTEDCONTROL_PAS_T}
{$define NSSEGMENTEDCONTROL_PAS_T}

{ Constants }

const
  NSSegmentStyleAutomatic = 0;
  NSSegmentStyleRounded = 1;
  NSSegmentStyleTexturedRounded = 2;
  NSSegmentStyleRoundRect = 3;
  NSSegmentStyleTexturedSquare = 4;
  NSSegmentStyleCapsule = 5;
  NSSegmentStyleSmallSquare = 6;

{ Types }
type
  NSSegmentStyle = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSEGMENTEDCONTROL_PAS_R}
{$define NSSEGMENTEDCONTROL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSEGMENTEDCONTROL_PAS_F}
{$define NSSEGMENTEDCONTROL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSEGMENTEDCONTROL_PAS_S}
{$define NSSEGMENTEDCONTROL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSegmentedControl = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSEGMENTEDCONTROL_PAS_C}
{$define NSSEGMENTEDCONTROL_PAS_C}

{ NSSegmentedControl }
  NSSegmentedControl = objcclass(NSControl)
  private
    __reserved1: NSInteger;
    __reserved2: NSInteger;
    __reserved3: NSInteger;
    __reserved4: NSInteger;
    
  public
    class function alloc: NSSegmentedControl; message 'alloc';

    procedure setSegmentCount(count: NSInteger); message 'setSegmentCount:';
    function segmentCount: NSInteger; message 'segmentCount';
    procedure setSelectedSegment(selectedSegment_: NSInteger); message 'setSelectedSegment:';
    function selectedSegment: NSInteger; message 'selectedSegment';
    function selectSegmentWithTag(tag_: NSInteger): Boolean; message 'selectSegmentWithTag:';
    procedure setWidth_forSegment(width: CGFloat; segment: NSInteger); message 'setWidth:forSegment:';
    function widthForSegment(segment: NSInteger): CGFloat; message 'widthForSegment:';
    procedure setImage_forSegment(image: NSImage; segment: NSInteger); message 'setImage:forSegment:';
    function imageForSegment(segment: NSInteger): NSImage; message 'imageForSegment:';
    procedure setImageScaling_forSegment(scaling: NSImageScaling; segment: NSInteger); message 'setImageScaling:forSegment:';
    function imageScalingForSegment(segment: NSInteger): NSImageScaling; message 'imageScalingForSegment:';
    procedure setLabel_forSegment(label_: NSString; segment: NSInteger); message 'setLabel:forSegment:';
    function labelForSegment(segment: NSInteger): NSString; message 'labelForSegment:';
    procedure setMenu_forSegment(menu_: NSMenu; segment: NSInteger); message 'setMenu:forSegment:';
    function menuForSegment(segment: NSInteger): NSMenu; message 'menuForSegment:';
    procedure setSelected_forSegment(selected: Boolean; segment: NSInteger); message 'setSelected:forSegment:';
    function isSelectedForSegment(segment: NSInteger): Boolean; message 'isSelectedForSegment:';
    procedure setEnabled_forSegment(enabled: Boolean; segment: NSInteger); message 'setEnabled:forSegment:';
    function isEnabledForSegment(segment: NSInteger): Boolean; message 'isEnabledForSegment:';
    procedure setSegmentStyle(segmentStyle_: NSSegmentStyle); message 'setSegmentStyle:';
    function segmentStyle: NSSegmentStyle; message 'segmentStyle';
  end; external;

{$endif}
{$endif}
