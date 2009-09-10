{ Parsed from Appkit.framework NSSegmentedControl.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

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
  NSSegmentStyleRounded = 1;
  NSSegmentStyleTexturedRounded = 2;
  NSSegmentStyleRoundRect = 3;
  NSSegmentStyleTexturedSquare = 4;
  NSSegmentStyleCapsule = 5;
  NSSegmentStyleSmallSquare = 6;

{ Types }
type
  NSSegmentStyle = clong;

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
{$ifndef NSSEGMENTEDCONTROL_PAS_T}
{$define NSSEGMENTEDCONTROL_PAS_T}

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
    __reserved1: clong;
    __reserved2: clong;
    __reserved3: clong;
    __reserved4: clong;
    
  public
    class function alloc: NSSegmentedControl; message 'alloc';

    procedure setSegmentCount(count: clong); message 'setSegmentCount:';
    function segmentCount: clong; message 'segmentCount';
    procedure setSelectedSegment(selectedSegment_: clong); message 'setSelectedSegment:';
    function selectedSegment: clong; message 'selectedSegment';
    function selectSegmentWithTag(tag_: clong): Boolean; message 'selectSegmentWithTag:';
    procedure setWidth_forSegment(width: CGFloat; segment: clong); message 'setWidth:forSegment:';
    function widthForSegment(segment: clong): CGFloat; message 'widthForSegment:';
    procedure setImage_forSegment(image: NSImage; segment: clong); message 'setImage:forSegment:';
    function imageForSegment(segment: clong): NSImage; message 'imageForSegment:';
    procedure setImageScaling_forSegment(scaling: NSImageScaling; segment: clong); message 'setImageScaling:forSegment:';
    function imageScalingForSegment(segment: clong): NSImageScaling; message 'imageScalingForSegment:';
    procedure setLabel_forSegment(label_: NSString; segment: clong); message 'setLabel:forSegment:';
    function labelForSegment(segment: clong): NSString; message 'labelForSegment:';
    procedure setMenu_forSegment(menu_: NSMenu; segment: clong); message 'setMenu:forSegment:';
    function menuForSegment(segment: clong): NSMenu; message 'menuForSegment:';
    procedure setSelected_forSegment(selected: Boolean; segment: clong); message 'setSelected:forSegment:';
    function isSelectedForSegment(segment: clong): Boolean; message 'isSelectedForSegment:';
    procedure setEnabled_forSegment(enabled: Boolean; segment: clong); message 'setEnabled:forSegment:';
    function isEnabledForSegment(segment: clong): Boolean; message 'isEnabledForSegment:';
    procedure setSegmentStyle(segmentStyle_: NSSegmentStyle); message 'setSegmentStyle:';
    function segmentStyle: NSSegmentStyle; message 'segmentStyle';
  end; external;

{$endif}
{$endif}
