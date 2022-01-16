{ Parsed from AppKit.framework NSSegmentedCell.h }


{$ifdef TYPES}
type
  NSSegmentedCellPtr = ^NSSegmentedCell;
{$endif}

{$ifdef TYPES}
type
  NSSegmentSwitchTracking = NSUInteger;
  NSSegmentSwitchTrackingPtr = ^NSSegmentSwitchTracking;

const
  NSSegmentSwitchTrackingSelectOne = 0;
  NSSegmentSwitchTrackingSelectAny = 1;
  NSSegmentSwitchTrackingMomentary = 2;
{$endif}

{$ifdef CLASSES}

type
  NSSegmentedCell = objcclass external (NSActionCell)
  private
    _segmentItems: NSMutableArray;
    _selectedSegment: NSInteger;
    _keySegment: NSInteger;
    _lastBounds: NSRect;
    _seFlags: bitpacked record
      case byte of
        0: (_anonBitField__seFlags0: cuint);
        1: (
          trackingMode: 0..((1 shl 3)-1);
          trimmedLabels: 0..1;
          drawing: 0..1;
          reserved1: 0..((1 shl 2)-1);
          recalcToolTips: 0..1;
          usesWindowsStyle: 0..1;
          dontShowSelectedAndPressedAppearance: 0..1;
          menuShouldBeUniquedAgainstMain: 0..1;
          style: 0..((1 shl 8)-1);
          flatMinX: 0..1;
          flatMaxX: 0..1;
          segmentedSeparated: 0..1;
          reserved: 0..((1 shl 10)-1);
        );
      end;
    _segmentTrackingInfo: id;
    _menuUniquer: id;
    _reserved3: NSInteger;
    _reserved4: NSInteger;
  public
    procedure setSegmentCount(newValue: NSInteger); message 'setSegmentCount:';
    function segmentCount: NSInteger; message 'segmentCount';
    procedure setSelectedSegment(newValue: NSInteger); message 'setSelectedSegment:';
    function selectedSegment: NSInteger; message 'selectedSegment';
    function selectSegmentWithTag (tag_: NSInteger): ObjCBOOL; message 'selectSegmentWithTag:';
    procedure makeNextSegmentKey; message 'makeNextSegmentKey';
    procedure makePreviousSegmentKey; message 'makePreviousSegmentKey';
    procedure setTrackingMode(newValue: NSSegmentSwitchTracking); message 'setTrackingMode:';
    function trackingMode: NSSegmentSwitchTracking; message 'trackingMode';
    procedure setWidth_forSegment (width: CGFloat; segment: NSInteger); message 'setWidth:forSegment:';
    function widthForSegment (segment: NSInteger): CGFloat; message 'widthForSegment:';
    procedure setImage_forSegment (image_: NSImage; segment: NSInteger); message 'setImage:forSegment:';
    function imageForSegment (segment: NSInteger): NSImage; message 'imageForSegment:';
    procedure setImageScaling_forSegment (scaling: NSImageScaling; segment: NSInteger); message 'setImageScaling:forSegment:'; { available in 10_5 }
    function imageScalingForSegment (segment: NSInteger): NSImageScaling; message 'imageScalingForSegment:'; { available in 10_5 }
    procedure setLabel_forSegment (label_: NSString; segment: NSInteger); message 'setLabel:forSegment:';
    function labelForSegment (segment: NSInteger): NSString; message 'labelForSegment:';
    procedure setSelected_forSegment (selected: ObjCBOOL; segment: NSInteger); message 'setSelected:forSegment:';
    function isSelectedForSegment (segment: NSInteger): ObjCBOOL; message 'isSelectedForSegment:';
    procedure setEnabled_forSegment (enabled: ObjCBOOL; segment: NSInteger); message 'setEnabled:forSegment:';
    function isEnabledForSegment (segment: NSInteger): ObjCBOOL; message 'isEnabledForSegment:';
    procedure setMenu_forSegment (menu_: NSMenu; segment: NSInteger); message 'setMenu:forSegment:';
    function menuForSegment (segment: NSInteger): NSMenu; message 'menuForSegment:';
    procedure setToolTip_forSegment (toolTip: NSString; segment: NSInteger); message 'setToolTip:forSegment:';
    function toolTipForSegment (segment: NSInteger): NSString; message 'toolTipForSegment:';
    procedure setTag_forSegment (tag_: NSInteger; segment: NSInteger); message 'setTag:forSegment:';
    function tagForSegment (segment: NSInteger): NSInteger; message 'tagForSegment:';
    procedure setSegmentStyle(newValue: NSSegmentStyle); message 'setSegmentStyle:';
    function segmentStyle: NSSegmentStyle; message 'segmentStyle';
    procedure drawSegment_inFrame_withView (segment: NSInteger; frame: NSRect; controlView_: NSView); message 'drawSegment:inFrame:withView:';
  end;


type
  NSSegmentBackgroundStyle = objccategory external (NSSegmentedCell)
    function interiorBackgroundStyleForSegment (segment: NSInteger): NSBackgroundStyle; message 'interiorBackgroundStyleForSegment:'; { available in 10_5 }
  end;
{$endif}

