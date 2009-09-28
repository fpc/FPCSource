{ Parsed from Appkit.framework NSSplitView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSPLITVIEW_PAS_H}
{$define NSSPLITVIEW_PAS_H}
type
  NSSplitViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSPLITVIEW_PAS_T}
{$define NSSPLITVIEW_PAS_T}

{ Constants }

const
  NSSplitViewDividerStyleThick = 1;
  NSSplitViewDividerStyleThin = 0;

{ Types }
type
  NSSplitViewDividerStyle = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPLITVIEW_PAS_R}
{$define NSSPLITVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPLITVIEW_PAS_F}
{$define NSSPLITVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPLITVIEW_PAS_S}
{$define NSSPLITVIEW_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSplitView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPLITVIEW_PAS_C}
{$define NSSPLITVIEW_PAS_C}

{ NSSplitView }
  NSSplitView = objcclass(NSView)
  private
    __variables: id;
    
  public
    class function alloc: NSSplitView; message 'alloc';

    procedure setVertical(flag: Boolean); message 'setVertical:';
    function isVertical: Boolean; message 'isVertical';
    procedure setDividerStyle(dividerStyle_: NSSplitViewDividerStyle); message 'setDividerStyle:';
    function dividerStyle: NSSplitViewDividerStyle; message 'dividerStyle';
    procedure setAutosaveName(autosaveName_: NSString); message 'setAutosaveName:';
    function autosaveName: NSString; message 'autosaveName';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure drawDividerInRect(rect: NSRect); message 'drawDividerInRect:';
    function dividerColor: NSColor; message 'dividerColor';
    function dividerThickness: CGFloat; message 'dividerThickness';
    procedure adjustSubviews; message 'adjustSubviews';
    function isSubviewCollapsed(subview: NSView): Boolean; message 'isSubviewCollapsed:';
    function minPossiblePositionOfDividerAtIndex(dividerIndex: NSInteger): CGFloat; message 'minPossiblePositionOfDividerAtIndex:';
    function maxPossiblePositionOfDividerAtIndex(dividerIndex: NSInteger): CGFloat; message 'maxPossiblePositionOfDividerAtIndex:';
    procedure setPosition_ofDividerAtIndex(position: CGFloat; dividerIndex: NSInteger); message 'setPosition:ofDividerAtIndex:';
    procedure setIsPaneSplitter(flag: Boolean); message 'setIsPaneSplitter:';
    function isPaneSplitter: Boolean; message 'isPaneSplitter';
  end; external;

{$endif}
{$endif}
