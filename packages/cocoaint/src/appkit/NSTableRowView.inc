{ Parsed from AppKit.framework NSTableRowView.h }


{$ifdef TYPES}
type
  NSTableRowViewPtr = ^NSTableRowView;
{$endif}

{$ifdef CLASSES}

type
  NSTableRowView = objcclass external (NSView, NSAccessibilityRowProtocol)
  private
    _columnViews: NSViewPtr;
    _columnCount: NSInteger;
    _selectionHighlightStyle: NSTableViewSelectionHighlightStyle;
    _draggingDestinationFeedbackStyle: NSTableViewDraggingDestinationFeedbackStyle;
    _dropOperationIndentation: CGFloat;
    _backgroundColor: NSColor;
    _selectionAlpha: CGFloat;
    _anonStruct_NSTableRowView0: record
      case byte of
        0: (anonBitField_NSTableRowView0: cuint);
        1: (data: bitpacked record
          _targetForDropOperation: 0..1;
          _groupRowStyle: 0..((1 shl 2)-1);
          _priorRowSelected: 0..1;
          _nextRowSelected: 0..1;
          _selected: 0..1;
          _emphasized: 0..1;
          _hasAssociatedViews: 0..1;
          _backgroundStylesDirty: 0..1;
          _isFloating: 0..1;
          _hasCustomGridColor: 0..1;
          _usingCachedImageOnly: 0..1;
          _gridStyleMask: 0..((1 shl 4)-1);
          _updatingBackgroundStyle: 0..1;
          _locationNeedsUpdating: 0..1;
          _isStatic: 0..1;
          _hasSelectedBackgroundView: 0..1;
          _hasDropTargetBackgroundView: 0..1;
          _selectionBlendingMode: 0..1;
          _checkingFontRefColor: 0..1;
          {$if  not defined(__LP64__)}
          _anonStruct_NSTableRowView1: record
            case byte of
              0: (anonBitField_NSTableRowView1: cuint);
              1: (data: bitpacked record
                _reserved2: 0..((1 shl 8)-1);
              end;
            );
                end;
          {$endif}
        end;
      );
          end;
  public
    procedure setSelectionHighlightStyle(newValue: NSTableViewSelectionHighlightStyle); message 'setSelectionHighlightStyle:';
    function selectionHighlightStyle: NSTableViewSelectionHighlightStyle; message 'selectionHighlightStyle';
    procedure setEmphasized(newValue: ObjCBOOL); message 'setEmphasized:';
    function isEmphasized: ObjCBOOL; message 'isEmphasized';
    procedure setGroupRowStyle(newValue: ObjCBOOL); message 'setGroupRowStyle:';
    function isGroupRowStyle: ObjCBOOL; message 'isGroupRowStyle';
    procedure setSelected(newValue: ObjCBOOL); message 'setSelected:';
    function isSelected: ObjCBOOL; message 'isSelected';
    procedure setPreviousRowSelected(newValue: ObjCBOOL); message 'setPreviousRowSelected:';
    function isPreviousRowSelected: ObjCBOOL; message 'isPreviousRowSelected';
    procedure setNextRowSelected(newValue: ObjCBOOL); message 'setNextRowSelected:';
    function isNextRowSelected: ObjCBOOL; message 'isNextRowSelected';
    procedure setFloating(newValue: ObjCBOOL); message 'setFloating:';
    function isFloating: ObjCBOOL; message 'isFloating';
    procedure setTargetForDropOperation(newValue: ObjCBOOL); message 'setTargetForDropOperation:';
    function isTargetForDropOperation: ObjCBOOL; message 'isTargetForDropOperation';
    procedure setDraggingDestinationFeedbackStyle(newValue: NSTableViewDraggingDestinationFeedbackStyle); message 'setDraggingDestinationFeedbackStyle:';
    function draggingDestinationFeedbackStyle: NSTableViewDraggingDestinationFeedbackStyle; message 'draggingDestinationFeedbackStyle';
    procedure setIndentationForDropOperation(newValue: CGFloat); message 'setIndentationForDropOperation:';
    function indentationForDropOperation: CGFloat; message 'indentationForDropOperation';
    function interiorBackgroundStyle: NSBackgroundStyle; message 'interiorBackgroundStyle';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure drawBackgroundInRect (dirtyRect: NSRect); message 'drawBackgroundInRect:';
    procedure drawSelectionInRect (dirtyRect: NSRect); message 'drawSelectionInRect:';
    procedure drawSeparatorInRect (dirtyRect: NSRect); message 'drawSeparatorInRect:';
    procedure drawDraggingDestinationFeedbackInRect (dirtyRect: NSRect); message 'drawDraggingDestinationFeedbackInRect:';
    function viewAtColumn (column: NSInteger): id; message 'viewAtColumn:';
    function numberOfColumns: NSInteger; message 'numberOfColumns';
  end;
{$endif}

