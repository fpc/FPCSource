{ Parsed from AppKit.framework NSTextTable.h }


{$ifdef TYPES}
type
  NSTextBlockPtr = ^NSTextBlock;
  NSTextTableBlockPtr = ^NSTextTableBlock;
  NSTextTablePtr = ^NSTextTable;
{$endif}

{$ifdef TYPES}
type
  NSTextBlockValueType = NSUInteger;
  NSTextBlockValueTypePtr = ^NSTextBlockValueType;

const
  NSTextBlockAbsoluteValueType = 0;
  NSTextBlockPercentageValueType = 1;

type
  NSTextBlockDimension = NSUInteger;
  NSTextBlockDimensionPtr = ^NSTextBlockDimension;

const
  NSTextBlockWidth = 0;
  NSTextBlockMinimumWidth = 1;
  NSTextBlockMaximumWidth = 2;
  NSTextBlockHeight = 4;
  NSTextBlockMinimumHeight = 5;
  NSTextBlockMaximumHeight = 6;

type
  NSTextBlockLayer = NSInteger;
  NSTextBlockLayerPtr = ^NSTextBlockLayer;

const
  NSTextBlockPadding = -1;
  NSTextBlockBorder = 0;
  NSTextBlockMargin = 1;

type
  NSTextBlockVerticalAlignment = NSUInteger;
  NSTextBlockVerticalAlignmentPtr = ^NSTextBlockVerticalAlignment;

const
  NSTextBlockTopAlignment = 0;
  NSTextBlockMiddleAlignment = 1;
  NSTextBlockBottomAlignment = 2;
  NSTextBlockBaselineAlignment = 3;

type
  NSTextTableLayoutAlgorithm = NSUInteger;
  NSTextTableLayoutAlgorithmPtr = ^NSTextTableLayoutAlgorithm;

const
  NSTextTableAutomaticLayoutAlgorithm = 0;
  NSTextTableFixedLayoutAlgorithm = 1;
{$endif}

{$ifdef CLASSES}

type
  NSTextBlock = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _propVals: pointer;
    _propMask: NSUInteger;
    _typeMask: NSUInteger;
    _primParamVal: id;
    _otherParamVals: id;
    _blockPrimary: pointer;
    _blockSecondary: pointer;
  public
    function init: instancetype; message 'init';
    procedure setValue_type_forDimension (val: CGFloat; type_: NSTextBlockValueType; dimension: NSTextBlockDimension); message 'setValue:type:forDimension:';
    function valueForDimension (dimension: NSTextBlockDimension): CGFloat; message 'valueForDimension:';
    function valueTypeForDimension (dimension: NSTextBlockDimension): NSTextBlockValueType; message 'valueTypeForDimension:';
    procedure setContentWidth_type (val: CGFloat; type_: NSTextBlockValueType); message 'setContentWidth:type:';
    function contentWidth: CGFloat; message 'contentWidth';
    function contentWidthValueType: NSTextBlockValueType; message 'contentWidthValueType';
    procedure setWidth_type_forLayer_edge (val: CGFloat; type_: NSTextBlockValueType; layer: NSTextBlockLayer; edge: NSRectEdge); message 'setWidth:type:forLayer:edge:';
    procedure setWidth_type_forLayer (val: CGFloat; type_: NSTextBlockValueType; layer: NSTextBlockLayer); message 'setWidth:type:forLayer:';
    function widthForLayer_edge (layer: NSTextBlockLayer; edge: NSRectEdge): CGFloat; message 'widthForLayer:edge:';
    function widthValueTypeForLayer_edge (layer: NSTextBlockLayer; edge: NSRectEdge): NSTextBlockValueType; message 'widthValueTypeForLayer:edge:';
    procedure setVerticalAlignment(newValue: NSTextBlockVerticalAlignment); message 'setVerticalAlignment:';
    function verticalAlignment: NSTextBlockVerticalAlignment; message 'verticalAlignment';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBorderColor_forEdge (color: NSColor; edge: NSRectEdge); message 'setBorderColor:forEdge:';
    procedure setBorderColor (color: NSColor); message 'setBorderColor:';
    function borderColorForEdge (edge: NSRectEdge): NSColor; message 'borderColorForEdge:';
    function rectForLayoutAtPoint_inRect_textContainer_characterRange (startingPoint: NSPoint; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'rectForLayoutAtPoint:inRect:textContainer:characterRange:';
    function boundsRectForContentRect_inRect_textContainer_characterRange (contentRect: NSRect; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'boundsRectForContentRect:inRect:textContainer:characterRange:';
    procedure drawBackgroundWithFrame_inView_characterRange_layoutManager (frameRect: NSRect; controlView: NSView; charRange: NSRange; layoutManager: NSLayoutManager); message 'drawBackgroundWithFrame:inView:characterRange:layoutManager:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;

type
  NSTextTableBlock = objcclass external (NSTextBlock)
  private
    _table: NSTextTable;
    _rowNum: NSInteger;
    _colNum: NSInteger;
    _rowSpan: NSInteger;
    _colSpan: NSInteger;
    _tableBlockPrimary: pointer;
    _tableBlockSecondary: pointer;
  public
    function initWithTable_startingRow_rowSpan_startingColumn_columnSpan (table: NSTextTable; row: NSInteger; rowSpan: NSInteger; col: NSInteger; colSpan: NSInteger): instancetype; message 'initWithTable:startingRow:rowSpan:startingColumn:columnSpan:';
    function table: NSTextTable; message 'table';
    function startingRow: NSInteger; message 'startingRow';
    function rowSpan: NSInteger; message 'rowSpan';
    function startingColumn: NSInteger; message 'startingColumn';
    function columnSpan: NSInteger; message 'columnSpan';
  end;

type
  NSTextTable = objcclass external (NSTextBlock)
  private
    _numCols: NSUInteger;
    _tableFlags: NSUInteger;
    _lcache: id;
    _tablePrimary: pointer;
    _tableSecondary: pointer;
  public
    procedure setNumberOfColumns(newValue: NSUInteger); message 'setNumberOfColumns:';
    function numberOfColumns: NSUInteger; message 'numberOfColumns';
    procedure setLayoutAlgorithm(newValue: NSTextTableLayoutAlgorithm); message 'setLayoutAlgorithm:';
    function layoutAlgorithm: NSTextTableLayoutAlgorithm; message 'layoutAlgorithm';
    procedure setCollapsesBorders(newValue: ObjCBOOL); message 'setCollapsesBorders:';
    function collapsesBorders: ObjCBOOL; message 'collapsesBorders';
    procedure setHidesEmptyCells(newValue: ObjCBOOL); message 'setHidesEmptyCells:';
    function hidesEmptyCells: ObjCBOOL; message 'hidesEmptyCells';
    function rectForBlock_layoutAtPoint_inRect_textContainer_characterRange (block: NSTextTableBlock; startingPoint: NSPoint; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:';
    function boundsRectForBlock_contentRect_inRect_textContainer_characterRange (block: NSTextTableBlock; contentRect: NSRect; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'boundsRectForBlock:contentRect:inRect:textContainer:characterRange:';
    procedure drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager (block: NSTextTableBlock; frameRect: NSRect; controlView: NSView; charRange: NSRange; layoutManager: NSLayoutManager); message 'drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:';
  end;
{$endif}

