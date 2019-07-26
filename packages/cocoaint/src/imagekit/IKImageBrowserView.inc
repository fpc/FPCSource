{ Parsed from ImageKit.framework IKImageBrowserView.h }


{ Types from IKAppearance }
{$ifdef TYPES}

{$endif}


{ Types from IKBrowsing }
{$ifdef TYPES}


{$endif}


{ Types from IKSelectionReorderingAndGrouping }
{$ifdef TYPES}

{$endif}


{ Types from IKDragNDrop }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  IKImageBrowserViewPtr = ^IKImageBrowserView;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef TYPES}
const
  IKCellsStyleNone = 0;
  IKCellsStyleShadowed = 1;
  IKCellsStyleOutlined = 2;
  IKCellsStyleTitled = 4;
  IKCellsStyleSubtitled = 8;
const
  IKGroupBezelStyle = 0;
  IKGroupDisclosureStyle = 1;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef TYPES}
const
  IKImageBrowserDropOn = 0;
  IKImageBrowserDropBefore = 1;
type
  IKImageBrowserDropOperation = clong;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  IKImageBrowserDataSource = objccategory external (NSObject)
    function numberOfItemsInImageBrowser (aBrowser: IKImageBrowserView): NSUInteger; message 'numberOfItemsInImageBrowser:';
    function imageBrowser_itemAtIndex (aBrowser: IKImageBrowserView; index: NSUInteger): id; message 'imageBrowser:itemAtIndex:';
    procedure imageBrowser_removeItemsAtIndexes (aBrowser: IKImageBrowserView; indexes: NSIndexSet); message 'imageBrowser:removeItemsAtIndexes:';
    function imageBrowser_moveItemsAtIndexes_toIndex (aBrowser: IKImageBrowserView; indexes: NSIndexSet; destinationIndex: NSUInteger): ObjCBOOL; message 'imageBrowser:moveItemsAtIndexes:toIndex:';
    function imageBrowser_writeItemsAtIndexes_toPasteboard (aBrowser: IKImageBrowserView; itemIndexes: NSIndexSet; pasteboard: NSPasteboard): NSUInteger; message 'imageBrowser:writeItemsAtIndexes:toPasteboard:';
    function numberOfGroupsInImageBrowser (aBrowser: IKImageBrowserView): NSUInteger; message 'numberOfGroupsInImageBrowser:';
    function imageBrowser_groupAtIndex (aBrowser: IKImageBrowserView; index: NSUInteger): NSDictionary; message 'imageBrowser:groupAtIndex:';
  end;

type
  IKImageBrowserItem = objccategory external (NSObject)
    function imageUID: NSString; message 'imageUID';
    function imageRepresentationType: NSString; message 'imageRepresentationType';
    function imageRepresentation: id; message 'imageRepresentation';
    function imageVersion: NSUInteger; message 'imageVersion';
    function imageTitle: NSString; message 'imageTitle';
    function imageSubtitle: NSString; message 'imageSubtitle';
    function isSelectable: ObjCBOOL; message 'isSelectable';
  end;


type
  IKImageBrowserView = objcclass external (NSView, NSDraggingSourceProtocol)
  private
    _dataSource: id;
    _dragDestinationDelegate: id;
    _verticalScroller: NSScroller;
    _horizontalScroller: NSScroller;
    _delegate: id;
    _reserved: pointer;

    { Adopted protocols }
    procedure draggingSession_endedAtPoint_operation (session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'draggingSession:endedAtPoint:operation:';
    procedure draggingSession_movedToPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:movedToPoint:';
    function draggingSession_sourceOperationMaskForDraggingContext (session: NSDraggingSession; context: NSDraggingContext): NSDragOperation; message 'draggingSession:sourceOperationMaskForDraggingContext:';
    procedure draggingSession_willBeginAtPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:willBeginAtPoint:';
    function ignoreModifierKeysForDraggingSession (session: NSDraggingSession): ObjCBOOL; message 'ignoreModifierKeysForDraggingSession:';
  end;


type
  IKMainMethods = objccategory external (IKImageBrowserView)
    function initWithFrame (frame: NSRect): id; message 'initWithFrame:';
    procedure setDataSource (source: id); message 'setDataSource:';
    function dataSource: id; message 'dataSource';
    procedure reloadData; message 'reloadData';
    procedure setDelegate (aDelegate: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
  end;

type
  IKAppearance = objccategory external (IKImageBrowserView)
    procedure setCellsStyleMask (mask: NSUInteger); message 'setCellsStyleMask:';
    function cellsStyleMask: NSUInteger; message 'cellsStyleMask';
    procedure setConstrainsToOriginalSize (flag: ObjCBOOL); message 'setConstrainsToOriginalSize:';
    function constrainsToOriginalSize: ObjCBOOL; message 'constrainsToOriginalSize';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    procedure setBackgroundLayer (aLayer: CALayer); message 'setBackgroundLayer:';
    function backgroundLayer: CALayer; message 'backgroundLayer';
    procedure setForegroundLayer (aLayer: CALayer); message 'setForegroundLayer:';
    function foregroundLayer: CALayer; message 'foregroundLayer';
    function newCellForRepresentedItem (anItem: id): IKImageBrowserCell; message 'newCellForRepresentedItem:';
    function cellForItemAtIndex (index: NSUInteger): IKImageBrowserCell; message 'cellForItemAtIndex:';
    {$endif}
  end;

type
  IKBrowsing = objccategory external (IKImageBrowserView)
    procedure setZoomValue (aValue: single); message 'setZoomValue:';
    function zoomValue: single; message 'zoomValue';
    procedure setContentResizingMask (mask: NSUInteger); message 'setContentResizingMask:';
    function contentResizingMask: NSUInteger; message 'contentResizingMask';
    procedure scrollIndexToVisible (index: NSInteger); message 'scrollIndexToVisible:';
    procedure setCellSize (size: NSSize); message 'setCellSize:';
    function cellSize: NSSize; message 'cellSize';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function intercellSpacing: NSSize; message 'intercellSpacing';
    procedure setIntercellSpacing (aSize: NSSize); message 'setIntercellSpacing:';
    {$endif}
    function indexOfItemAtPoint (point: NSPoint): NSInteger; message 'indexOfItemAtPoint:';
    function itemFrameAtIndex (index: NSInteger): NSRect; message 'itemFrameAtIndex:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function visibleItemIndexes: NSIndexSet; message 'visibleItemIndexes';
    function rowIndexesInRect (rect: NSRect): NSIndexSet; message 'rowIndexesInRect:';
    function columnIndexesInRect (rect: NSRect): NSIndexSet; message 'columnIndexesInRect:';
    function rectOfColumn (columnIndex: NSUInteger): NSRect; message 'rectOfColumn:';
    function rectOfRow (rowIndex: NSUInteger): NSRect; message 'rectOfRow:';
    function numberOfRows: NSUInteger; message 'numberOfRows';
    function numberOfColumns: NSUInteger; message 'numberOfColumns';
    procedure setCanControlQuickLookPanel (flag: ObjCBOOL); message 'setCanControlQuickLookPanel:';
    function canControlQuickLookPanel: ObjCBOOL; message 'canControlQuickLookPanel';
    {$endif}
  end;

type
  IKSelectionReorderingAndGrouping = objccategory external (IKImageBrowserView)
    function selectionIndexes: NSIndexSet; message 'selectionIndexes';
    procedure setSelectionIndexes_byExtendingSelection (indexes: NSIndexSet; extendSelection: ObjCBOOL); message 'setSelectionIndexes:byExtendingSelection:';
    procedure setAllowsMultipleSelection (flag: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    procedure setAllowsEmptySelection (flag: ObjCBOOL); message 'setAllowsEmptySelection:';
    function allowsEmptySelection: ObjCBOOL; message 'allowsEmptySelection';
    procedure setAllowsReordering (flag: ObjCBOOL); message 'setAllowsReordering:';
    function allowsReordering: ObjCBOOL; message 'allowsReordering';
    procedure setAnimates (flag: ObjCBOOL); message 'setAnimates:';
    function animates: ObjCBOOL; message 'animates';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    procedure expandGroupAtIndex (index: NSUInteger); message 'expandGroupAtIndex:';
    procedure collapseGroupAtIndex (index: NSUInteger); message 'collapseGroupAtIndex:';
    function isGroupExpandedAtIndex (index: NSUInteger): ObjCBOOL; message 'isGroupExpandedAtIndex:';
    {$endif}
  end;

type
  IKDragNDrop = objccategory external (IKImageBrowserView)
    procedure setDraggingDestinationDelegate (delegate: id); message 'setDraggingDestinationDelegate:';
    function draggingDestinationDelegate: id; message 'draggingDestinationDelegate';
    function indexAtLocationOfDroppedItem: NSUInteger; message 'indexAtLocationOfDroppedItem';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function dropOperation: IKImageBrowserDropOperation; message 'dropOperation';
    procedure setAllowsDroppingOnItems (flag: ObjCBOOL); message 'setAllowsDroppingOnItems:';
    function allowsDroppingOnItems: ObjCBOOL; message 'allowsDroppingOnItems';
    procedure setDropIndex_dropOperation (index: NSInteger; operation: IKImageBrowserDropOperation); message 'setDropIndex:dropOperation:';
    {$endif}
  end;

type
  IKImageBrowserDelegate = objccategory external (NSObject)
    procedure imageBrowserSelectionDidChange (aBrowser: IKImageBrowserView); message 'imageBrowserSelectionDidChange:';
    procedure imageBrowser_cellWasDoubleClickedAtIndex (aBrowser: IKImageBrowserView; index: NSUInteger); message 'imageBrowser:cellWasDoubleClickedAtIndex:';
    procedure imageBrowser_cellWasRightClickedAtIndex_withEvent (aBrowser: IKImageBrowserView; index: NSUInteger; event: NSEvent); message 'imageBrowser:cellWasRightClickedAtIndex:withEvent:';
    procedure imageBrowser_backgroundWasRightClickedWithEvent (aBrowser: IKImageBrowserView; event: NSEvent); message 'imageBrowser:backgroundWasRightClickedWithEvent:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  IKImageBrowserPathRepresentationType: NSString; cvar; external;
  IKImageBrowserNSURLRepresentationType: NSString; cvar; external;
  IKImageBrowserNSImageRepresentationType: NSString; cvar; external;
  IKImageBrowserCGImageRepresentationType: NSString; cvar; external;
  IKImageBrowserCGImageSourceRepresentationType: NSString; cvar; external;
  IKImageBrowserNSDataRepresentationType: NSString; cvar; external;
  IKImageBrowserNSBitmapImageRepresentationType: NSString; cvar; external;
  IKImageBrowserQTMovieRepresentationType: NSString; cvar; external;
  IKImageBrowserQTMoviePathRepresentationType: NSString; cvar; external;
  IKImageBrowserQCCompositionRepresentationType: NSString; cvar; external;
  IKImageBrowserQCCompositionPathRepresentationType: NSString; cvar; external;
  IKImageBrowserQuickLookPathRepresentationType: NSString; cvar; external;
  IKImageBrowserIconRefPathRepresentationType: NSString; cvar; external;
  IKImageBrowserIconRefRepresentationType: NSString; cvar; external;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef EXTERNAL_SYMBOLS}
var
  IKImageBrowserPDFPageRepresentationType: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  IKImageBrowserBackgroundColorKey: NSString; cvar; external;
  IKImageBrowserSelectionColorKey: NSString; cvar; external;
  IKImageBrowserCellsOutlineColorKey: NSString; cvar; external;
  IKImageBrowserCellsTitleAttributesKey: NSString; cvar; external;
  IKImageBrowserCellsHighlightedTitleAttributesKey: NSString; cvar; external;
  IKImageBrowserCellsSubtitleAttributesKey: NSString; cvar; external;
  IKImageBrowserGroupRangeKey: NSString; cvar; external;
  IKImageBrowserGroupBackgroundColorKey: NSString; cvar; external;
  IKImageBrowserGroupTitleKey: NSString; cvar; external;
  IKImageBrowserGroupStyleKey: NSString; cvar; external;
  IKImageBrowserGroupHeaderLayer: NSString; cvar; external;
  IKImageBrowserGroupFooterLayer: NSString; cvar; external;
{$endif}

{$endif}
