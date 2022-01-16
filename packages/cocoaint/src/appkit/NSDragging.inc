{ Parsed from AppKit.framework NSDragging.h }


{$ifdef TYPES}
type
  NSDraggingInfoProtocolPtr = ^NSDraggingInfoProtocol;
  NSDraggingDestinationProtocolPtr = ^NSDraggingDestinationProtocol;
  NSDraggingSourceProtocolPtr = ^NSDraggingSourceProtocol;
{$endif}

{$ifdef TYPES}
type
  NSDragOperation = NSUInteger;
  NSDragOperationPtr = ^NSDragOperation;

const
  NSDragOperationNone = 0;
  NSDragOperationCopy = 1;
  NSDragOperationLink = 2;
  NSDragOperationGeneric = 4;
  NSDragOperationPrivate = 8;
  NSDragOperationMove = 16;
  NSDragOperationDelete = 32;
  NSDragOperationEvery = NSUIntegerMax;
  NSDragOperationAll_Obsolete = 15 deprecated 'in 10_0, 10_10';
  NSDragOperationAll = NSDragOperationAll_Obsolete deprecated 'in 10_0, 10_10';

type
  NSDraggingFormation = NSInteger;
  NSDraggingFormationPtr = ^NSDraggingFormation;

const
  NSDraggingFormationDefault = 0;
  NSDraggingFormationNone = 1;
  NSDraggingFormationPile = 2;
  NSDraggingFormationList = 3;
  NSDraggingFormationStack = 4;

type
  NSDraggingContext = NSInteger;
  NSDraggingContextPtr = ^NSDraggingContext;

const
  NSDraggingContextOutsideApplication = 0;
  NSDraggingContextWithinApplication = 1;

type
  NSDraggingItemEnumerationOptions = NSUInteger;
  NSDraggingItemEnumerationOptionsPtr = ^NSDraggingItemEnumerationOptions;

const
  NSDraggingItemEnumerationConcurrent = NSEnumerationConcurrent;
  NSDraggingItemEnumerationClearNonenumeratedImages = 1 shl 16;
{$endif}

{$ifdef PROTOCOLS}

type
  NSDraggingInfoProtocol = objcprotocol external name 'NSDraggingInfo' (NSObjectProtocol)
  required
    function draggingDestinationWindow: NSWindow; message 'draggingDestinationWindow';
    function draggingSourceOperationMask: NSDragOperation; message 'draggingSourceOperationMask';
    function draggingLocation: NSPoint; message 'draggingLocation';
    function draggedImageLocation: NSPoint; message 'draggedImageLocation';
    function draggedImage: NSImage; message 'draggedImage';
    function draggingPasteboard: NSPasteboard; message 'draggingPasteboard';
    function draggingSource: id; message 'draggingSource';
    function draggingSequenceNumber: NSInteger; message 'draggingSequenceNumber';
    procedure slideDraggedImageTo (screenPoint: NSPoint); message 'slideDraggedImageTo:';
    function namesOfPromisedFilesDroppedAtDestination (dropDestination: NSURL): NSArray; message 'namesOfPromisedFilesDroppedAtDestination:';
    procedure setDraggingFormation(newValue: NSDraggingFormation); message 'setDraggingFormation:';
    function draggingFormation: NSDraggingFormation; message 'draggingFormation';
    procedure setAnimatesToDestination(newValue: ObjCBOOL); message 'setAnimatesToDestination:';
    function animatesToDestination: ObjCBOOL; message 'animatesToDestination';
    procedure setNumberOfValidItemsForDrop(newValue: NSInteger); message 'setNumberOfValidItemsForDrop:';
    function numberOfValidItemsForDrop: NSInteger; message 'numberOfValidItemsForDrop';
    procedure enumerateDraggingItemsWithOptions_forView_classes_searchOptions_usingBlock (enumOpts: NSDraggingItemEnumerationOptions; view: NSView; classArray: NSArray; searchOptions: NSDictionary; block: OpaqueCBlock); message 'enumerateDraggingItemsWithOptions:forView:classes:searchOptions:usingBlock:'; { available in 10_7 }
  end;

type
  NSDraggingDestinationProtocol = objcprotocol external name 'NSDraggingDestination' (NSObjectProtocol)
  optional
    function draggingEntered (sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingEntered:';
    function draggingUpdated (sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingUpdated:';
    procedure draggingExited (sender: NSDraggingInfoProtocol); message 'draggingExited:';
    function prepareForDragOperation (sender: NSDraggingInfoProtocol): ObjCBOOL; message 'prepareForDragOperation:';
    function performDragOperation (sender: NSDraggingInfoProtocol): ObjCBOOL; message 'performDragOperation:';
    procedure concludeDragOperation (sender: NSDraggingInfoProtocol); message 'concludeDragOperation:';
    procedure draggingEnded (sender: NSDraggingInfoProtocol); message 'draggingEnded:';
    function wantsPeriodicDraggingUpdates: ObjCBOOL; message 'wantsPeriodicDraggingUpdates';
    procedure updateDraggingItemsForDrag (sender: NSDraggingInfoProtocol); message 'updateDraggingItemsForDrag:'; { available in 10_7 }
  end;

type
  NSDraggingSourceProtocol = objcprotocol external name 'NSDraggingSource' (NSObjectProtocol)
  required
    function draggingSession_sourceOperationMaskForDraggingContext (session: NSDraggingSession; context: NSDraggingContext): NSDragOperation; message 'draggingSession:sourceOperationMaskForDraggingContext:';
  optional
    procedure draggingSession_willBeginAtPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:willBeginAtPoint:';
    procedure draggingSession_movedToPoint (session: NSDraggingSession; screenPoint: NSPoint); message 'draggingSession:movedToPoint:';
    procedure draggingSession_endedAtPoint_operation (session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation); message 'draggingSession:endedAtPoint:operation:';
    function ignoreModifierKeysForDraggingSession (session: NSDraggingSession): ObjCBOOL; message 'ignoreModifierKeysForDraggingSession:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSDraggingSourceDeprecated = objccategory external (NSObject)
    function namesOfPromisedFilesDroppedAtDestination (dropDestination: NSURL): NSArray; message 'namesOfPromisedFilesDroppedAtDestination:';
    function draggingSourceOperationMaskForLocal (flag: ObjCBOOL): NSDragOperation; message 'draggingSourceOperationMaskForLocal:'; deprecated 'in 10_0, 10_7, "Use -draggingSession:sourceOperationMaskForDraggingContext: instead"';
    procedure draggedImage_beganAt (image: NSImage; screenPoint: NSPoint); message 'draggedImage:beganAt:'; deprecated 'in 10_0, 10_7, "Use -draggingSession:willBeginAtPoint: instead"';
    procedure draggedImage_endedAt_operation (image: NSImage; screenPoint: NSPoint; operation: NSDragOperation); message 'draggedImage:endedAt:operation:'; deprecated 'in 10_0, 10_7, "Use -draggingSession:endedAtPoint:operation: instead"';
    procedure draggedImage_movedTo (image: NSImage; screenPoint: NSPoint); message 'draggedImage:movedTo:'; deprecated 'in 10_0, 10_7, "Use -draggingSession:movedToPoint: instead"';
    function ignoreModifierKeysWhileDragging: ObjCBOOL; message 'ignoreModifierKeysWhileDragging'; deprecated 'in 10_0, 10_7, "Use -ignoreModifierKeysForDraggingSession: instead"';
    procedure draggedImage_endedAt_deposited (image: NSImage; screenPoint: NSPoint; flag: ObjCBOOL); message 'draggedImage:endedAt:deposited:'; deprecated 'in 10.1 and later';
  end;
{$endif}

