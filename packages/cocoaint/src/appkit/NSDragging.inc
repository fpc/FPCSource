{ Parsed from Appkit.framework NSDragging.h }

{$ifdef TYPES}
{$ifndef NSDRAGGING_PAS_T}
{$define NSDRAGGING_PAS_T}

{ Types }
type
  NSDragOperation = NSUInteger;
  NSDragOperationPtr = ^NSDragOperation;

{ Constants }

const
  NSDragOperationNone = 0;
  NSDragOperationCopy = 1;
  NSDragOperationLink = 2;
  NSDragOperationGeneric = 4;
  NSDragOperationPrivate = 8;
  NSDragOperationAll_Obsolete = 15;
  NSDragOperationMove = 16;
  NSDragOperationDelete = 32;
  NSDragOperationEvery = NSUIntegerMax;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDRAGGING_PAS_R}
{$define NSDRAGGING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDRAGGING_PAS_F}
{$define NSDRAGGING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDRAGGING_PAS_S}
{$define NSDRAGGING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDraggingInfoProtocol = objcprotocol;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDRAGGING_PAS_C}
{$define NSDRAGGING_PAS_C}

{ NSDraggingDestinationCategory }
  NSDraggingDestinationCategory = objccategory external (NSObject)
    function draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingEntered:';
    function draggingUpdated(sender: NSDraggingInfoProtocol): NSDragOperation; message 'draggingUpdated:';
    procedure draggingExited(sender: NSDraggingInfoProtocol); message 'draggingExited:';
    function prepareForDragOperation(sender: NSDraggingInfoProtocol): Boolean; message 'prepareForDragOperation:';
    function performDragOperation(sender: NSDraggingInfoProtocol): Boolean; message 'performDragOperation:';
    procedure concludeDragOperation(sender: NSDraggingInfoProtocol); message 'concludeDragOperation:';
    procedure draggingEnded(sender: NSDraggingInfoProtocol); message 'draggingEnded:';
    function wantsPeriodicDraggingUpdates: Boolean; message 'wantsPeriodicDraggingUpdates';
  end;

{ NSDraggingSourceCategory }
  NSDraggingSourceCategory = objccategory external (NSObject)
    function draggingSourceOperationMaskForLocal(flag: Boolean): NSDragOperation; message 'draggingSourceOperationMaskForLocal:';
    function namesOfPromisedFilesDroppedAtDestination(dropDestination: NSURL): NSArray; message 'namesOfPromisedFilesDroppedAtDestination:';
    procedure draggedImage_beganAt(image: NSImage; screenPoint: NSPoint); message 'draggedImage:beganAt:';
    procedure draggedImage_endedAt_operation(image: NSImage; screenPoint: NSPoint; operation: NSDragOperation); message 'draggedImage:endedAt:operation:';
    procedure draggedImage_movedTo(image: NSImage; screenPoint: NSPoint); message 'draggedImage:movedTo:';
    function ignoreModifierKeysWhileDragging: Boolean; message 'ignoreModifierKeysWhileDragging';
  end;

{ NSDraggingSourceDeprecatedCategory }
  NSDraggingSourceDeprecatedCategory = objccategory external (NSObject)
    procedure draggedImage_endedAt_deposited(image: NSImage; screenPoint: NSPoint; flag: Boolean); message 'draggedImage:endedAt:deposited:'; deprecated 'in Mac OS X 10.1 and later';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSDRAGGING_PAS_P}
{$define NSDRAGGING_PAS_P}
  
{ NSDraggingInfo Protocol }
  NSDraggingInfoProtocol = objcprotocol external name 'NSDraggingInfo'
    function draggingDestinationWindow: NSWindow; message 'draggingDestinationWindow';
    function draggingSourceOperationMask: NSDragOperation; message 'draggingSourceOperationMask';
    function draggingLocation: NSPoint; message 'draggingLocation';
    function draggedImageLocation: NSPoint; message 'draggedImageLocation';
    function draggedImage: NSImage; message 'draggedImage';
    function draggingPasteboard: NSPasteboard; message 'draggingPasteboard';
    function draggingSource: id; message 'draggingSource';
    function draggingSequenceNumber: NSInteger; message 'draggingSequenceNumber';
    procedure slideDraggedImageTo(screenPoint: NSPoint); message 'slideDraggedImageTo:';
    function namesOfPromisedFilesDroppedAtDestination(dropDestination: NSURL): NSArray; message 'namesOfPromisedFilesDroppedAtDestination:';
  end;
{$endif}
{$endif}
