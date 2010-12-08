{ Parsed from Appkit.framework NSDragging.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
  NSDraggingDestinationCategory = objccategory(NSObject)
    function draggingEntered(sender: id): NSDragOperation; message 'draggingEntered:';
    function draggingUpdated(sender: id): NSDragOperation; message 'draggingUpdated:';
    procedure draggingExited(sender: id); message 'draggingExited:';
    function prepareForDragOperation(sender: id): Boolean; message 'prepareForDragOperation:';
    function performDragOperation(sender: id): Boolean; message 'performDragOperation:';
    procedure concludeDragOperation(sender: id); message 'concludeDragOperation:';
    procedure draggingEnded(sender: id); message 'draggingEnded:';
    function wantsPeriodicDraggingUpdates: Boolean; message 'wantsPeriodicDraggingUpdates';
  end; external;

{ NSDraggingSourceCategory }
  NSDraggingSourceCategory = objccategory(NSObject)
    function draggingSourceOperationMaskForLocal(flag: Boolean): NSDragOperation; message 'draggingSourceOperationMaskForLocal:';
    function namesOfPromisedFilesDroppedAtDestination(dropDestination: NSURL): NSArray; message 'namesOfPromisedFilesDroppedAtDestination:';
    procedure draggedImage_beganAt(image: NSImage; screenPoint: NSPoint); message 'draggedImage:beganAt:';
    procedure draggedImage_endedAt_operation(image: NSImage; screenPoint: NSPoint; operation: NSDragOperation); message 'draggedImage:endedAt:operation:';
    procedure draggedImage_movedTo(image: NSImage; screenPoint: NSPoint); message 'draggedImage:movedTo:';
    function ignoreModifierKeysWhileDragging: Boolean; message 'ignoreModifierKeysWhileDragging';
  end; external;

{ NSDraggingSourceDeprecatedCategory }
  NSDraggingSourceDeprecatedCategory = objccategory(NSObject)
    procedure draggedImage_endedAt_deposited(image: NSImage; screenPoint: NSPoint; flag: Boolean); message 'draggedImage:endedAt:deposited:'; deprecated 'in Mac OS X 10.1 and later';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSDRAGGING_PAS_P}
{$define NSDRAGGING_PAS_P}
  
{ NSDraggingInfo Protocol }
  NSDraggingInfoProtocol = objcprotocol
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
  end; external name 'NSDraggingInfo';
{$endif}
{$endif}
