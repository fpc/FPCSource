{ Parsed from Appkit.framework NSDragging.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSDRAGGING_PAS_T}
{$define NSDRAGGING_PAS_T}

{ Types }
type
  NSDragOperation = NSUInteger;

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
{$ifndef NSDRAGGING_PAS_T}
{$define NSDRAGGING_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDraggingInfoProtocol = objcprotocol;

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
