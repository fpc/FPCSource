{ Parsed from Appkit.framework NSImage.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSIMAGE_PAS_H}
{$define NSIMAGE_PAS_H}
type
  NSImagePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSIMAGE_PAS_T}
{$define NSIMAGE_PAS_T}

{ Constants }

const
  NSImageLoadStatusCompleted = 0;
  NSImageLoadStatusCancelled = 1;
  NSImageLoadStatusInvalidData = 2;
  NSImageLoadStatusUnexpectedEOF = 3;
  NSImageLoadStatusReadError = 4;

{ Types }
type
  NSImageLoadStatus = culong;
  NSImageCacheMode = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSIMAGE_PAS_R}
{$define NSIMAGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSIMAGE_PAS_F}
{$define NSIMAGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSIMAGE_PAS_T}
{$define NSIMAGE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSImage = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSIMAGE_PAS_C}
{$define NSIMAGE_PAS_C}

{ NSImage }
  NSImage = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __name: NSString;
    __size: NSSize;
    __flags: bitpacked record
        scalable: 0..1;
        dataRetained: 0..1;
        uniqueWindow: 0..1;
        sizeWasExplicitlySet: 0..1;
        builtIn: 0..1;
        needsToExpand: 0..1;
        useEPSOnResolutionMismatch: 0..1;
        colorMatchPreferred: 0..1;
        multipleResolutionMatching: 0..1;
        subImage: 0..1;
        archiveByName: 0..1;
        unboundedCacheDepth: 0..1;
        flipped: 0..1;
        aliased: 0..1;
        dirtied: 0..1;
        cacheMode: 0..((1 shl 2)-1);
        sampleMode: 0..((1 shl 2)-1);
        focusedWhilePrinting: 0..1;
        imageEffectsRequested: 0..1;
        isTemplate: 0..1;
        failedToExpand: 0..1;
        reserved1: 0..((1 shl 9)-1);
      end;
    __reps: id;
    __imageAuxiliary: _NSImageAuxiliary;
    
  public
    class function alloc: NSImage; message 'alloc';

    class function imageNamed(name_: NSString): id; message 'imageNamed:';
    function initWithSize(aSize: NSSize): id; message 'initWithSize:';
    function initWithData(data: NSData): id; message 'initWithData:';
    function initWithContentsOfFile(fileName: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
    function initByReferencingFile(fileName: NSString): id; message 'initByReferencingFile:';
    function initByReferencingURL(url: NSURL): id; message 'initByReferencingURL:';
    function initWithIconRef(iconRef: IconRef): id; message 'initWithIconRef:';
    function initWithPasteboard(pasteboard: NSPasteboard): id; message 'initWithPasteboard:';
    procedure setSize(aSize: NSSize); message 'setSize:';
    function size: NSSize; message 'size';
    function setName(string_: NSString): Boolean; message 'setName:';
    function name: NSString; message 'name';
    procedure setScalesWhenResized(flag: Boolean); message 'setScalesWhenResized:';
    function scalesWhenResized: Boolean; message 'scalesWhenResized';
    procedure setDataRetained(flag: Boolean); message 'setDataRetained:';
    function isDataRetained: Boolean; message 'isDataRetained';
    procedure setCachedSeparately(flag: Boolean); message 'setCachedSeparately:';
    function isCachedSeparately: Boolean; message 'isCachedSeparately';
    procedure setCacheDepthMatchesImageDepth(flag: Boolean); message 'setCacheDepthMatchesImageDepth:';
    function cacheDepthMatchesImageDepth: Boolean; message 'cacheDepthMatchesImageDepth';
    procedure setBackgroundColor(aColor: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setUsesEPSOnResolutionMismatch(flag: Boolean); message 'setUsesEPSOnResolutionMismatch:';
    function usesEPSOnResolutionMismatch: Boolean; message 'usesEPSOnResolutionMismatch';
    procedure setPrefersColorMatch(flag: Boolean); message 'setPrefersColorMatch:';
    function prefersColorMatch: Boolean; message 'prefersColorMatch';
    procedure setMatchesOnMultipleResolution(flag: Boolean); message 'setMatchesOnMultipleResolution:';
    function matchesOnMultipleResolution: Boolean; message 'matchesOnMultipleResolution';
    procedure dissolveToPoint_fraction(point: NSPoint; aFloat: CGFloat); message 'dissolveToPoint:fraction:';
    procedure dissolveToPoint_fromRect_fraction(point: NSPoint; rect: NSRect; aFloat: CGFloat); message 'dissolveToPoint:fromRect:fraction:';
    procedure compositeToPoint_operation(point: NSPoint; op: NSCompositingOperation); message 'compositeToPoint:operation:';
    procedure compositeToPoint_fromRect_operation(point: NSPoint; rect: NSRect; op: NSCompositingOperation); message 'compositeToPoint:fromRect:operation:';
    procedure compositeToPoint_operation_fraction(point: NSPoint; op: NSCompositingOperation; delta: CGFloat); message 'compositeToPoint:operation:fraction:';
    procedure compositeToPoint_fromRect_operation_fraction(point: NSPoint; rect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'compositeToPoint:fromRect:operation:fraction:';
    procedure drawAtPoint_fromRect_operation_fraction(point: NSPoint; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawAtPoint:fromRect:operation:fraction:';
    procedure drawInRect_fromRect_operation_fraction(rect: NSRect; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawInRect:fromRect:operation:fraction:';
    function drawRepresentation_inRect(imageRep: NSImageRep; rect: NSRect): Boolean; message 'drawRepresentation:inRect:';
    procedure recache; message 'recache';
    function TIFFRepresentation: NSData; message 'TIFFRepresentation';
    function TIFFRepresentationUsingCompression_factor(comp: NSTIFFCompression; aFloat: single): NSData; message 'TIFFRepresentationUsingCompression:factor:';
    function representations: NSArray; message 'representations';
    procedure addRepresentations(imageReps: NSArray); message 'addRepresentations:';
    procedure addRepresentation(imageRep: NSImageRep); message 'addRepresentation:';
    procedure removeRepresentation(imageRep: NSImageRep); message 'removeRepresentation:';
    function isValid: Boolean; message 'isValid';
    procedure lockFocus; message 'lockFocus';
    procedure lockFocusOnRepresentation(imageRepresentation: NSImageRep); message 'lockFocusOnRepresentation:';
    procedure unlockFocus; message 'unlockFocus';
    function bestRepresentationForDevice(deviceDescription: NSDictionary): NSImageRep; message 'bestRepresentationForDevice:';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    class function imageUnfilteredFileTypes: NSArray; message 'imageUnfilteredFileTypes';
    class function imageUnfilteredPasteboardTypes: NSArray; message 'imageUnfilteredPasteboardTypes';
    class function imageFileTypes: NSArray; message 'imageFileTypes';
    class function imagePasteboardTypes: NSArray; message 'imagePasteboardTypes';
    class function imageTypes: NSArray; message 'imageTypes';
    class function imageUnfilteredTypes: NSArray; message 'imageUnfilteredTypes';
    class function canInitWithPasteboard(pasteboard: NSPasteboard): Boolean; message 'canInitWithPasteboard:';
    procedure setFlipped(flag: Boolean); message 'setFlipped:';
    function isFlipped: Boolean; message 'isFlipped';
    procedure cancelIncrementalLoad; message 'cancelIncrementalLoad';
    procedure setCacheMode(mode: NSImageCacheMode); message 'setCacheMode:';
    function cacheMode: NSImageCacheMode; message 'cacheMode';
    function alignmentRect: NSRect; message 'alignmentRect';
    procedure setAlignmentRect(rect: NSRect); message 'setAlignmentRect:';
    function isTemplate: Boolean; message 'isTemplate';
    procedure setTemplate(isTemplate_: Boolean); message 'setTemplate:';
  end; external;

{$endif}
{$endif}
