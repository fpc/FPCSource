{ Parsed from AppKit.framework NSImage.h }


{$ifdef TYPES}
type
  NSImagePtr = ^NSImage;
  NSImageDelegateProtocolPtr = ^NSImageDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSImageLoadStatus = NSUInteger;
  NSImageLoadStatusPtr = ^NSImageLoadStatus;

const
  NSImageLoadStatusCompleted = 0;
  NSImageLoadStatusCancelled = 1;
  NSImageLoadStatusInvalidData = 2;
  NSImageLoadStatusUnexpectedEOF = 3;
  NSImageLoadStatusReadError = 4;

type
  NSImageCacheMode = NSUInteger;
  NSImageCacheModePtr = ^NSImageCacheMode;

const
  NSImageCacheDefault = 0;
  NSImageCacheAlways = 1;
  NSImageCacheBySize = 2;
  NSImageCacheNever = 3;

type
  NSImageResizingMode = NSInteger;
  NSImageResizingModePtr = ^NSImageResizingMode;

const
  NSImageResizingModeStretch = 0;
  NSImageResizingModeTile = 1;
{$endif}

{$ifdef CLASSES}

type
  NSImage = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol, NSSecureCodingProtocol, NSPasteboardReadingProtocol, NSPasteboardWritingProtocol)
  private
    _name: NSString;
    _size: NSSize;
    __imageFlags: bitpacked record
      case byte of
        0: (_anonBitField___imageFlags0: cuint);
        1: (
          scalable: 0..1;
          dataRetained: 0..1;
          uniqueWindow: 0..1;
          sizeWasExplicitlySet: 0..1;
          builtIn: 0..1;
          needsToExpand: 0..1;
          useEPSOnResolutionMismatch: 0..1;
          matchesOnlyOnBestFittingAxis: 0..1;
          colorMatchPreferred: 0..1;
          multipleResolutionMatching: 0..1;
          focusedWhilePrinting: 0..1;
          archiveByName: 0..1;
          unboundedCacheDepth: 0..1;
          flipped: 0..1;
          aliased: 0..1;
          dirtied: 0..1;
          cacheMode: 0..((1 shl 2)-1);
          sampleMode: 0..((1 shl 3)-1);
          resMatchPreferred: 0..1;
          isTemplate_: 0..1;
          failedToExpand: 0..1;
          reserved1: 0..((1 shl 8)-1);
        );
      end;
    _reps: id;
    _imageAuxiliary: _NSImageAuxiliary;
  public
    class function imageNamed (name: NSString): NSImage; message 'imageNamed:';
    function initWithSize (aSize: NSSize): instancetype; message 'initWithSize:';
    function initWithData (data: NSData): instancetype; message 'initWithData:';
    function initWithContentsOfFile (fileName: NSString): instancetype; message 'initWithContentsOfFile:';
    function initWithContentsOfURL (url: NSURL): instancetype; message 'initWithContentsOfURL:';
    function initByReferencingFile (fileName: NSString): instancetype; message 'initByReferencingFile:';
    function initByReferencingURL (url: NSURL): instancetype; message 'initByReferencingURL:';
    function initWithIconRef (iconRef_: IconRef): instancetype; message 'initWithIconRef:'; { available in 10_5 }
    function initWithPasteboard (pasteboard: NSPasteboard): instancetype; message 'initWithPasteboard:';
    function initWithDataIgnoringOrientation (data: NSData): instancetype; message 'initWithDataIgnoringOrientation:'; { available in 10_6 }
    class function imageWithSize_flipped_drawingHandler (size: NSSize; drawingHandlerShouldBeCalledWithFlippedContext: ObjCBOOL; drawingHandler: OpaqueCBlock): NSImage; message 'imageWithSize:flipped:drawingHandler:'; { available in 10_8 }
    procedure setSize(newValue: NSSize); message 'setSize:';
    function size: NSSize; message 'size';
    function setName (string_: NSString): ObjCBOOL; message 'setName:';
    function name: NSString; message 'name';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setUsesEPSOnResolutionMismatch(newValue: ObjCBOOL); message 'setUsesEPSOnResolutionMismatch:';
    function usesEPSOnResolutionMismatch: ObjCBOOL; message 'usesEPSOnResolutionMismatch';
    procedure setPrefersColorMatch(newValue: ObjCBOOL); message 'setPrefersColorMatch:';
    function prefersColorMatch: ObjCBOOL; message 'prefersColorMatch';
    procedure setMatchesOnMultipleResolution(newValue: ObjCBOOL); message 'setMatchesOnMultipleResolution:';
    function matchesOnMultipleResolution: ObjCBOOL; message 'matchesOnMultipleResolution';
    procedure setMatchesOnlyOnBestFittingAxis(newValue: ObjCBOOL); message 'setMatchesOnlyOnBestFittingAxis:';
    function matchesOnlyOnBestFittingAxis: ObjCBOOL; message 'matchesOnlyOnBestFittingAxis';
    procedure drawAtPoint_fromRect_operation_fraction (point: NSPoint; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawAtPoint:fromRect:operation:fraction:';
    procedure drawInRect_fromRect_operation_fraction (rect: NSRect; fromRect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'drawInRect:fromRect:operation:fraction:';
    procedure drawInRect_fromRect_operation_fraction_respectFlipped_hints (dstSpacePortionRect: NSRect; srcSpacePortionRect: NSRect; op: NSCompositingOperation; requestedAlpha: CGFloat; respectContextIsFlipped: ObjCBOOL; hints: NSDictionary); message 'drawInRect:fromRect:operation:fraction:respectFlipped:hints:'; { available in 10_6 }
    function drawRepresentation_inRect (imageRep: NSImageRep; rect: NSRect): ObjCBOOL; message 'drawRepresentation:inRect:';
    procedure drawInRect (rect: NSRect); message 'drawInRect:'; { available in 10_9 }
    procedure recache; message 'recache';
    function TIFFRepresentation: NSData; message 'TIFFRepresentation';
    function TIFFRepresentationUsingCompression_factor (comp: NSTIFFCompression; aFloat: single): NSData; message 'TIFFRepresentationUsingCompression:factor:';
    function representations: NSArray; message 'representations';
    procedure addRepresentations (imageReps: NSArray); message 'addRepresentations:';
    procedure addRepresentation (imageRep: NSImageRep); message 'addRepresentation:';
    procedure removeRepresentation (imageRep: NSImageRep); message 'removeRepresentation:';
    function isValid: ObjCBOOL; message 'isValid';
    procedure lockFocus; message 'lockFocus';
    procedure lockFocusFlipped (flipped: ObjCBOOL); message 'lockFocusFlipped:'; { available in 10_6 }
    procedure unlockFocus; message 'unlockFocus';
    function bestRepresentationForDevice (deviceDescription: NSDictionary): NSImageRep; message 'bestRepresentationForDevice:'; deprecated 'in 10_0, 10_6';
    procedure setDelegate(newValue: NSImageDelegateProtocol); message 'setDelegate:';
    function delegate: NSImageDelegateProtocol; message 'delegate';
    class function imageUnfilteredFileTypes: NSArray; message 'imageUnfilteredFileTypes'; deprecated 'in 10_0, 10_10, "Use +imageUnfilteredTypes instead"';
    class function imageUnfilteredPasteboardTypes: NSArray; message 'imageUnfilteredPasteboardTypes'; deprecated 'in 10_0, 10_10, "Use +imageUnfilteredTypes instead"';
    class function imageFileTypes: NSArray; message 'imageFileTypes'; deprecated 'in 10_0, 10_10, "Use +imageTypes instead"';
    class function imagePasteboardTypes: NSArray; message 'imagePasteboardTypes'; deprecated 'in 10_0, 10_10, "Use +imageTypes instead"';
    class function imageTypes: NSArray; message 'imageTypes'; { available in 10_5 }
    class function imageUnfilteredTypes: NSArray; message 'imageUnfilteredTypes'; { available in 10_5 }
    class function canInitWithPasteboard (pasteboard: NSPasteboard): ObjCBOOL; message 'canInitWithPasteboard:';
    procedure cancelIncrementalLoad; message 'cancelIncrementalLoad';
    procedure setCacheMode(newValue: NSImageCacheMode); message 'setCacheMode:';
    function cacheMode: NSImageCacheMode; message 'cacheMode';
    procedure setAlignmentRect(newValue: NSRect); message 'setAlignmentRect:';
    function alignmentRect: NSRect; message 'alignmentRect';
    function isTemplate: ObjCBOOL; message 'isTemplate'; { available in 10_5 }
    procedure setTemplate (isTemplate_: ObjCBOOL); message 'setTemplate:'; { available in 10_5 }
    procedure setAccessibilityDescription(newValue: NSString); message 'setAccessibilityDescription:';
    function accessibilityDescription: NSString; message 'accessibilityDescription';
    function initWithCGImage_size (cgImage: CGImageRef; size_: NSSize): instancetype; message 'initWithCGImage:size:'; { available in 10_6 }
    function CGImageForProposedRect_context_hints (proposedDestRect: NSRectPtr; referenceContext: NSGraphicsContext; hints: NSDictionary): CGImageRef; message 'CGImageForProposedRect:context:hints:'; { available in 10_6 }
    function bestRepresentationForRect_context_hints (rect: NSRect; referenceContext: NSGraphicsContext; hints: NSDictionary): NSImageRep; message 'bestRepresentationForRect:context:hints:'; { available in 10_6 }
    function hitTestRect_withImageDestinationRect_context_hints_flipped (testRectDestSpace: NSRect; imageRectDestSpace: NSRect; context: NSGraphicsContext; hints: NSDictionary; flipped: ObjCBOOL): ObjCBOOL; message 'hitTestRect:withImageDestinationRect:context:hints:flipped:'; { available in 10_6 }
    function recommendedLayerContentsScale (preferredContentsScale: CGFloat): CGFloat; message 'recommendedLayerContentsScale:'; { available in 10_7 }
    function layerContentsForContentsScale (layerContentsScale: CGFloat): id; message 'layerContentsForContentsScale:'; { available in 10_7 }
    procedure setCapInsets(newValue: NSEdgeInsets); message 'setCapInsets:';
    function capInsets: NSEdgeInsets; message 'capInsets';
    procedure setResizingMode(newValue: NSImageResizingMode); message 'setResizingMode:';
    function resizingMode: NSImageResizingMode; message 'resizingMode';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function initWithPasteboardPropertyList_ofType (propertyList: id; type_: NSString): id; message 'initWithPasteboardPropertyList:ofType:';
    function pasteboardPropertyListForType (type_: NSString): id; message 'pasteboardPropertyListForType:';
    class function readableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'readableTypesForPasteboard:';
    class function readingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardReadingOptions; message 'readingOptionsForType:pasteboard:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
    function writableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'writableTypesForPasteboard:';
    function writingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardWritingOptions; message 'writingOptionsForType:pasteboard:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSImageHintCTM: NSString { available in 10_6 }; cvar; external;
  NSImageHintInterpolation: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSImageDelegateProtocol = objcprotocol external name 'NSImageDelegate' (NSObjectProtocol)
  optional
    function imageDidNotDraw_inRect (sender: id; aRect: NSRect): NSImage; message 'imageDidNotDraw:inRect:';
    procedure image_willLoadRepresentation (image: NSImage; rep: NSImageRep); message 'image:willLoadRepresentation:';
    procedure image_didLoadRepresentationHeader (image: NSImage; rep: NSImageRep); message 'image:didLoadRepresentationHeader:';
    procedure image_didLoadPartOfRepresentation_withValidRows (image: NSImage; rep: NSImageRep; rows: NSInteger); message 'image:didLoadPartOfRepresentation:withValidRows:';
    procedure image_didLoadRepresentation_withStatus (image: NSImage; rep: NSImageRep; status: NSImageLoadStatus); message 'image:didLoadRepresentation:withStatus:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSBundleImageExtension = objccategory external (NSBundle)
    function imageForResource (name: NSString): NSImage; message 'imageForResource:'; { available in 10_7 }
    function pathForImageResource (name: NSString): NSString; message 'pathForImageResource:';
    function URLForImageResource (name: NSString): NSURL; message 'URLForImageResource:'; { available in 10_6 }
  end;

type
  NSImage_NSDeprecated = objccategory external name 'NSDeprecated' (NSImage)
    procedure setFlipped (flag: ObjCBOOL); message 'setFlipped:'; deprecated 'in 10_0, 10_6';
    function isFlipped: ObjCBOOL; message 'isFlipped'; deprecated 'in 10_0, 10_6';
    procedure dissolveToPoint_fraction (point: NSPoint; aFloat: CGFloat); message 'dissolveToPoint:fraction:'; deprecated 'in 10_0, 10_6, "Use -drawAtPoint:... or -drawInRect:... methods instead"';
    procedure dissolveToPoint_fromRect_fraction (point: NSPoint; rect: NSRect; aFloat: CGFloat); message 'dissolveToPoint:fromRect:fraction:'; deprecated 'in 10_0, 10_6, "Use -drawAtPoint:... or -drawInRect:... methods instead"';
    procedure compositeToPoint_operation (point: NSPoint; op: NSCompositingOperation); message 'compositeToPoint:operation:'; deprecated 'in 10_0, 10_6, "Use -drawAtPoint:... or -drawInRect:... methods instead"';
    procedure compositeToPoint_fromRect_operation (point: NSPoint; rect: NSRect; op: NSCompositingOperation); message 'compositeToPoint:fromRect:operation:'; deprecated 'in 10_0, 10_6, "Use -drawAtPoint:... or -drawInRect:... methods instead"';
    procedure compositeToPoint_operation_fraction (point: NSPoint; op: NSCompositingOperation; delta: CGFloat); message 'compositeToPoint:operation:fraction:'; deprecated 'in 10_0, 10_6, "Use -drawAtPoint:... or -drawInRect:... methods instead"';
    procedure compositeToPoint_fromRect_operation_fraction (point: NSPoint; rect: NSRect; op: NSCompositingOperation; delta: CGFloat); message 'compositeToPoint:fromRect:operation:fraction:'; deprecated 'in 10_0, 10_6, "Use -drawAtPoint:... or -drawInRect:... methods instead"';
    procedure lockFocusOnRepresentation (imageRepresentation: NSImageRep); message 'lockFocusOnRepresentation:'; deprecated 'in 10_0, 10_6, "Use -lockFocus followed by -[NSImageRep drawInRect:] instead. See documentation for more info."';
    procedure setScalesWhenResized (flag: ObjCBOOL); message 'setScalesWhenResized:'; deprecated 'in 10_0, 10_6';
    function scalesWhenResized: ObjCBOOL; message 'scalesWhenResized'; deprecated 'in 10_0, 10_6';
    procedure setDataRetained (flag: ObjCBOOL); message 'setDataRetained:'; deprecated 'in 10_0, 10_6';
    function isDataRetained: ObjCBOOL; message 'isDataRetained'; deprecated 'in 10_0, 10_6';
    procedure setCachedSeparately (flag: ObjCBOOL); message 'setCachedSeparately:'; deprecated 'in 10_0, 10_6';
    function isCachedSeparately: ObjCBOOL; message 'isCachedSeparately'; deprecated 'in 10_0, 10_6';
    procedure setCacheDepthMatchesImageDepth (flag: ObjCBOOL); message 'setCacheDepthMatchesImageDepth:'; deprecated 'in 10_0, 10_6';
    function cacheDepthMatchesImageDepth: ObjCBOOL; message 'cacheDepthMatchesImageDepth'; deprecated 'in 10_0, 10_6';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSImageNameQuickLookTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameBluetoothTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameIChatTheaterTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameSlideshowTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameActionTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameSmartBadgeTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameIconViewTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameListViewTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameColumnViewTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameFlowViewTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNamePathTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameInvalidDataFreestandingTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameLockLockedTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameLockUnlockedTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameGoRightTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameGoLeftTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameRightFacingTriangleTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameLeftFacingTriangleTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameAddTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameRemoveTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameRevealFreestandingTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameFollowLinkFreestandingTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameEnterFullScreenTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameExitFullScreenTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameStopProgressTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameStopProgressFreestandingTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameRefreshTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameRefreshFreestandingTemplate: NSString { available in 10_5 }; cvar; external;
  NSImageNameBonjour: NSString { available in 10_5 }; cvar; external;
  NSImageNameComputer: NSString { available in 10_5 }; cvar; external;
  NSImageNameFolderBurnable: NSString { available in 10_5 }; cvar; external;
  NSImageNameFolderSmart: NSString { available in 10_5 }; cvar; external;
  NSImageNameFolder: NSString { available in 10_6 }; cvar; external;
  NSImageNameNetwork: NSString { available in 10_5 }; cvar; external;
  NSImageNameDotMac: NSString deprecated 'in 10_5, 10_7'; cvar; external;
  NSImageNameMobileMe: NSString { available in 10_6 }; cvar; external;
  NSImageNameMultipleDocuments: NSString { available in 10_5 }; cvar; external;
  NSImageNameUserAccounts: NSString { available in 10_5 }; cvar; external;
  NSImageNamePreferencesGeneral: NSString { available in 10_5 }; cvar; external;
  NSImageNameAdvanced: NSString { available in 10_5 }; cvar; external;
  NSImageNameInfo: NSString { available in 10_5 }; cvar; external;
  NSImageNameFontPanel: NSString { available in 10_5 }; cvar; external;
  NSImageNameColorPanel: NSString { available in 10_5 }; cvar; external;
  NSImageNameUser: NSString { available in 10_5 }; cvar; external;
  NSImageNameUserGroup: NSString { available in 10_5 }; cvar; external;
  NSImageNameEveryone: NSString { available in 10_5 }; cvar; external;
  NSImageNameUserGuest: NSString { available in 10_6 }; cvar; external;
  NSImageNameMenuOnStateTemplate: NSString { available in 10_6 }; cvar; external;
  NSImageNameMenuMixedStateTemplate: NSString { available in 10_6 }; cvar; external;
  NSImageNameApplicationIcon: NSString { available in 10_6 }; cvar; external;
  NSImageNameTrashEmpty: NSString { available in 10_6 }; cvar; external;
  NSImageNameTrashFull: NSString { available in 10_6 }; cvar; external;
  NSImageNameHomeTemplate: NSString { available in 10_6 }; cvar; external;
  NSImageNameBookmarksTemplate: NSString { available in 10_6 }; cvar; external;
  NSImageNameCaution: NSString { available in 10_6 }; cvar; external;
  NSImageNameStatusAvailable: NSString { available in 10_6 }; cvar; external;
  NSImageNameStatusPartiallyAvailable: NSString { available in 10_6 }; cvar; external;
  NSImageNameStatusUnavailable: NSString { available in 10_6 }; cvar; external;
  NSImageNameStatusNone: NSString { available in 10_6 }; cvar; external;
  NSImageNameShareTemplate: NSString { available in 10_8 }; cvar; external;
{$endif}

