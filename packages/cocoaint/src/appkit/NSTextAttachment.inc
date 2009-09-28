{ Parsed from Appkit.framework NSTextAttachment.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTEXTATTACHMENT_PAS_H}
{$define NSTEXTATTACHMENT_PAS_H}
type
  NSTextAttachmentCellPointer = Pointer;
  NSTextAttachmentPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTEXTATTACHMENT_PAS_T}
{$define NSTEXTATTACHMENT_PAS_T}

{ Constants }

const
  NSAttachmentCharacter = $fffc;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTATTACHMENT_PAS_R}
{$define NSTEXTATTACHMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTATTACHMENT_PAS_F}
{$define NSTEXTATTACHMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTATTACHMENT_PAS_S}
{$define NSTEXTATTACHMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextAttachmentCellProtocol = objcprotocol;
  NSTextAttachmentCell = objcclass;
  NSTextAttachment = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTATTACHMENT_PAS_C}
{$define NSTEXTATTACHMENT_PAS_C}

{ NSTextAttachmentCell }
  NSTextAttachmentCell = objcclass(NSCell, NSTextAttachmentCellProtocol)
  private
    __attachment: NSTextAttachment;
    
  public
    class function alloc: NSTextAttachmentCell; message 'alloc';
  end; external;

{ NSTextAttachment }
  NSTextAttachment = objcclass(NSObject, NSCodingProtocol)
  private
    __fileWrapper: NSFileWrapper;
    __cell: id;
    __flags: bitpacked record
        cellWasExplicitlySet: 0..1;
        int: 0..((1 shl 31)-1);
      end;
    
  public
    class function alloc: NSTextAttachment; message 'alloc';

    function initWithFileWrapper(fileWrapper_: NSFileWrapper): id; message 'initWithFileWrapper:';
    procedure setFileWrapper(fileWrapper_: NSFileWrapper); message 'setFileWrapper:';
    function fileWrapper: NSFileWrapper; message 'fileWrapper';
    function attachmentCell: id; message 'attachmentCell';
    procedure setAttachmentCell(cell: id); message 'setAttachmentCell:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTEXTATTACHMENT_PAS_P}
{$define NSTEXTATTACHMENT_PAS_P}
  
{ NSTextAttachmentCell Protocol }
  NSTextAttachmentCellProtocol = objcprotocol
    procedure drawWithFrame_inView(cellFrame: NSRect; controlView: NSView); message 'drawWithFrame:inView:';
    function wantsToTrackMouse: Boolean; message 'wantsToTrackMouse';
    procedure highlight_withFrame_inView(flag: Boolean; cellFrame: NSRect; controlView: NSView); message 'highlight:withFrame:inView:';
    function trackMouse_inRect_ofView_untilMouseUp(theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; flag: Boolean): Boolean; message 'trackMouse:inRect:ofView:untilMouseUp:';
    function cellSize: NSSize; message 'cellSize';
    function cellBaselineOffset: NSPoint; message 'cellBaselineOffset';
    procedure setAttachment(anObject: NSTextAttachment); message 'setAttachment:';
    function attachment: NSTextAttachment; message 'attachment';
    procedure drawWithFrame_inView_characterIndex(cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger); message 'drawWithFrame:inView:characterIndex:';
    procedure drawWithFrame_inView_characterIndex_layoutManager(cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; layoutManager: NSLayoutManager); message 'drawWithFrame:inView:characterIndex:layoutManager:';
    function wantsToTrackMouseForEvent_inRect_ofView_atCharacterIndex(theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger): Boolean; message 'wantsToTrackMouseForEvent:inRect:ofView:atCharacterIndex:';
    function trackMouse_inRect_ofView_atCharacterIndex_untilMouseUp(theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; flag: Boolean): Boolean; message 'trackMouse:inRect:ofView:atCharacterIndex:untilMouseUp:';
    function cellFrameForTextContainer_proposedLineFragment_glyphPosition_characterIndex(textContainer: NSTextContainer; lineFrag: NSRect; position: NSPoint; charIndex: NSUInteger): NSRect; message 'cellFrameForTextContainer:proposedLineFragment:glyphPosition:characterIndex:';
  end; external name 'NSTextAttachmentCell';
{$endif}
{$endif}
