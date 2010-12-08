{ Parsed from Appkit.framework NSTextAttachment.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
  NSTextAttachmentCellPointer = ^NSTextAttachmentCell;
  NSTextAttachmentCellPtr = NSTextAttachmentCellPointer;
  NSTextAttachment = objcclass;
  NSTextAttachmentPointer = ^NSTextAttachment;
  NSTextAttachmentPtr = NSTextAttachmentPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTATTACHMENT_PAS_C}
{$define NSTEXTATTACHMENT_PAS_C}

{ NSTextAttachmentCell }
  NSTextAttachmentCell = objcclass(NSCell, NSTextAttachmentCellProtocol)
  private
    _attachment: NSTextAttachment;
    
  public
    class function alloc: NSTextAttachmentCell; message 'alloc';

    { Adopted Protocols }
    procedure drawWithFrame_inView(cellFrame: NSRect; controlView: NSView);
    function wantsToTrackMouse: Boolean;
    procedure highlight_withFrame_inView(flag: Boolean; cellFrame: NSRect; controlView: NSView);
    function trackMouse_inRect_ofView_untilMouseUp(theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; flag: Boolean): Boolean;
    function cellSize: NSSize;
    function cellBaselineOffset: NSPoint;
    procedure setAttachment(anObject: NSTextAttachment);
    function attachment: NSTextAttachment;
    procedure drawWithFrame_inView_characterIndex(cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger);
    procedure drawWithFrame_inView_characterIndex_layoutManager(cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; layoutManager: NSLayoutManager);
    function wantsToTrackMouseForEvent_inRect_ofView_atCharacterIndex(theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger): Boolean;
    function trackMouse_inRect_ofView_atCharacterIndex_untilMouseUp(theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; flag: Boolean): Boolean;
    function cellFrameForTextContainer_proposedLineFragment_glyphPosition_characterIndex(textContainer: NSTextContainer; lineFrag: NSRect; position: NSPoint; charIndex: NSUInteger): NSRect;
  end; external;

{ NSTextAttachment }
  NSTextAttachment = objcclass(NSObject, NSCodingProtocol)
  private
    _fileWrapper: NSFileWrapper;
    _cell: NSTextAttachmentCellProtocol;
    __flags: bitpacked record
      case byte of
       0: (_anonbitfield_NSTextAttachment1: cuint);
       1: (
        cellWasExplicitlySet: 0..1;
        ignoresOrientation: 0..1;
        _anonbitfield_NSTextAttachment0: 0..((1 shl 30)-1);
      );
      end;
    
  public
    class function alloc: NSTextAttachment; message 'alloc';

    function initWithFileWrapper(fileWrapper_: NSFileWrapper): id; message 'initWithFileWrapper:';
    procedure setFileWrapper(fileWrapper_: NSFileWrapper); message 'setFileWrapper:';
    function fileWrapper: NSFileWrapper; message 'fileWrapper';
    function attachmentCell: id; message 'attachmentCell';
    procedure setAttachmentCell(cell: id); message 'setAttachmentCell:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSAttributedStringAttachmentConveniencesCategory }
  NSAttributedStringAttachmentConveniencesCategory = objccategory(NSAttributedString)
    class function attributedStringWithAttachment(attachment: NSTextAttachment): NSAttributedString; message 'attributedStringWithAttachment:';
  end; external;

{ NSMutableAttributedStringAttachmentConveniencesCategory }
  NSMutableAttributedStringAttachmentConveniencesCategory = objccategory(NSMutableAttributedString)
    procedure updateAttachmentsFromPath(path: NSString); message 'updateAttachmentsFromPath:';
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
