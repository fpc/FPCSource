{ Parsed from AppKit.framework NSTextAttachment.h }


{$ifdef TYPES}
type
  NSTextAttachmentCellPtr = ^NSTextAttachmentCell;
  NSTextAttachmentPtr = ^NSTextAttachment;
  NSTextAttachmentCellProtocolPtr = ^NSTextAttachmentCellProtocol;
{$endif}

{$ifdef TYPES}
const
  NSAttachmentCharacter = $fffc;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTextAttachmentCellProtocol = objcprotocol external name 'NSTextAttachmentCell' (NSObjectProtocol)
    procedure drawWithFrame_inView (cellFrame: NSRect; controlView: NSView); message 'drawWithFrame:inView:';
    function wantsToTrackMouse: ObjCBOOL; message 'wantsToTrackMouse';
    procedure highlight_withFrame_inView (flag: ObjCBOOL; cellFrame: NSRect; controlView: NSView); message 'highlight:withFrame:inView:';
    function trackMouse_inRect_ofView_untilMouseUp (theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; flag: ObjCBOOL): ObjCBOOL; message 'trackMouse:inRect:ofView:untilMouseUp:';
    function cellSize: NSSize; message 'cellSize';
    function cellBaselineOffset: NSPoint; message 'cellBaselineOffset';
    procedure setAttachment (anObject: NSTextAttachment); message 'setAttachment:';
    function attachment: NSTextAttachment; message 'attachment';
    procedure drawWithFrame_inView_characterIndex (cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger); message 'drawWithFrame:inView:characterIndex:';
    procedure drawWithFrame_inView_characterIndex_layoutManager (cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; layoutManager: NSLayoutManager); message 'drawWithFrame:inView:characterIndex:layoutManager:';
    function wantsToTrackMouseForEvent_inRect_ofView_atCharacterIndex (theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger): ObjCBOOL; message 'wantsToTrackMouseForEvent:inRect:ofView:atCharacterIndex:';
    function trackMouse_inRect_ofView_atCharacterIndex_untilMouseUp (theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; flag: ObjCBOOL): ObjCBOOL; message 'trackMouse:inRect:ofView:atCharacterIndex:untilMouseUp:';
    function cellFrameForTextContainer_proposedLineFragment_glyphPosition_characterIndex (textContainer: NSTextContainer; lineFrag: NSRect; position: NSPoint; charIndex: NSUInteger): NSRect; message 'cellFrameForTextContainer:proposedLineFragment:glyphPosition:characterIndex:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSTextAttachmentCell = objcclass external (NSCell, NSTextAttachmentCellProtocol)
  private
    _attachment: NSTextAttachment;

    { Adopted protocols }
    function attachment: NSTextAttachment; message 'attachment';
    function cellBaselineOffset: NSPoint; message 'cellBaselineOffset';
    function cellFrameForTextContainer_proposedLineFragment_glyphPosition_characterIndex (textContainer: NSTextContainer; lineFrag: NSRect; position: NSPoint; charIndex: NSUInteger): NSRect; message 'cellFrameForTextContainer:proposedLineFragment:glyphPosition:characterIndex:';
    procedure drawWithFrame_inView_characterIndex (cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger); message 'drawWithFrame:inView:characterIndex:';
    procedure drawWithFrame_inView_characterIndex_layoutManager (cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; layoutManager: NSLayoutManager); message 'drawWithFrame:inView:characterIndex:layoutManager:';
    procedure setAttachment (anObject: NSTextAttachment); message 'setAttachment:';
    function trackMouse_inRect_ofView_atCharacterIndex_untilMouseUp (theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger; flag: ObjCBOOL): ObjCBOOL; message 'trackMouse:inRect:ofView:atCharacterIndex:untilMouseUp:';
    function wantsToTrackMouse: ObjCBOOL; message 'wantsToTrackMouse';
    function wantsToTrackMouseForEvent_inRect_ofView_atCharacterIndex (theEvent: NSEvent; cellFrame: NSRect; controlView: NSView; charIndex: NSUInteger): ObjCBOOL; message 'wantsToTrackMouseForEvent:inRect:ofView:atCharacterIndex:';
  end;

type
  NSTextAttachment = objcclass external (NSObject, NSCodingProtocol)
  private
    _fileWrapper: NSFileWrapper;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          cellWasExplicitlySet: 0..1;
          ignoresOrientation: 0..1;
          allowsEditingContents: 0..1;
          int: 0..((1 shl 29)-1);
        );
      end;
  public
    function initWithFileWrapper (fileWrapper: NSFileWrapper): instancetype; message 'initWithFileWrapper:';
    procedure setFileWrapper(newValue: NSFileWrapper); message 'setFileWrapper:';
    function fileWrapper: NSFileWrapper; message 'fileWrapper';
    procedure setAttachmentCell(newValue: NSTextAttachmentCellProtocol); message 'setAttachmentCell:';
    function attachmentCell: NSTextAttachmentCellProtocol; message 'attachmentCell';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSAttributedStringAttachmentConveniences = objccategory external (NSAttributedString)
    class function attributedStringWithAttachment (attachment: NSTextAttachment): NSAttributedString; message 'attributedStringWithAttachment:';
  end;

type
  NSMutableAttributedStringAttachmentConveniences = objccategory external (NSMutableAttributedString)
    procedure updateAttachmentsFromPath (path: NSString); message 'updateAttachmentsFromPath:';
  end;
{$endif}

