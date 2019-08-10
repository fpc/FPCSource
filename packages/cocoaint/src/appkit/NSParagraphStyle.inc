{ Parsed from AppKit.framework NSParagraphStyle.h }


{$ifdef TYPES}
type
  NSTextTabPtr = ^NSTextTab;
  NSParagraphStylePtr = ^NSParagraphStyle;
  NSMutableParagraphStylePtr = ^NSMutableParagraphStyle;
{$endif}

{$ifdef TYPES}
type
  NSTextTabType = NSUInteger;
  NSTextTabTypePtr = ^NSTextTabType;

const
  NSLeftTabStopType = 0;
  NSRightTabStopType = 1;
  NSCenterTabStopType = 2;
  NSDecimalTabStopType = 3;

type
  NSLineBreakMode = NSUInteger;
  NSLineBreakModePtr = ^NSLineBreakMode;

const
  NSLineBreakByWordWrapping = 0;
  NSLineBreakByCharWrapping = 1;
  NSLineBreakByClipping = 2;
  NSLineBreakByTruncatingHead = 3;
  NSLineBreakByTruncatingTail = 4;
  NSLineBreakByTruncatingMiddle = 5;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTabColumnTerminatorsAttributeName: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSTextTab = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          alignment: 0..((1 shl 4)-1);
          refCount: 0..((1 shl 24)-1);
          unused: 0..((1 shl 4)-1);
          {$if defined(__LP64__)}
          int: 0..((1 shl 32)-1);
          {$endif}
        );
      end;
    _location: CGFloat;
    _reserved: id;
  public
    function initWithTextAlignment_location_options (alignment: NSTextAlignment; loc: CGFloat; options: NSDictionary): instancetype; message 'initWithTextAlignment:location:options:';
    function alignment: NSTextAlignment; message 'alignment';
    function options: NSDictionary; message 'options';
    function initWithType_location (type_: NSTextTabType; loc: CGFloat): instancetype; message 'initWithType:location:';
    function location: CGFloat; message 'location';
    function tabStopType: NSTextTabType; message 'tabStopType';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;

type
  NSParagraphStyle = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSSecureCodingProtocol)
  private
    _lineSpacing: CGFloat;
    _paragraphSpacing: CGFloat;
    _headIndent: CGFloat;
    _tailIndent: CGFloat;
    _firstLineHeadIndent: CGFloat;
    _minimumLineHeight: CGFloat;
    _maximumLineHeight: CGFloat;
    _tabStops: NSArray;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          alignment: 0..((1 shl 4)-1);
          lineBreakMode: 0..((1 shl 4)-1);
          tabStopsIsMutable: 0..1;
          isNaturalDirection: 0..1;
          rightToLeftDirection: 0..1;
          fixedMultiple: 0..((1 shl 2)-1);
          refCount: 0..((1 shl 19)-1);
          {$if defined(__LP64__)}
          int: 0..((1 shl 32)-1);
          {$endif}
        );
      end;
    _defaultTabInterval: CGFloat;
    _extraData: id;
  public
    class function defaultParagraphStyle: NSParagraphStyle; message 'defaultParagraphStyle';
    class function defaultWritingDirectionForLanguage (languageName: NSString): NSWritingDirection; message 'defaultWritingDirectionForLanguage:';
    function lineSpacing: CGFloat; message 'lineSpacing';
    function paragraphSpacing: CGFloat; message 'paragraphSpacing';
    function alignment: NSTextAlignment; message 'alignment';
    function headIndent: CGFloat; message 'headIndent';
    function tailIndent: CGFloat; message 'tailIndent';
    function firstLineHeadIndent: CGFloat; message 'firstLineHeadIndent';
    function tabStops: NSArray; message 'tabStops';
    function minimumLineHeight: CGFloat; message 'minimumLineHeight';
    function maximumLineHeight: CGFloat; message 'maximumLineHeight';
    function lineBreakMode: NSLineBreakMode; message 'lineBreakMode';
    function baseWritingDirection: NSWritingDirection; message 'baseWritingDirection';
    function lineHeightMultiple: CGFloat; message 'lineHeightMultiple';
    function paragraphSpacingBefore: CGFloat; message 'paragraphSpacingBefore';
    function defaultTabInterval: CGFloat; message 'defaultTabInterval';
    function textBlocks: NSArray; message 'textBlocks';
    function textLists: NSArray; message 'textLists';
    function hyphenationFactor: single; message 'hyphenationFactor';
    function tighteningFactorForTruncation: single; message 'tighteningFactorForTruncation';
    function headerLevel: NSInteger; message 'headerLevel';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

type
  NSMutableParagraphStyle = objcclass external (NSParagraphStyle)
  public
    procedure setLineSpacing(newValue: CGFloat); message 'setLineSpacing:';
    function lineSpacing: CGFloat; message 'lineSpacing';
    procedure setParagraphSpacing(newValue: CGFloat); message 'setParagraphSpacing:';
    function paragraphSpacing: CGFloat; message 'paragraphSpacing';
    procedure setAlignment(newValue: NSTextAlignment); message 'setAlignment:';
    function alignment: NSTextAlignment; message 'alignment';
    procedure setHeadIndent(newValue: CGFloat); message 'setHeadIndent:';
    function headIndent: CGFloat; message 'headIndent';
    procedure setTailIndent(newValue: CGFloat); message 'setTailIndent:';
    function tailIndent: CGFloat; message 'tailIndent';
    procedure setFirstLineHeadIndent(newValue: CGFloat); message 'setFirstLineHeadIndent:';
    function firstLineHeadIndent: CGFloat; message 'firstLineHeadIndent';
    procedure addTabStop (anObject: NSTextTab); message 'addTabStop:';
    procedure removeTabStop (anObject: NSTextTab); message 'removeTabStop:';
    procedure setTabStops(newValue: NSArray); message 'setTabStops:';
    function tabStops: NSArray; message 'tabStops';
    procedure setMinimumLineHeight(newValue: CGFloat); message 'setMinimumLineHeight:';
    function minimumLineHeight: CGFloat; message 'minimumLineHeight';
    procedure setMaximumLineHeight(newValue: CGFloat); message 'setMaximumLineHeight:';
    function maximumLineHeight: CGFloat; message 'maximumLineHeight';
    procedure setLineBreakMode(newValue: NSLineBreakMode); message 'setLineBreakMode:';
    function lineBreakMode: NSLineBreakMode; message 'lineBreakMode';
    procedure setParagraphStyle (obj: NSParagraphStyle); message 'setParagraphStyle:';
    procedure setBaseWritingDirection(newValue: NSWritingDirection); message 'setBaseWritingDirection:';
    function baseWritingDirection: NSWritingDirection; message 'baseWritingDirection';
    procedure setLineHeightMultiple(newValue: CGFloat); message 'setLineHeightMultiple:';
    function lineHeightMultiple: CGFloat; message 'lineHeightMultiple';
    procedure setParagraphSpacingBefore(newValue: CGFloat); message 'setParagraphSpacingBefore:';
    function paragraphSpacingBefore: CGFloat; message 'paragraphSpacingBefore';
    procedure setDefaultTabInterval(newValue: CGFloat); message 'setDefaultTabInterval:';
    function defaultTabInterval: CGFloat; message 'defaultTabInterval';
    procedure setTextBlocks(newValue: NSArray); message 'setTextBlocks:';
    function textBlocks: NSArray; message 'textBlocks';
    procedure setTextLists(newValue: NSArray); message 'setTextLists:';
    function textLists: NSArray; message 'textLists';
    procedure setHyphenationFactor(newValue: single); message 'setHyphenationFactor:';
    function hyphenationFactor: single; message 'hyphenationFactor';
    procedure setTighteningFactorForTruncation(newValue: single); message 'setTighteningFactorForTruncation:';
    function tighteningFactorForTruncation: single; message 'tighteningFactorForTruncation';
    procedure setHeaderLevel(newValue: NSInteger); message 'setHeaderLevel:';
    function headerLevel: NSInteger; message 'headerLevel';
  end;
{$endif}

