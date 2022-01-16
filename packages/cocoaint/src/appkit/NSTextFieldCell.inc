{ Parsed from AppKit.framework NSTextFieldCell.h }


{$ifdef TYPES}
type
  NSTextFieldCellPtr = ^NSTextFieldCell;
{$endif}

{$ifdef TYPES}
type
  NSTextFieldBezelStyle = NSUInteger;
  NSTextFieldBezelStylePtr = ^NSTextFieldBezelStyle;

const
  NSTextFieldSquareBezel = 0;
  NSTextFieldRoundedBezel = 1;
{$endif}

{$ifdef CLASSES}

type
  NSTextFieldCell = objcclass external (NSActionCell)
  private
    _backgroundColor: NSColor;
    _textColor: NSColor;
    _tfFlags: bitpacked record
      case byte of
        0: (_anonBitField__tfFlags0: cuint);
        1: (
          drawsBackground: 0..1;
          bezelStyle: 0..((1 shl 3)-1);
          thcSortDirection: 0..((1 shl 2)-1);
          thcSortPriority: 0..((1 shl 4)-1);
          mini: 0..1;
          textColorIgnoresNormalDisableFlag: 0..1;
          textColorDisableFlag: 0..1;
          thcForceHighlightForSort: 0..1;
          invalidTextColor: 0..1;
          notificationForMarkedText: 0..1;
          inToolbar: 0..1;
          hasTextLayer: 0..1;
          isButtonTitle: 0..1;
          allowTightening: 0..1;
          thcHighlighted: 0..1;
          reservedTextFieldCell: 0..((1 shl 11)-1);
        );
      end;
  public
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(newValue: ObjCBOOL); message 'setDrawsBackground:';
    function drawsBackground: ObjCBOOL; message 'drawsBackground';
    procedure setTextColor(newValue: NSColor); message 'setTextColor:';
    function textColor: NSColor; message 'textColor';
    function setUpFieldEditorAttributes (textObj: NSText): NSText; message 'setUpFieldEditorAttributes:';
    procedure setBezelStyle(newValue: NSTextFieldBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSTextFieldBezelStyle; message 'bezelStyle';
    procedure setPlaceholderString(newValue: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(newValue: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';
    procedure setWantsNotificationForMarkedText (flag: ObjCBOOL); message 'setWantsNotificationForMarkedText:'; { available in 10_5 }
    procedure setAllowedInputSourceLocales(newValue: NSArray); message 'setAllowedInputSourceLocales:';
    function allowedInputSourceLocales: NSArray; message 'allowedInputSourceLocales';
  end;
{$endif}

