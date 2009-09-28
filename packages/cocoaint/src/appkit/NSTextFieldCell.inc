{ Parsed from Appkit.framework NSTextFieldCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTEXTFIELDCELL_PAS_H}
{$define NSTEXTFIELDCELL_PAS_H}
type
  NSTextFieldCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTEXTFIELDCELL_PAS_T}
{$define NSTEXTFIELDCELL_PAS_T}

{ Constants }

const
  NSTextFieldSquareBezel = 0;
  NSTextFieldRoundedBezel = 1;

{ Types }
type
  NSTextFieldBezelStyle = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTFIELDCELL_PAS_R}
{$define NSTEXTFIELDCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTFIELDCELL_PAS_F}
{$define NSTEXTFIELDCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTFIELDCELL_PAS_S}
{$define NSTEXTFIELDCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextFieldCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTFIELDCELL_PAS_C}
{$define NSTEXTFIELDCELL_PAS_C}

{ NSTextFieldCell }
  NSTextFieldCell = objcclass(NSActionCell)
  private
    __backgroundColor: NSColor;
    __textColor: NSColor;
    __tfFlags: bitpacked record
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
        reservedTextFieldCell: 0..((1 shl 16)-1);
      end;
    
  public
    class function alloc: NSTextFieldCell; message 'alloc';

    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDrawsBackground(flag: Boolean); message 'setDrawsBackground:';
    function drawsBackground: Boolean; message 'drawsBackground';
    procedure setTextColor(color: NSColor); message 'setTextColor:';
    function textColor: NSColor; message 'textColor';
    function setUpFieldEditorAttributes(textObj: NSText): NSText; message 'setUpFieldEditorAttributes:';
    procedure setBezelStyle(style: NSTextFieldBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSTextFieldBezelStyle; message 'bezelStyle';
    procedure setPlaceholderString(string_: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(string_: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';
    procedure setWantsNotificationForMarkedText(flag: Boolean); message 'setWantsNotificationForMarkedText:';
    function allowedInputSourceLocales: NSArray; message 'allowedInputSourceLocales';
    procedure setAllowedInputSourceLocales(localeIdentifiers: NSArray); message 'setAllowedInputSourceLocales:';
  end; external;

{$endif}
{$endif}
