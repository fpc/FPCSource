{ Parsed from Foundation.framework NSByteCountFormatter.h }


{$ifdef TYPES}
type
  NSByteCountFormatterPtr = ^NSByteCountFormatter;
{$endif}

{$ifdef TYPES}
type
  NSByteCountFormatterUnits = NSUInteger;
  NSByteCountFormatterUnitsPtr = ^NSByteCountFormatterUnits;

const
  NSByteCountFormatterUseDefault = 0;
  NSByteCountFormatterUseBytes = 1 shl 0;
  NSByteCountFormatterUseKB = 1 shl 1;
  NSByteCountFormatterUseMB = 1 shl 2;
  NSByteCountFormatterUseGB = 1 shl 3;
  NSByteCountFormatterUseTB = 1 shl 4;
  NSByteCountFormatterUsePB = 1 shl 5;
  NSByteCountFormatterUseEB = 1 shl 6;
  NSByteCountFormatterUseZB = 1 shl 7;
  NSByteCountFormatterUseYBOrHigher = $0FF shl 8;
  NSByteCountFormatterUseAll = $0FFFF;

type
  NSByteCountFormatterCountStyle = NSInteger;
  NSByteCountFormatterCountStylePtr = ^NSByteCountFormatterCountStyle;

const
  NSByteCountFormatterCountStyleFile = 0;
  NSByteCountFormatterCountStyleMemory = 1;
  NSByteCountFormatterCountStyleDecimal = 2;
  NSByteCountFormatterCountStyleBinary = 3;
{$endif}

{$ifdef CLASSES}

type
  NSByteCountFormatter = objcclass external (NSFormatter)
  private
    _allowedUnits: cuint;
    _countStyle: char;
    _allowsNonnumericFormatting, _includesUnit, _includesCount, _includesActualByteCount, _adaptive, _zeroPadsFractionDigits: ObjCBOOL;
    _formattingContext: cint;
    _reserved:array[0..4] of cint;
  public
    class function stringFromByteCount_countStyle (byteCount: clonglong; countStyle: NSByteCountFormatterCountStyle): NSString; message 'stringFromByteCount:countStyle:';
    function stringFromByteCount (byteCount: clonglong): NSString; message 'stringFromByteCount:';
    procedure setAllowedUnits(newValue: NSByteCountFormatterUnits); message 'setAllowedUnits:';
    function allowedUnits: NSByteCountFormatterUnits; message 'allowedUnits';
    procedure setCountStyle(newValue: NSByteCountFormatterCountStyle); message 'setCountStyle:';
    function countStyle: NSByteCountFormatterCountStyle; message 'countStyle';
    procedure setAllowsNonnumericFormatting(newValue: ObjCBOOL); message 'setAllowsNonnumericFormatting:';
    function allowsNonnumericFormatting: ObjCBOOL; message 'allowsNonnumericFormatting';
    procedure setIncludesUnit(newValue: ObjCBOOL); message 'setIncludesUnit:';
    function includesUnit: ObjCBOOL; message 'includesUnit';
    procedure setIncludesCount(newValue: ObjCBOOL); message 'setIncludesCount:';
    function includesCount: ObjCBOOL; message 'includesCount';
    procedure setIncludesActualByteCount(newValue: ObjCBOOL); message 'setIncludesActualByteCount:';
    function includesActualByteCount: ObjCBOOL; message 'includesActualByteCount';
    procedure setAdaptive(newValue: ObjCBOOL); message 'setAdaptive:';
    function isAdaptive: ObjCBOOL; message 'isAdaptive';
    procedure setZeroPadsFractionDigits(newValue: ObjCBOOL); message 'setZeroPadsFractionDigits:';
    function zeroPadsFractionDigits: ObjCBOOL; message 'zeroPadsFractionDigits';
    procedure setFormattingContext(newValue: NSFormattingContext); message 'setFormattingContext:';
    function formattingContext: NSFormattingContext; message 'formattingContext';
  end;
{$endif}

