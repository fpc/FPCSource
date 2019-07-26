{ Parsed from QuartzCore.framework CATextLayer.h }


{$ifdef TYPES}
type
  CATextLayerPtr = ^CATextLayer;
{$endif}

{$ifdef CLASSES}

type
  CATextLayer = objcclass external (CALayer)
  private
    _state: CATextLayerPrivatePtr;
  public
    procedure setString(newValue: id); message 'setString:';
    function string_: id; message 'string';
    procedure setFont(newValue: CFTypeRef); message 'setFont:';
    function font: CFTypeRef; message 'font';
    procedure setFontSize(newValue: CGFloat); message 'setFontSize:';
    function fontSize: CGFloat; message 'fontSize';
    procedure setForegroundColor(newValue: CGColorRef); message 'setForegroundColor:';
    function foregroundColor: CGColorRef; message 'foregroundColor';
    procedure setWrapped(newValue: ObjCBOOL); message 'setWrapped:';
    function isWrapped: ObjCBOOL; message 'isWrapped';
    procedure setTruncationMode(newValue: NSString); message 'setTruncationMode:';
    function truncationMode: NSString; message 'truncationMode';
    procedure setAlignmentMode(newValue: NSString); message 'setAlignmentMode:';
    function alignmentMode: NSString; message 'alignmentMode';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCATruncationNone: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCATruncationStart: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCATruncationEnd: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCATruncationMiddle: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCAAlignmentNatural: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCAAlignmentLeft: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCAAlignmentRight: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCAAlignmentCenter: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
  kCAAlignmentJustified: NSString { available starting in __MAC_10_5, __IPHONE_3_2 }; cvar; external;
{$endif}

