{ Parsed from CoreImage.framework CIColor.h }


{$ifdef TYPES}
type
  CIColorPtr = ^CIColor;
{$endif}

{$ifdef CLASSES}

type
  CIColor = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _priv: pointer;
    _pad:array[0..2] of pointer;
  public
    class function colorWithCGColor (c: CGColorRef): CIColor; message 'colorWithCGColor:';
    class function colorWithRed_green_blue_alpha (r: CGFloat; g: CGFloat; b: CGFloat; a: CGFloat): CIColor; message 'colorWithRed:green:blue:alpha:';
    class function colorWithRed_green_blue (r: CGFloat; g: CGFloat; b: CGFloat): CIColor; message 'colorWithRed:green:blue:';
    class function colorWithString (representation: NSString): CIColor; message 'colorWithString:';
    function initWithCGColor (c: CGColorRef): id; message 'initWithCGColor:';
    function numberOfComponents: size_t; message 'numberOfComponents';
    function components: CGFloatPtr; message 'components';
    function alpha: CGFloat; message 'alpha';
    function colorSpace: CGColorSpaceRef; message 'colorSpace';
    function red: CGFloat; message 'red';
    function green: CGFloat; message 'green';
    function blue: CGFloat; message 'blue';
    function stringRepresentation: NSString; message 'stringRepresentation';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

