{ Parsed from AppKit.framework NSNib.h }


{$ifdef TYPES}
type
  NSNibPtr = ^NSNib;
{$endif}

{$ifdef CLASSES}

type
  NSNib = objcclass external (NSObject, NSCodingProtocol)
  private
    _data: NSData;
    _images: NSArray;
    _sounds: NSArray;
    _bundle: NSBundle;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          _isKeyed: 0..1;
          _inheritsDecodeTimeBundle: 0..1;
          _inheritsDecodeTimePath: 0..1;
          _reserved: 0..((1 shl 29)-1);
        );
      end;
    _path: NSString;
    reserved2: id;
  public
    function initWithNibNamed_bundle (nibName: NSString; bundle: NSBundle): instancetype; message 'initWithNibNamed:bundle:';
    function initWithNibData_bundle (nibData: NSData; bundle: NSBundle): instancetype; message 'initWithNibData:bundle:'; { available in 10_8 }
    function instantiateWithOwner_topLevelObjects (owner: id; topLevelObjects: NSArrayPtr): ObjCBOOL; message 'instantiateWithOwner:topLevelObjects:'; { available in 10_8 }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSNib_NSDeprecated = objccategory external name 'NSDeprecated' (NSNib)
    function initWithContentsOfURL (nibFileURL: NSURL): id; message 'initWithContentsOfURL:'; deprecated 'in 10_0, 10_8';
    function instantiateNibWithExternalNameTable (externalNameTable: NSDictionary): ObjCBOOL; message 'instantiateNibWithExternalNameTable:'; deprecated 'in 10_0, 10_8';
    function instantiateNibWithOwner_topLevelObjects (owner: id; topLevelObjects: NSArrayPtr): ObjCBOOL; message 'instantiateNibWithOwner:topLevelObjects:'; deprecated 'in 10_3, 10_8';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSNibOwner: NSString deprecated 'in 10_0, 10_8'; cvar; external;
  NSNibTopLevelObjects: NSString deprecated 'in 10_0, 10_8'; cvar; external;
{$endif}

