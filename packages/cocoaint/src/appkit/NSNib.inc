{ Parsed from Appkit.framework NSNib.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSNIB_PAS_T}
{$define NSNIB_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSNIB_PAS_R}
{$define NSNIB_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSNIB_PAS_F}
{$define NSNIB_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSNIB_PAS_S}
{$define NSNIB_PAS_S}

{ External string constants }
var
  NSNibOwner: NSString; cvar; external;
  NSNibTopLevelObjects: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNib = objcclass;
  NSNibPointer = ^NSNib;
  NSNibPtr = NSNibPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNIB_PAS_C}
{$define NSNIB_PAS_C}

{ NSNib }
  NSNib = objcclass(NSObject, NSCodingProtocol)
  private
    _data: NSData;
    _images: NSArray;
    _sounds: NSArray;
    _bundle: NSBundle;
    __flags: bitpacked record
      case byte of
       0: (_anonbitfield_NSNib0: cuint);
       1: (
        _isKeyed: 0..1;
        _reserved: 0..((1 shl 31)-1);
      );
      end;
    _path: NSString;
    reserved2: id;
    
  public
    class function alloc: NSNib; message 'alloc';

    function initWithContentsOfURL(nibFileURL: NSURL): id; message 'initWithContentsOfURL:';
    function initWithNibNamed_bundle(nibName: NSString; bundle: NSBundle): id; message 'initWithNibNamed:bundle:';
    function instantiateNibWithExternalNameTable(externalNameTable: NSDictionary): Boolean; message 'instantiateNibWithExternalNameTable:';
    function instantiateNibWithOwner_topLevelObjects(owner: id; topLevelObjects: NSArrayPointer): Boolean; message 'instantiateNibWithOwner:topLevelObjects:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{$endif}
{$endif}
