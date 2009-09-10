{ Parsed from Appkit.framework NSNib.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSNIB_PAS_H}
{$define NSNIB_PAS_H}
type
  NSNibPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSNIB_PAS_T}
{$define NSNIB_PAS_T}

{ CFString constants }
var
  NSNibOwner: CFStringRef; external name '_NSNibOwner';
  NSNibTopLevelObjects: CFStringRef; external name '_NSNibTopLevelObjects';

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
{$ifndef NSNIB_PAS_T}
{$define NSNIB_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSNib = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSNIB_PAS_C}
{$define NSNIB_PAS_C}

{ NSNib }
  NSNib = objcclass(NSObject, NSCodingProtocol)
  private
    __data: NSData;
    __images: NSArray;
    __sounds: NSArray;
    __bundle: NSBundle;
    __flags: bitpacked record
        _isKeyed: 0..1;
        _reserved: 0..((1 shl 31)-1);
      end;
    _reserved1: id;
    _reserved2: id;
    
  public
    class function alloc: NSNib; message 'alloc';

    function initWithContentsOfURL(nibFileURL: NSURL): id; message 'initWithContentsOfURL:';
    function initWithNibNamed_bundle(nibName: NSString; bundle: NSBundle): id; message 'initWithNibNamed:bundle:';
    function instantiateNibWithExternalNameTable(externalNameTable: NSDictionary): Boolean; message 'instantiateNibWithExternalNameTable:';
    function instantiateNibWithOwner_topLevelObjects(owner: id; var topLevelObjects: NSArray): Boolean; message 'instantiateNibWithOwner:topLevelObjects:';
  end; external;

{$endif}
{$endif}
