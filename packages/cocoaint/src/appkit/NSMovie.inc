{ Parsed from AppKit.framework NSMovie.h }


{ Types from NSMovie }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSMoviePtr = ^NSMovie;
{$endif}

{$ifdef CLASSES}

type
  NSMovie = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _movie: pointer;
    _url: NSURL;
    _movieFlags: bitpacked record
      case byte of
        0: (_anonBitField__movieFlags0: cuint);
        1: (
          dispose_: 0..1;
          reserved: 0..((1 shl 31)-1);
        );
      end;
    _reserved1: clong;
    _reserved2: clong;
  public
    {$if  not defined(__LP64__)}
    function initWithMovie (movie: pointer): id; message 'initWithMovie:'; deprecated 'in 10_0, 10_5';
    function initWithURL_byReference (url: NSURL; byRef: ObjCBOOL): id; message 'initWithURL:byReference:'; deprecated 'in 10_0, 10_5';
    function initWithPasteboard (pasteboard: NSPasteboard): id; message 'initWithPasteboard:'; deprecated 'in 10_0, 10_5';
    function QTMovie_: pointer; message 'QTMovie'; deprecated 'in 10_0, 10_5';
    function URL: NSURL; message 'URL'; deprecated 'in 10_0, 10_5';
    class function movieUnfilteredFileTypes: NSArray; message 'movieUnfilteredFileTypes'; deprecated 'in 10_0, 10_5';
    class function movieUnfilteredPasteboardTypes: NSArray; message 'movieUnfilteredPasteboardTypes'; deprecated 'in 10_0, 10_5';
    class function canInitWithPasteboard (pasteboard: NSPasteboard): ObjCBOOL; message 'canInitWithPasteboard:'; deprecated 'in 10_0, 10_5';
    {$else}
    function initWithMovie (movie: QTMovie): id; message 'initWithMovie:';
    function QTMovie_: QTMovie; message 'QTMovie';
    {$endif}

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

