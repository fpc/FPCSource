{ Parsed from Appkit.framework NSMovie.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSMOVIE_PAS_T}
{$define NSMOVIE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSMOVIE_PAS_R}
{$define NSMOVIE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSMOVIE_PAS_F}
{$define NSMOVIE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSMOVIE_PAS_S}
{$define NSMOVIE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMovie = objcclass;
  NSMoviePointer = ^NSMovie;
  NSMoviePtr = NSMoviePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMOVIE_PAS_C}
{$define NSMOVIE_PAS_C}

{ NSMovie }
  NSMovie = objcclass(NSObject, NSCodingProtocol)
  private
    _movie: Pointer;
    _url: NSURL;
    __movieFlags: bitpacked record
      case byte of
       0: (_anonbitfield_NSMovie0: cint);
       1: (
        _dispose: 0..1;
        reserved: 0..((1 shl 31)-1);
      );
      end;
    _reserved1: clong;
    _reserved2: clong;
    
  public
    class function alloc: NSMovie; message 'alloc';

    function initWithMovie(movie: Pointer): id; message 'initWithMovie:'; deprecated 'in Mac OS X 10.5 and later';
    function initWithURL_byReference(url: NSURL; byRef: Boolean): id; message 'initWithURL:byReference:'; deprecated 'in Mac OS X 10.5 and later';
    function initWithPasteboard(pasteboard: NSPasteboard): id; message 'initWithPasteboard:'; deprecated 'in Mac OS X 10.5 and later';
    function QTMovie_: Pointer; message 'QTMovie'; deprecated 'in Mac OS X 10.5 and later';
    function URL: NSURL; message 'URL'; deprecated 'in Mac OS X 10.5 and later';
    class function movieUnfilteredFileTypes: NSArray; message 'movieUnfilteredFileTypes'; deprecated 'in Mac OS X 10.5 and later';
    class function movieUnfilteredPasteboardTypes: NSArray; message 'movieUnfilteredPasteboardTypes'; deprecated 'in Mac OS X 10.5 and later';
    class function canInitWithPasteboard(pasteboard: NSPasteboard): Boolean; message 'canInitWithPasteboard:'; deprecated 'in Mac OS X 10.5 and later';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{$endif}
{$endif}
