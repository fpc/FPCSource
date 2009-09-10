{ Parsed from Appkit.framework NSMovie.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSMOVIE_PAS_H}
{$define NSMOVIE_PAS_H}
type
  NSMoviePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSMOVIE_PAS_T}
{$define NSMOVIE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSMovie = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMOVIE_PAS_C}
{$define NSMOVIE_PAS_C}

{ NSMovie }
  NSMovie = objcclass(NSObject, NSCodingProtocol)
  private
    __movie: Pointer;
    __url: NSURL;
    __movieFlags: bitpacked record
        dispose_: 0..1;
        reserved: 0..((1 shl 31)-1);
      end;
    __reserved1: clong;
    __reserved2: clong;
    
  public
    class function alloc: NSMovie; message 'alloc';

    function initWithMovie(movie: Pointer): id; message 'initWithMovie:';
    function initWithURL_byReference(URL_: NSURL; byRef: Boolean): id; message 'initWithURL:byReference:';
    function initWithPasteboard(pasteboard: NSPasteboard): id; message 'initWithPasteboard:';
    function QTMovie: Pointer; message 'QTMovie';
    function URL: NSURL; message 'URL';
    class function movieUnfilteredFileTypes: NSArray; message 'movieUnfilteredFileTypes';
    class function movieUnfilteredPasteboardTypes: NSArray; message 'movieUnfilteredPasteboardTypes';
    class function canInitWithPasteboard(pasteboard: NSPasteboard): Boolean; message 'canInitWithPasteboard:';
  end; external;

{$endif}
{$endif}
