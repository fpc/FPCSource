{ Parsed from Appkit.framework NSPDFImageRep.h }

{$ifdef TYPES}
{$ifndef NSPDFIMAGEREP_PAS_T}
{$define NSPDFIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPDFIMAGEREP_PAS_R}
{$define NSPDFIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPDFIMAGEREP_PAS_F}
{$define NSPDFIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPDFIMAGEREP_PAS_S}
{$define NSPDFIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPDFImageRep = objcclass;
  NSPDFImageRepPointer = ^NSPDFImageRep;
  NSPDFImageRepPtr = NSPDFImageRepPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPDFIMAGEREP_PAS_C}
{$define NSPDFIMAGEREP_PAS_C}

{ NSPDFImageRep }
  NSPDFImageRep = objcclass external (NSImageRep)
  private
    _pdfData: NSData;
    _reserved1: cint;
    _reserved2: cint;
    _private: id;
    
  public
    class function imageRepWithData(pdfData: NSData): id; message 'imageRepWithData:';
    function initWithData(pdfData: NSData): id; message 'initWithData:';
    function PDFRepresentation: NSData; message 'PDFRepresentation';
    function bounds: NSRect; message 'bounds';
    procedure setCurrentPage(page: NSInteger); message 'setCurrentPage:';
    function currentPage: NSInteger; message 'currentPage';
    function pageCount: NSInteger; message 'pageCount';
  end;

{$endif}
{$endif}
