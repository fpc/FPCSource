{ Parsed from Appkit.framework NSPDFImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPDFIMAGEREP_PAS_H}
{$define NSPDFIMAGEREP_PAS_H}
type
  NSPDFImageRepPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSPDFIMAGEREP_PAS_T}
{$define NSPDFIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPDFImageRep = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPDFIMAGEREP_PAS_C}
{$define NSPDFIMAGEREP_PAS_C}

{ NSPDFImageRep }
  NSPDFImageRep = objcclass(NSImageRep)
  private
    __pdfData: NSData;
    __reserved1: cint;
    __reserved2: cint;
    __private: id;
    
  public
    class function alloc: NSPDFImageRep; message 'alloc';

    class function imageRepWithData(pdfData: NSData): id; message 'imageRepWithData:';
    function initWithData(pdfData: NSData): id; message 'initWithData:';
    function PDFRepresentation: NSData; message 'PDFRepresentation';
    function bounds: NSRect; message 'bounds';
    procedure setCurrentPage(page: clong); message 'setCurrentPage:';
    function currentPage: clong; message 'currentPage';
    function pageCount: clong; message 'pageCount';
  end; external;

{$endif}
{$endif}
