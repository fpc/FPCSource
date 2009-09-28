{ Parsed from Appkit.framework NSPDFImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

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
{$ifndef NSPDFIMAGEREP_PAS_S}
{$define NSPDFIMAGEREP_PAS_S}

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
    procedure setCurrentPage(page: NSInteger); message 'setCurrentPage:';
    function currentPage: NSInteger; message 'currentPage';
    function pageCount: NSInteger; message 'pageCount';
  end; external;

{$endif}
{$endif}
