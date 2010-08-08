{ Parsed from Webkit.framework DOMStyleSheet.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMSTYLESHEET_PAS_T}
{$define DOMSTYLESHEET_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMSTYLESHEET_PAS_R}
{$define DOMSTYLESHEET_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMSTYLESHEET_PAS_F}
{$define DOMSTYLESHEET_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMSTYLESHEET_PAS_S}
{$define DOMSTYLESHEET_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMStyleSheet = objcclass;
  DOMStyleSheetPointer = ^DOMStyleSheet;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMSTYLESHEET_PAS_C}
{$define DOMSTYLESHEET_PAS_C}

{ DOMStyleSheet }
  DOMStyleSheet = objcclass(DOMObject)
    
  public
    class function alloc: DOMStyleSheet; message 'alloc';

    function type_: NSString; message 'type_';
    function ownerNode: DOMNode; message 'ownerNode';
    function parentStyleSheet: DOMStyleSheet; message 'parentStyleSheet';
    function href: NSString; message 'href';
    function title: NSString; message 'title';
    function media: DOMMediaList; message 'media';
  end; external;

{$endif}
{$endif}
