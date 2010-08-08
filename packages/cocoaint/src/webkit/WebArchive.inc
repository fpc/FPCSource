{ Parsed from Webkit.framework WebArchive.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBARCHIVE_PAS_T}
{$define WEBARCHIVE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBARCHIVE_PAS_R}
{$define WEBARCHIVE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBARCHIVE_PAS_F}
{$define WEBARCHIVE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBARCHIVE_PAS_S}
{$define WEBARCHIVE_PAS_S}

{ External string constants }
var
  WebArchivePboardType: NSString; external name '_WebArchivePboardType';

{$endif}
{$endif}

{$ifdef FORWARD}
  WebArchive = objcclass;
  WebArchivePointer = ^WebArchive;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBARCHIVE_PAS_C}
{$define WEBARCHIVE_PAS_C}

{ WebArchive }
  WebArchive = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __private: WebArchivePrivate;
    
  public
    class function alloc: WebArchive; message 'alloc';

    function initWithMainResource_subresources_subframeArchives(mainResource_: WebResource; subresources_: NSArray; subframeArchives_: NSArray): id; message 'initWithMainResource:subresources:subframeArchives:';
    function initWithData(data_: NSData): id; message 'initWithData:';
    function mainResource: WebResource; message 'mainResource';
    function subresources: NSArray; message 'subresources';
    function subframeArchives: NSArray; message 'subframeArchives';
    function data: NSData; message 'data';
  end; external;

{$endif}
{$endif}
