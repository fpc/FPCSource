{ Parsed from Webkit.framework WebArchive.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  WebArchivePboardType: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebArchive = objcclass;
  WebArchivePointer = ^WebArchive;
  WebArchivePtr = WebArchivePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBARCHIVE_PAS_C}
{$define WEBARCHIVE_PAS_C}

{ WebArchive }
  WebArchive = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _private: WebArchivePrivate;
    
  public
    class function alloc: WebArchive; message 'alloc';

    function initWithMainResource_subresources_subframeArchives(mainResource_: WebResource; subresources_: NSArray; subframeArchives_: NSArray): id; message 'initWithMainResource:subresources:subframeArchives:';
    function initWithData(data_: NSData): id; message 'initWithData:';
    function mainResource: WebResource; message 'mainResource';
    function subresources: NSArray; message 'subresources';
    function subframeArchives: NSArray; message 'subframeArchives';
    function data: NSData; message 'data';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end; external;

{$endif}
{$endif}
