{ Parsed from WebKit.framework WebArchive.h }


{$ifdef TYPES}
type
  WebArchivePtr = ^WebArchive;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  WebArchivePboardType: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  WebArchive = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _private: WebArchivePrivate;
  public
    function initWithMainResource_subresources_subframeArchives (mainResource: WebResource; subresources: NSArray; subframeArchives: NSArray): instancetype; message 'initWithMainResource:subresources:subframeArchives:';
    function initWithData (data: NSData): instancetype; message 'initWithData:';
    function mainResource: WebResource; message 'mainResource';
    function subresources: NSArray; message 'subresources';
    function subframeArchives: NSArray; message 'subframeArchives';
    function data: NSData; message 'data';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

