{ Parsed from MediaLibrary.framework MLMediaObject.h }


{$ifdef TYPES}
type
  MLMediaObjectPtr = ^MLMediaObject;
{$endif}

{$ifdef CLASSES}

type
  MLMediaObject = objcclass external (NSObject)
  public
    function mediaLibrary: MLMediaLibrary; message 'mediaLibrary';
    function identifier: NSString; message 'identifier';
    function mediaSourceIdentifier: NSString; message 'mediaSourceIdentifier';
    function attributes: NSDictionary; message 'attributes';
    function mediaType: MLMediaType; message 'mediaType';
    function contentType: NSString; message 'contentType';
    function name: NSString; message 'name';
    function URL: NSURL; message 'URL';
    function originalURL: NSURL; message 'originalURL';
    function fileSize: NSUInteger; message 'fileSize';
    function modificationDate: NSDate; message 'modificationDate';
    function thumbnailURL: NSURL; message 'thumbnailURL';
    function artworkImage: NSImage; message 'artworkImage';
  end;
{$endif}

