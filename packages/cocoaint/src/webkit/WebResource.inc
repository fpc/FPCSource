{ Parsed from WebKit.framework WebResource.h }


{$ifdef TYPES}
type
  WebResourcePtr = ^WebResource;
{$endif}

{$ifdef CLASSES}

type
  WebResource = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _private: WebResourcePrivate;
  public
    function initWithData_URL_MIMEType_textEncodingName_frameName (data: NSData; URL: NSURL; MIMEType: NSString; textEncodingName: NSString; frameName: NSString): instancetype; message 'initWithData:URL:MIMEType:textEncodingName:frameName:';
    function data: NSData; message 'data';
    function URL: NSURL; message 'URL';
    function MIMEType: NSString; message 'MIMEType';
    function textEncodingName: NSString; message 'textEncodingName';
    function frameName: NSString; message 'frameName';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

