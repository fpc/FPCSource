{ Parsed from Webkit.framework WebResource.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef WEBRESOURCE_PAS_T}
{$define WEBRESOURCE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBRESOURCE_PAS_R}
{$define WEBRESOURCE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBRESOURCE_PAS_F}
{$define WEBRESOURCE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBRESOURCE_PAS_S}
{$define WEBRESOURCE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  WebResource = objcclass;
  WebResourcePointer = ^WebResource;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBRESOURCE_PAS_C}
{$define WEBRESOURCE_PAS_C}

{ WebResource }
  WebResource = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __private: WebResourcePrivate;
    
  public
    class function alloc: WebResource; message 'alloc';

    function initWithData_URL_MIMEType_textEncodingName_frameName(data_: NSDataPointer; URL_: NSURLPointer; MIMEType_: NSStringPointer; textEncodingName_: NSStringPointer; frameName_: NSStringPointer): id; message 'initWithData:URL:MIMEType:textEncodingName:frameName:';
    function data: NSData; message 'data';
    function URL: NSURL; message 'URL';
    function MIMEType: NSString; message 'MIMEType';
    function textEncodingName: NSString; message 'textEncodingName';
    function frameName: NSString; message 'frameName';
  end; external;

{$endif}
{$endif}
