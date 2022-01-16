{ Parsed from AddressBook.framework ABImageLoading.h }


{$ifdef TYPES}
type
  ABImageClientProtocolPtr = ^ABImageClientProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  ABImageClientProtocol = objcprotocol external name 'ABImageClient' (NSObjectProtocol)
    procedure consumeImageData_forTag (data: NSData; tag: NSInteger); message 'consumeImageData:forTag:';
  end;
{$endif}

{$ifdef CLASSES}

type
  ABPersonImageAdditions = objccategory external (ABPerson)
    function setImageData (data: NSData): ObjCBOOL; message 'setImageData:';
    function imageData: NSData; message 'imageData';
    function beginLoadingImageDataForClient (client: ABImageClientProtocol): NSInteger; message 'beginLoadingImageDataForClient:';
    class procedure cancelLoadingImageDataForTag (tag: NSInteger); message 'cancelLoadingImageDataForTag:';
  end;
{$endif}

