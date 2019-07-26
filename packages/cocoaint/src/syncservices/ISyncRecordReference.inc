{ Parsed from SyncServices.framework ISyncRecordReference.h }


{$ifdef TYPES}
type
  ISyncRecordReferencePtr = ^ISyncRecordReference;
{$endif}

{$ifdef CLASSES}

type
  ISyncRecordReference = objcclass external (NSObject, NSCodingProtocol)
  private

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

