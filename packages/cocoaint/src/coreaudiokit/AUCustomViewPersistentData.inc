{ Parsed from CoreAudioKit.framework AUCustomViewPersistentData.h }


{$ifdef TYPES}
type
  AUCustomViewPersistentDataProtocolPtr = ^AUCustomViewPersistentDataProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  AUCustomViewPersistentDataProtocol = objcprotocol external name 'AUCustomViewPersistentData'
    function customViewPersistentData: NSDictionary; message 'customViewPersistentData';
    procedure setCustomViewPersistentData (data: NSDictionary); message 'setCustomViewPersistentData:';
  end;
{$endif}

