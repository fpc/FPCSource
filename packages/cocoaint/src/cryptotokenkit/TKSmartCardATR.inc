{ Parsed from CryptoTokenKit.framework TKSmartCardATR.h }


{$ifdef TYPES}
type
  TKSmartCardATRInterfaceGroupPtr = ^TKSmartCardATRInterfaceGroup;
  TKSmartCardATRPtr = ^TKSmartCardATR;
{$endif}

{$ifdef TYPES}
type
  TKSmartCardProtocol = NSUInteger;
  TKSmartCardProtocolPtr = ^TKSmartCardProtocol;

const
  TKSmartCardProtocolNone = 0;
  TKSmartCardProtocolT0 = 1 shl 0;
  TKSmartCardProtocolT1 = 1 shl 1;
  TKSmartCardProtocolT15 = 1 shl 15;
  TKSmartCardProtocolAny = 1 shl 16;
{$endif}

{$ifdef CLASSES}

type
  TKSmartCardATRInterfaceGroup = objcclass external (NSObject)
  public
    function TA: NSNumber; message 'TA';
    function TB: NSNumber; message 'TB';
    function TC: NSNumber; message 'TC';
    function protocol: NSNumber; message 'protocol';
  end;

type
  TKSmartCardATR = objcclass external (NSObject)
  public
    function initWithBytes (bytes: NSData): instancetype; message 'initWithBytes:';
    function initWithSource (source: OpaqueCBlock): instancetype; message 'initWithSource:';
    function bytes: NSData; message 'bytes';
    function protocols: NSArray; message 'protocols';
    function interfaceGroupAtIndex (index: NSInteger): TKSmartCardATRInterfaceGroup; message 'interfaceGroupAtIndex:';
    function interfaceGroupForProtocol (protocol: TKSmartCardProtocol): TKSmartCardATRInterfaceGroup; message 'interfaceGroupForProtocol:';
    function historicalBytes: NSData; message 'historicalBytes';
  end;
{$endif}

