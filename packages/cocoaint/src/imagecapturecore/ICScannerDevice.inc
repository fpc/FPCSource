{ Parsed from ImageCaptureCore.framework ICScannerDevice.h }


{$ifdef TYPES}
type
  ICScannerDevicePtr = ^ICScannerDevice;
  ICScannerDeviceDelegateProtocolPtr = ^ICScannerDeviceDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ICScannerStatusWarmingUp: NSString; cvar; external;
  ICScannerStatusWarmUpDone: NSString; cvar; external;
  ICScannerStatusRequestsOverviewScan: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
const
  ICScannerTransferModeFileBased = 0;
  ICScannerTransferModeMemoryBased = 1;

type
  ICScannerTransferMode = NSUInteger;
  ICScannerTransferModePtr = ^ICScannerTransferMode;
{$endif}

{$ifdef PROTOCOLS}

type
  ICScannerDeviceDelegateProtocol = objcprotocol external name 'ICScannerDeviceDelegate' (ICDeviceDelegateProtocol)
  optional
    procedure scannerDeviceDidBecomeAvailable (scanner: ICScannerDevice); message 'scannerDeviceDidBecomeAvailable:';
    procedure scannerDevice_didSelectFunctionalUnit_error (scanner: ICScannerDevice; functionalUnit: ICScannerFunctionalUnit; error: NSError); message 'scannerDevice:didSelectFunctionalUnit:error:';
    procedure scannerDevice_didScanToURL_data (scanner: ICScannerDevice; url: NSURL; data: NSData); message 'scannerDevice:didScanToURL:data:'; deprecated 'in 10_6, 10_7, NA, NA';
    procedure scannerDevice_didScanToURL (scanner: ICScannerDevice; url: NSURL); message 'scannerDevice:didScanToURL:';
    procedure scannerDevice_didScanToBandData (scanner: ICScannerDevice; data: ICScannerBandData); message 'scannerDevice:didScanToBandData:';
    procedure scannerDevice_didCompleteOverviewScanWithError (scanner: ICScannerDevice; error: NSError); message 'scannerDevice:didCompleteOverviewScanWithError:';
    procedure scannerDevice_didCompleteScanWithError (scanner: ICScannerDevice; error: NSError); message 'scannerDevice:didCompleteScanWithError:';
  end;
{$endif}

{$ifdef CLASSES}

type
  ICScannerDevice = objcclass external (ICDevice)
  private
    _scannerProperties: id;
  public
    function availableFunctionalUnitTypes: NSArray; message 'availableFunctionalUnitTypes';
    function selectedFunctionalUnit: ICScannerFunctionalUnit; message 'selectedFunctionalUnit';
    procedure setTransferMode(newValue: ICScannerTransferMode); message 'setTransferMode:';
    function transferMode: ICScannerTransferMode; message 'transferMode';
    procedure setMaxMemoryBandSize(newValue: UInt32); message 'setMaxMemoryBandSize:';
    function maxMemoryBandSize: UInt32; message 'maxMemoryBandSize';
    procedure setDownloadsDirectory(newValue: NSURL); message 'setDownloadsDirectory:';
    function downloadsDirectory: NSURL; message 'downloadsDirectory';
    procedure setDocumentName(newValue: NSString); message 'setDocumentName:';
    function documentName: NSString; message 'documentName';
    procedure setDocumentUTI(newValue: NSString); message 'setDocumentUTI:';
    function documentUTI: NSString; message 'documentUTI';
    procedure requestSelectFunctionalUnit (type_: ICScannerFunctionalUnitType); message 'requestSelectFunctionalUnit:';
    procedure requestOverviewScan; message 'requestOverviewScan';
    procedure requestScan; message 'requestScan';
    procedure cancelScan; message 'cancelScan';
  end;
{$endif}

