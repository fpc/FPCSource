{ Parsed from ImageCaptureCore.framework ICCameraDevice.h }


{$ifdef TYPES}
type
  ICCameraDevicePtr = ^ICCameraDevice;
  ICCameraDeviceDelegateProtocolPtr = ^ICCameraDeviceDelegateProtocol;
  ICCameraDeviceDownloadDelegateProtocolPtr = ^ICCameraDeviceDownloadDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ICCameraDeviceCanTakePicture: NSString; cvar; external;
  ICCameraDeviceCanTakePictureUsingShutterReleaseOnCamera: NSString; cvar; external;
  ICCameraDeviceCanDeleteOneFile: NSString; cvar; external;
  ICCameraDeviceCanDeleteAllFiles: NSString; cvar; external;
  ICCameraDeviceCanSyncClock: NSString; cvar; external;
  ICCameraDeviceCanReceiveFile: NSString; cvar; external;
  ICCameraDeviceCanAcceptPTPCommands: NSString; cvar; external;
  ICDownloadsDirectoryURL: NSString; cvar; external;
  ICSaveAsFilename: NSString; cvar; external;
  ICSavedFilename: NSString; cvar; external;
  ICSavedAncillaryFiles: NSString; cvar; external;
  ICOverwrite: NSString; cvar; external;
  ICDeleteAfterSuccessfulDownload: NSString; cvar; external;
  ICDownloadSidecarFiles: NSString; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  ICCameraDeviceDelegateProtocol = objcprotocol external name 'ICCameraDeviceDelegate' (ICDeviceDelegateProtocol)
  optional
    procedure cameraDevice_didAddItem (camera: ICCameraDevice; item: ICCameraItem); message 'cameraDevice:didAddItem:';
    procedure cameraDevice_didAddItems (camera: ICCameraDevice; items: NSArray); message 'cameraDevice:didAddItems:';
    procedure cameraDevice_didRemoveItem (camera: ICCameraDevice; item: ICCameraItem); message 'cameraDevice:didRemoveItem:';
    procedure cameraDevice_didRemoveItems (camera: ICCameraDevice; items: NSArray); message 'cameraDevice:didRemoveItems:';
    procedure cameraDevice_didRenameItems (camera: ICCameraDevice; items: NSArray); message 'cameraDevice:didRenameItems:';
    procedure cameraDevice_didCompleteDeleteFilesWithError (scanner: ICCameraDevice; error: NSError); message 'cameraDevice:didCompleteDeleteFilesWithError:';
    procedure cameraDeviceDidChangeCapability (camera: ICCameraDevice); message 'cameraDeviceDidChangeCapability:';
    procedure cameraDevice_didReceiveThumbnailForItem (camera: ICCameraDevice; item: ICCameraItem); message 'cameraDevice:didReceiveThumbnailForItem:';
    procedure cameraDevice_didReceiveMetadataForItem (camera: ICCameraDevice; item: ICCameraItem); message 'cameraDevice:didReceiveMetadataForItem:';
    procedure cameraDevice_didReceivePTPEvent (camera: ICCameraDevice; eventData: NSData); message 'cameraDevice:didReceivePTPEvent:';
    procedure deviceDidBecomeReadyWithCompleteContentCatalog (device: ICDevice); message 'deviceDidBecomeReadyWithCompleteContentCatalog:';
    function cameraDevice_shouldGetThumbnailOfItem (cameraDevice: ICCameraDevice; item: ICCameraItem): ObjCBOOL; message 'cameraDevice:shouldGetThumbnailOfItem:';
    function cameraDevice_shouldGetMetadataOfItem (cameraDevice: ICCameraDevice; item: ICCameraItem): ObjCBOOL; message 'cameraDevice:shouldGetMetadataOfItem:';
  end;

type
  ICCameraDeviceDownloadDelegateProtocol = objcprotocol external name 'ICCameraDeviceDownloadDelegate' (NSObjectProtocol)
  optional
    procedure didDownloadFile_error_options_contextInfo (file_: ICCameraFile; error: NSError; options: NSDictionary; contextInfo: pointer); message 'didDownloadFile:error:options:contextInfo:';
    procedure didReceiveDownloadProgressForFile_downloadedBytes_maxBytes (file_: ICCameraFile; downloadedBytes: off_t; maxBytes: off_t); message 'didReceiveDownloadProgressForFile:downloadedBytes:maxBytes:';
  end;
{$endif}

{$ifdef CLASSES}

type
  ICCameraDevice = objcclass external (ICDevice)
  private
    _cameraProperties: id;
  public
    function batteryLevelAvailable: ObjCBOOL; message 'batteryLevelAvailable';
    function batteryLevel: NSUInteger; message 'batteryLevel';
    function contentCatalogPercentCompleted: NSUInteger; message 'contentCatalogPercentCompleted';
    function contents: NSArray; message 'contents';
    function mediaFiles: NSArray; message 'mediaFiles';
    function timeOffset: NSTimeInterval; message 'timeOffset';
    function isAccessRestrictedAppleDevice: ObjCBOOL; message 'isAccessRestrictedAppleDevice';
    function mountPoint: NSString; message 'mountPoint';
    procedure setTetheredCaptureEnabled(newValue: ObjCBOOL); message 'setTetheredCaptureEnabled:';
    function tetheredCaptureEnabled: ObjCBOOL; message 'tetheredCaptureEnabled';
    function filesOfType (fileUTType: NSString): NSArray; message 'filesOfType:';
    procedure requestSyncClock; message 'requestSyncClock';
    procedure requestEnableTethering; message 'requestEnableTethering';
    procedure requestDisableTethering; message 'requestDisableTethering';
    procedure requestTakePicture; message 'requestTakePicture';
    procedure requestDeleteFiles (files: NSArray); message 'requestDeleteFiles:';
    procedure cancelDelete; message 'cancelDelete';
    procedure requestDownloadFile_options_downloadDelegate_didDownloadSelector_contextInfo (file_: ICCameraFile; options: NSDictionary; downloadDelegate: ICCameraDeviceDownloadDelegateProtocol; selector: SEL; contextInfo: pointer); message 'requestDownloadFile:options:downloadDelegate:didDownloadSelector:contextInfo:';
    procedure cancelDownload; message 'cancelDownload';
    procedure requestUploadFile_options_uploadDelegate_didUploadSelector_contextInfo (fileURL: NSURL; options: NSDictionary; uploadDelegate: id; selector: SEL; contextInfo: pointer); message 'requestUploadFile:options:uploadDelegate:didUploadSelector:contextInfo:';
    procedure requestReadDataFromFile_atOffset_length_readDelegate_didReadDataSelector_contextInfo (file_: ICCameraFile; offset: off_t; length: off_t; readDelegate: id; selector: SEL; contextInfo: pointer); message 'requestReadDataFromFile:atOffset:length:readDelegate:didReadDataSelector:contextInfo:';
    procedure requestSendPTPCommand_outData_sendCommandDelegate_didSendCommandSelector_contextInfo (command: NSData; data: NSData; sendCommandDelegate: id; selector: SEL; contextInfo: pointer); message 'requestSendPTPCommand:outData:sendCommandDelegate:didSendCommandSelector:contextInfo:';
  end;
{$endif}

