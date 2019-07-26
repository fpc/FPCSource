{ Parsed from ImageCaptureCore.framework ICCameraItem.h }


{$ifdef TYPES}
type
  ICCameraItemPtr = ^ICCameraItem;
  ICCameraFolderPtr = ^ICCameraFolder;
  ICCameraFilePtr = ^ICCameraFile;
{$endif}

{$ifdef CLASSES}

type
  ICCameraItem = objcclass external (NSObject)
  private
    _itemProperties: id;
  public
    function device: ICCameraDevice; message 'device';
    function parentFolder: ICCameraFolder; message 'parentFolder';
    function name: NSString; message 'name';
    function UTI: NSString; message 'UTI';
    function fileSystemPath: NSString; message 'fileSystemPath';
    function isLocked: ObjCBOOL; message 'isLocked';
    function isRaw: ObjCBOOL; message 'isRaw';
    function isInTemporaryStore: ObjCBOOL; message 'isInTemporaryStore';
    function creationDate: NSDate; message 'creationDate';
    function modificationDate: NSDate; message 'modificationDate';
    function thumbnailIfAvailable: CGImageRef; message 'thumbnailIfAvailable';
    function largeThumbnailIfAvailable: CGImageRef; message 'largeThumbnailIfAvailable';
    function metadataIfAvailable: NSDictionary; message 'metadataIfAvailable';
    function userData: NSMutableDictionary; message 'userData';
    function ptpObjectHandle: cuint; message 'ptpObjectHandle';
    function wasAddedAfterContentCatalogCompleted: ObjCBOOL; message 'wasAddedAfterContentCatalogCompleted';
  end;

type
  ICCameraFolder = objcclass external (ICCameraItem)
  private
    _folderProperties: id;
  public
    function contents: NSArray; message 'contents';
  end;

type
  ICCameraFile = objcclass external (ICCameraItem)
  private
    _fileProperties: id;
  public
    function fileSize: off_t; message 'fileSize';
    procedure setOrientation(newValue: ICEXIFOrientationType); message 'setOrientation:';
    function orientation: ICEXIFOrientationType; message 'orientation';
    function duration: double; message 'duration';
    function sidecarFiles: NSArray; message 'sidecarFiles';
  end;
{$endif}

