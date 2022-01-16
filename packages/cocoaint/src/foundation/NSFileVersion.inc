{ Parsed from Foundation.framework NSFileVersion.h }


{$ifdef TYPES}
type
  NSFileVersionPtr = ^NSFileVersion;
{$endif}

{$ifdef TYPES}
type
  NSFileVersionAddingOptions = NSUInteger;
  NSFileVersionAddingOptionsPtr = ^NSFileVersionAddingOptions;

const
  NSFileVersionAddingByMoving = 1 shl 0;

type
  NSFileVersionReplacingOptions = NSUInteger;
  NSFileVersionReplacingOptionsPtr = ^NSFileVersionReplacingOptions;

const
  NSFileVersionReplacingByMoving = 1 shl 0;
{$endif}

{$ifdef CLASSES}

type
  NSFileVersion = objcclass external (NSObject)
  private
    _fileURL: NSURL;
    _addition: id;
    _deadVersionIdentifier: id;
    _nonLocalVersion: id;
    _contentsURL: NSURL;
    _isBackup: ObjCBOOL;
    _localizedName: NSString;
    _localizedComputerName: NSString;
    _modificationDate: NSDate;
    _isResolved: ObjCBOOL;
    _contentsURLIsAccessed: ObjCBOOL;
    _clientID: NSString;
    _name: NSString;
  public
    class function currentVersionOfItemAtURL (url: NSURL): NSFileVersion; message 'currentVersionOfItemAtURL:';
    class function otherVersionsOfItemAtURL (url: NSURL): NSArray; message 'otherVersionsOfItemAtURL:';
    class function unresolvedConflictVersionsOfItemAtURL (url: NSURL): NSArray; message 'unresolvedConflictVersionsOfItemAtURL:';
    class procedure getNonlocalVersionsOfItemAtURL_completionHandler (url: NSURL; completionHandler: OpaqueCBlock); message 'getNonlocalVersionsOfItemAtURL:completionHandler:'; { available in 10_10, 8_0 }
    class function versionOfItemAtURL_forPersistentIdentifier (url: NSURL; persistentIdentifier: id): NSFileVersion; message 'versionOfItemAtURL:forPersistentIdentifier:';
    class function addVersionOfItemAtURL_withContentsOfURL_options_error (url: NSURL; contentsURL: NSURL; options: NSFileVersionAddingOptions; outError: NSErrorPtr): NSFileVersion; message 'addVersionOfItemAtURL:withContentsOfURL:options:error:'; { available in 10_7 }
    class function temporaryDirectoryURLForNewVersionOfItemAtURL (url: NSURL): NSURL; message 'temporaryDirectoryURLForNewVersionOfItemAtURL:'; { available in 10_7 }
    function URL: NSURL; message 'URL';
    function localizedName: NSString; message 'localizedName';
    function localizedNameOfSavingComputer: NSString; message 'localizedNameOfSavingComputer';
    function modificationDate: NSDate; message 'modificationDate';
    function persistentIdentifier: NSCodingProtocol; message 'persistentIdentifier';
    function isConflict: ObjCBOOL; message 'isConflict';
    procedure setResolved(newValue: ObjCBOOL); message 'setResolved:';
    function isResolved: ObjCBOOL; message 'isResolved';
    procedure setDiscardable(newValue: ObjCBOOL); message 'setDiscardable:';
    function isDiscardable: ObjCBOOL; message 'isDiscardable';
    function hasLocalContents: ObjCBOOL; message 'hasLocalContents';
    function hasThumbnail: ObjCBOOL; message 'hasThumbnail';
    function replaceItemAtURL_options_error (url_: NSURL; options: NSFileVersionReplacingOptions; error: NSErrorPtr): NSURL; message 'replaceItemAtURL:options:error:';
    function removeAndReturnError (outError: NSErrorPtr): ObjCBOOL; message 'removeAndReturnError:';
    class function removeOtherVersionsOfItemAtURL_error (url_: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'removeOtherVersionsOfItemAtURL:error:';
  end;
{$endif}

