{ Parsed from ITunesLibrary.framework ITLibrary.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibraryPtr = ^ITLibrary;
{$endif}

{$ifdef TYPES}
const
  ITLibExportFeatureNone = 0;

type
  ITLibExportFeature = NSUInteger;
  ITLibExportFeaturePtr = ^ITLibExportFeature;
{$endif}

{$ifdef CLASSES}

type
  ITLibrary = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    function applicationVersion: NSString; message 'applicationVersion';
    function features: ITLibExportFeature; message 'features';
    function apiMajorVersion: NSUInteger; message 'apiMajorVersion';
    function apiMinorVersion: NSUInteger; message 'apiMinorVersion';
    function musicFolderLocation: NSURL; message 'musicFolderLocation';
    function shouldShowContentRating: ObjCBOOL; message 'shouldShowContentRating';
    function allMediaItems: NSArray; message 'allMediaItems';
    function allPlaylists: NSArray; message 'allPlaylists';
    class function libraryWithAPIVersion_error (requestedAPIVersion: NSString; error: NSErrorPtr): id; message 'libraryWithAPIVersion:error:';
    function initWithAPIVersion_error (requestedAPIVersion: NSString; error: NSErrorPtr): id; message 'initWithAPIVersion:error:';
    function artworkForMediaFile (mediaFileURL: NSURL): ITLibArtwork; message 'artworkForMediaFile:';
    function reloadData: ObjCBOOL; message 'reloadData';
  end;
{$endif}

