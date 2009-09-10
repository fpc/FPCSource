{ Parsed from Appkit.framework NSWorkspace.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSWORKSPACE_PAS_H}
{$define NSWORKSPACE_PAS_H}
type
  NSWorkspacePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSWORKSPACE_PAS_T}
{$define NSWORKSPACE_PAS_T}

{ Types }
type
  NSWorkspaceLaunchOptions = culong;
  NSWorkspaceIconCreationOptions = culong;

{ Constants }

const
  NSWorkspaceLaunchAndPrint = $00000002;
  NSWorkspaceLaunchInhibitingBackgroundOnly = $00000080;
  NSWorkspaceLaunchWithoutAddingToRecents = $00000100;
  NSWorkspaceLaunchWithoutActivation = $00000200;
  NSWorkspaceLaunchAsync = $00010000;
  NSWorkspaceLaunchAllowingClassicStartup = $00020000;
  NSWorkspaceLaunchPreferringClassic = $00040000;
  NSWorkspaceLaunchNewInstance = $00080000;
  NSWorkspaceLaunchAndHide = $00100000;
  NSWorkspaceLaunchAndHideOthers = $00200000;

const
  NSExcludeQuickDrawElementsIconCreationOption = 1  shl  1;
  NSExclude10_4ElementsIconCreationOption = 1  shl  2;

{ CFString constants }
var
  NSWorkspaceDidLaunchApplicationNotification: CFStringRef; external name '_NSWorkspaceDidLaunchApplicationNotification';
  NSWorkspaceDidMountNotification: CFStringRef; external name '_NSWorkspaceDidMountNotification';
  NSWorkspaceDidPerformFileOperationNotification: CFStringRef; external name '_NSWorkspaceDidPerformFileOperationNotification';
  NSWorkspaceDidTerminateApplicationNotification: CFStringRef; external name '_NSWorkspaceDidTerminateApplicationNotification';
  NSWorkspaceDidUnmountNotification: CFStringRef; external name '_NSWorkspaceDidUnmountNotification';
  NSWorkspaceWillLaunchApplicationNotification: CFStringRef; external name '_NSWorkspaceWillLaunchApplicationNotification';
  NSWorkspaceWillPowerOffNotification: CFStringRef; external name '_NSWorkspaceWillPowerOffNotification';
  NSWorkspaceWillUnmountNotification: CFStringRef; external name '_NSWorkspaceWillUnmountNotification';
  NSWorkspaceMoveOperation: CFStringRef; external name '_NSWorkspaceMoveOperation';
  NSWorkspaceCopyOperation: CFStringRef; external name '_NSWorkspaceCopyOperation';
  NSWorkspaceLinkOperation: CFStringRef; external name '_NSWorkspaceLinkOperation';
  NSWorkspaceCompressOperation: CFStringRef; external name '_NSWorkspaceCompressOperation';
  NSWorkspaceDecompressOperation: CFStringRef; external name '_NSWorkspaceDecompressOperation';
  NSWorkspaceEncryptOperation: CFStringRef; external name '_NSWorkspaceEncryptOperation';
  NSWorkspaceDecryptOperation: CFStringRef; external name '_NSWorkspaceDecryptOperation';
  NSWorkspaceDestroyOperation: CFStringRef; external name '_NSWorkspaceDestroyOperation';
  NSWorkspaceRecycleOperation: CFStringRef; external name '_NSWorkspaceRecycleOperation';
  NSWorkspaceDuplicateOperation: CFStringRef; external name '_NSWorkspaceDuplicateOperation';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSWORKSPACE_PAS_R}
{$define NSWORKSPACE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSWORKSPACE_PAS_F}
{$define NSWORKSPACE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSWORKSPACE_PAS_T}
{$define NSWORKSPACE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSWorkspace = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSWORKSPACE_PAS_C}
{$define NSWORKSPACE_PAS_C}

{ NSWorkspace }
  NSWorkspace = objcclass(NSObject)
  private
    _notificationCenter: NSNotificationCenter;
    {$ifndef cpu64}
    _deviceStatusCount: cint;
    _applicationStatusCount: cint;
    {$endif}
    __reservedWorkspace1: Pointer;
    
  public
    class function alloc: NSWorkspace; message 'alloc';

    class function sharedWorkspace: NSWorkspace; message 'sharedWorkspace';
    function notificationCenter: NSNotificationCenter; message 'notificationCenter';
    function openFile(fullPath: NSString): Boolean; message 'openFile:';
    function openFile_withApplication(fullPath: NSString; appName: NSString): Boolean; message 'openFile:withApplication:';
    function openFile_withApplication_andDeactivate(fullPath: NSString; appName: NSString; flag: Boolean): Boolean; message 'openFile:withApplication:andDeactivate:';
    function openTempFile(fullPath: NSString): Boolean; message 'openTempFile:';
    function openFile_fromImage_at_inView(fullPath: NSString; anImage: NSImage; point: NSPoint; aView: NSView): Boolean; message 'openFile:fromImage:at:inView:';
    function openURL(url: NSURL): Boolean; message 'openURL:';
    function launchApplication(appName: NSString): Boolean; message 'launchApplication:';
    function launchApplication_showIcon_autolaunch(appName: NSString; showIcon: Boolean; autolaunch: Boolean): Boolean; message 'launchApplication:showIcon:autolaunch:';
    function fullPathForApplication(appName: NSString): NSString; message 'fullPathForApplication:';
    function selectFile_inFileViewerRootedAtPath(fullPath: NSString; rootFullpath: NSString): Boolean; message 'selectFile:inFileViewerRootedAtPath:';
    procedure findApplications; message 'findApplications';
    procedure noteFileSystemChanged; message 'noteFileSystemChanged';
    function fileSystemChanged: Boolean; message 'fileSystemChanged';
    procedure noteUserDefaultsChanged; message 'noteUserDefaultsChanged';
    function userDefaultsChanged: Boolean; message 'userDefaultsChanged';
    function getInfoForFile_application_type(fullPath: NSString; var appName: NSString; var type_: NSString): Boolean; message 'getInfoForFile:application:type:';
    function isFilePackageAtPath(fullPath: NSString): Boolean; message 'isFilePackageAtPath:';
    function iconForFile(fullPath: NSString): NSImage; message 'iconForFile:';
    function iconForFiles(fullPaths: NSArray): NSImage; message 'iconForFiles:';
    function iconForFileType(fileType: NSString): NSImage; message 'iconForFileType:';
    function setIcon_forFile_options(image: NSImage; fullPath: NSString; options: NSWorkspaceIconCreationOptions): Boolean; message 'setIcon:forFile:options:';
    function getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type(fullPath: NSString; var removableFlag: Boolean; var writableFlag: Boolean; var unmountableFlag: Boolean; var description_: NSString; var fileSystemType: NSString): Boolean; message 'getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:';
    function performFileOperation_source_destination_files_tag(operation: NSString; source: NSString; destination: NSString; files: NSArray; var tag: clong): Boolean; message 'performFileOperation:source:destination:files:tag:';
    function unmountAndEjectDeviceAtPath(path: NSString): Boolean; message 'unmountAndEjectDeviceAtPath:';
    function extendPowerOffBy(requested: clong): clong; message 'extendPowerOffBy:';
    procedure slideImage_from_to(image: NSImage; fromPoint: NSPoint; toPoint: NSPoint); message 'slideImage:from:to:';
    procedure hideOtherApplications; message 'hideOtherApplications';
    function mountedLocalVolumePaths: NSArray; message 'mountedLocalVolumePaths';
    function mountedRemovableMedia: NSArray; message 'mountedRemovableMedia';
    function mountNewRemovableMedia: NSArray; message 'mountNewRemovableMedia';
    procedure checkForRemovableMedia; message 'checkForRemovableMedia';
    function absolutePathForAppBundleWithIdentifier(bundleIdentifier: NSString): NSString; message 'absolutePathForAppBundleWithIdentifier:';
    function launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier(bundleIdentifier: NSString; options: NSWorkspaceLaunchOptions; descriptor: NSAppleEventDescriptor; var identifier: NSNumber): Boolean; message 'launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:';
    function openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers(urls: NSArray; bundleIdentifier: NSString; options: NSWorkspaceLaunchOptions; descriptor: NSAppleEventDescriptor; var identifiers: NSArray): Boolean; message 'openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:';
    function launchedApplications: NSArray; message 'launchedApplications';
    function activeApplication: NSDictionary; message 'activeApplication';
    function typeOfFile_error(absoluteFilePath: NSString; var outError: NSError): NSString; message 'typeOfFile:error:';
    function localizedDescriptionForType(typeName: NSString): NSString; message 'localizedDescriptionForType:';
    function preferredFilenameExtensionForType(typeName: NSString): NSString; message 'preferredFilenameExtensionForType:';
    function filenameExtension_isValidForType(filenameExtension: NSString; typeName: NSString): Boolean; message 'filenameExtension:isValidForType:';
    function type_conformsToType(firstTypeName: NSString; secondTypeName: NSString): Boolean; message 'type:conformsToType:';
  end; external;

{$endif}
{$endif}
