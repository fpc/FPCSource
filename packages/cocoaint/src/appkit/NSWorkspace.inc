{ Parsed from AppKit.framework NSWorkspace.h }


{$ifdef TYPES}
type
  NSWorkspacePtr = ^NSWorkspace;
{$endif}

{$ifdef TYPES}
type
  NSWorkspaceLaunchOptions = NSUInteger;
  NSWorkspaceLaunchOptionsPtr = ^NSWorkspaceLaunchOptions;

const
  NSWorkspaceLaunchAndPrint = $00000002;
  NSWorkspaceLaunchWithErrorPresentation = $00000040;
  NSWorkspaceLaunchInhibitingBackgroundOnly = $00000080;
  NSWorkspaceLaunchWithoutAddingToRecents = $00000100;
  NSWorkspaceLaunchWithoutActivation = $00000200;
  NSWorkspaceLaunchAsync = $00010000;
  NSWorkspaceLaunchAllowingClassicStartup = $00020000;
  NSWorkspaceLaunchPreferringClassic = $00040000;
  NSWorkspaceLaunchNewInstance = $00080000;
  NSWorkspaceLaunchAndHide = $00100000;
  NSWorkspaceLaunchAndHideOthers = $00200000;
  NSWorkspaceLaunchDefault = NSWorkspaceLaunchAsync + NSWorkspaceLaunchAllowingClassicStartup;

type
  NSWorkspaceIconCreationOptions = NSUInteger;
  NSWorkspaceIconCreationOptionsPtr = ^NSWorkspaceIconCreationOptions;

const
  NSExcludeQuickDrawElementsIconCreationOption = 1 shl 1;
  NSExclude10_4ElementsIconCreationOption = 1 shl 2;
{$endif}

{$ifdef CLASSES}

type
  NSWorkspace = objcclass external (NSObject)
  private
    _notificationCenter: NSNotificationCenter;
    {$if  not defined(__LP64__)}
    deviceStatusCount: cint;
    applicationStatusCount: cint;
    _reservedWorkspace1: id;
    {$endif}
  public
    class function sharedWorkspace: NSWorkspace; message 'sharedWorkspace';
    function notificationCenter: NSNotificationCenter; message 'notificationCenter';
    function openFile (fullPath: NSString): ObjCBOOL; message 'openFile:';
    function openFile_withApplication (fullPath: NSString; appName: NSString): ObjCBOOL; message 'openFile:withApplication:';
    function openFile_withApplication_andDeactivate (fullPath: NSString; appName: NSString; flag: ObjCBOOL): ObjCBOOL; message 'openFile:withApplication:andDeactivate:';
    function openFile_fromImage_at_inView (fullPath: NSString; anImage: NSImage; point: NSPoint; aView: NSView): ObjCBOOL; message 'openFile:fromImage:at:inView:';
    function openURL (url: NSURL): ObjCBOOL; message 'openURL:';
    function launchApplication (appName: NSString): ObjCBOOL; message 'launchApplication:';
    function launchApplicationAtURL_options_configuration_error (url: NSURL; options: NSWorkspaceLaunchOptions; configuration: NSDictionary; error: NSErrorPtr): NSRunningApplication; message 'launchApplicationAtURL:options:configuration:error:'; { available in 10_6 }
    function openURL_options_configuration_error (url: NSURL; options: NSWorkspaceLaunchOptions; configuration: NSDictionary; error: NSErrorPtr): NSRunningApplication; message 'openURL:options:configuration:error:'; { available in 10_10 }
    function openURLs_withApplicationAtURL_options_configuration_error (urls: NSArray; applicationURL: NSURL; options: NSWorkspaceLaunchOptions; configuration: NSDictionary; error: NSErrorPtr): NSRunningApplication; message 'openURLs:withApplicationAtURL:options:configuration:error:'; { available in 10_10 }
    function launchApplication_showIcon_autolaunch (appName: NSString; showIcon: ObjCBOOL; autolaunch: ObjCBOOL): ObjCBOOL; message 'launchApplication:showIcon:autolaunch:';
    function fullPathForApplication (appName: NSString): NSString; message 'fullPathForApplication:';
    function selectFile_inFileViewerRootedAtPath (fullPath: NSString; rootFullPath: NSString): ObjCBOOL; message 'selectFile:inFileViewerRootedAtPath:';
    procedure activateFileViewerSelectingURLs (fileURLs: NSArray); message 'activateFileViewerSelectingURLs:'; { available in 10_6 }
    function showSearchResultsForQueryString (queryString: NSString): ObjCBOOL; message 'showSearchResultsForQueryString:'; { available in 10_6 }
    procedure noteFileSystemChanged (path: NSString); message 'noteFileSystemChanged:';
    function getInfoForFile_application_type (fullPath: NSString; appName: NSStringPtr; type_: NSStringPtr): ObjCBOOL; message 'getInfoForFile:application:type:';
    function isFilePackageAtPath (fullPath: NSString): ObjCBOOL; message 'isFilePackageAtPath:';
    function iconForFile (fullPath: NSString): NSImage; message 'iconForFile:';
    function iconForFiles (fullPaths: NSArray): NSImage; message 'iconForFiles:';
    function iconForFileType (fileType: NSString): NSImage; message 'iconForFileType:';
    function setIcon_forFile_options (image: NSImage; fullPath: NSString; options: NSWorkspaceIconCreationOptions): ObjCBOOL; message 'setIcon:forFile:options:';
    function fileLabels: NSArray; message 'fileLabels';
    function fileLabelColors: NSArray; message 'fileLabelColors';
    procedure recycleURLs_completionHandler (URLs: NSArray; handler: OpaqueCBlock); message 'recycleURLs:completionHandler:'; { available in 10_6 }
    procedure duplicateURLs_completionHandler (URLs: NSArray; handler: OpaqueCBlock); message 'duplicateURLs:completionHandler:'; { available in 10_6 }
    function getFileSystemInfoForPath_isRemovable_isWritable_isUnmountable_description_type (fullPath: NSString; removableFlag: pObjCBOOL; writableFlag: pObjCBOOL; unmountableFlag: pObjCBOOL; description: NSStringPtr; fileSystemType: NSStringPtr): ObjCBOOL; message 'getFileSystemInfoForPath:isRemovable:isWritable:isUnmountable:description:type:';
    function performFileOperation_source_destination_files_tag (operation: NSString; source: NSString; destination: NSString; files: NSArray; tag: NSIntegerPtr): ObjCBOOL; message 'performFileOperation:source:destination:files:tag:';
    function unmountAndEjectDeviceAtPath (path: NSString): ObjCBOOL; message 'unmountAndEjectDeviceAtPath:';
    function unmountAndEjectDeviceAtURL_error (url: NSURL; error: NSErrorPtr): ObjCBOOL; message 'unmountAndEjectDeviceAtURL:error:'; { available in 10_6 }
    function extendPowerOffBy (requested: NSInteger): NSInteger; message 'extendPowerOffBy:';
    procedure hideOtherApplications; message 'hideOtherApplications';
    function URLForApplicationWithBundleIdentifier (bundleIdentifier: NSString): NSURL; message 'URLForApplicationWithBundleIdentifier:'; { available in 10_6 }
    function URLForApplicationToOpenURL (url: NSURL): NSURL; message 'URLForApplicationToOpenURL:'; { available in 10_6 }
    function absolutePathForAppBundleWithIdentifier (bundleIdentifier: NSString): NSString; message 'absolutePathForAppBundleWithIdentifier:';
    function launchAppWithBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifier (bundleIdentifier: NSString; options: NSWorkspaceLaunchOptions; descriptor: NSAppleEventDescriptor; identifier: NSNumberPtr): ObjCBOOL; message 'launchAppWithBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifier:';
    function openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers (urls: NSArray; bundleIdentifier: NSString; options: NSWorkspaceLaunchOptions; descriptor: NSAppleEventDescriptor; identifiers: NSArrayPtr): ObjCBOOL; message 'openURLs:withAppBundleIdentifier:options:additionalEventParamDescriptor:launchIdentifiers:';
    function frontmostApplication: NSRunningApplication; message 'frontmostApplication';
    function menuBarOwningApplication: NSRunningApplication; message 'menuBarOwningApplication';
    function typeOfFile_error (absoluteFilePath: NSString; outError: NSErrorPtr): NSString; message 'typeOfFile:error:'; { available in 10_5 }
    function localizedDescriptionForType (typeName: NSString): NSString; message 'localizedDescriptionForType:'; { available in 10_5 }
    function preferredFilenameExtensionForType (typeName: NSString): NSString; message 'preferredFilenameExtensionForType:'; { available in 10_5 }
    function filenameExtension_isValidForType (filenameExtension: NSString; typeName: NSString): ObjCBOOL; message 'filenameExtension:isValidForType:'; { available in 10_5 }
    function type_conformsToType (firstTypeName: NSString; secondTypeName: NSString): ObjCBOOL; message 'type:conformsToType:'; { available in 10_5 }
  end;


type
  NSDesktopImages = objccategory external (NSWorkspace)
    function setDesktopImageURL_forScreen_options_error (url: NSURL; screen: NSScreen; options: NSDictionary; error: NSErrorPtr): ObjCBOOL; message 'setDesktopImageURL:forScreen:options:error:'; { available in 10_6 }
    function desktopImageURLForScreen (screen: NSScreen): NSURL; message 'desktopImageURLForScreen:'; { available in 10_6 }
    function desktopImageOptionsForScreen (screen: NSScreen): NSDictionary; message 'desktopImageOptionsForScreen:'; { available in 10_6 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSWorkspaceDesktopImageScalingKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceDesktopImageAllowClippingKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceDesktopImageFillColorKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceApplicationKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceWillLaunchApplicationNotification: NSString; cvar; external;
  NSWorkspaceDidLaunchApplicationNotification: NSString; cvar; external;
  NSWorkspaceDidTerminateApplicationNotification: NSString; cvar; external;
  NSWorkspaceDidHideApplicationNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceDidUnhideApplicationNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceDidActivateApplicationNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceDidDeactivateApplicationNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceVolumeLocalizedNameKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceVolumeURLKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceVolumeOldLocalizedNameKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceVolumeOldURLKey: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceDidMountNotification: NSString; cvar; external;
  NSWorkspaceDidUnmountNotification: NSString; cvar; external;
  NSWorkspaceWillUnmountNotification: NSString; cvar; external;
  NSWorkspaceDidRenameVolumeNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceWillPowerOffNotification: NSString; cvar; external;
  NSWorkspaceWillSleepNotification: NSString; cvar; external;
  NSWorkspaceDidWakeNotification: NSString; cvar; external;
  NSWorkspaceScreensDidSleepNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceScreensDidWakeNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceSessionDidBecomeActiveNotification: NSString; cvar; external;
  NSWorkspaceSessionDidResignActiveNotification: NSString; cvar; external;
  NSWorkspaceDidPerformFileOperationNotification: NSString; cvar; external;
  NSWorkspaceDidChangeFileLabelsNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceActiveSpaceDidChangeNotification: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceLaunchConfigurationAppleEvent: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceLaunchConfigurationArguments: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceLaunchConfigurationEnvironment: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceLaunchConfigurationArchitecture: NSString { available in 10_6 }; cvar; external;
  NSWorkspaceMoveOperation: NSString; cvar; external;
  NSWorkspaceCopyOperation: NSString; cvar; external;
  NSWorkspaceLinkOperation: NSString; cvar; external;
  NSWorkspaceCompressOperation: NSString; cvar; external;
  NSWorkspaceDecompressOperation: NSString; cvar; external;
  NSWorkspaceEncryptOperation: NSString; cvar; external;
  NSWorkspaceDecryptOperation: NSString; cvar; external;
  NSWorkspaceDestroyOperation: NSString; cvar; external;
  NSWorkspaceRecycleOperation: NSString; cvar; external;
  NSWorkspaceDuplicateOperation: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSWorkspace_NSDeprecated = objccategory external name 'NSDeprecated' (NSWorkspace)
    function openTempFile (fullPath: NSString): ObjCBOOL; message 'openTempFile:'; deprecated 'in 10_0, 10_6';
    procedure findApplications; message 'findApplications'; deprecated 'in 10_0, 10_6';
    procedure noteUserDefaultsChanged; message 'noteUserDefaultsChanged'; deprecated 'in 10_0, 10_6';
    procedure slideImage_from_to (image: NSImage; fromPoint: NSPoint; toPoint: NSPoint); message 'slideImage:from:to:'; deprecated 'in 10_0, 10_6';
    procedure checkForRemovableMedia; message 'checkForRemovableMedia'; deprecated 'in 10_0, 10_6';
    procedure noteFileSystemChanged_; message 'noteFileSystemChanged'; deprecated 'in 10_0, 10_6';
    function fileSystemChanged: ObjCBOOL; message 'fileSystemChanged'; deprecated 'in 10_0, 10_6';
    function userDefaultsChanged: ObjCBOOL; message 'userDefaultsChanged'; deprecated 'in 10_0, 10_6';
    function mountNewRemovableMedia: NSArray; message 'mountNewRemovableMedia'; deprecated 'in 10_0, 10_6';
    function activeApplication: NSDictionary; message 'activeApplication';
    function mountedLocalVolumePaths: NSArray; message 'mountedLocalVolumePaths';
    function mountedRemovableMedia: NSArray; message 'mountedRemovableMedia';
    function launchedApplications: NSArray; message 'launchedApplications'; deprecated 'in 10_0, 10_7';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPlainFileType: NSString deprecated 'in 10_0, 10_6'; cvar; external;
  NSDirectoryFileType: NSString deprecated 'in 10_0, 10_6'; cvar; external;
  NSApplicationFileType: NSString deprecated 'in 10_0, 10_6'; cvar; external;
  NSFilesystemFileType: NSString deprecated 'in 10_0, 10_6'; cvar; external;
  NSShellCommandFileType: NSString deprecated 'in 10_0, 10_6'; cvar; external;
{$endif}

