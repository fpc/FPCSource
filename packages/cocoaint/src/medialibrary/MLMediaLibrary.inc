{ Parsed from MediaLibrary.framework MLMediaLibrary.h }


{$ifdef TYPES}
type
  MLMediaLibraryPtr = ^MLMediaLibrary;
{$endif}

{$ifdef CLASSES}

type
  MLMediaLibrary = objcclass external (NSObject)
  public
    function initWithOptions (options: NSDictionary): instancetype; message 'initWithOptions:'; { NS_DESIGNATED_INITIALIZER }
    function mediaSources: NSDictionary; message 'mediaSources';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  MLMediaSourcePhotosIdentifier: NSString; cvar; external;
  MLMediaSourceiPhotoIdentifier: NSString; cvar; external;
  MLMediaSourceiTunesIdentifier: NSString; cvar; external;
  MLMediaSourceApertureIdentifier: NSString; cvar; external;
  MLMediaSourceiMovieIdentifier: NSString; cvar; external;
  MLMediaSourceFinalCutIdentifier: NSString; cvar; external;
  MLMediaSourceGarageBandIdentifier: NSString; cvar; external;
  MLMediaSourceLogicIdentifier: NSString; cvar; external;
  MLMediaSourcePhotoBoothIdentifier: NSString; cvar; external;
  MLMediaSourceCustomFoldersIdentifier: NSString; cvar; external;
  MLMediaSourceMoviesFolderIdentifier: NSString; cvar; external;
  MLMediaSourceAppDefinedFoldersIdentifier: NSString; cvar; external;
  MLMediaLoadSourceTypesKey: NSString; cvar; external;
  MLMediaLoadIncludeSourcesKey: NSString; cvar; external;
  MLMediaLoadExcludeSourcesKey: NSString; cvar; external;
  MLMediaLoadFoldersKey: NSString; cvar; external;
  MLMediaLoadAppleLoops: NSString; cvar; external;
  MLMediaLoadMoviesFolder: NSString; cvar; external;
  MLMediaLoadAppFoldersKey: NSString; cvar; external;
{$endif}

