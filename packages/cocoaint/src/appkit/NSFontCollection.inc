{ Parsed from AppKit.framework NSFontCollection.h }


{$ifdef TYPES}
type
  NSFontCollectionPtr = ^NSFontCollection;
  NSMutableFontCollectionPtr = ^NSMutableFontCollection;
{$endif}

{$ifdef TYPES}
type
  NSFontCollectionVisibility = NSUInteger;
  NSFontCollectionVisibilityPtr = ^NSFontCollectionVisibility;

const
  NSFontCollectionVisibilityProcess = 1 shl 0;
  NSFontCollectionVisibilityUser = 1 shl 1;
  NSFontCollectionVisibilityComputer = 1 shl 2;
{$endif}

{$ifdef CLASSES}

type
  NSFontCollection = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol)
  public
    class function fontCollectionWithDescriptors (queryDescriptors: NSArray): NSFontCollection; message 'fontCollectionWithDescriptors:';
    class function fontCollectionWithAllAvailableDescriptors: NSFontCollection; message 'fontCollectionWithAllAvailableDescriptors';
    class function fontCollectionWithLocale (locale: NSLocale): NSFontCollection; message 'fontCollectionWithLocale:';
    class function showFontCollection_withName_visibility_error (collection: NSFontCollection; name: NSString; visibility: NSFontCollectionVisibility; error: NSErrorPtr): ObjCBOOL; message 'showFontCollection:withName:visibility:error:';
    class function hideFontCollectionWithName_visibility_error (name: NSString; visibility: NSFontCollectionVisibility; error: NSErrorPtr): ObjCBOOL; message 'hideFontCollectionWithName:visibility:error:';
    class function renameFontCollectionWithName_visibility_toName_error (name: NSString; visibility: NSFontCollectionVisibility; name3: NSString; error: NSErrorPtr): ObjCBOOL; message 'renameFontCollectionWithName:visibility:toName:error:';
    class function allFontCollectionNames: NSArray; message 'allFontCollectionNames';
    class function fontCollectionWithName (name: NSString): NSFontCollection; message 'fontCollectionWithName:';
    class function fontCollectionWithName_visibility (name: NSString; visibility: NSFontCollectionVisibility): NSFontCollection; message 'fontCollectionWithName:visibility:';
    function queryDescriptors: NSArray; message 'queryDescriptors';
    function exclusionDescriptors: NSArray; message 'exclusionDescriptors';
    function matchingDescriptors: NSArray; message 'matchingDescriptors';
    function matchingDescriptorsWithOptions (options: NSDictionary): NSArray; message 'matchingDescriptorsWithOptions:';
    function matchingDescriptorsForFamily (family: NSString): NSArray; message 'matchingDescriptorsForFamily:';
    function matchingDescriptorsForFamily_options (family: NSString; options: NSDictionary): NSArray; message 'matchingDescriptorsForFamily:options:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  NSMutableFontCollection = objcclass external (NSFontCollection)
  public
    class function fontCollectionWithDescriptors (queryDescriptors_: NSArray): NSMutableFontCollection; message 'fontCollectionWithDescriptors:';
    class function fontCollectionWithAllAvailableDescriptors: NSMutableFontCollection; message 'fontCollectionWithAllAvailableDescriptors';
    class function fontCollectionWithLocale (locale: NSLocale): NSMutableFontCollection; message 'fontCollectionWithLocale:';
    class function fontCollectionWithName (name: NSString): NSMutableFontCollection; message 'fontCollectionWithName:';
    class function fontCollectionWithName_visibility (name: NSString; visibility: NSFontCollectionVisibility): NSMutableFontCollection; message 'fontCollectionWithName:visibility:';
    procedure setQueryDescriptors(newValue: NSArray); message 'setQueryDescriptors:';
    function queryDescriptors: NSArray; message 'queryDescriptors';
    procedure setExclusionDescriptors(newValue: NSArray); message 'setExclusionDescriptors:';
    function exclusionDescriptors: NSArray; message 'exclusionDescriptors';
    procedure addQueryForDescriptors (descriptors: NSArray); message 'addQueryForDescriptors:';
    procedure removeQueryForDescriptors (descriptors: NSArray); message 'removeQueryForDescriptors:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSFontCollectionIncludeDisabledFontsOption: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionRemoveDuplicatesOption: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionDisallowAutoActivationOption: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionDidChangeNotification: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionActionKey: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionNameKey: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionOldNameKey: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionVisibilityKey: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionWasShown: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionWasHidden: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionWasRenamed: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionAllFonts: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionUser: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionFavorites: NSString { available in 10_7 }; cvar; external;
  NSFontCollectionRecentlyUsed: NSString { available in 10_7 }; cvar; external;
{$endif}

