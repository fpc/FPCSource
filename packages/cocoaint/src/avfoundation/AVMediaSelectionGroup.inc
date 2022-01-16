{ Parsed from AVFoundation.framework AVMediaSelectionGroup.h }


{$ifdef TYPES}
type
  AVMediaSelectionGroupPtr = ^AVMediaSelectionGroup;
  AVMediaSelectionOptionPtr = ^AVMediaSelectionOption;
{$endif}

{$ifdef CLASSES}

type
  AVMediaSelectionGroup = objcclass external (NSObject, NSCopyingProtocol)
  private
    _mediaSelectionGroup: AVMediaSelectionGroupInternal;
  public
    function options: NSArray; message 'options';
    function defaultOption: AVMediaSelectionOption; message 'defaultOption';
    function allowsEmptySelection: ObjCBOOL; message 'allowsEmptySelection';
    function mediaSelectionOptionWithPropertyList (plist: id): AVMediaSelectionOption; message 'mediaSelectionOptionWithPropertyList:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  AVMediaSelectionOptionFiltering = objccategory external (AVMediaSelectionGroup)
    class function playableMediaSelectionOptionsFromArray (mediaSelectionOptions: NSArray): NSArray; message 'playableMediaSelectionOptionsFromArray:';
    class function mediaSelectionOptionsFromArray_filteredAndSortedAccordingToPreferredLanguages (mediaSelectionOptions: NSArray; preferredLanguages: NSArray): NSArray; message 'mediaSelectionOptionsFromArray:filteredAndSortedAccordingToPreferredLanguages:'; { available in 10_8, 6_0 }
    class function mediaSelectionOptionsFromArray_withLocale (mediaSelectionOptions: NSArray; locale: NSLocale): NSArray; message 'mediaSelectionOptionsFromArray:withLocale:';
    class function mediaSelectionOptionsFromArray_withMediaCharacteristics (mediaSelectionOptions: NSArray; mediaCharacteristics: NSArray): NSArray; message 'mediaSelectionOptionsFromArray:withMediaCharacteristics:';
    class function mediaSelectionOptionsFromArray_withoutMediaCharacteristics (mediaSelectionOptions: NSArray; mediaCharacteristics: NSArray): NSArray; message 'mediaSelectionOptionsFromArray:withoutMediaCharacteristics:';
  end;


type
  AVMediaSelectionOption = objcclass external (NSObject, NSCopyingProtocol)
  private
    _mediaSelectionOption: AVMediaSelectionOptionInternal;
  public
    function mediaType: NSString; message 'mediaType';
    function mediaSubTypes: NSArray; message 'mediaSubTypes';
    function hasMediaCharacteristic (mediaCharacteristic: NSString): ObjCBOOL; message 'hasMediaCharacteristic:';
    function isPlayable: ObjCBOOL; message 'isPlayable';
    function extendedLanguageTag: NSString; message 'extendedLanguageTag';
    function locale: NSLocale; message 'locale';
    function commonMetadata: NSArray; message 'commonMetadata';
    function availableMetadataFormats: NSArray; message 'availableMetadataFormats';
    function metadataForFormat (format: NSString): NSArray; message 'metadataForFormat:';
    function associatedMediaSelectionOptionInMediaSelectionGroup (mediaSelectionGroup: AVMediaSelectionGroup): AVMediaSelectionOption; message 'associatedMediaSelectionOptionInMediaSelectionGroup:';
    function propertyList: id; message 'propertyList';
    function displayNameWithLocale (locale_: NSLocale): NSString; message 'displayNameWithLocale:'; { available in 10_9, 7_0 }
    function displayName: NSString; message 'displayName';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

