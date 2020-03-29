{ Parsed from AVFoundation.framework AVMetadataItem.h }


{$ifdef TYPES}
type
  AVMetadataItemPtr = ^AVMetadataItem;
  AVMutableMetadataItemPtr = ^AVMutableMetadataItem;
  AVMetadataItemFilterPtr = ^AVMetadataItemFilter;
{$endif}

{$ifdef CLASSES}

type
  AVMetadataItem = objcclass external (NSObject, AVAsynchronousKeyValueLoadingProtocol, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _priv: AVMetadataItemInternal;
  public
    function identifier: NSString; message 'identifier';
    function extendedLanguageTag: NSString; message 'extendedLanguageTag';
    function locale: NSLocale; message 'locale';
    function time: CMTime; message 'time';
    function duration: CMTime; message 'duration';
    function dataType: NSString; message 'dataType';
    function value: id; message 'value';
    function extraAttributes: NSDictionary; message 'extraAttributes';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure loadValuesAsynchronouslyForKeys_completionHandler (keys: NSArray; handler: OpaqueCBlock); message 'loadValuesAsynchronouslyForKeys:completionHandler:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    function statusOfValueForKey_error (key: NSString; outError: NSErrorPtr): AVKeyValueStatus; message 'statusOfValueForKey:error:';
  end;


type
  AVMetadataItemTypeCoercion = objccategory external (AVMetadataItem)
    function stringValue: NSString; message 'stringValue';
    function numberValue: NSNumber; message 'numberValue';
    function dateValue: NSDate; message 'dateValue';
    function dataValue: NSData; message 'dataValue';
  end;

type
  AVAsynchronousKeyValueLoading = objccategory external (AVMetadataItem)
    function statusOfValueForKey_error (key: NSString; outError: NSErrorPtr): AVKeyValueStatus; message 'statusOfValueForKey:error:'; { available in 10_7, 4_2 }
    procedure loadValuesAsynchronouslyForKeys_completionHandler (keys: NSArray; handler: OpaqueCBlock); message 'loadValuesAsynchronouslyForKeys:completionHandler:'; { available in 10_7, 4_2 }
  end;

type
  AVMetadataItemArrayFiltering = objccategory external (AVMetadataItem)
    class function metadataItemsFromArray_filteredAndSortedAccordingToPreferredLanguages (metadataItems: NSArray; preferredLanguages: NSArray): NSArray; message 'metadataItemsFromArray:filteredAndSortedAccordingToPreferredLanguages:'; { available in 10_8, 6_0 }
    class function metadataItemsFromArray_filteredByIdentifier (metadataItems: NSArray; identifier_: NSString): NSArray; message 'metadataItemsFromArray:filteredByIdentifier:'; { available in 10_10, 8_0 }
    class function metadataItemsFromArray_filteredByMetadataItemFilter (metadataItems: NSArray; metadataItemFilter: AVMetadataItemFilter): NSArray; message 'metadataItemsFromArray:filteredByMetadataItemFilter:'; { available in 10_9, 7_0 }
  end;

type
  AVMetadataItemKeyAndKeyspace = objccategory external (AVMetadataItem)
    class function identifierForKey_keySpace (key: id; keySpace: NSString): NSString; message 'identifierForKey:keySpace:'; { available in 10_10, 8_0 }
    class function keySpaceForIdentifier (identifier_: NSString): NSString; message 'keySpaceForIdentifier:'; { available in 10_10, 8_0 }
    class function keyForIdentifier (identifier_: NSString): id; message 'keyForIdentifier:'; { available in 10_10, 8_0 }
    function key: id; message 'key';
    function commonKey: NSString; message 'commonKey';
    function keySpace: NSString; message 'keySpace';
  end;


type
  AVMutableMetadataItem = objcclass external (AVMetadataItem)
  private
    _mutablePriv: AVMutableMetadataItemInternal;
  public
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
    procedure setExtendedLanguageTag(newValue: NSString); message 'setExtendedLanguageTag:';
    function extendedLanguageTag: NSString; message 'extendedLanguageTag';
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setTime(newValue: CMTime); message 'setTime:';
    function time: CMTime; message 'time';
    procedure setDuration(newValue: CMTime); message 'setDuration:';
    function duration: CMTime; message 'duration';
    procedure setDataType(newValue: NSString); message 'setDataType:';
    function dataType: NSString; message 'dataType';
    procedure setValue(newValue: id); message 'setValue:';
    function value: id; message 'value';
    procedure setExtraAttributes(newValue: NSDictionary); message 'setExtraAttributes:';
    function extraAttributes: NSDictionary; message 'extraAttributes';
    class function metadataItem: AVMutableMetadataItem; message 'metadataItem';
  end;


type
  AVMutableMetadataItemKeyAndKeyspace = objccategory external (AVMutableMetadataItem)
    procedure setKeySpace(newValue: NSString); message 'setKeySpace:';
    function keySpace: NSString; message 'keySpace';
    procedure setKey(newValue: id); message 'setKey:';
    function key: id; message 'key';
  end;


type
  AVMetadataItemFilter = objcclass external (NSObject)
  private
    _itemFilterInternal: AVMetadataItemFilterInternal;
  public
    class function metadataItemFilterForSharing: AVMetadataItemFilter; message 'metadataItemFilterForSharing';
  end;


type
  AVMetadataItemArrayFilteringDeprecable = objccategory external (AVMetadataItem)
    class function metadataItemsFromArray_withLocale (metadataItems: NSArray; locale_: NSLocale): NSArray; message 'metadataItemsFromArray:withLocale:';
    class function metadataItemsFromArray_withKey_keySpace (metadataItems: NSArray; key: id; keySpace: NSString): NSArray; message 'metadataItemsFromArray:withKey:keySpace:';
  end;
{$endif}

