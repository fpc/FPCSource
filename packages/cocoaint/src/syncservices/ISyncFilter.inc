{ Parsed from SyncServices.framework ISyncFilter.h }


{$ifdef TYPES}
type
  ISyncFilterPtr = ^ISyncFilter;
  ISyncFilteringProtocolPtr = ^ISyncFilteringProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  ISyncFilteringProtocol = objcprotocol external name 'ISyncFiltering' (NSCodingProtocol)
    function isEqual (anotherFilter: id): ObjCBOOL; message 'isEqual:'; deprecated 'in 10_4, 10_7';
    function supportedEntityNames: NSArray; message 'supportedEntityNames'; deprecated 'in 10_4, 10_7';
    function shouldApplyRecord_withRecordIdentifier (record_: NSDictionary; recordId: NSString): ObjCBOOL; message 'shouldApplyRecord:withRecordIdentifier:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

{$ifdef CLASSES}

type
  ISyncFilter = objcclass external (NSObject)
  public
    class function filterMatchingAllFilters (filters: NSArray): ISyncFilteringProtocol; message 'filterMatchingAllFilters:'; deprecated 'in 10_4, 10_7';
    class function filterMatchingAtLeastOneFilter (filters: NSArray): ISyncFilteringProtocol; message 'filterMatchingAtLeastOneFilter:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

