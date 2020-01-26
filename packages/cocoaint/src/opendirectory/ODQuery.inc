{ Parsed from OpenDirectory.framework ODQuery.h }


{$ifdef TYPES}
type
  ODQueryPtr = ^ODQuery;
  ODQueryDelegateProtocolPtr = ^ODQueryDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  ODQueryDelegateProtocol = objcprotocol external name 'ODQueryDelegate' (NSObjectProtocol)
  required
    procedure query_foundResults_error (inQuery: ODQuery; inResults: NSArray; inError: NSError); message 'query:foundResults:error:'; { available in 10_6, NA }
  end;
{$endif}

{$ifdef CLASSES}

type
  ODQuery = objcclass external (NSObject, NSCopyingProtocol)
  private
    _internal: pointer;
  public
    class function queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error (inNode: ODNode; inRecordTypeOrList: id; inAttribute: ODAttributeType; inMatchType: ODMatchType; inQueryValueOrList: id; inReturnAttributeOrList: id; inMaximumResults: NSInteger; outError: NSErrorPtr): ODQuery; message 'queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:'; { available in 10_6, NA }
    function initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error (inNode: ODNode; inRecordTypeOrList: id; inAttribute: ODAttributeType; inMatchType: ODMatchType; inQueryValueOrList: id; inReturnAttributeOrList: id; inMaximumResults: NSInteger; outError: NSErrorPtr): instancetype; message 'initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:'; { available in 10_6, NA }
    function resultsAllowingPartial_error (inAllowPartialResults: ObjCBOOL; outError: NSErrorPtr): NSArray; message 'resultsAllowingPartial:error:'; { available in 10_6, NA }
    procedure setDelegate(newValue: ODQueryDelegateProtocol); message 'setDelegate:';
    function delegate: ODQueryDelegateProtocol; message 'delegate';
    procedure scheduleInRunLoop_forMode (inRunLoop: NSRunLoop; inMode: NSString); message 'scheduleInRunLoop:forMode:'; { available in 10_6, NA }
    procedure removeFromRunLoop_forMode (inRunLoop: NSRunLoop; inMode: NSString); message 'removeFromRunLoop:forMode:'; { available in 10_6, NA }
    procedure synchronize; message 'synchronize'; { available in 10_6, NA }
    procedure setOperationQueue(newValue: NSOperationQueue); message 'setOperationQueue:';
    function operationQueue: NSOperationQueue; message 'operationQueue';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

