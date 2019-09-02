{ Parsed from SyncServices.framework ISyncRecordSnapshot.h }


{$ifdef TYPES}
type
  ISyncRecordSnapshotPtr = ^ISyncRecordSnapshot;
{$endif}

{$ifdef CLASSES}

type
  ISyncRecordSnapshot = objcclass external (NSObject)
  public
    function recordsWithIdentifiers (recordIds: NSArray): NSDictionary; message 'recordsWithIdentifiers:'; deprecated 'in 10_4, 10_7';
    function targetIdentifiersForRelationshipName_withSourceIdentifier (relationshipName: NSString; sourceId: NSString): NSArray; message 'targetIdentifiersForRelationshipName:withSourceIdentifier:'; deprecated 'in 10_4, 10_7';
    function sourceIdentifiersForRelationshipName_withTargetIdentifier (relationshipName: NSString; sourceId: NSString): NSArray; message 'sourceIdentifiersForRelationshipName:withTargetIdentifier:'; deprecated 'in 10_4, 10_7';
    function recordsWithMatchingAttributes (attributes: NSDictionary): NSDictionary; message 'recordsWithMatchingAttributes:'; deprecated 'in 10_4, 10_7';
  end;


type
  ISyncRecordSnapshot_ISyncRecordReference = objccategory external name 'ISyncRecordReference' (ISyncRecordSnapshot)
    function recordReferenceForRecordWithIdentifier (identifier: NSString): ISyncRecordReference; message 'recordReferenceForRecordWithIdentifier:'; deprecated 'in 10_4, 10_7';
    function recordIdentifierForReference_isModified (reference: ISyncRecordReference; pModified: pObjCBOOL): NSString; message 'recordIdentifierForReference:isModified:'; deprecated 'in 10_4, 10_7';
  end;
{$endif}

