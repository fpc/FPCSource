{ Parsed from AddressBook.framework ABGroup.h }


{$ifdef TYPES}
type
  ABGroupPtr = ^ABGroup;
{$endif}

{$ifdef CLASSES}

type
  ABGroup = objcclass external (ABRecord)
  private
    _groupReserved1: id;
    _groupReserved2: id;
  public
    function members: NSArray; message 'members';
    function addMember (person: ABPerson): ObjCBOOL; message 'addMember:';
    function removeMember (person: ABPerson): ObjCBOOL; message 'removeMember:';
    function subgroups: NSArray; message 'subgroups';
    function addSubgroup (group: ABGroup): ObjCBOOL; message 'addSubgroup:';
    function removeSubgroup (group: ABGroup): ObjCBOOL; message 'removeSubgroup:';
    function parentGroups: NSArray; message 'parentGroups';
    function setDistributionIdentifier_forProperty_person (identifier: NSString; property_: NSString; person: ABPerson): ObjCBOOL; message 'setDistributionIdentifier:forProperty:person:';
    function distributionIdentifierForProperty_person (property_: NSString; person: ABPerson): NSString; message 'distributionIdentifierForProperty:person:';
  end;


type
  ABGroup_Properties = objccategory external (ABGroup)
    class function addPropertiesAndTypes (properties: NSDictionary): NSInteger; message 'addPropertiesAndTypes:';
    class function removeProperties (properties: NSArray): NSInteger; message 'removeProperties:';
    class function properties: NSArray; message 'properties';
    class function typeOfProperty (property_: NSString): ABPropertyType; message 'typeOfProperty:';
  end;

type
  ABGroup_Searching = objccategory external (ABGroup)
    class function searchElementForProperty_label_key_value_comparison (property_: NSString; label_: NSString; key: NSString; value: id; comparison: ABSearchComparison): ABSearchElement; message 'searchElementForProperty:label:key:value:comparison:';
  end;
{$endif}

