{ Parsed from AddressBook.framework ABPerson.h }


{$ifdef TYPES}
type
  ABPersonPtr = ^ABPerson;
{$endif}

{$ifdef CLASSES}

type
  ABPerson = objcclass external (ABRecord)
  public
    function parentGroups: NSArray; message 'parentGroups';
    function linkedPeople: NSArray; message 'linkedPeople';
  end;


type
  ABPerson_Properties = objccategory external (ABPerson)
    class function addPropertiesAndTypes (properties: NSDictionary): NSInteger; message 'addPropertiesAndTypes:';
    class function removeProperties (properties: NSArray): NSInteger; message 'removeProperties:';
    class function properties: NSArray; message 'properties';
    class function typeOfProperty (property_: NSString): ABPropertyType; message 'typeOfProperty:';
  end;

type
  ABPerson_Searching = objccategory external (ABPerson)
    class function searchElementForProperty_label_key_value_comparison (property_: NSString; label_: NSString; key: NSString; value: id; comparison: ABSearchComparison): ABSearchElement; message 'searchElementForProperty:label:key:value:comparison:';
  end;

type
  ABPerson_vCard = objccategory external (ABPerson)
    function initWithVCardRepresentation (vCardData: NSData): id; message 'initWithVCardRepresentation:';
    function vCardRepresentation: NSData; message 'vCardRepresentation';
  end;
{$endif}

