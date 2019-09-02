{ Parsed from CoreData.framework NSFetchedPropertyDescription.h }


{$ifdef TYPES}
type
  NSFetchedPropertyDescriptionPtr = ^NSFetchedPropertyDescription;
{$endif}

{$ifdef CLASSES}

type
  NSFetchedPropertyDescription = objcclass external (NSPropertyDescription)
  private
    _reserved5: pointer;
    _reserved6: pointer;
    _fetchRequest: NSFetchRequest;
    _lazyFetchRequestEntityName: NSString;
  public
    procedure setFetchRequest(newValue: NSFetchRequest); message 'setFetchRequest:';
    function fetchRequest: NSFetchRequest; message 'fetchRequest';
  end;
{$endif}

