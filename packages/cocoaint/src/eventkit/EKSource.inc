{ Parsed from EventKit.framework EKSource.h }


{$ifdef TYPES}
type
  EKSourcePtr = ^EKSource;
{$endif}

{$ifdef CLASSES}

type
  EKSource = objcclass external (EKObject)
  public
    function sourceIdentifier: NSString; message 'sourceIdentifier';
    function sourceType: EKSourceType; message 'sourceType';
    function title: NSString; message 'title';
    function calendarsForEntityType (entityType: EKEntityType): NSSet; message 'calendarsForEntityType:'; { available starting in __MAC_10_8,__IPHONE_6_0 }
  end;
{$endif}

