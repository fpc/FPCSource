{ Parsed from Coredata.framework NSFetchedPropertyDescription.h }

{$ifdef TYPES}
{$ifndef NSFETCHEDPROPERTYDESCRIPTION_PAS_T}
{$define NSFETCHEDPROPERTYDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFETCHEDPROPERTYDESCRIPTION_PAS_R}
{$define NSFETCHEDPROPERTYDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFETCHEDPROPERTYDESCRIPTION_PAS_F}
{$define NSFETCHEDPROPERTYDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFETCHEDPROPERTYDESCRIPTION_PAS_S}
{$define NSFETCHEDPROPERTYDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFetchedPropertyDescription = objcclass;
  NSFetchedPropertyDescriptionPointer = ^NSFetchedPropertyDescription;
  NSFetchedPropertyDescriptionPtr = NSFetchedPropertyDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFETCHEDPROPERTYDESCRIPTION_PAS_C}
{$define NSFETCHEDPROPERTYDESCRIPTION_PAS_C}

{ NSFetchedPropertyDescription }
  NSFetchedPropertyDescription = objcclass external (NSPropertyDescription)
  private
    _reserved5: Pointer;
    _reserved6: Pointer;
    _fetchRequest: NSFetchRequest;
    _lazyFetchRequestEntityName: NSString;
    
  public
    function fetchRequest: NSFetchRequest; message 'fetchRequest';
    procedure setFetchRequest(fetchRequest_: NSFetchRequest); message 'setFetchRequest:';
  end;

{$endif}
{$endif}
