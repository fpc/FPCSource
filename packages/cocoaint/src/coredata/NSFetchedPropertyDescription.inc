{ Parsed from Coredata.framework NSFetchedPropertyDescription.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


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
  NSFetchedPropertyDescription = objcclass(NSPropertyDescription)
  private
    _reserved5: Pointer;
    _reserved6: Pointer;
    _fetchRequest: NSFetchRequest;
    _lazyFetchRequestEntityName: NSString;
    
  public
    class function alloc: NSFetchedPropertyDescription; message 'alloc';

    function fetchRequest: NSFetchRequest; message 'fetchRequest';
    procedure setFetchRequest(fetchRequest_: NSFetchRequest); message 'setFetchRequest:';
  end; external;

{$endif}
{$endif}
