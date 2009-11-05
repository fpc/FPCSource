{ Parsed from Coredata.framework NSFetchedPropertyDescription.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSFETCHEDPROPERTYDESCRIPTION_PAS_C}
{$define NSFETCHEDPROPERTYDESCRIPTION_PAS_C}

{ NSFetchedPropertyDescription }
  NSFetchedPropertyDescription = objcclass(NSPropertyDescription)
  private
    __reserved5: Pointer;
    __reserved6: Pointer;
    __fetchRequest: NSFetchRequest;
    __lazyFetchRequestEntityName: NSString;
    
  public
    class function alloc: NSFetchedPropertyDescription; message 'alloc';

    function fetchRequest: NSFetchRequest; message 'fetchRequest';
    procedure setFetchRequest(fetchRequest_: NSFetchRequest); message 'setFetchRequest:';
  end; external;

{$endif}
{$endif}
