{ Parsed from Foundation.framework NSSortDescriptor.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSORTDESCRIPTOR_PAS_T}
{$define NSSORTDESCRIPTOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSORTDESCRIPTOR_PAS_R}
{$define NSSORTDESCRIPTOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSORTDESCRIPTOR_PAS_F}
{$define NSSORTDESCRIPTOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSORTDESCRIPTOR_PAS_S}
{$define NSSORTDESCRIPTOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSortDescriptor = objcclass;
  NSSortDescriptorPointer = ^NSSortDescriptor;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSORTDESCRIPTOR_PAS_C}
{$define NSSORTDESCRIPTOR_PAS_C}

{ NSSortDescriptor }
  NSSortDescriptor = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __sortDescriptorFlags: NSUInteger;
    __key: NSString;
    __selector: SEL;
    __selectorName: NSString;
    
  public
    class function alloc: NSSortDescriptor; message 'alloc';

    function initWithKey_ascending(key_: NSString; ascending_: Boolean): id; message 'initWithKey:ascending:';
    function initWithKey_ascending_selector(key_: NSString; ascending_: Boolean; selector_: SEL): id; message 'initWithKey:ascending:selector:';
    function key: NSString; message 'key';
    function ascending: Boolean; message 'ascending';
    function selector: SEL; message 'selector';
    function compareObject_toObject(object_: id; object_1: id): NSComparisonResult; message 'compareObject:toObject:';
    function reversedSortDescriptor: id; message 'reversedSortDescriptor';
  end; external;

{$endif}
{$endif}
