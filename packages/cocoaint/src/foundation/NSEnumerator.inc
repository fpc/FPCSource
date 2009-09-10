{ Parsed from Foundation.framework NSEnumerator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSENUMERATOR_PAS_H}
{$define NSENUMERATOR_PAS_H}
type
  NSEnumeratorPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSENUMERATOR_PAS_T}
{$define NSENUMERATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSENUMERATOR_PAS_R}
{$define NSENUMERATOR_PAS_R}

{ Records }
type
  NSFastEnumerationState = record
    state: culong;
    itemsPtr: id;
    mutationsPtr: culong;
    extra: culong;
  end;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSENUMERATOR_PAS_F}
{$define NSENUMERATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSENUMERATOR_PAS_T}
{$define NSENUMERATOR_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFastEnumerationProtocol = objcprotocol;
  NSEnumerator = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSENUMERATOR_PAS_C}
{$define NSENUMERATOR_PAS_C}

{ NSEnumerator }
  NSEnumerator = objcclass(NSObject, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSEnumerator; message 'alloc';

    function nextObject: id; message 'nextObject';

    { Category: NSExtendedEnumerator }
    function allObjects: NSArray; message 'allObjects';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSENUMERATOR_PAS_P}
{$define NSENUMERATOR_PAS_P}
  
{ NSFastEnumeration Protocol }
  NSFastEnumerationProtocol = objcprotocol
    function countByEnumeratingWithState_objects_count(var state: NSFastEnumerationState; stackbuf: id; len: culong): culong; message 'countByEnumeratingWithState:objects:count:';
  end; external name 'NSFastEnumeration';
{$endif}
{$endif}
