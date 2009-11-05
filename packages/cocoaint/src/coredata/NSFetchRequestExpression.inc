{ Parsed from Coredata.framework NSFetchRequestExpression.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSFETCHREQUESTEXPRESSION_PAS_T}
{$define NSFETCHREQUESTEXPRESSION_PAS_T}

{ Constants }

const
  NSFetchRequestExpressionType = 50;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFETCHREQUESTEXPRESSION_PAS_R}
{$define NSFETCHREQUESTEXPRESSION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFETCHREQUESTEXPRESSION_PAS_F}
{$define NSFETCHREQUESTEXPRESSION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFETCHREQUESTEXPRESSION_PAS_S}
{$define NSFETCHREQUESTEXPRESSION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFetchRequestExpression = objcclass;
  NSFetchRequestExpressionPointer = ^NSFetchRequestExpression;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFETCHREQUESTEXPRESSION_PAS_C}
{$define NSFETCHREQUESTEXPRESSION_PAS_C}

{ NSFetchRequestExpression }
  NSFetchRequestExpression = objcclass(NSExpression)
  private
    __reserved1: Pointer;
    __reserved2: Pointer;
    __reserved3: Pointer;
    __reserved4: Pointer;
    __fetchRequest: NSExpression;
    __managedObjectContext: NSExpression;
    __flags: bitpacked record
        isCountOnly: 0..1;
        _RESERVED: 0..((1 shl 31)-1);
      end;
    
  public
    class function alloc: NSFetchRequestExpression; message 'alloc';

    class function expressionForFetch_context_countOnly(fetch: NSExpression; context: NSExpression; countFlag: Boolean): NSExpression; message 'expressionForFetch:context:countOnly:';
    function requestExpression: NSExpression; message 'requestExpression';
    function contextExpression: NSExpression; message 'contextExpression';
    function isCountOnlyRequest: Boolean; message 'isCountOnlyRequest';
  end; external;

{$endif}
{$endif}
