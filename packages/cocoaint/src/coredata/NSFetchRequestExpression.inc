{ Parsed from CoreData.framework NSFetchRequestExpression.h }


{$ifdef TYPES}
type
  NSFetchRequestExpressionPtr = ^NSFetchRequestExpression;
{$endif}

{$ifdef TYPES}
const
  NSFetchRequestExpressionType = 50;
{$endif}

{$ifdef CLASSES}

type
  NSFetchRequestExpression = objcclass external (NSExpression)
  private
    _reserved1: pointer;
    _reserved2: pointer;
    _reserved3: pointer;
    _reserved4: pointer;
    _fetchRequest: NSExpression;
    _managedObjectContext: NSExpression;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          isCountOnly: 0..1;
          _RESERVED: 0..((1 shl 31)-1);
        );
      end;
  public
    class function expressionForFetch_context_countOnly (fetch: NSExpression; context: NSExpression; countFlag: ObjCBOOL): NSExpression; message 'expressionForFetch:context:countOnly:';
    function requestExpression: NSExpression; message 'requestExpression';
    function contextExpression: NSExpression; message 'contextExpression';
    function isCountOnlyRequest: ObjCBOOL; message 'isCountOnlyRequest';
  end;
{$endif}

