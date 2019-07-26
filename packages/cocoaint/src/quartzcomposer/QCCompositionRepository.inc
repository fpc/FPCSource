{ Parsed from QuartzComposer.framework QCCompositionRepository.h }


{$ifdef TYPES}
type
  QCCompositionRepositoryPtr = ^QCCompositionRepository;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef EXTERNAL_SYMBOLS}
var
  QCCompositionRepositoryDidUpdateNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  QCComposition_QCCompositionRepository = objccategory external name 'QCCompositionRepository' (QCComposition)
    function identifier: NSString; message 'identifier';
  end;


type
  QCCompositionRepository = objcclass external (NSObject)
  private
    cq: dispatch_queue_t;
  public
    class function sharedCompositionRepository: QCCompositionRepository; message 'sharedCompositionRepository';
    function compositionWithIdentifier (identifier: NSString): QCComposition; message 'compositionWithIdentifier:';
    function compositionsWithProtocols_andAttributes (protocols: NSArray; attributes: NSDictionary): NSArray; message 'compositionsWithProtocols:andAttributes:';
    function allCompositions: NSArray; message 'allCompositions';
  end;
{$endif}

{$endif}
