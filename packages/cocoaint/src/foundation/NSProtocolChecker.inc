{ Parsed from Foundation.framework NSProtocolChecker.h }

{$ifdef TYPES}
{$ifndef NSPROTOCOLCHECKER_PAS_T}
{$define NSPROTOCOLCHECKER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROTOCOLCHECKER_PAS_R}
{$define NSPROTOCOLCHECKER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROTOCOLCHECKER_PAS_F}
{$define NSPROTOCOLCHECKER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROTOCOLCHECKER_PAS_S}
{$define NSPROTOCOLCHECKER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSProtocolChecker = objcclass;
  NSProtocolCheckerPointer = ^NSProtocolChecker;
  NSProtocolCheckerPtr = NSProtocolCheckerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROTOCOLCHECKER_PAS_C}
{$define NSPROTOCOLCHECKER_PAS_C}

{ NSProtocolChecker }
  NSProtocolChecker = objcclass external (NSProxy)
    
  public
    function protocol: Protocol; message 'protocol';
    function target: NSObject; message 'target';
  end;

{ NSProtocolCheckerCreationCategory }
  NSProtocolCheckerCreationCategory = objccategory external (NSProtocolChecker)
    class function protocolCheckerWithTarget_protocol(anObject: NSObject; aProtocol: objc_protocol): id; message 'protocolCheckerWithTarget:protocol:';
    function initWithTarget_protocol(anObject: NSObject; aProtocol: objc_protocol): id; message 'initWithTarget:protocol:';
  end;

{$endif}
{$endif}
