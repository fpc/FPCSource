{ Parsed from Foundation.framework NSProtocolChecker.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPROTOCOLCHECKER_PAS_H}
{$define NSPROTOCOLCHECKER_PAS_H}
type
  NSProtocolCheckerPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSPROTOCOLCHECKER_PAS_T}
{$define NSPROTOCOLCHECKER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSProtocolChecker = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROTOCOLCHECKER_PAS_C}
{$define NSPROTOCOLCHECKER_PAS_C}

{ NSProtocolChecker }
  NSProtocolChecker = objcclass(NSProxy)
    
  public
    class function alloc: NSProtocolChecker; message 'alloc';

    function protocol: objc_protocol; message 'protocol';
    function target: NSObject; message 'target';

    { Category: NSProtocolCheckerCreation }
    class function protocolCheckerWithTarget_protocol(anObject: NSObject; aProtocol: objc_protocol): id; message 'protocolCheckerWithTarget:protocol:';
    function initWithTarget_protocol(anObject: NSObject; aProtocol: objc_protocol): id; message 'initWithTarget:protocol:';
  end; external;

{$endif}
{$endif}
