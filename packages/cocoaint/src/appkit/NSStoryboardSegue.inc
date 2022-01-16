{ Parsed from AppKit.framework NSStoryboardSegue.h }


{$ifdef TYPES}
type
  NSStoryboardSeguePtr = ^NSStoryboardSegue;
  NSSeguePerformingProtocolPtr = ^NSSeguePerformingProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSStoryboardSegue = objcclass external (NSObject)
  private
    _performHandler: OpaqueCBlock;
    _identifier: NSString;
    _sourceController: id;
    _destinationController: id;
    _reserved: id;
  public
    class function segueWithIdentifier_source_destination_performHandler (identifier: NSString; sourceController: id; destinationController: id; performHandler: OpaqueCBlock): instancetype; message 'segueWithIdentifier:source:destination:performHandler:';
    function initWithIdentifier_source_destination (identifier: NSString; sourceController: id; destinationController: id): instancetype; message 'initWithIdentifier:source:destination:'; { NS_DESIGNATED_INITIALIZER }
    function identifier: NSString; message 'identifier';
    function sourceController: id; message 'sourceController';
    function destinationController: id; message 'destinationController';
    procedure perform; message 'perform';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSeguePerformingProtocol = objcprotocol external name 'NSSeguePerforming' (NSObjectProtocol)
  optional
    procedure prepareForSegue_sender (segue: NSStoryboardSegue; sender: id); message 'prepareForSegue:sender:'; { available in 10_10 }
    procedure performSegueWithIdentifier_sender (identifier: NSString; sender: id); message 'performSegueWithIdentifier:sender:'; { available in 10_10 }
    function shouldPerformSegueWithIdentifier_sender (identifier: NSString; sender: id): ObjCBOOL; message 'shouldPerformSegueWithIdentifier:sender:'; { available in 10_10 }
  end;
{$endif}

