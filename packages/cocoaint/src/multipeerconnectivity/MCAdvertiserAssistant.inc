{ Parsed from MultipeerConnectivity.framework MCAdvertiserAssistant.h }


{$ifdef TYPES}
type
  MCAdvertiserAssistantPtr = ^MCAdvertiserAssistant;
  MCAdvertiserAssistantDelegateProtocolPtr = ^MCAdvertiserAssistantDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  MCAdvertiserAssistant = objcclass external (NSObject)
  public
    function initWithServiceType_discoveryInfo_session (serviceType: NSString; info: NSDictionary; session: MCSession): instancetype; message 'initWithServiceType:discoveryInfo:session:'; { NS_DESIGNATED_INITIALIZER }
    procedure start; message 'start';
    procedure stop; message 'stop';
    procedure setDelegate(newValue: MCAdvertiserAssistantDelegateProtocol); message 'setDelegate:';
    function delegate: MCAdvertiserAssistantDelegateProtocol; message 'delegate';
    function session: MCSession; message 'session';
    function discoveryInfo: NSDictionary; message 'discoveryInfo';
    function serviceType: NSString; message 'serviceType';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  MCAdvertiserAssistantDelegateProtocol = objcprotocol external name 'MCAdvertiserAssistantDelegate' (NSObjectProtocol)
  optional
    procedure advertiserAssistantWillPresentInvitation (advertiserAssistant: MCAdvertiserAssistant); message 'advertiserAssistantWillPresentInvitation:';
    procedure advertiserAssistantDidDismissInvitation (advertiserAssistant: MCAdvertiserAssistant); message 'advertiserAssistantDidDismissInvitation:';
  end;
{$endif}

