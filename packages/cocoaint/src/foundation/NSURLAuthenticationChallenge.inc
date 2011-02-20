{ Parsed from Foundation.framework NSURLAuthenticationChallenge.h }

{$ifdef TYPES}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_T}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_R}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_F}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_S}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLAuthenticationChallengeSenderProtocol = objcprotocol;
  NSURLAuthenticationChallenge = objcclass;
  NSURLAuthenticationChallengePointer = ^NSURLAuthenticationChallenge;
  NSURLAuthenticationChallengePtr = NSURLAuthenticationChallengePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_C}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_C}

{ NSURLAuthenticationChallenge }
  NSURLAuthenticationChallenge = objcclass external (NSObject)
  private
    _internal: NSURLAuthenticationChallengeInternal;
    
  public
    function initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_sender(space: NSURLProtectionSpace; credential: NSURLCredential; previousFailureCount_: NSInteger; response: NSURLResponse; error_: NSError; sender_: NSURLAuthenticationChallengeSenderProtocol): id; message 'initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:';
    function initWithAuthenticationChallenge_sender(challenge: NSURLAuthenticationChallenge; sender_: NSURLAuthenticationChallengeSenderProtocol): id; message 'initWithAuthenticationChallenge:sender:';
    function protectionSpace: NSURLProtectionSpace; message 'protectionSpace';
    function proposedCredential: NSURLCredential; message 'proposedCredential';
    function previousFailureCount: NSInteger; message 'previousFailureCount';
    function failureResponse: NSURLResponse; message 'failureResponse';
    function error: NSError; message 'error';
    function sender: NSURLAuthenticationChallengeSenderProtocol; message 'sender';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_P}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_P}
  
{ NSURLAuthenticationChallengeSender Protocol }
  NSURLAuthenticationChallengeSenderProtocol = objcprotocol external name 'NSURLAuthenticationChallengeSender'
    procedure useCredential_forAuthenticationChallenge(credential: NSURLCredential; challenge: NSURLAuthenticationChallenge); message 'useCredential:forAuthenticationChallenge:';
    procedure continueWithoutCredentialForAuthenticationChallenge(challenge: NSURLAuthenticationChallenge); message 'continueWithoutCredentialForAuthenticationChallenge:';
    procedure cancelAuthenticationChallenge(challenge: NSURLAuthenticationChallenge); message 'cancelAuthenticationChallenge:';
  end;
{$endif}
{$endif}
