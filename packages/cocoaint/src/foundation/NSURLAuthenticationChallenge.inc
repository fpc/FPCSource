{ Parsed from Foundation.framework NSURLAuthenticationChallenge.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_H}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_H}
type
  NSURLAuthenticationChallengePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_T}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSURLAuthenticationChallengeSenderProtocol = objcprotocol;
  NSURLAuthenticationChallenge = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_C}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_C}

{ NSURLAuthenticationChallenge }
  NSURLAuthenticationChallenge = objcclass(NSObject)
  private
    __internal: NSURLAuthenticationChallengeInternal;
    
  public
    class function alloc: NSURLAuthenticationChallenge; message 'alloc';

    function initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_sender(space: NSURLProtectionSpace; credential: NSURLCredential; previousFailureCount_: NSInteger; response: NSURLResponse; error_: NSError; sender_: id): id; message 'initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:';
    function initWithAuthenticationChallenge_sender(challenge: NSURLAuthenticationChallenge; sender_: id): id; message 'initWithAuthenticationChallenge:sender:';
    function protectionSpace: NSURLProtectionSpace; message 'protectionSpace';
    function proposedCredential: NSURLCredential; message 'proposedCredential';
    function previousFailureCount: NSInteger; message 'previousFailureCount';
    function failureResponse: NSURLResponse; message 'failureResponse';
    function error: NSError; message 'error';
    function sender: id; message 'sender';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSURLAUTHENTICATIONCHALLENGE_PAS_P}
{$define NSURLAUTHENTICATIONCHALLENGE_PAS_P}
  
{ NSURLAuthenticationChallengeSender Protocol }
  NSURLAuthenticationChallengeSenderProtocol = objcprotocol
    procedure useCredential_forAuthenticationChallenge(credential: NSURLCredential; challenge: NSURLAuthenticationChallenge); message 'useCredential:forAuthenticationChallenge:';
    procedure continueWithoutCredentialForAuthenticationChallenge(challenge: NSURLAuthenticationChallenge); message 'continueWithoutCredentialForAuthenticationChallenge:';
    procedure cancelAuthenticationChallenge(challenge: NSURLAuthenticationChallenge); message 'cancelAuthenticationChallenge:';
  end; external name 'NSURLAuthenticationChallengeSender';
{$endif}
{$endif}
