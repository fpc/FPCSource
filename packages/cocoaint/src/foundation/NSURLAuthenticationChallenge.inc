{ Parsed from Foundation.framework NSURLAuthenticationChallenge.h }


{$ifdef TYPES}
type
  NSURLAuthenticationChallengePtr = ^NSURLAuthenticationChallenge;
  NSURLAuthenticationChallengeSenderProtocolPtr = ^NSURLAuthenticationChallengeSenderProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSURLAuthenticationChallengeSenderProtocol = objcprotocol external name 'NSURLAuthenticationChallengeSender' (NSObjectProtocol)
    procedure useCredential_forAuthenticationChallenge (credential: NSURLCredential; challenge: NSURLAuthenticationChallenge); message 'useCredential:forAuthenticationChallenge:';
    procedure continueWithoutCredentialForAuthenticationChallenge (challenge: NSURLAuthenticationChallenge); message 'continueWithoutCredentialForAuthenticationChallenge:';
    procedure cancelAuthenticationChallenge (challenge: NSURLAuthenticationChallenge); message 'cancelAuthenticationChallenge:';
  optional
    procedure performDefaultHandlingForAuthenticationChallenge (challenge: NSURLAuthenticationChallenge); message 'performDefaultHandlingForAuthenticationChallenge:';
    procedure rejectProtectionSpaceAndContinueWithChallenge (challenge: NSURLAuthenticationChallenge); message 'rejectProtectionSpaceAndContinueWithChallenge:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSURLAuthenticationChallenge = objcclass external (NSObject, NSSecureCodingProtocol)
  private
    _internal: NSURLAuthenticationChallengeInternal;
  public
    function initWithProtectionSpace_proposedCredential_previousFailureCount_failureResponse_error_sender (space: NSURLProtectionSpace; credential: NSURLCredential; previousFailureCount: NSInteger; response: NSURLResponse; error: NSError; sender: NSURLAuthenticationChallengeSenderProtocol): instancetype; message 'initWithProtectionSpace:proposedCredential:previousFailureCount:failureResponse:error:sender:';
    function initWithAuthenticationChallenge_sender (challenge: NSURLAuthenticationChallenge; sender: NSURLAuthenticationChallengeSenderProtocol): instancetype; message 'initWithAuthenticationChallenge:sender:';
    function protectionSpace: NSURLProtectionSpace; message 'protectionSpace';
    function proposedCredential: NSURLCredential; message 'proposedCredential';
    function previousFailureCount: NSInteger; message 'previousFailureCount';
    function failureResponse: NSURLResponse; message 'failureResponse';
    function error: NSError; message 'error';
    function sender: NSURLAuthenticationChallengeSenderProtocol; message 'sender';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

