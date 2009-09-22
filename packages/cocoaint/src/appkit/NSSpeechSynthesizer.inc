{ Parsed from Appkit.framework NSSpeechSynthesizer.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSPEECHSYNTHESIZER_PAS_H}
{$define NSSPEECHSYNTHESIZER_PAS_H}
type
  NSSpeechSynthesizerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSPEECHSYNTHESIZER_PAS_T}
{$define NSSPEECHSYNTHESIZER_PAS_T}

{ Constants }

const
  NSSpeechImmediateBoundary = 0;
  NSSpeechWordBoundary = 0;
  NSSpeechSentenceBoundary = 1;

{ Types }
type
  NSSpeechBoundary = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_R}
{$define NSSPEECHSYNTHESIZER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_F}
{$define NSSPEECHSYNTHESIZER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPEECHSYNTHESIZER_PAS_T}
{$define NSSPEECHSYNTHESIZER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSpeechSynthesizer = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPEECHSYNTHESIZER_PAS_C}
{$define NSSPEECHSYNTHESIZER_PAS_C}

{ NSSpeechSynthesizer }
  NSSpeechSynthesizer = objcclass(NSObject)
  private
    __privateNSSpeechSynthesizerVars: id;
    
  public
    class function alloc: NSSpeechSynthesizer; message 'alloc';

    function initWithVoice(voice_: NSString): id; message 'initWithVoice:';
    function startSpeakingString(string_: NSString): Boolean; message 'startSpeakingString:';
    function startSpeakingString_toURL(string_: NSString; url: NSURL): Boolean; message 'startSpeakingString:toURL:';
    function isSpeaking: Boolean; message 'isSpeaking';
    procedure stopSpeaking; message 'stopSpeaking';
    procedure stopSpeakingAtBoundary(boundary: NSSpeechBoundary); message 'stopSpeakingAtBoundary:';
    procedure pauseSpeakingAtBoundary(boundary: NSSpeechBoundary); message 'pauseSpeakingAtBoundary:';
    procedure continueSpeaking; message 'continueSpeaking';
    function delegate: id; message 'delegate';
    procedure setDelegate(anObject: id); message 'setDelegate:';
    function voice: NSString; message 'voice';
    function setVoice(voice_: NSString): Boolean; message 'setVoice:';
    function rate: single; message 'rate';
    procedure setRate(rate_: single); message 'setRate:';
    function volume: single; message 'volume';
    procedure setVolume(volume_: single); message 'setVolume:';
    function usesFeedbackWindow: Boolean; message 'usesFeedbackWindow';
    procedure setUsesFeedbackWindow(flag: Boolean); message 'setUsesFeedbackWindow:';
    procedure addSpeechDictionary(speechDictionary: NSDictionary); message 'addSpeechDictionary:';
    function phonemesFromText(text: NSString): NSString; message 'phonemesFromText:';
    function objectForProperty_error(property_: NSString; outError: NSErrorPointer): id; message 'objectForProperty:error:';
    function setObject_forProperty_error(object_: id; property_: NSString; outError: NSErrorPointer): Boolean; message 'setObject:forProperty:error:';
    class function isAnyApplicationSpeaking: Boolean; message 'isAnyApplicationSpeaking';
    class function defaultVoice: NSString; message 'defaultVoice';
    class function availableVoices: NSArray; message 'availableVoices';
    class function attributesForVoice(voice_: NSString): NSDictionary; message 'attributesForVoice:';
  end; external;

{$endif}
{$endif}
