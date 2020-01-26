{ Parsed from AppKit.framework NSSpellChecker.h }


{ Types from NSSpellChecker }
{$ifdef TYPES}

type
  NSCorrectionResponse = NSInteger;
type
  NSCorrectionResponsePtr = ^NSCorrectionResponse;

const
  NSCorrectionResponseNone = 0;
  NSCorrectionResponseAccepted = 1;
  NSCorrectionResponseRejected = 2;
  NSCorrectionResponseIgnored = 3;
  NSCorrectionResponseEdited = 4;
  NSCorrectionResponseReverted = 5;

type
  NSCorrectionIndicatorType = NSInteger;
type
  NSCorrectionIndicatorTypePtr = ^NSCorrectionIndicatorType;

const
  NSCorrectionIndicatorTypeDefault = 0;
  NSCorrectionIndicatorTypeReversion = 1;
  NSCorrectionIndicatorTypeGuesses = 2;
{$endif}


{$ifdef TYPES}
type
  NSSpellCheckerPtr = ^NSSpellChecker;
{$endif}

{$ifdef CLASSES}

type
  NSSpellChecker = objcclass external (NSObject)
  private
    _guessesBrowser: id;
    _wordField: id;
    _languagePopUp: id;
    _reserved1: id;
    _panel: id;
    _reserved2: id;
    _correctButton: id;
    _guessButton: id;
    _ignoreButton: id;
    _reserved3: id;
    _languagesBrowser: id;
    _quotesBrowser: id;
    _replacementsBrowser: id;
    _defineButton: id;
    __scFlags: bitpacked record
      case byte of
        0: (_anonBitField___scFlags0: cuint);
        1: (
          autoShowGuesses: 0..1;
          needDelayedGuess: 0..1;
          unignoreInProgress: 0..1;
          wordFieldEdited: 0..1;
          inSpelling: 0..1;
          reconnectSpelling: 0..1;
          inGrammar: 0..1;
          reconnectGrammar: 0..1;
          languageIdentification: 0..1;
          languagesHidden: 0..1;
          quotesByLanguage: 0..1;
          _reserved: 0..((1 shl 21)-1);
        );
      end;
    _substitutionsPanel: id;
    _reserved4: id;
    _learnButton: id;
    _infoField: id;
    _grammarControl: id;
  public
    class function sharedSpellChecker: NSSpellChecker; message 'sharedSpellChecker';
    class function sharedSpellCheckerExists: ObjCBOOL; message 'sharedSpellCheckerExists';
    class function uniqueSpellDocumentTag: NSInteger; message 'uniqueSpellDocumentTag';
    function checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount (stringToCheck: NSString; startingOffset: NSInteger; language: NSString; wrapFlag: ObjCBOOL; tag: NSInteger; wordCount: NSIntegerPtr): NSRange; message 'checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:';
    function checkSpellingOfString_startingAt (stringToCheck: NSString; startingOffset: NSInteger): NSRange; message 'checkSpellingOfString:startingAt:';
    function countWordsInString_language (stringToCount: NSString; language: NSString): NSInteger; message 'countWordsInString:language:';
    function checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details (stringToCheck: NSString; startingOffset: NSInteger; language: NSString; wrapFlag: ObjCBOOL; tag: NSInteger; details: NSArrayPtr): NSRange; message 'checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:'; { available in 10_5 }
    function checkString_range_types_options_inSpellDocumentWithTag_orthography_wordCount (stringToCheck: NSString; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; tag: NSInteger; orthography: NSOrthographyPtr; wordCount: NSIntegerPtr): NSArray; message 'checkString:range:types:options:inSpellDocumentWithTag:orthography:wordCount:'; { available in 10_6 }
    function requestCheckingOfString_range_types_options_inSpellDocumentWithTag_completionHandler (stringToCheck: NSString; range: NSRange; checkingTypes: NSTextCheckingTypes; options: NSDictionary; tag: NSInteger; completionHandler: OpaqueCBlock): NSInteger; message 'requestCheckingOfString:range:types:options:inSpellDocumentWithTag:completionHandler:'; { available in 10_6 }
    function menuForResult_string_options_atLocation_inView (result_: NSTextCheckingResult; checkedString: NSString; options: NSDictionary; location: NSPoint; view: NSView): NSMenu; message 'menuForResult:string:options:atLocation:inView:'; { available in 10_6 }
    function userQuotesArrayForLanguage (language: NSString): NSArray; message 'userQuotesArrayForLanguage:'; { available in 10_6 }
    function userReplacementsDictionary: NSDictionary; message 'userReplacementsDictionary';
    procedure updateSpellingPanelWithMisspelledWord (word: NSString); message 'updateSpellingPanelWithMisspelledWord:';
    procedure updateSpellingPanelWithGrammarString_detail (string_: NSString; detail: NSDictionary); message 'updateSpellingPanelWithGrammarString:detail:'; { available in 10_5 }
    function spellingPanel: NSPanel; message 'spellingPanel';
    procedure setAccessoryView(newValue: NSView); message 'setAccessoryView:';
    function accessoryView: NSView; message 'accessoryView';
    function substitutionsPanel: NSPanel; message 'substitutionsPanel';
    procedure setSubstitutionsPanelAccessoryViewController(newValue: NSViewController); message 'setSubstitutionsPanelAccessoryViewController:';
    function substitutionsPanelAccessoryViewController: NSViewController; message 'substitutionsPanelAccessoryViewController';
    procedure updatePanels; message 'updatePanels'; { available in 10_6 }
    procedure ignoreWord_inSpellDocumentWithTag (wordToIgnore: NSString; tag: NSInteger); message 'ignoreWord:inSpellDocumentWithTag:';
    function ignoredWordsInSpellDocumentWithTag (tag: NSInteger): NSArray; message 'ignoredWordsInSpellDocumentWithTag:';
    procedure setIgnoredWords_inSpellDocumentWithTag (words: NSArray; tag: NSInteger); message 'setIgnoredWords:inSpellDocumentWithTag:';
    function guessesForWordRange_inString_language_inSpellDocumentWithTag (range: NSRange; string_: NSString; language: NSString; tag: NSInteger): NSArray; message 'guessesForWordRange:inString:language:inSpellDocumentWithTag:'; { available in 10_6 }
    function correctionForWordRange_inString_language_inSpellDocumentWithTag (range: NSRange; string_: NSString; language: NSString; tag: NSInteger): NSString; message 'correctionForWordRange:inString:language:inSpellDocumentWithTag:'; { available in 10_7 }
    function completionsForPartialWordRange_inString_language_inSpellDocumentWithTag (range: NSRange; string_: NSString; language: NSString; tag: NSInteger): NSArray; message 'completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:';
    function languageForWordRange_inString_orthography (range: NSRange; string_: NSString; orthography: NSOrthography): NSString; message 'languageForWordRange:inString:orthography:'; { available in 10_7 }
    procedure closeSpellDocumentWithTag (tag: NSInteger); message 'closeSpellDocumentWithTag:';
    procedure recordResponse_toCorrection_forWord_language_inSpellDocumentWithTag (response: NSCorrectionResponse; correction: NSString; word: NSString; language: NSString; tag: NSInteger); message 'recordResponse:toCorrection:forWord:language:inSpellDocumentWithTag:'; { available in 10_7 }
    procedure showCorrectionIndicatorOfType_primaryString_alternativeStrings_forStringInRect_view_completionHandler (type_: NSCorrectionIndicatorType; primaryString: NSString; alternativeStrings: NSArray; rectOfTypedString: NSRect; view: NSView; completionBlock: OpaqueCBlock); message 'showCorrectionIndicatorOfType:primaryString:alternativeStrings:forStringInRect:view:completionHandler:'; { available in 10_7 }
    procedure dismissCorrectionIndicatorForView (view: NSView); message 'dismissCorrectionIndicatorForView:'; { available in 10_7 }
    function availableLanguages: NSArray; message 'availableLanguages';
    function userPreferredLanguages: NSArray; message 'userPreferredLanguages';
    procedure setAutomaticallyIdentifiesLanguages(newValue: ObjCBOOL); message 'setAutomaticallyIdentifiesLanguages:';
    function automaticallyIdentifiesLanguages: ObjCBOOL; message 'automaticallyIdentifiesLanguages';
    procedure setWordFieldStringValue (aString: NSString); message 'setWordFieldStringValue:';
    procedure learnWord (word: NSString); message 'learnWord:';
    function hasLearnedWord (word: NSString): ObjCBOOL; message 'hasLearnedWord:'; { available in 10_5 }
    procedure unlearnWord (word: NSString); message 'unlearnWord:'; { available in 10_5 }
    class function isAutomaticTextReplacementEnabled: ObjCBOOL; message 'isAutomaticTextReplacementEnabled'; { available in 10_7 }
    class function isAutomaticSpellingCorrectionEnabled: ObjCBOOL; message 'isAutomaticSpellingCorrectionEnabled'; { available in 10_7 }
    class function isAutomaticQuoteSubstitutionEnabled: ObjCBOOL; message 'isAutomaticQuoteSubstitutionEnabled'; { available in 10_9 }
    class function isAutomaticDashSubstitutionEnabled: ObjCBOOL; message 'isAutomaticDashSubstitutionEnabled'; { available in 10_9 }
    function language: NSString; message 'language';
    function setLanguage (language_: NSString): ObjCBOOL; message 'setLanguage:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSpellCheckerDidChangeAutomaticSpellingCorrectionNotification: NSString { available in 10_7 }; cvar; external;
  NSSpellCheckerDidChangeAutomaticTextReplacementNotification: NSString { available in 10_7 }; cvar; external;
  NSSpellCheckerDidChangeAutomaticQuoteSubstitutionNotification: NSString { available in 10_9 }; cvar; external;
  NSSpellCheckerDidChangeAutomaticDashSubstitutionNotification: NSString { available in 10_9 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSSpellChecker_NSDeprecated = objccategory external name 'NSDeprecated' (NSSpellChecker)
    function guessesForWord (word: NSString): NSArray; message 'guessesForWord:'; deprecated 'in 10_0, 10_6, "Use -guessesForWordRange:inString:language:inSpellDocumentWithTag instead"';
    procedure forgetWord (word: NSString); message 'forgetWord:'; deprecated 'in 10_0, 10_5';
  end;
{$endif}

