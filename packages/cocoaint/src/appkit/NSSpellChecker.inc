{ Parsed from Appkit.framework NSSpellChecker.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSPELLCHECKER_PAS_T}
{$define NSSPELLCHECKER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPELLCHECKER_PAS_R}
{$define NSSPELLCHECKER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPELLCHECKER_PAS_F}
{$define NSSPELLCHECKER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPELLCHECKER_PAS_S}
{$define NSSPELLCHECKER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSpellChecker = objcclass;
  NSSpellCheckerPointer = ^NSSpellChecker;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSPELLCHECKER_PAS_C}
{$define NSSPELLCHECKER_PAS_C}

{ NSSpellChecker }
  NSSpellChecker = objcclass(NSObject)
  private
    __guessesBrowser: id;
    __wordField: id;
    __languagePopUp: id;
    __guessesList: id;
    __panel: id;
    __userDictionaries: id;
    __correctButton: id;
    __guessButton: id;
    __ignoreButton: id;
    __accessoryView: id;
    __dictionaryBrowser: id;
    __selectionString: NSString;
    __spellServers: id;
    __lastGuess: NSString;
    __scFlags: bitpacked record
        autoShowGuesses: 0..1;
        needDelayedGuess: 0..1;
        unignoreInProgress: 0..1;
        wordFieldEdited: 0..1;
        inSpelling: 0..1;
        reconnectSpelling: 0..1;
        inGrammar: 0..1;
        reconnectGrammar: 0..1;
        _reserved: 0..((1 shl 24)-1);
      end;
    __deleteButton: id;
    __openButton: id;
    __learnButton: id;
    __infoField: id;
    __grammarControl: id;
    
  public
    class function alloc: NSSpellChecker; message 'alloc';

    class function sharedSpellChecker: NSSpellChecker; message 'sharedSpellChecker';
    class function sharedSpellCheckerExists: Boolean; message 'sharedSpellCheckerExists';
    class function uniqueSpellDocumentTag: NSInteger; message 'uniqueSpellDocumentTag';
    function checkSpellingOfString_startingAt_language_wrap_inSpellDocumentWithTag_wordCount(stringToCheck: NSString; startingOffset: NSInteger; language_: NSString; wrapFlag: Boolean; tag: NSInteger; wordCount: NSIntegerPointer): NSRange; message 'checkSpellingOfString:startingAt:language:wrap:inSpellDocumentWithTag:wordCount:';
    function checkSpellingOfString_startingAt(stringToCheck: NSString; startingOffset: NSInteger): NSRange; message 'checkSpellingOfString:startingAt:';
    function countWordsInString_language(stringToCount: NSString; language_: NSString): NSInteger; message 'countWordsInString:language:';
    function checkGrammarOfString_startingAt_language_wrap_inSpellDocumentWithTag_details(stringToCheck: NSString; startingOffset: NSInteger; language_: NSString; wrapFlag: Boolean; tag: NSInteger; details: NSArrayPointer): NSRange; message 'checkGrammarOfString:startingAt:language:wrap:inSpellDocumentWithTag:details:';
    procedure updateSpellingPanelWithMisspelledWord(word: NSString); message 'updateSpellingPanelWithMisspelledWord:';
    procedure updateSpellingPanelWithGrammarString_detail(string_: NSString; detail: NSDictionary); message 'updateSpellingPanelWithGrammarString:detail:';
    function spellingPanel: NSPanel; message 'spellingPanel';
    function accessoryView: NSView; message 'accessoryView';
    procedure setAccessoryView(aView: NSView); message 'setAccessoryView:';
    procedure ignoreWord_inSpellDocumentWithTag(wordToIgnore: NSString; tag: NSInteger); message 'ignoreWord:inSpellDocumentWithTag:';
    function ignoredWordsInSpellDocumentWithTag(tag: NSInteger): NSArray; message 'ignoredWordsInSpellDocumentWithTag:';
    procedure setIgnoredWords_inSpellDocumentWithTag(words: NSArray; tag: NSInteger); message 'setIgnoredWords:inSpellDocumentWithTag:';
    function guessesForWord(word: NSString): NSArray; message 'guessesForWord:';
    function completionsForPartialWordRange_inString_language_inSpellDocumentWithTag(range: NSRange; string_: NSString; language_: NSString; tag: NSInteger): NSArray; message 'completionsForPartialWordRange:inString:language:inSpellDocumentWithTag:';
    procedure closeSpellDocumentWithTag(tag: NSInteger); message 'closeSpellDocumentWithTag:';
    function language: NSString; message 'language';
    function setLanguage(language_: NSString): Boolean; message 'setLanguage:';
    function availableLanguages: NSArray; message 'availableLanguages';
    procedure setWordFieldStringValue(aString: NSString); message 'setWordFieldStringValue:';
    procedure learnWord(word: NSString); message 'learnWord:';
    function hasLearnedWord(word: NSString): Boolean; message 'hasLearnedWord:';
    procedure unlearnWord(word: NSString); message 'unlearnWord:';

    { Category: NSDeprecated }
    procedure forgetWord(word: NSString); message 'forgetWord:';
  end; external;

{$endif}
{$endif}
