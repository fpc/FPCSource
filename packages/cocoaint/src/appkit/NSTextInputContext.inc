{ Parsed from AppKit.framework NSTextInputContext.h }


{$ifdef TYPES}
type
  NSTextInputContextPtr = ^NSTextInputContext;
{$endif}

{$ifdef CLASSES}

type
  NSTextInputContext = objcclass external (NSObject)
  private
    _client: id;
    _inputLocales: NSArray;
    _documentID: pointer;
    _keyBindings: id;
    _keyBindingManager: id;
    _commandCharacters: NSString;
    _documentRefcon: NSInteger;
    _reserved:array[0..1] of id;
    _auxiliary: id;
    _ticFlags: bitpacked record
      case byte of
        0: (_anonBitField__ticFlags0: cuint);
        1: (
          _acceptsGlyphInfo: 0..1;
          _secureInput: 0..1;
          _attributedString: 0..1;
          _fractionOfDistance: 0..1;
          _baselineDelta: 0..1;
          _windowLevel: 0..1;
          _drawsVertically: 0..1;
          _compatibilityMode: 0..1;
          _hasTS: 0..1;
          _hasLM: 0..1;
          _inputLocaleGeneration: 0..((1 shl 16)-1);
          _blockTSM: 0..1;
          _keyProcess: 0..1;
          _preflight: 0..1;
          _bindingFound: 0..1;
          _unmarkbeforeinsert: 0..1;
          _completionHandlingClient: 0..1;
        );
      end;
  public
    class function currentInputContext: NSTextInputContext; message 'currentInputContext';
    function initWithClient (theClient: NSTextInputClientProtocol): instancetype; message 'initWithClient:';
    function client: NSTextInputClientProtocol; message 'client';
    procedure setAcceptsGlyphInfo(newValue: ObjCBOOL); message 'setAcceptsGlyphInfo:';
    function acceptsGlyphInfo: ObjCBOOL; message 'acceptsGlyphInfo';
    procedure setAllowedInputSourceLocales(newValue: NSArray); message 'setAllowedInputSourceLocales:';
    function allowedInputSourceLocales: NSArray; message 'allowedInputSourceLocales';
    procedure activate; message 'activate';
    procedure deactivate; message 'deactivate';
    function handleEvent (theEvent: NSEvent): ObjCBOOL; message 'handleEvent:';
    procedure discardMarkedText; message 'discardMarkedText';
    procedure invalidateCharacterCoordinates; message 'invalidateCharacterCoordinates';
    function keyboardInputSources: NSArray; message 'keyboardInputSources';
    procedure setSelectedKeyboardInputSource(newValue: NSString); message 'setSelectedKeyboardInputSource:';
    function selectedKeyboardInputSource: NSString; message 'selectedKeyboardInputSource';
    class function localizedNameForInputSource (inputSourceIdentifier: NSString): NSString; message 'localizedNameForInputSource:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTextInputContextKeyboardSelectionDidChangeNotification: NSString { available in 10_6 }; cvar; external;
{$endif}

