{ Parsed from OSAKit.framework OSALanguageInstance.h }


{$ifdef TYPES}
type
  OSALanguageInstancePtr = ^OSALanguageInstance;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef CLASSES}

type
  OSALanguageInstance = objcclass external (NSObject)
  private
    _private: OSALanguageInstancePrivate;
  public
    class function languageInstanceWithLanguage (language: OSALanguage): instancetype; message 'languageInstanceWithLanguage:';
    function initWithLanguage (language: OSALanguage): instancetype; message 'initWithLanguage:';
    function language: OSALanguage; message 'language';
    function componentInstance: ComponentInstance; message 'componentInstance';
    procedure setDefaultTarget(newValue: NSAppleEventDescriptor); message 'setDefaultTarget:';
    function defaultTarget: NSAppleEventDescriptor; message 'defaultTarget';
    function richTextFromDescriptor (descriptor: NSAppleEventDescriptor): NSAttributedString; message 'richTextFromDescriptor:'; { available in 10_10 }
  end;
{$endif}

{$endif}
