{ Parsed from AppKit.framework NSTextStorageScripting.h }

{$ifdef CLASSES}

type
  Scripting = objccategory external (NSTextStorage)
    procedure setAttributeRuns(newValue: NSArray); message 'setAttributeRuns:';
    function attributeRuns: NSArray; message 'attributeRuns';
    procedure setParagraphs(newValue: NSArray); message 'setParagraphs:';
    function paragraphs: NSArray; message 'paragraphs';
    procedure setWords(newValue: NSArray); message 'setWords:';
    function words: NSArray; message 'words';
    procedure setCharacters(newValue: NSArray); message 'setCharacters:';
    function characters: NSArray; message 'characters';
    procedure setFont(newValue: NSFont); message 'setFont:';
    function font: NSFont; message 'font';
    procedure setForegroundColor(newValue: NSColor); message 'setForegroundColor:';
    function foregroundColor: NSColor; message 'foregroundColor';
  end;
{$endif}

