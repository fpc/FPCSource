{ Parsed from Appkit.framework NSTextStorageScripting.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSTEXTSTORAGESCRIPTING_PAS_T}
{$define NSTEXTSTORAGESCRIPTING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTSTORAGESCRIPTING_PAS_R}
{$define NSTEXTSTORAGESCRIPTING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTSTORAGESCRIPTING_PAS_F}
{$define NSTEXTSTORAGESCRIPTING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTSTORAGESCRIPTING_PAS_S}
{$define NSTEXTSTORAGESCRIPTING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTSTORAGESCRIPTING_PAS_C}
{$define NSTEXTSTORAGESCRIPTING_PAS_C}

{ ScriptingCategory }
  ScriptingCategory = objccategory(NSTextStorage)
    function attributeRuns: NSArray; message 'attributeRuns';
    procedure setAttributeRuns(attributeRuns: NSArray); message 'setAttributeRuns:';
    function paragraphs: NSArray; message 'paragraphs';
    procedure setParagraphs(paragraphs: NSArray); message 'setParagraphs:';
    function words: NSArray; message 'words';
    procedure setWords(words: NSArray); message 'setWords:';
    function characters: NSArray; message 'characters';
    procedure setCharacters(characters: NSArray); message 'setCharacters:';
    function font: NSFont; message 'font';
    procedure setFont(font: NSFont); message 'setFont:';
    function foregroundColor: NSColor; message 'foregroundColor';
    procedure setForegroundColor(color: NSColor); message 'setForegroundColor:';
  end; external;

{$endif}
{$endif}
