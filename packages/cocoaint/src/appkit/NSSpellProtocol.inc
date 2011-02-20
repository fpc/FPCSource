{ Parsed from Appkit.framework NSSpellProtocol.h }

{$ifdef TYPES}
{$ifndef NSSPELLPROTOCOL_PAS_T}
{$define NSSPELLPROTOCOL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSPELLPROTOCOL_PAS_R}
{$define NSSPELLPROTOCOL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSPELLPROTOCOL_PAS_F}
{$define NSSPELLPROTOCOL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSPELLPROTOCOL_PAS_S}
{$define NSSPELLPROTOCOL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSChangeSpellingProtocol = objcprotocol;
  NSIgnoreMisspelledWordsProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSSPELLPROTOCOL_PAS_P}
{$define NSSPELLPROTOCOL_PAS_P}
  
{ NSChangeSpelling Protocol }
  NSChangeSpellingProtocol = objcprotocol external name 'NSChangeSpelling'
    procedure changeSpelling(sender: id); message 'changeSpelling:';
  end;
  
{ NSIgnoreMisspelledWords Protocol }
  NSIgnoreMisspelledWordsProtocol = objcprotocol external name 'NSIgnoreMisspelledWords'
    procedure ignoreSpelling(sender: id); message 'ignoreSpelling:';
  end;
{$endif}
{$endif}
