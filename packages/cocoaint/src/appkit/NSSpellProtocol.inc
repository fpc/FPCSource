{ Parsed from Appkit.framework NSSpellProtocol.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
  NSChangeSpellingProtocol = objcprotocol
    procedure changeSpelling(sender: id); message 'changeSpelling:';
  end; external name 'NSChangeSpelling';
  
{ NSIgnoreMisspelledWords Protocol }
  NSIgnoreMisspelledWordsProtocol = objcprotocol
    procedure ignoreSpelling(sender: id); message 'ignoreSpelling:';
  end; external name 'NSIgnoreMisspelledWords';
{$endif}
{$endif}
