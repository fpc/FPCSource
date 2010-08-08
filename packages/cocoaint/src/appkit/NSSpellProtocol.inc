{ Parsed from Appkit.framework NSSpellProtocol.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
