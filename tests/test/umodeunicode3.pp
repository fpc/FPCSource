{ Used by tmodeunicode3 only: a unit with no mode directive, compiled after a program whose mode selected the
  ansistring default. -Municodestrings gives this unit the unicodestring default, and with it both macros.
  tmodeunicode4 has umodeunicode4, so that each test compiles its unit after its own program. }
unit umodeunicode3;

interface

{$ifndef UNICODE}
  {$error UNICODE is not defined in a unit with the unicodestring default}
{$endif}
{$ifndef FPC_UNICODESTRINGS}
  {$error FPC_UNICODESTRINGS is not defined in a unit with the unicodestring default}
{$endif}

const
  CharSize = SizeOf(Char);

implementation

end.
