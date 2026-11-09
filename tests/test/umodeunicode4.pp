{ Used by tmodeunicode4, as umodeunicode3 by tmodeunicode3: a unit with no mode directive, compiled after a
  program whose mode sets neither string default. -Municodestrings gives this unit the unicodestring
  default, and with it both macros. A unit of its own, so that each test compiles it after its program. }
unit umodeunicode4;

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
