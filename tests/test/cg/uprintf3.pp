unit uprintf3;

{$mode macpas}

interface

{$calling mwpascal}

{$ifdef WINDOWS}
const
{$ifdef wince}
  CrtLib = 'coredll.dll';
{$else}
  CrtLib = 'msvcrt.dll';
{$endif}

procedure printf(const formatstr : pchar; ...); external CrtLib name 'printf';
procedure sprintf(p : pchar;const formatstr : pchar; ...); external CrtLib name 'sprintf';
const
  int64prefix='I64';
{$else}
{$linklib c}
{$ifndef darwin}
procedure printf(const formatstr : pchar; ...); external;
procedure sprintf(p : pchar;const formatstr : pchar; ...); external;
{$else darwin}
procedure printf(const formatstr : pchar; ...); external name '_printf';
procedure sprintf(p : pchar;const formatstr : pchar; ...); external name '_sprintf';
{$endif darwin}
const
  int64prefix='ll';
{$endif}


end.
