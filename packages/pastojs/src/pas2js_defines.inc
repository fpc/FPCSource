
{$inline on}

{$IFDEF Windows}
  {$define CaseInsensitiveFilenames}
  {$define HasUNCPaths}
{$ENDIF}
{$IFDEF darwin}
  {$define CaseInsensitiveFilenames}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
  {$DEFINE NotLiteralFilenames} // e.g. HFS+ normalizes file names
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE UsePChar}
  {$DEFINE HasInt64}
  {$DEFINE HasStreams}
  {$DEFINE UTF8_RTL}
  {$DEFINE HasStdErr}
  {$DEFINE HasPas2jsFiler}
  {$DEFINE HASFILESYSTEM}
{$ENDIF}

{$IFDEF NODEJS}
{$DEFINE HASFILESYSTEM}
{$ENDIF}


