
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
  {$DEFINE UTF8_RTL}
  {$DEFINE HasStdErr}
{$ENDIF}


