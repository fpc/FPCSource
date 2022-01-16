{ Parsed from WebKit.framework WKFoundation.h }

{$if  not (defined(WK_API_ENABLED))}

{$if defined(TARGET_OS_IPHONE)}
{$ifdef TYPES}
const
  WK_API_ENABLED = 1;
{$endif}

{$else}
{$endif}
{$endif}

