{ Parsed from WebKit.framework WKFoundation.h }

{$if  not (defined(WK_API_ENABLED))}

{$define WK_API_ENABLED}

{$ifdef TYPES}
const
  WK_API_ENABLED = 1;
{$endif}

{$endif}

