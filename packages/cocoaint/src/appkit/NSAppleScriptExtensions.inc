{ Parsed from Appkit.framework NSAppleScriptExtensions.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSAPPLESCRIPTEXTENSIONS_PAS_T}
{$define NSAPPLESCRIPTEXTENSIONS_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSAPPLESCRIPTEXTENSIONS_PAS_R}
{$define NSAPPLESCRIPTEXTENSIONS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSAPPLESCRIPTEXTENSIONS_PAS_F}
{$define NSAPPLESCRIPTEXTENSIONS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSAPPLESCRIPTEXTENSIONS_PAS_S}
{$define NSAPPLESCRIPTEXTENSIONS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSAPPLESCRIPTEXTENSIONS_PAS_C}
{$define NSAPPLESCRIPTEXTENSIONS_PAS_C}

{ NSExtensionsCategory }
  NSExtensionsCategory = objccategory(NSAppleScript)
    function richTextSource: NSAttributedString; message 'richTextSource';
  end; external;

{$endif}
{$endif}
