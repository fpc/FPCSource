{ Parsed from Quartzcore.framework CACIFilterAdditions.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:07 CET 2010 }


{$ifdef TYPES}
{$ifndef CACIFILTERADDITIONS_PAS_T}
{$define CACIFILTERADDITIONS_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CACIFILTERADDITIONS_PAS_R}
{$define CACIFILTERADDITIONS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CACIFILTERADDITIONS_PAS_F}
{$define CACIFILTERADDITIONS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CACIFILTERADDITIONS_PAS_S}
{$define CACIFILTERADDITIONS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef CACIFILTERADDITIONS_PAS_C}
{$define CACIFILTERADDITIONS_PAS_C}

{ CACIFilterAdditionsCategory }
  CACIFilterAdditionsCategory = objccategory(CIFilter)
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setEnabled (newValue: Boolean); message 'setEnabled:';
    function isEnabled: Boolean; message 'isEnabled';
  end; external;

{$endif}
{$endif}
