{ Parsed from Quartzcore.framework CACIFilterAdditions.h }

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
  CACIFilterAdditionsCategory = objccategory external (CIFilter)
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setEnabled (newValue: Boolean); message 'setEnabled:';
    function isEnabled: Boolean; message 'isEnabled';
  end;

{$endif}
{$endif}
