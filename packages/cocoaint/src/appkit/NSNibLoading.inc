{ Parsed from Appkit.framework NSNibLoading.h }

{$ifdef TYPES}
{$ifndef NSNIBLOADING_PAS_T}
{$define NSNIBLOADING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSNIBLOADING_PAS_R}
{$define NSNIBLOADING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSNIBLOADING_PAS_F}
{$define NSNIBLOADING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSNIBLOADING_PAS_S}
{$define NSNIBLOADING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSNIBLOADING_PAS_C}
{$define NSNIBLOADING_PAS_C}

{ NSNibLoadingCategory }
  NSNibLoadingCategory = objccategory external (NSBundle)
    class function loadNibFile_externalNameTable_withZone(fileName: NSString; context: NSDictionary; zone_: NSZonePtr): Boolean; message 'loadNibFile:externalNameTable:withZone:';
    class function loadNibNamed_owner(nibName: NSString; owner: id): Boolean; message 'loadNibNamed:owner:';
  end;

{ NSNibAwakingCategory }
  NSNibAwakingCategory = objccategory external (NSObject)
    procedure awakeFromNib; message 'awakeFromNib';
  end;

{$endif}
{$endif}
