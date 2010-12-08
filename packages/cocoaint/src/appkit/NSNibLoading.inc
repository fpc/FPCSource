{ Parsed from Appkit.framework NSNibLoading.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


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
  NSNibLoadingCategory = objccategory(NSBundle)
    class function loadNibFile_externalNameTable_withZone(fileName: NSString; context: NSDictionary; zone_: NSZonePtr): Boolean; message 'loadNibFile:externalNameTable:withZone:';
    class function loadNibNamed_owner(nibName: NSString; owner: id): Boolean; message 'loadNibNamed:owner:';
  end; external;

{ NSNibAwakingCategory }
  NSNibAwakingCategory = objccategory(NSObject)
    procedure awakeFromNib; message 'awakeFromNib';
  end; external;

{$endif}
{$endif}
