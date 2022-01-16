{ Parsed from AppKit.framework NSNibLoading.h }

{$ifdef CLASSES}

type
  NSNibLoading = objccategory external (NSBundle)
    function loadNibNamed_owner_topLevelObjects (nibName: NSString; owner: id; topLevelObjects: NSArrayPtr): ObjCBOOL; message 'loadNibNamed:owner:topLevelObjects:'; { available in 10_8 }
  end;

type
  NSNibAwaking = objccategory external (NSObject)
    procedure awakeFromNib; message 'awakeFromNib';
    procedure prepareForInterfaceBuilder; message 'prepareForInterfaceBuilder'; { available in 10_10 }
  end;

type
  NSNibLoadingDeprecated = objccategory external (NSBundle)
    class function loadNibFile_externalNameTable_withZone (fileName: NSString; context: NSDictionary; zone: NSZonePtr): ObjCBOOL; message 'loadNibFile:externalNameTable:withZone:'; deprecated 'in 10_0, 10_8';
    class function loadNibNamed_owner (nibName: NSString; owner: id): ObjCBOOL; message 'loadNibNamed:owner:'; deprecated 'in 10_0, 10_8';
    function loadNibFile_externalNameTable_withZone_ (fileName: NSString; context: NSDictionary; zone: NSZonePtr): ObjCBOOL; message 'loadNibFile:externalNameTable:withZone:'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

