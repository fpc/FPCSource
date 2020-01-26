{ Parsed from QuartzCore.framework CACIFilterAdditions.h }

{$ifdef CLASSES}

type
  CACIFilterAdditions = objccategory external (CIFilter)
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
  end;
{$endif}

