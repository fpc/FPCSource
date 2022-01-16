{ Parsed from CoreImage.framework CIPlugIn.h }


{$ifdef TYPES}
type
  CIPlugInPtr = ^CIPlugIn;
{$endif}

{$ifdef CLASSES}

type
  CIPlugIn = objcclass external (NSObject)
  private
    _priv:array[0..7] of pointer;
  public
    class procedure loadAllPlugIns; message 'loadAllPlugIns';
    class procedure loadNonExecutablePlugIns; message 'loadNonExecutablePlugIns';
    class procedure loadPlugIn_allowNonExecutable (url: NSURL; allowNonExecutable: ObjCBOOL); message 'loadPlugIn:allowNonExecutable:'; deprecated 'available in 10.0 and later but deprecated in 10.7';
    class procedure loadPlugIn_allowExecutableCode (url: NSURL; allowExecutableCode: ObjCBOOL); message 'loadPlugIn:allowExecutableCode:';
  end;
{$endif}

