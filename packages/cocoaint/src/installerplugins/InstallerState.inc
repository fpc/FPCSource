{ Parsed from InstallerPlugins.framework InstallerState.h }


{$ifdef TYPES}
type
  InstallerStatePtr = ^InstallerState;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  InstallerState_Choice_Identifier: NSString; cvar; external;
  InstallerState_Choice_Installed: NSString; cvar; external;
  InstallerState_Choice_CustomLocation: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  InstallerState_Private = OpaqueType;
  InstallerState_PrivatePtr = ^InstallerState_Private;
{$endif}

{$ifdef CLASSES}

type
  InstallerState = objcclass external (NSObject)
  private
    _private: InstallerState_PrivatePtr;
  public
    function licenseAgreed: ObjCBOOL; message 'licenseAgreed';
    function licenseAgreedLanguage: NSString; message 'licenseAgreedLanguage';
    function targetVolumePath: NSString; message 'targetVolumePath';
    function targetPath: NSString; message 'targetPath';
    function choiceDictionaries: NSArray; message 'choiceDictionaries';
    function choiceDictionaryForIdentifier (choiceIdentifier: NSString): NSDictionary; message 'choiceDictionaryForIdentifier:';
    function installStarted: ObjCBOOL; message 'installStarted';
    function installSucceeded: ObjCBOOL; message 'installSucceeded';
  end;
{$endif}

