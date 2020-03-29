{ Parsed from InstallerPlugins.framework InstallerSection.h }


{$ifdef TYPES}
type
  InstallerSectionPtr = ^InstallerSection;
{$endif}

{$ifdef TYPES}
type
  InstallerSection_Private = OpaqueType;
  InstallerSection_PrivatePtr = ^InstallerSection_Private;
{$endif}

{$ifdef CLASSES}

type
  InstallerSection = objcclass external (NSObject)
  private
    firstPane: InstallerPane;
    _private: InstallerSection_PrivatePtr;
  public
    function bundle: NSBundle; message 'bundle';
    function title: NSString; message 'title';
    function firstPane_: InstallerPane; message 'firstPane';
    function shouldLoad: ObjCBOOL; message 'shouldLoad';
    procedure willLoadMainNib; message 'willLoadMainNib';
    procedure didLoadMainNib; message 'didLoadMainNib';
    function sharedDictionary: NSMutableDictionary; message 'sharedDictionary';
    function installerState: InstallerState; message 'installerState';
    function activePane: InstallerPane; message 'activePane';
    function gotoPane (pane: InstallerPane): ObjCBOOL; message 'gotoPane:';
  end;
{$endif}

