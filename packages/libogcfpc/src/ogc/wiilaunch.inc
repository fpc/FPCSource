{$ifdef OGC_INTERFACE}

{$ifdef HW_RVL}

const
  WII_ENOTINIT  = -$9001;
  WII_EINTERNAL = -$9002;
  WII_ECHECKSUM = -$9003;
  WII_EINSTALL  = -$9004;
  WII_E2BIG     = -$9005;

// you probably shouldn't use anything not in this list, since those may change
// these are guaranteed to exist because Nintendo hardcodes them
// any category not on this list will cause a hang when the settings menu tries to do the animation
// however, settings items contained in one of the following categories will work
// nonexistent items will cause a 404
{$define SETTINGS_CALENDAR := 'Calendar/Calendar_index.html'}
{$define SETTINGS_DISPLAY  := 'Display/Display_index.html'}
{$define SETTINGS_SOUND    := 'Sound/Sound_index.html'}
{$define SETTINGS_PARENTAL := 'Parental_Control/Parental_Control_index.html'}
{$define SETTINGS_INTERNET := 'Internet/Internet_index.html'}
{$define SETTINGS_WC24     := 'WiiConnect24/Wiiconnect24_index.html'}
{$define SETTINGS_UPDATE   := 'Update/Update_index.html'}


function WII_Initialize(): cint32; cdecl; external;
function WII_ReturnToMenu(): cint32; cdecl; external;
function WII_ReturnToSettings(): cint32; cdecl; external;
function WII_ReturnToSettingsPage(const page: pcchar): cint32; cdecl; external;
function WII_LaunchTitle(titleID: cuint64): cint32; cdecl; external;
function WII_LaunchTitleWithArgs(titleID: cuint64; launchcode: cint; par: array of const): cint32; cdecl; external;
function WII_OpenURL(const url: pcchar): cint32; cdecl; external;

{$endif HW_RVL}

{$endif OGC_INTERFACE}
