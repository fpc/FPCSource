(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Preferences.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header for the system preferences
 *
 * History:
 *    02/31/95 rsf   Created by Roger Flores
 *    06/26/99 kwk   Added LanguageType.
 *    06/30/99 CS    Added MeasurementSystemType, then added it to both
 *                   CountryPreferencesType and SystemPreferencesType,
 *                   and bumped the version to 7.
 *             CS    Added prefMeasurementSystem to select this
 *                   preference.
 *             CS    Added filler fields to CountryPreferencesType
 *                   structure, since this guy gets saved as a
 *                   resource.
 *    09/20/99 gap   added additional cXXXX country values.
 *    09/20/99 gap   cPRC -> cRepChina.
 *    10/4/99  jmp   Add support for auto-off duration times in seconds
 *                   instead of minutes (the new seconds-based auto-off
 *                   duration time is preferred; the minutes-based auto-ff
 *                   duration times are maintained for compatibility).
 *    10/5/99  jmp   Make the seconds auto-off duration field a UInt16
 *                   instead of a UInt8; also define constants for the
 *                   "pegged" auto-off duration values (when the value
 *                   is pegged, we no longer automatically shut off).
 *    12/23/99 jmp   Fix <> vs. "" problem.
 *    04/30/00 CS    Use LmCountryType instead of CountryType.  Also removed
 *                   deprecated countryNameLength, currencyNameLength, and
 *                   currencySymbolLength, replacing usage with new
 *                   kMaxCountryNameLen, kMaxCurrencyNameLen, and
 *                   kMaxCurrencySymbolLen.
 *    05/16/00 CS    LmCountryType/LmLanguageType are now back to
 *                   CountryType/LanguageType.
 *    08/01/00 CS    Added prefLanguage & prefLocale to selector set, and
 *                   locale to SystemPreferencesType.
 *    08/01/00 kwk   Added timeZoneCountry to SystemPreferencesType, and
 *                   prefTimeZoneCountry to SystemPreferencesChoice.
 *    08/08/00 CS    Moved obsolete CountryPreferencesType to RezConvert.cp,
 *                   since that's the only code that still needs access to
 *                   this private, obsolete resource.
 *    08/08/00 peter Added attentionFlags to SystemPreferencesType, and
 *                   prefAttentionFlags to SystemPreferencesChoice.
 *    11/07/00 grant Added button default assignment resource type.
 *
 *****************************************************************************)

unit preferences;

interface

uses palmos, coretraps, datamgr, localemgr, datetime, localize, attentionmgr, systemmgr;

(***********************************************************************
 *  Constants
 ***********************************************************************)

const
  noPreferenceFound          = -1;

// Preference version constants
  preferenceDataVer2         = 2; // Palm OS 2.0
  preferenceDataVer3         = 3; // Palm OS 3.0
  preferenceDataVer4         = 4; // Palm OS 3.1
  preferenceDataVer5         = 5; // Palm OS 3.2a
  preferenceDataVer6         = 6; // Palm OS 3.2b/3.3
  preferenceDataVer8         = 8; // Palm OS 3.5
  preferenceDataVer9         = 9; // Palm OS 4.0

// Be SURE to update "preferenceDataVerLatest" when adding a new prefs version...
  preferenceDataVerLatest    = preferenceDataVer9;

  defaultAutoOffDuration     = 2;                    // minutes
  defaultAutoOffDurationSecs = 2 * minutesInSeconds; // seconds

  peggedAutoOffDuration      = $FF;                  // minutes (UInt8)
  peggedAutoOffDurationSecs  = $FFFF;                // seconds (UInt16)

  defaultAutoLockType        = 0; //!!!never;                // Never auto lock device
  defaultAutoLockTime        = 0;
  defaultAutoLockTimeFlag    = 0;

// Obsolete after V20
{!!!
  defaultSysSoundLevel       = slOn;
  defaultGameSoundLevel      = slOn;
  defaultAlarmSoundLevel     = slOn;

  defaultSysSoundVolume      = sndMaxAmp;
  defaultGameSoundVolume     = sndMaxAmp;
  defaultAlarmSoundVolume    = sndMaxAmp;
!!!}

type
  MeasurementSystemType = Enum;

const
  unitsEnglish = 0;                 // Feet, yards, miles, gallons, pounds, slugs, etc.
  unitsMetric = Succ(unitsEnglish); //  Meters, liters, grams, newtons, etc.

//  These sound levels must corrospond to positions in the popup lists
//  used by the preferences app.  These are made obsolete after V20.  The
// loudness of the sound is now represented as a number from 0 to sndMaxAmp.

type
  SoundLevelTypeV20 = Enum;

const
  slOn = 0;
  slOff = 1;

// Device Automatic Locking options.
type
  SecurityAutoLockType = Enum;

const
  never = 0;                             // Auto-Lock disabled.
  uponPowerOff = Succ(never);            // Auto lock when the device powers off.
  atPresetTime = Succ(uponPowerOff);     // Auto lock at HH:MM every day.
  afterPresetDelay = Succ(atPresetTime); // Auto lock after x minutes or hours.

// The number format (thousands separator and decimal point).  This defines
// how numbers are formatted and not neccessarily currency numbers (i.e. Switzerland).
type
  AnimationLevelType = Enum;

const
  alOff = 0;                                       // Never show an animation
  alEventsOnly = Succ(alOff);                      // Show an animation for an event
  alEventsAndRandom = Succ(alEventsOnly);          // Also show random animation
  alEventsAndMoreRandom = Succ(alEventsAndRandom); // Show random animations more frequently

type
  SystemPreferencesChoice = Enum;

const
  prefVersion = 0;
  prefCountry = Succ(prefVersion);
  prefDateFormat = Succ(prefCountry);
  prefLongDateFormat = Succ(prefDateFormat);
  prefWeekStartDay = Succ(prefLongDateFormat);
  prefTimeFormat = Succ(prefWeekStartDay);
  prefNumberFormat = Succ(prefTimeFormat);
  prefAutoOffDuration = Succ(prefNumberFormat);                         // prefAutoOffDurationSecs is now preferred (prefAutoOffDuration is in minutes)
  prefSysSoundLevelV20 = Succ(prefAutoOffDuration);                     // slOn or slOff - error beeps and other non-alarm/game sounds
  prefGameSoundLevelV20 = Succ(prefSysSoundLevelV20);                   // slOn or slOff - game sound effects
  prefAlarmSoundLevelV20 = Succ(prefGameSoundLevelV20);                 // slOn or slOff - alarm sound effects
  prefHidePrivateRecordsV33 = Succ(prefAlarmSoundLevelV20);
  prefDeviceLocked = Succ(prefHidePrivateRecordsV33);
  prefLocalSyncRequiresPassword = Succ(prefDeviceLocked);
  prefRemoteSyncRequiresPassword = Succ(prefLocalSyncRequiresPassword);
  prefSysBatteryKind = Succ(prefRemoteSyncRequiresPassword);
  prefAllowEasterEggs = Succ(prefSysBatteryKind);
  prefMinutesWestOfGMT = Succ(prefAllowEasterEggs);                     // deprecated old unsigned minutes EAST of GMT
  prefDaylightSavings = Succ(prefMinutesWestOfGMT);                     // deprecated old daylight saving time rule
  prefRonamaticChar = Succ(prefDaylightSavings);
  prefHard1CharAppCreator = Succ(prefRonamaticChar);                    // App creator for hard key #1
  prefHard2CharAppCreator = Succ(prefHard1CharAppCreator);              // App creator for hard key #2
  prefHard3CharAppCreator = Succ(prefHard2CharAppCreator);              // App creator for hard key #3
  prefHard4CharAppCreator = Succ(prefHard3CharAppCreator);              // App creator for hard key #4
  prefCalcCharAppCreator = Succ(prefHard4CharAppCreator);               // App creator for calculator soft key
  prefHardCradleCharAppCreator = Succ(prefCalcCharAppCreator);          // App creator for hard cradle key
  prefLauncherAppCreator = Succ(prefHardCradleCharAppCreator);          // App creator for launcher soft key
  prefSysPrefFlags = Succ(prefLauncherAppCreator);
  prefHardCradle2CharAppCreator = Succ(prefSysPrefFlags);               // App creator for 2nd hard cradle key
  prefAnimationLevel = Succ(prefHardCradle2CharAppCreator);

  // Additions for PalmOS 3.0:
  prefSysSoundVolume = Succ(prefAnimationLevel);                        // actual amplitude - error beeps and other non-alarm/game sounds
  prefGameSoundVolume = Succ(prefSysSoundVolume);                       // actual amplitude - game sound effects
  prefAlarmSoundVolume = Succ(prefGameSoundVolume);                     // actual amplitude - alarm sound effects
  prefBeamReceive = Succ(prefAlarmSoundVolume);                         // not used - use ExgLibControl with ir(Get/Set)ScanningMode instead
  prefCalibrateDigitizerAtReset = Succ(prefBeamReceive);                // True makes the user calibrate at soft reset time
  prefSystemKeyboardID = Succ(prefCalibrateDigitizerAtReset);           // ID of the preferred keyboard resource
  prefDefSerialPlugIn = Succ(prefSystemKeyboardID);                     // creator ID of the default serial plug-in

  // Additions for PalmOS 3.1:
  prefStayOnWhenPluggedIn = Succ(prefDefSerialPlugIn);                  // don't sleep after timeout when using line current
  prefStayLitWhenPluggedIn = Succ(prefStayOnWhenPluggedIn);             // keep backlight on when not sleeping on line current

  // Additions for PalmOS 3.2:
  prefAntennaCharAppCreator = Succ(prefStayLitWhenPluggedIn);           // App creator for antenna key

  // Additions for PalmOS 3.3:
  prefMeasurementSystem = Succ(prefAntennaCharAppCreator);              // English, Metric, etc.

  // Additions for PalmOS 3.5:
  prefShowPrivateRecords = Succ(prefMeasurementSystem);                 // returns privateRecordViewEnum
  prefAutoOffDurationSecs = Succ(prefShowPrivateRecords);               // auto-off duration in seconds

   // Additions for PalmOS 4.0:
  prefTimeZone = Succ(prefAutoOffDurationSecs);                         // GMT offset in minutes = Succ(); + for east of GMT = Succ(); - for west
  prefDaylightSavingAdjustment = Succ(prefTimeZone);                    // current DST adjustment in minutes (typically 0 or 60)

  prefAutoLockType = Succ(prefDaylightSavingAdjustment);                // Never = Succ(); on poweroff = Succ(); after preset delay or at preset time.
  prefAutoLockTime = Succ(prefAutoLockType);                            // Auto lock preset time or delay.
  prefAutoLockTimeFlag = Succ(prefAutoLockTime);                        // For Minutes or Hours.

  prefLanguage = Succ(prefAutoLockTimeFlag);                            // Language spoken in country selected via Setup app/Formats panel
  prefLocale = Succ(prefLanguage);                                      // Locale for country selected via Setup app/Formats panel

  prefTimeZoneCountry = Succ(prefLocale);                               // Country used to specify time zone.

  prefAttentionFlags = Succ(prefTimeZoneCountry);                       // User prefs for getting user's attention

  prefDefaultAppCreator = Succ(prefAttentionFlags);                     // Default application launched on reset.

type
  SystemPreferencesTypeV10 = record
    version: UInt16;                    // Version of preference info

    // International preferences
    country: CountryType;               // Country the device is in
    dateFormat: DateFormatType;         // Format to display date in
    longDateFormat: DateFormatType;     // Format to display date in
    weekStartDay: UInt8;                // Sunday or Monday
    timeFormat: TimeFormatType;         // Format to display time in
    numberFormat: NumberFormatType;     // Format to display numbers in

    // system preferences
    autoOffDuration: UInt8;             // Time period before shutting off (in minutes)
    sysSoundLevel: SoundLevelTypeV20;   //  slOn or slOff - error beeps and other non-alarm sounds
    alarmSoundLevel: SoundLevelTypeV20; //  slOn or slOff - alarm only
    hideSecretRecords: Boolean;         // True to not display records with
                                        // their secret bit attribute set
    deviceLocked: Boolean;              // Device locked until the system
                                        // password is entered
    reserved1: UInt8;
    sysPrefFlags: UInt16;               // Miscellaneous system pref flags
                                        //  copied into the global GSysPrefFlags
                                        //  at boot time.
    sysBatteryKind: SysBatteryKind;     // The type of batteries installed. This
                                        // is copied into the globals GSysbatteryKind
                                        //  at boot time.
    reserved2: UInt8;
  end;

// Any entries added to this structure must be initialized in
// Prefereces.c:GetPreferenceResource

// DOLATER CS -   We should move SystemPreferencesType, SystemPreferencesTypeV10,
//                PrefGetPreferences, and PrefSetPreferences to a private header
//                file, since any code compiled against an old version of this
//                struct will trash memory when run on a version of the Palm OS
//                that makes the struct longer.

  SystemPreferencesType = record
    version: UInt16;                          // Version of preference info

    // International preferences
    country: CountryType;                     // Country the device is in (see PalmLocale.pas)
    dateFormat: DateFormatType;               // Format to display date in
    longDateFormat: DateFormatType;           // Format to display date in
    weekStartDay: Int8;                       // Sunday or Monday
    timeFormat: TimeFormatType;               // Format to display time in
    numberFormat: NumberFormatType;           // Format to display numbers in

    // system preferences
    autoOffDuration: UInt8;                   // Time period in minutes before shutting off (use autoOffDurationSecs instead).
    sysSoundLevelV20: SoundLevelTypeV20;      //  slOn or slOff - error beeps and other non-alarm/game sounds
    gameSoundLevelV20: SoundLevelTypeV20;     //  slOn or slOff - game sound effects
    alarmSoundLevelV20: SoundLevelTypeV20;    //  slOn or slOff - alarm sound effects
    hideSecretRecords: Boolean;               // True to not display records with
                                              // their secret bit attribute set
    deviceLocked: Boolean;                    // Device locked until the system
                                              // password is entered
    localSyncRequiresPassword: Boolean;       // User must enter password on Pilot
    remoteSyncRequiresPassword: Boolean;      // User must enter password on Pilot
    sysPrefFlags: UInt16;                     // Miscellaneous system pref flags
                                              //  copied into the global GSysPrefFlags
                                              //  at boot time. Constants are
                                              //  sysPrefFlagXXX defined in SystemPrv.h
    sysBatteryKind: SysBatteryKind;           // The type of batteries installed. This
                                              // is copied into the globals GSysbatteryKind
                                              //  at boot time.
    reserved1: UInt8;
    minutesWestOfGMT: UInt32;                 // minutes west of Greenwich
    daylightSavings: DaylightSavingsTypes;    // Type of daylight savings correction
    reserved2: UInt8;
    ronamaticChar: UInt16;                    // character to generate from ronamatic stroke.
                                              //  Typically it popups the onscreen keyboard.
    hard1CharAppCreator: UInt32;              // creator of application to launch in response
                                              //  to the hard button #1. Used by SysHandleEvent.
    hard2CharAppCreator: UInt32;              // creator of application to launch in response
                                              //  to the hard button #2. Used by SysHandleEvent.
    hard3CharAppCreator: UInt32;              // creator of application to launch in response
                                              //  to the hard button #3. Used by SysHandleEvent.
    hard4CharAppCreator: UInt32;              // creator of application to launch in response
                                              //  to the hard button #4. Used by SysHandleEvent.
    calcCharAppCreator: UInt32;               // creator of application to launch in response
                                              //  to the Calculator icon. Used by SysHandleEvent.
    hardCradleCharAppCreator: UInt32;         // creator of application to launch in response
                                              //  to the Cradle button. Used by SysHandleEvent.
    launcherCharAppCreator: UInt32;           // creator of application to launch in response
                                              //  to the launcher button. Used by SysHandleEvent.
    hardCradle2CharAppCreator: UInt32;        // creator of application to launch in response
                                              //  to the 2nd Cradle button. Used by SysHandleEvent.
    animationLevel: AnimationLevelType;       // amount of animation to display

    maskPrivateRecords: Boolean;              // Only meaningful if hideSecretRecords is true.
                                              //true to show a grey placeholder box for secret records.
                                              //was reserved3 - added for 3.5

    // Additions for PalmOS 3.0:
    sysSoundVolume: UInt16;                   //  system amplitude (0 - sndMaxAmp) - taps, beeps
    gameSoundVolume: UInt16;                  //  game amplitude (0 - sndMaxAmp) - explosions
    alarmSoundVolume: UInt16;                 //  alarm amplitude (0 - sndMaxAmp)
    beamReceive: Boolean;                     // False turns off IR sniffing, sends still work.
    calibrateDigitizerAtReset: Boolean;       // True makes the user calibrate at soft reset time
    systemKeyboardID: UInt16;                 // ID of the preferred keyboard resource
    defSerialPlugIn: UInt32;                  // creator ID of the default serial plug-in

    // Additions for PalmOS 3.1:
    stayOnWhenPluggedIn: Boolean;             // don't sleep after timeout when using line current
    stayLitWhenPluggedIn: Boolean;            // keep backlight on when not sleeping on line current

    // Additions for PalmOS 3.2:
    antennaCharAppCreator: UInt32;            // creator of application to launch in response
                                              //  to the antenna key. Used by SysHandleEvent.

    // Additions for PalmOS 3.5:
    measurementSystem: MeasurementSystemType; // metric, english, etc.
    reserved3: UInt8;
    autoOffDurationSecs: UInt16;              // Time period in seconds before shutting off.

    // Additions for PalmOS 4.0:
    timeZone: Int16;                          // minutes east of Greenwich
    daylightSavingAdjustment: Int16;          // current daylight saving correction in minutes
    timeZoneCountry: CountryType;             // country used to specify time zone.
    autoLockType: SecurityAutoLockType;       // Never, on power off, after preset delay or at preset time
    autoLockTime: UInt32;                     // Auto lock preset time or delay.
    autoLockTimeFlag: Boolean;                // For Minutes or Hours.
    language: LanguageType;                   // Language spoken in country selected via Setup app/Formats panel

    attentionFlags: AttnFlagsType;            // User prefs for getting user's attention

    defaultAppCreator: UInt32;                // Creator of the default "safe" app that is launched
                                              // on a reset.
  end;

  SystemPreferencesPtr = ^SystemPreferencesType;

// structure of the resource that holds hard/soft button defaults
type
  ButtonDefaultAppType = record
   keyCode: UInt16;                  // virtual key code of the hard/soft button
   creator: UInt32;                  // app creator code
 end;

type
  ButtonDefaultListType = record
   numButtons: UInt16;                            // number of default button assignments
   button: array [0..0] of ButtonDefaultAppType; // array of button assignments
 end;

//-------------------------------------------------------------------
// Preferences routines
//-------------------------------------------------------------------

function PrefOpenPreferenceDBV10: DmOpenRef; syscall sysTrapPrefOpenPreferenceDBV10;

function PrefOpenPreferenceDB(saved: Boolean): DmOpenRef; syscall sysTrapPrefOpenPreferenceDB;

procedure PrefGetPreferences(p: SystemPreferencesPtr); syscall sysTrapPrefGetPreferences;

procedure PrefSetPreferences(p: SystemPreferencesPtr); syscall sysTrapPrefSetPreferences;

function PrefGetPreference(choice: SystemPreferencesChoice): UInt32; syscall sysTrapPrefGetPreference;

procedure PrefSetPreference(choice: SystemPreferencesChoice; value: UInt32); syscall sysTrapPrefSetPreference;

function PrefGetAppPreferences(creator: UInt32; id: UInt16; prefs: Pointer;
                               var prefsSize: UInt16; saved: Boolean): Int16; syscall sysTrapPrefGetAppPreferences;

function PrefGetAppPreferencesV10(type_: UInt32; version: Int16; prefs: Pointer; prefsSize: UInt16): Boolean; syscall sysTrapPrefGetAppPreferencesV10;

procedure PrefSetAppPreferences(creator: UInt32; id: UInt16; version: Int16;
                                const prefs: Pointer; prefsSize: UInt16; saved: Boolean); syscall sysTrapPrefSetAppPreferences;

procedure PrefSetAppPreferencesV10(creator: UInt32; version: Int16; prefs: Pointer; prefsSize: UInt16); syscall sysTrapPrefSetAppPreferencesV10;

implementation

end.
