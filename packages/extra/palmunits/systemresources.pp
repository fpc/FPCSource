{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: SystemResources.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Include file for both PalmRez and the C Compiler. This file contains
 *  equates used by both tools. When compiling using the C compiler
 *  the variable RESOURCE_COMPILER must be defined.
 *
 * History:
 *    02/27/95 ron   Created by Ron Marianetti
 *    08/04/95 vmk   Added system resource id for Desktop Link user info
 *    02/03/98 tlw   Changed sysFileCDefaultApp from sysFileCMemory which
 *                   no longer exists to sysFileCPreferences.
 *    6/23/98  jhl   Added FlashMgr resource
 *    06/23/98 jhl   Added FlashMgr resource
 *    05/05/99 kwk   Added simulator creator/file types, also the
 *                   Japanese user dict panel creator and the TSM
 *                   library creator.
 *    05/06/99 lyl   Added OEM System File type
 *    06/25/99 kwk   Added sysResIDAppPrefs & sysResIDOverlayFeatures.
 *    07/14/99 kwk   Added sysResTSilkscreen.
 *    08/08/99 kwk   Added sysFileCJEDict.
 *    09/20/99 kwk   Added keyboard feature for reentrancy check.
 *    04/11/00 CS    Added sysFileCLZ77Lib, sysResTCompressedDB, and
 *                   sysResIDCompressedDB to support saving compressed DBs
 *                   inside apps (e.g. GraffitiDemo inside Setup).
 *    05/12/00 kwk   Added sysFileTLocaleModule.
 *    08/29/00 spk   added sysFileCPalmDevice
 *    09/13/00 djk   added bluetooth creator types
 *    10/27/00 CS    Added sysFileCSmsMessenger, sysFileCNetTrace, sysFileCPing,
 *                   sysResTFontMap, sysFileTStdIO, and sysFileCLanguagePicker.
 *             CS    Moved sysFileCSdSpiCard up to live with other DB creators.
 *
 *****************************************************************************)

unit systemresources;

interface

//-----------------------------------------------------------
// This section is common to both the C and Resource Compiler
//-----------------------------------------------------------

//................................................................
// File types and creators
//
//  Each database shall have a creator ID and a type.
//
//  The creator ID shall establish which application, patch, or extension
//  a particular database is associated with.  The creator ID should identify
//  the application/patch/extension, NOT who created it.
//
//  The type will determine which part of an application,
//  patch, or extension a particular database is.
//
//  There can be only one database per application, patch, or extension
//  which has type 'application', 'patch', or 'extension'.
//
//  Creators:
//
//  ROM-based applications created by Palm Computing have all-lower case
//  creator ID's.  Third-party applications have creator ID's which
//  are either all caps, or mixed case.  The same requirements go for
//  system patches and extensions.
//
//  All applications, system patches and extensions shall have unique creator
//  ID's.
//
//  Types:
//
//  'Application', 'Extension', and 'Patch' file/database types for ROM-based
//  apps shall be all-lower case (they are defined below).  Other
//  file/database types must be mixed-case,
//  or all caps.  These other types are internal to the applications, and
//  therefore the system is unconcerned with their exact values.
//................................................................
{$define Rsc := }
const
  sysFileCSystem                  = Rsc('psys'); // Creator type for System files
  sysFileCOEMSystem               = Rsc('poem'); // Creator type for OEM System files
  sysFileCPalmDevice              = Rsc('pdvc'); // Creator type for Palm Devices, analogous to 'poem'
  sysFileCGraffiti                = Rsc('graf'); // Creator type for Graffiti databases
  sysFileCSystemPatch             = Rsc('ptch'); // Creator for System resource file patches

  sysFileCCalculator              = Rsc('calc'); // Creator type for Calculator App
  sysFileCSecurity                = Rsc('secr'); // Creator type for Security App
  sysFileCPreferences             = Rsc('pref'); // Creator type for Preferences App
  sysFileCAddress                 = Rsc('addr'); // Creator type for Address App
  sysFileCToDo                    = Rsc('todo'); // Creator type for To Do App
  sysFileCDatebook                = Rsc('date'); // Creator type for Datebook App
  sysFileCMemo                    = Rsc('memo'); // Creator type for MemoPad App
  sysFileCSync                    = Rsc('sync'); // Creator type for HotSync App
  sysFileCMemory                  = Rsc('memr'); // Creator type for Memory App
  sysFileCMail                    = Rsc('mail'); // Creator type for Mail App
  sysFileCExpense                 = Rsc('exps'); // Creator type for Expense App
  sysFileCLauncher                = Rsc('lnch'); // Creator type for Launcher App
  sysFileCClipper                 = Rsc('clpr'); // Creator type for clipper app.
  sysFileCDial                    = Rsc('dial'); // Creator type for dial app.
  sysFileCSetup                   = Rsc('setp'); // Creator type for setup app.
  sysFileCActivate                = Rsc('actv'); // Creator type for activation app.
  sysFileCGenenicActivate         = Rsc('gafd'); // New Generic Activation application working for all Palm models
  sysFileCFlashInstaller          = Rsc('fins'); // Creator type for FlashInstaller app.
  sysFileCRFDiag                  = Rsc('rfdg'); // Creator type for RF diagnostics app.
  sysFileCMessaging               = Rsc('msgs'); // Creator type for Messaging App
  sysFileCModemFlashTool          = Rsc('gsmf'); // Creator type for Palm V modem flash app.
  sysFileCJEDict                  = Rsc('dict'); // Creator type for JEDict app.
  sysFileHotSyncServer            = Rsc('srvr'); // Creator type for HotSync(R) Server app.
  sysFileHotSyncServerUpdate      = Rsc('hssu'); // Creator type for HotSync(R) Server update app.

   sysFileCCardInfo               = Rsc('cinf'); // Creator type for the Card info app.
   sysFileCPhone                  = Rsc('fone'); // Creator type for integrated phone components.
   sysFileCSmsMessenger           = Rsc('smsm'); // Creator type for SMS messenger app.
   sysFileCNetTrace               = Rsc('nett'); // Creator type for Net Trace StdIO app.
   sysFileCPing                   = Rsc('ping'); // Creator type for Ping StdIO app.
   sysFileCLanguagePicker         = Rsc('lpkr'); // Creator type for Language Picker app.

// The following apps are manufacturing, calibration and maintenance related
   sysFileCMfgExtension           = Rsc('mfx1'); // Creator type for Manufacturing Extension.
   sysFileCMfgFunctional          = Rsc('mfgf'); // Creator type for Manufacturing functional test autostart app.
   sysFileCMfgCalibration         = Rsc('mfgc'); // Creator type for Manufacturing radio calibration app.

// Demo Apps
  sysFileCGraffitiDemo            = Rsc('gdem'); // Creator type for Graffiti Demo
  sysFileCMailDemo                = Rsc('mdem'); // Creator type for Mail Demo

  sysFileCFirstApp                = sysFileCPreferences; // Creator type for First App after reset
  sysFileCAltFirstApp             = sysFileCSetup;       // Creator type for First alternate App after reset (with hard key pressed)
  sysFileCDefaultApp              = sysFileCPreferences; // Creator type for Default app
  sysFileCDefaultButton1App       = sysFileCDatebook;    // Creator type for dflt hard button 1 app
  sysFileCDefaultButton2App       = sysFileCAddress;     // Creator type for dflt hard button 2 app
  sysFileCDefaultButton3App       = sysFileCToDo;        // Creator type for dflt hard button 3 app
  sysFileCDefaultButton4App       = sysFileCMemo;        // Creator type for dflt hard button 4 app
  sysFileCDefaultCalcButtonApp    = sysFileCCalculator;  // Creator type for dflt calc button app
  sysFileCDefaultCradleApp        = sysFileCSync;        // Creator type for dflt hot sync button app
  sysFileCDefaultModemApp         = sysFileCSync;        // Creator type for dflt modem button app
  sysFileCDefaultAntennaButtonApp = sysFileCLauncher;    // Creator type for dflt antenna up button app
  sysFileCNullApp                 = Rsc('0000'); // Creator type for non-existing app
  sysFileCSimulator               = Rsc('????'); // Creator type for Simulator files (app.tres, sys.tres)
                                                 //  '????' does not compile with VC++ (Elaine Server)

  sysFileCDigitizer               = Rsc('digi'); // Creator type for Digitizer Panel
  sysFileCDateTime                = Rsc('dttm'); // Creator type for Date & Time Panel
  sysFileCGeneral                 = Rsc('gnrl'); // Creator type for General Panel
  sysFileCFormats                 = Rsc('frmt'); // Creator type for Formats Panel
  sysFileCShortCuts               = Rsc('shct'); // Creator type for ShortCuts Panel
  sysFileCButtons                 = Rsc('bttn'); // Creator type for Buttons Panel
  sysFileCOwner                   = Rsc('ownr'); // Creator type for Owner Panel
  sysFileCModemPanel              = Rsc('modm'); // Creator type for Modem Panel
  sysFileCDialPanel               = Rsc('dial'); // Creator type for Dial Panel
  sysFileCNetworkPanel            = Rsc('netw'); // Creator type for Network Panel
  sysFileCWirelessPanel           = Rsc('wclp'); // Creator type for the wireless Panel.
  sysFileCUserDict                = Rsc('udic'); // Creator type for the UserDict panel.
  sysFileCPADHtal                 = Rsc('hpad'); // Creator type for PAD HTAL lirary
  sysFileCTCPHtal                 = Rsc('htcp'); // Creator type for TCP HTAL lirary
  sysFileCRELHtal                 = Rsc('hrel'); // Creator type for REL HTAL library
  sysFileCMineHunt                = Rsc('mine'); // Creator type for MineHunt App
  sysFileCPuzzle15                = Rsc('puzl'); // Creator type for Puzzle "15" App
  sysFileCOpenLibInfo             = Rsc('olbi'); // Creator type for Feature Manager features
                                                                                                   // used for saving open library info under PalmOS v1.x
  sysFileCHwrFlashMgr             = Rsc('flsh'); // Creator type for HwrFlashMgr features
  sysFileCPhonePanel              = Rsc('phop'); // Creator type for Phone Panel

// Added by BGT, 08/01/2000
  sysFileDRAMFixOriginal          = Rsc('mmfx'); // Creator type for 1.0 DRAM Fix
  sysFileDRAMFix                  = Rsc('dmfx'); // Creator type for 1.0.3 DRAM Fix and later

// Libraries.  If the resource used by these are expected to be treated as part of
// the system's usage then the Memory app must be changed.
  sysFileTLibrary                 = Rsc('libr'); // File type of Shared Libraries
  sysFileTLibraryExtension        = Rsc('libx'); // File type of library extensions

  sysFileCNet                     = Rsc('netl'); // Creator type for Net (TCP/IP) Library
  sysFileCRmpLib                  = Rsc('netp'); // Creator type for RMP Library (NetLib plug-in)
  sysFileCINetLib                 = Rsc('inet'); // Creator type for INet Library
  sysFileCSecLib                  = Rsc('secl'); // Creator type for Ir Library
  sysFileCWebLib                  = Rsc('webl'); // Creator type for Web Library
  sysFileCIrLib                   = Rsc('irda'); // Creator type for Ir Library

  sysFileCBtLib                   = Rsc('blth'); // Creator type for Bt Library
  sysFileCBtTransLib              = Rsc('bttx'); // Creator for the Bt HCI Transport library
  sysFileCLocalLib                = Rsc('locl'); // Creator type for Local exchange library
  sysFileCLz77Lib                 = Rsc('lz77'); // Creator type for LZ77 Library (Registered)
  sysFileCSmsLib                  = Rsc('smsl'); // Creator type for SMS Library
  sysFileCBtExgLib                = Rsc('btex'); // Creator type for Bluetooth Exchange Library
  sysFileCPdiLib                  = Rsc('pdil'); // Creator type for PDI Library
  sysFileCTelMgrLib               = Rsc('tmgr'); // Creator type for Telephony Manager Library
  sysFileCTelTaskSerial           = Rsc('spht'); // Creator type for Serial Telephony Task
  sysFileTTelTaskSerial           = Rsc('ttsk'); // File type for Serial Telephony Task
  sysFileCBaseATDriver            = Rsc('patd'); // Creator type for the Base AT Driver
  sysFileTBaseATDriver            = Rsc('patd'); // File type for the Base AT Driver (same as Creator)
  sysFileCStandardGsm             = Rsc('stgd'); // Creator type for the Standard GSM Driver
  sysFileTPhoneDriver             = Rsc('pdrv'); // File type for Phone Drivers of Telephony Task

  sysFileCSerialMgr               = Rsc('smgr'); // Creator for SerialMgrNew used for features.
  sysFileCSerialWrapper           = Rsc('swrp'); // Creator type for Serial Wrapper Library.
  sysFileCIrSerialWrapper         = Rsc('iwrp'); // Creator type for Ir Serial Wrapper Library.
  sysFileCTextServices            = Rsc('tsml'); // Creator type for Text Services Library.

  sysFileTUartPlugIn              = Rsc('sdrv'); // File type for SerialMgrNew physical port plug-in.
  sysFileTVirtPlugin              = Rsc('vdrv'); // Flir type for SerialMgrNew virtual port plug-in.
  sysFileCUart328                 = Rsc('u328'); // Creator type for '328 UART plug-in
  sysFileCUart328EZ               = Rsc('u8EZ'); // Creator type for '328EZ UART plug-in
  sysFileCUart650                 = Rsc('u650'); // Creator type for '650 UART plug-in
  sysFileCVirtIrComm              = Rsc('ircm'); // Creator type for IrComm virtual port plug-in.

  sysFileCVirtRfComm              = Rsc('rfcm'); // Creator type for RfComm (Bluetooth) virtual port plug-in.
  sysFileCBtConnectPanelHelper    = Rsc('btcp'); // Creator type for the Bt Connection Panel helper app.
  sysFileCPDIUSBD12               = Rsc('pusb'); // Creator type for USB database
  sysPortUSBDesktop               = Rsc('usbd'); // Creator type for USB Desktop
  sysPortUSBConsole               = Rsc('usbc'); // Creator type for USB Console
  sysPortUSBPeripheral            = Rsc('usbp'); // Creator type for USB Peripheral
  sysFileCsyscallConnector       = Rsc('econ'); // Creator type for the syscall connector
  sysFileCExpansionMgr            = Rsc('expn'); // Creator of Expansion Manager extension database
  sysFileCVFSMgr                  = Rsc('vfsm'); // Creator code for VFSMgr...
  sysFileCFATFS                   = Rsc('tatf'); // Creator type for FAT filesystem library
  sysFileCSdSpiCard               = Rsc('sdsd'); // Creator type for Slot Driver: SD bus, SPI mode, memory cards
  sysFileCSlotDriverPnps          = Rsc('pnps'); // Creator ID for Pnps Serial Peripheral Slot Driver

  sysFileTSystem                  = Rsc('rsrc'); // File type for Main System File
  sysFileTSystemPatch             = Rsc('ptch'); // File type for System resource file patches
  sysFileTKernel                  = Rsc('krnl'); // File type for System Kernel (AMX)
  sysFileTBoot                    = Rsc('boot'); // File type for SmallROM System File
  sysFileTSmallHal                = Rsc('shal'); // File type for SmallROM HAL File
  sysFileTBigHal                  = Rsc('bhal'); // File type for Main ROM HAL File
  sysFileTSplash                  = Rsc('spls'); // File type for Main ROM Splash File
  sysFileTUIAppShell              = Rsc('uish'); // File type for UI Application Shell
  sysFileTOverlay                 = Rsc('ovly'); // File type for UI overlay database
  sysFileTExtension               = Rsc('extn'); // File type for System Extensions
  sysFileTApplication             = Rsc('appl'); // File type for applications
  sysFileTPanel                   = Rsc('panl'); // File type for preference panels
  sysFileTSavedPreferences        = Rsc('sprf'); // File type for saved preferences
  sysFileTPreferences             = Rsc('pref'); // File type for preferences
  sysFileTMidi                    = Rsc('smfr'); // File type for Standard MIDI File record databases
  sysFileTpqa                     = Rsc('pqa '); // File type for the PQA files.
  sysFileTLocaleModule            = Rsc('locm'); // File type for locale modules.
  sysFileTActivationPlugin        = Rsc('actp'); // File type for activation plug-ins.

  sysFileTUserDictionary          = Rsc('dict'); // File type for input method user dictionary.
  sysFileTLearningData            = Rsc('lean'); // File type for input method learning data.

  sysFileTGraffitiMacros          = Rsc('macr'); //  Graffiti Macros database

  sysFileTHtalLib                 = Rsc('htal'); //  HTAL library

  sysFileTExgLib                  = Rsc('exgl'); // Type of Exchange libraries

  sysFileTSlotDriver              = Rsc('libs'); // File type for slot driver libraries
  sysFileTFileSystem              = Rsc('libf'); // File type for file system libraries

  sysFileTFileStream              = Rsc('strm'); //  Default File Stream database type

  sysFileTTemp                    = Rsc('temp'); //  Temporary database type; as of Palm OS 4.0, the
                                                                                                   //  system WILL automatically delete any db's of
                                                                                                   //  before exiting to protect valuable storage space)
// Begin Change - BGT 03/21/2000

  sysFileTNetworkPanelPlugin      = Rsc('nppi'); // File type for network preference panel plug-ins

// End Change - BGT 03/21/2000

  sysFileTScriptPlugin            = Rsc('scpt'); // File type for plugin to the Network Panel to
                                                                                                   // extend scripting capabilities.
  sysFileTStdIO                   = Rsc('sdio'); // File type for standard IO apps

  sysFileTSimulator               = Rsc('????'); // File type for Simulator files (app.tres, sys.tres)
                                                                                                   // '????' does not compile with VC++ (Elaine Server)

//................................................................
// Resource types and IDs
//................................................................
  sysResTBootCode                 = Rsc('boot'); // Resource type of boot resources
  sysResIDBootReset               = 10000; // Reset code
  sysResIDBootInitCode            = 10001; // Init code
  sysResIDBootSysCodeStart        = 10100; // System code resources start here
  sysResIDBootSysCodeMin          = 10102; // IDs 'Start' to this must exist!!
  sysResIDBootUICodeStart         = 10200; // UI code resources start here
  sysResIDBootUICodeMin           = 10203; // IDs 'Start' to this must exist!!

  sysResIDBootHAL                 = 19000; // HAL initial code resource (from HAL.prc)
  sysResIDBootHALCodeStart        = 19100; // start of additional high-level HAL code resources

  sysResIDBitmapSplash            = 19000; // ID of (boot) splash screen bitmap
  sysResIDBitmapConfirm           = 19001; // ID of hard reset confirmation bitmap

  sysResTAppPrefs                 = Rsc('pref'); // Resource type of App preferences resources
  sysResIDAppPrefs                = 0;     // Application preference

  sysResTExtPrefs                 = Rsc('xprf'); // Resource type of extended preferences
  sysResIDExtPrefs                = 0;     // Extended preferences

  sysResTAppCode                  = Rsc('code'); // Resource type of App code resources
  sysResTAppGData                 = Rsc('data'); // Resource type of App global data resources

  sysResTExtensionCode            = Rsc('extn'); // Resource type of Extensions code
  sysResTExtensionOEMCode         = Rsc('exte'); // Resource type of OEM Extensions code

  sysResTFeatures                 = Rsc('feat'); // Resource type of System features table
  sysResIDFeatures                = 10000; // Resource ID of System features table
  sysResIDOverlayFeatures         = 10001; // Resource ID of system overlay feature table.

  sysResTLibrary                  = Rsc('libr'); // Resource type of System Libraries
// sysResIDLibrarySerMgr328       = 10000; // Dragonball (68328) UART
// sysResIDLibrarySerMgr681       = 10001; // 68681 UART
// sysResIDLibraryRMPPlugIn       = 10002; // Reliable Message Protocol NetLib Plug-in

  sysResTSilkscreen               = Rsc('silk'); // Resource type of silkscreen info.

  sysResTGrfTemplate              = Rsc('tmpl'); // Graffiti templates "file"
  sysResIDGrfTemplate             = 10000; // Graffiti templates "file" ID
  sysResTGrfDictionary            = Rsc('dict'); // Graffiti dictionary "file"
  sysResIDGrfDictionary           = 10000; // Graffiti dictionary "file" ID
  sysResIDGrfDefaultMacros        = 10000; // sysResTDefaultDB resource with Graffiti Macros database

  sysResTDefaultDB                = Rsc('dflt'); // Default database resource type
  sysResIDDefaultDB               = 1;     // resource ID of sysResTDefaultDB in each app
  sysResTCompressedDB             = Rsc('cpdb'); // Compressed database resource type
  sysResIDCompressedDB            = 10000; // resource ID of first sysResTCompressedDB

  sysResTErrStrings               = Rsc('tSTL'); // list of error strings
  sysResIDErrStrings              = 10000; // resource ID is (errno>>8)+sysResIDErrStrings

  sysResIDOEMDBVersion            = 20001; // resource ID of "tver" and "tint" versions in OEM stamped databases

  sysResTButtonDefaults           = Rsc('hsbd'); // Hard/soft button default apps
  sysResIDButtonDefaults          = 10000; // resource ID of system button defaults resource

// System Preferences
  sysResTSysPref                  = sysFileCSystem;
  sysResIDSysPrefMain             = 0; // Main preferences
  sysResIDSysPrefPassword         = 1; // Password
  sysResIDSysPrefFindStr          = 2; // Find string
  sysResIDSysPrefCalibration      = 3; // Digitizer calibration.
  sysResIDDlkUserInfo             = 4; // Desktop Link user information.
  sysResIDDlkLocalPC              = 5; // Desktop Link local PC host name
  sysResIDDlkCondFilterTab        = 6; // Desktop Link conduit filter table
  sysResIDModemMgrPref            = 7; // Modem Manager preferences
  sysResIDDlkLocalPCAddr          = 8; // Desktop Link local PC host address
  sysResIDDlkLocalPCMask          = 9; // Desktop Link local PC host subnet mask

// These prefs store parameters to pass to an app when launched with a button
  sysResIDButton1Param            = 10; // Parameter for hard button 1 app
  sysResIDButton2Param            = 11; // Parameter for hard button 2 app
  sysResIDButton3Param            = 12; // Parameter for hard button 3 app
  sysResIDButton4Param            = 13; // Parameter for hard button 4 app
  sysResIDCalcButtonParam         = 14; // Parameter for calc button app
  sysResIDCradleParam             = 15; // Parameter for hot sync button app
  sysResIDModemParam              = 16; // Parameter for modem button app
  sysResIDAntennaButtonParam      = 17; // Parameter for antenna up button app

// New for Color, user's color preferences
  sysResIDPrefUIColorTableBase    = 17; // base + depth = ID of actual pref
  sysResIDPrefUIColorTable1       = 18; // User's UI colors for 1bpp displays
  sysResIDPrefUIColorTable2       = 19; // User's UI colors for 2bpp displays
  sysResIDPrefUIColorTable4       = 21; // User's UI colors for 4bpp displays
  sysResIDPrefUIColorTable8       = 25; // User's UI colors for 8bpp displays
  sysResIDSysPrefPasswordHint     = 26; // Password hint
  sysResIDSysPrefPasswordHash     = 27; // Password hash (MD5)

// FlashMgr Resources - old
  sysResTFlashMgr                 = Rsc('flsh');
  sysResIDFlashMgrWorkspace       = 1; // RAM workspace during flash activity

// FlashMgr Resources - new
  sysResTHwrFlashIdent            = Rsc('flid'); // Flash identification code resource
  sysResIDHwrFlashIdent           = 10000; // Flash identification code resource

  sysResTHwrFlashCode             = Rsc('flcd'); // Flash programming code resource
                                                                                                   // (resource ID determined by device type)
// FontMgr Resources
  sysResTFontMap                  = Rsc('fntm'); // Font map resource

// OEM Feature type and id.
  sysFtrTOEMSys                   = sysFileCOEMSystem;
  sysFtrIDOEMSysHideBatteryGauge  = 1;

// Onscreen keyboard features
  sysFtrTKeyboard                 = Rsc('keyb');
  sysFtrIDKeyboardActive          = 1; // Boolean value, true => keyboard is active.
                                       // Currently only used for Japanese.

// Activation status values.
  sysActivateStatusFeatureIndex   = 1;
  sysActivateNeedGeorgeQuery      = 0;
  sysActivateNeedMortyQuery       = 1;
  sysActivateFullyActivated       = 2;

  sysMaxUserDomainNameLength      = 64;

// Current clipper feature indeces
  sysClipperPQACardNoIndex        = 1;
  sysClipperPQADbIDIndex          = 2;

//-----------------------------------------------------------
// This section is only valid when running the resource compiler
//
// Actually, this section is obsolete.  Instear, .r files should
// inlude SysResTypes.rh to get these definitions.
//
//-----------------------------------------------------------

implementation

end.
