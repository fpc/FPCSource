{$MODE MACPAS}
unit MacOS;

{API to Mac OS}
{This unit include (almost) all units available for classic Mac OS (non-Caron).
 Note that some units not present below, might be indirectly included.}

interface

{$SETC UsingIncludes:= 1}

{$IFC UNDEFINED __BALLOONS__}
{$I Balloons.p}
{$ENDC}

{$IFC UNDEFINED __EVENTS__}
{$I Events.p}
{$ENDC}

{$IFC UNDEFINED __PROCESSES__}
{$I Processes.p}
{$ENDC}

{$IFC UNDEFINED __NOTIFICATION__}
{$I Notification.p}
{$ENDC}

{$IFC UNDEFINED __DRAG__}
{$I Drag.p}
{$ENDC}

{$IFC UNDEFINED __ICONS__}
{$I Icons.p}
{$ENDC}

{$IFC UNDEFINED __CONTROLS__}
{$I Controls.p}
{$ENDC}

{$IFC UNDEFINED __APPEARANCE__}
{$I Appearance.p}
{$ENDC}

{$IFC UNDEFINED __MACWINDOWS__}
{$I MacWindows.p}
{$ENDC}

{$IFC UNDEFINED __TEXTEDIT__}
{$I TextEdit.p}
{$ENDC}

{$IFC UNDEFINED __MENUS__}
{$I Menus.p}
{$ENDC}

{$IFC UNDEFINED __DIALOGS__}
{$I Dialogs.p}
{$ENDC}

{$IFC UNDEFINED __LISTS__}
{$I Lists.p}
{$ENDC}

{$IFC UNDEFINED __TEXTSERVICES__}
{$I TextServices.p}
{$ENDC}

{$IFC UNDEFINED __SCRAP__}
{$I Scrap.p}
{$ENDC}

{$IFC UNDEFINED __MACTEXTEDITOR__}
{$I MacTextEditor.p}
{$ENDC}

{$IFC UNDEFINED __CONTROLDEFINITIONS__}
{$I ControlDefinitions.p}
{$ENDC}

{$IFC UNDEFINED __TSMTE__}
{$I TSMTE.p}
{$ENDC}

{$IFC UNDEFINED __TRANSLATIONEXTENSIONS__}
{$I TranslationExtensions.p}
{$ENDC}

{$IFC UNDEFINED __TRANSLATION__}
{$I Translation.p}
{$ENDC}

{$IFC UNDEFINED __AEINTERACTION__}
{$I AEInteraction.p}
{$ENDC}

{$IFC UNDEFINED __TYPESELECT__}
{$I TypeSelect.p}
{$ENDC}

{$IFC UNDEFINED __INTERNETCONFIG__}
{$I InternetConfig.p}
{$ENDC}

{$IFC UNDEFINED __KEYBOARDS__}
{$I Keyboards.p}
{$ENDC}

{$IFC UNDEFINED __SOUND__}
{$I Sound.p}
{$ENDC}

{$IFC UNDEFINED __OSA__}
{$I OSA.p}
{$ENDC}

{$IFC UNDEFINED __OSACOMP__}
{$I OSAComp.p}
{$ENDC}

{$IFC UNDEFINED __OSAGENERIC__}
{$I OSAGeneric.p}
{$ENDC}

{$IFC UNDEFINED __APPLESCRIPT__}
{$I AppleScript.p}
{$ENDC}

{$IFC UNDEFINED __ASDEBUGGING__}
{$I ASDebugging.p}
{$ENDC}

{$IFC UNDEFINED __ASREGISTRY__}
{$I ASRegistry.p}
{$ENDC}

{$IFC UNDEFINED __FINDERREGISTRY__}
{$I FinderRegistry.p}
{$ENDC}

{$IFC UNDEFINED __NAVIGATION__}
{$I Navigation.p}
{$ENDC}

{$IFC UNDEFINED __URLACCESS__}
{$I URLAccess.p}
{$ENDC}

{$IFC UNDEFINED __COLORPICKER__}
{$I ColorPicker.p}
{$ENDC}

{$IFC UNDEFINED __CMCALIBRATOR__}
{$I CMCalibrator.p}
{$ENDC}

{$IFC UNDEFINED __HTMLRENDERING__}
{$I HTMLRendering.p}
{$ENDC}

{$IFC UNDEFINED __SPEECHRECOGNITION__}
{$I SpeechRecognition.p}
{$ENDC}

{$IFC UNDEFINED __KEYCHAINHI__}
{$I KeychainHI.p}
{$ENDC}

{$IFC UNDEFINED __ICAAPPLICATION__}
{$I ICAApplication.p}
{$ENDC}

{$IFC UNDEFINED __OCADEVICE__}
{$I ICADevice.p}
{$ENDC}

{$IFC UNDEFINED __ICACAMERA__}
{$I ICACamera.p}
{$ENDC}

implementation
end.
