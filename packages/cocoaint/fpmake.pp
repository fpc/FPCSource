{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('cocoaint');
    P.ShortName := 'coc';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.2';
    P.CPUs:=[i386,x86_64,powerpc,powerpc64,aarch64];
    P.OSes:=[darwin];
    P.Dependencies.Add('univint');
    P.SourcePath.Add('src');
    P.Options.Add('-dMACOSALL');
    P.Options.Add('-dCOCOAALL');
    P.Options.Add('-dLEGACY_SETNEEDSDISPLAY');

    T:=P.Targets.AddUnit('CocoaAll.pas');
    T:=P.Targets.AddUnit('AVFoundation.pas');
    T:=P.Targets.AddUnit('AVKit.pas');
    T:=P.Targets.AddUnit('Accounts.pas');
    T:=P.Targets.AddUnit('AddressBook.pas');
    T:=P.Targets.AddUnit('AppleScriptObjC.pas');
    T:=P.Targets.AddUnit('AudioVideoBridging.pas');
    T:=P.Targets.AddUnit('Automator.pas');
    T:=P.Targets.AddUnit('CFOpenDirectory.pas');
    T:=P.Targets.AddUnit('CalendarStore.pas');
    T:=P.Targets.AddUnit('CloudKit.pas');
    T:=P.Targets.AddUnit('Collaboration.pas');
    T:=P.Targets.AddUnit('CoreAudio.pas');
    T:=P.Targets.AddUnit('CoreAudioKit.pas');
    T:=P.Targets.AddUnit('CoreBluetooth.pas');
    T:=P.Targets.AddUnit('CoreLocation.pas');
    T:=P.Targets.AddUnit('CoreMedia.pas');
    T:=P.Targets.AddUnit('CoreMediaIO.pas');
    T:=P.Targets.AddUnit('CoreVideo.pas');
    T:=P.Targets.AddUnit('CryptoTokenKit.pas');
    T:=P.Targets.AddUnit('EventKit.pas');
    T:=P.Targets.AddUnit('FinderSync.pas');
    T:=P.Targets.AddUnit('GLKit.pas');
    T:=P.Targets.AddUnit('GameController.pas');
    T:=P.Targets.AddUnit('GameKit.pas');
    T:=P.Targets.AddUnit('IOBluetooth.pas');
    T:=P.Targets.AddUnit('IOBluetoothUI.pas');
    T:=P.Targets.AddUnit('ImageCaptureCore.pas');
    T:=P.Targets.AddUnit('ImageKit.pas');
    T:=P.Targets.AddUnit('InputMethodKit.pas');
    T:=P.Targets.AddUnit('InstallerPlugins.pas');
    T:=P.Targets.AddUnit('InstantMessage.pas');
    T:=P.Targets.AddUnit('LocalAuthentication.pas');
    T:=P.Targets.AddUnit('MapKit.pas');
    T:=P.Targets.AddUnit('MediaAccessibility.pas');
    T:=P.Targets.AddUnit('MediaLibrary.pas');
    T:=P.Targets.AddUnit('MultipeerConnectivity.pas');
    T:=P.Targets.AddUnit('NotificationCenter.pas');
    T:=P.Targets.AddUnit('OSAKit.pas');
    T:=P.Targets.AddUnit('OpenDirectory.pas');
    T:=P.Targets.AddUnit('PDFKit.pas');
    T:=P.Targets.AddUnit('PreferencePanes.pas');
    T:=P.Targets.AddUnit('PubSub.pas');
    T:=P.Targets.AddUnit('QTKit.pas');
    T:=P.Targets.AddUnit('QuartzComposer.pas');
    T:=P.Targets.AddUnit('QuartzFilters.pas');
    T:=P.Targets.AddUnit('QuickLook.pas');
    T:=P.Targets.AddUnit('QuickLookUI.pas');
    T:=P.Targets.AddUnit('SceneKit.pas');
    T:=P.Targets.AddUnit('ScreenSaver.pas');
    T:=P.Targets.AddUnit('ScriptingBridge.pas');
    T:=P.Targets.AddUnit('SecurityFoundation.pas');
    T:=P.Targets.AddUnit('SecurityInterface.pas');
    T:=P.Targets.AddUnit('ServiceManagement.pas');
    T:=P.Targets.AddUnit('Social.pas');
    T:=P.Targets.AddUnit('SpriteKit.pas');
    T:=P.Targets.AddUnit('StoreKit.pas');
    T:=P.Targets.AddUnit('SyncServices.pas');
    T:=P.Targets.AddUnit('WebKit.pas');
    T:=P.Targets.AddUnit('iTunesLibrary.pas');

    T:=P.Targets.AddImplicitUnit('DefinedClassesAVFoundation.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAVKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAccounts.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAddressBook.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAppKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAppleScriptObjC.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAudioVideoBridging.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesAutomator.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCFOpenDirectory.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCalendarStore.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCloudKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCollaboration.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreAudio.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreAudioKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreBluetooth.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreData.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreGraphics.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreImage.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreLocation.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreMedia.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreMediaIO.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCoreVideo.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesCryptoTokenKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesEventKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesFinderSync.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesFoundation.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesGLKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesGameController.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesGameKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesIOBluetooth.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesIOBluetoothUI.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesImageCaptureCore.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesImageKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesInputMethodKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesInstallerPlugins.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesInstantMessage.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesLocalAuthentication.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesMapKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesMediaAccessibility.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesMediaLibrary.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesMultipeerConnectivity.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesNotificationCenter.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesOSAKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesOpenDirectory.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesPDFKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesPreferencePanes.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesPubSub.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesQTKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesQuartzComposer.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesQuartzCore.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesQuartzFilters.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesQuickLook.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesQuickLookUI.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesSceneKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesScreenSaver.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesScriptingBridge.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesSecurityFoundation.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesSecurityInterface.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesServiceManagement.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesSocial.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesSpriteKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesStoreKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesSyncServices.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesWebKit.pas');
    T:=P.Targets.AddImplicitUnit('DefinedClassesiTunesLibrary.pas');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
