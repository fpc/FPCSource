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

    P:=AddPackage('univint');
{$ifdef ALLPACKAGES}
    P.Directory:='univint';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.SourcePath.Add('src');
    P.OSes:=[darwin,iphonesim];
    P.Options.Add('-Mmacpas');

    P.SupportBuildModes:=[bmOneByOne];
  T:=P.Targets.AddImplicitUnit('ABActions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('abaddressbook');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('ABAddressBook.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('abtypedefs');
        AddUnit('abglobals');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cfdata');
      end;
  T:=P.Targets.AddImplicitUnit('ABGlobals.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('ABPeoplePicker.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('abaddressbook');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cggeometry');
        AddUnit('drag');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddImplicitUnit('ABTypedefs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AEDataModel.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('AEHelpers.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('appleevents');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddImplicitUnit('AEInteraction.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('aedatamodel');
        AddUnit('notification');
        AddUnit('events');
      end;
  T:=P.Targets.AddImplicitUnit('AEMach.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddImplicitUnit('AEObjects.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('osutils');
        AddUnit('appleevents');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('AEPackObject.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('appleevents');
      end;
  T:=P.Targets.AddImplicitUnit('AERegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atstypes');
        AddUnit('macerrors');
        AddUnit('appleevents');
      end;
  T:=P.Targets.AddImplicitUnit('AEUserTermTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('AIFF.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('ASDebugging.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('osa');
        AddUnit('files');
        AddUnit('components');
        AddUnit('appleevents');
        AddUnit('applescript');
      end;
  T:=P.Targets.AddImplicitUnit('ASRegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aeregistry');
        AddUnit('aeobjects');
      end;
  T:=P.Targets.AddImplicitUnit('ATSFont.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfrunloop');
        AddUnit('cfpropertylist');
        AddUnit('atstypes');
        AddUnit('cfstring');
        AddUnit('files');
        AddUnit('textcommon');
        AddUnit('sfnttypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSLayoutTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('sfntlayouttypes');
        AddUnit('atstypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeDirectAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atslayouttypes');
        AddUnit('atsunicodetypes');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeDrawing.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('quickdraw');
        AddUnit('atslayouttypes');
        AddUnit('atsunicodetypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeFlattening.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('sfnttypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeFonts.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('sfnttypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeGlyphs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('textcommon');
        AddUnit('atstypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeObjects.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('textcommon');
        AddUnit('sfntlayouttypes');
      end;
  T:=P.Targets.AddImplicitUnit('ATSUnicodeTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macmemory');
        AddUnit('atslayouttypes');
        AddUnit('fonts');
        AddUnit('quickdraw');
        AddUnit('sfnttypes');
        AddUnit('sfntlayouttypes');
        AddUnit('atstypes');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddImplicitUnit('AVLTree.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('AXActionConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AXAttributeConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AXErrors.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AXNotificationConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AXRoleConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AXTextAttributedString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('AXUIElement.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('axerrors');
        AddUnit('cfrunloop');
        AddUnit('cgremoteoperation');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddImplicitUnit('AXValue.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('AXValueConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('Aliases.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('Appearance.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgcontext');
        AddUnit('collections');
        AddUnit('processes');
        AddUnit('quickdrawtext');
        AddUnit('textcommon');
        AddUnit('quickdraw');
        AddUnit('textedit');
        AddUnit('qdoffscreen');
        AddUnit('macerrors');
        AddUnit('textutils');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('AppleDiskPartitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AppleEvents.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('aedatamodel');
        AddUnit('aeinteraction');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cfrunloop');
        AddUnit('cfstream');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('AppleHelp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('files');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('AppleScript.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('components');
        AddUnit('macerrors');
        AddUnit('appleevents');
        AddUnit('osa');
        AddUnit('textedit');
      end;
  T:=P.Targets.AddImplicitUnit('AudioHardware.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfrunloop');
        AddUnit('coreaudiotypes');
      end;
  T:=P.Targets.AddImplicitUnit('AuthSession.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('authorization');
      end;
  T:=P.Targets.AddImplicitUnit('Authorization.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('AuthorizationDB.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('authorization');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cfbundle');
      end;
  T:=P.Targets.AddImplicitUnit('AuthorizationTags.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('CFArray.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFAttributedString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('CFBag.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFBase.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('CFBinaryHeap.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFBitVector.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFBundle.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('CFByteOrders.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFCalendar.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cflocale');
        AddUnit('cfdate');
        AddUnit('cftimezone');
      end;
  T:=P.Targets.AddImplicitUnit('CFCharacterSet.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
      end;
  T:=P.Targets.AddImplicitUnit('CFData.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFDate.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFDateFormatter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cflocale');
      end;
  T:=P.Targets.AddImplicitUnit('CFDictionary.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFFTPStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstream');
        AddUnit('cfurl');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('CFHTTPMessage.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfstring');
        AddUnit('cfurl');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('CFHTTPStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfstream');
        AddUnit('cfbase');
        AddUnit('cfhttpmessage');
      end;
  T:=P.Targets.AddImplicitUnit('CFHost.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfarray');
        AddUnit('cfrunloop');
        AddUnit('cfstream');
      end;
  T:=P.Targets.AddImplicitUnit('CFLocale.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('CFMachPort.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfrunloop');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddImplicitUnit('CFMessagePort.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cfrunloop');
        AddUnit('cfdata');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddImplicitUnit('CFNetServices.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstream');
        AddUnit('cfarray');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddImplicitUnit('CFNotificationCenter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('CFNumber.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFNumberFormatter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfnumber');
        AddUnit('cflocale');
      end;
  T:=P.Targets.AddImplicitUnit('CFPlugIn.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfbundle');
        AddUnit('cfstring');
        AddUnit('cfurl');
        AddUnit('cfuuid');
      end;
  T:=P.Targets.AddImplicitUnit('CFPlugInCOM.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfplugin');
        AddUnit('cfuuid');
      end;
  T:=P.Targets.AddImplicitUnit('CFPreferences.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfdictionary');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfpropertylist');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('CFPropertyList.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('cfstream');
      end;
  T:=P.Targets.AddImplicitUnit('CFRunLoop.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdate');
        AddUnit('cfstring');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddImplicitUnit('CFSet.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFSocket.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('cfrunloop');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddImplicitUnit('CFSocketStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfstream');
        AddUnit('cfbase');
        AddUnit('cfhost');
        AddUnit('cfnetservices');
      end;
  T:=P.Targets.AddImplicitUnit('CFStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
        AddUnit('cfrunloop');
        AddUnit('cfsocket');
      end;
  T:=P.Targets.AddImplicitUnit('CFString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfcharacterset');
        AddUnit('cflocale');
      end;
  T:=P.Targets.AddImplicitUnit('CFStringEncodingExt.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFTimeZone.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfdate');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('CFTree.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CFURL.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('CFURLAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('CFUUID.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('CFUserNotification.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cfurl');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddImplicitUnit('CFXMLNode.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cftree');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('CFXMLParser.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cftree');
        AddUnit('cfurl');
        AddUnit('cfxmlnode');
      end;
  T:=P.Targets.AddImplicitUnit('CGAffineTransforms.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddImplicitUnit('CGBase.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('CGBitmapContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgcolorspace');
        AddUnit('cgimage');
        AddUnit('cgbase');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddImplicitUnit('CGColor.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cgcolorspace');
      end;
  T:=P.Targets.AddImplicitUnit('CGColorSpace.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cgbase');
        AddUnit('cgdataprovider');
      end;
  T:=P.Targets.AddImplicitUnit('CGContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cggeometry');
        AddUnit('cgbase');
        AddUnit('cfdictionary');
        AddUnit('cgaffinetransforms');
        AddUnit('cgcolorspace');
        AddUnit('cgfont');
        AddUnit('cgimage');
        AddUnit('cgpdfdocument');
        AddUnit('cgpath');
        AddUnit('cgcolor');
        AddUnit('cgshading');
        AddUnit('cgpdfpage');
      end;
  T:=P.Targets.AddImplicitUnit('CGDataConsumer.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cgbase');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('CGDataProvider.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cgbase');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('CGDirectDisplay.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cgcontext');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgerrors');
      end;
  T:=P.Targets.AddImplicitUnit('CGDirectPalette.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgdirectdisplay');
      end;
  T:=P.Targets.AddImplicitUnit('CGDisplayConfiguration.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgdirectdisplay');
        AddUnit('cgerrors');
        AddUnit('cfdictionary');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddImplicitUnit('CGDisplayFades.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgerrors');
        AddUnit('cgdirectdisplay');
        AddUnit('cgdisplayconfiguration');
      end;
  T:=P.Targets.AddImplicitUnit('CGErrors.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGEvent.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfmachport');
        AddUnit('cgbase');
        AddUnit('cgdirectdisplay');
        AddUnit('cgeventtypes');
        AddUnit('cggeometry');
        AddUnit('cgerrors');
        AddUnit('cgremoteoperation');
        AddUnit('cgeventsource');
      end;
  T:=P.Targets.AddImplicitUnit('CGEventSource.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cgremoteoperation');
        AddUnit('cgeventtypes');
      end;
  T:=P.Targets.AddImplicitUnit('CGEventTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macosxposix');
        AddUnit('cgremoteoperation');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGFont.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfarray');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGFunction.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGGLContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgcontext');
        AddUnit('cggeometry');
        AddUnit('cgcolorspace');
      end;
  T:=P.Targets.AddImplicitUnit('CGGeometry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGImage.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgcolorspace');
        AddUnit('cgdataprovider');
      end;
  T:=P.Targets.AddImplicitUnit('CGImageDestination.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
        AddUnit('cgdataconsumer');
        AddUnit('cgimage');
        AddUnit('cgimagesource');
      end;
  T:=P.Targets.AddImplicitUnit('CGImageProperties.pas');
    with T.Dependencies do
      begin
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGImageSource.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
        AddUnit('cgdataprovider');
        AddUnit('cgimage');
      end;
  T:=P.Targets.AddImplicitUnit('CGLayer.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFArray.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgpdfobject');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFContentStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cgpdfobject');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cggeometry');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
        AddUnit('cgbase');
        AddUnit('cgcontext');
        AddUnit('cgdataconsumer');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFDictionary.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgpdfobject');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFDocument.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cgdataprovider');
        AddUnit('cggeometry');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFObject.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFOperatorTable.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFPage.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgaffinetransforms');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cgpdfdocument');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFScanner.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgpdfobject');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cfdata');
      end;
  T:=P.Targets.AddImplicitUnit('CGPDFString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGPSConverter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
        AddUnit('cgdataconsumer');
        AddUnit('cgdataprovider');
      end;
  T:=P.Targets.AddImplicitUnit('CGPath.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgaffinetransforms');
        AddUnit('cfbase');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddImplicitUnit('CGPattern.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cggeometry');
        AddUnit('cgaffinetransforms');
        AddUnit('cgbase');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddImplicitUnit('CGRemoteOperation.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfmachport');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgerrors');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddImplicitUnit('CGSession.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGShading.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgcolorspace');
        AddUnit('cgfunction');
        AddUnit('cggeometry');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CGWindowLevels.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddImplicitUnit('CMCalibrator.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
      end;
  T:=P.Targets.AddImplicitUnit('CarbonEvents.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgcontext');
        AddUnit('quickdraw');
        AddUnit('axuielement');
        AddUnit('drag');
        AddUnit('cfarray');
        AddUnit('events');
        AddUnit('menus');
        AddUnit('controls');
        AddUnit('cfstring');
        AddUnit('aeregistry');
        AddUnit('aedatamodel');
        AddUnit('carboneventscore');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddImplicitUnit('CarbonEventsCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('CodeFragments.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbundle');
        AddUnit('files');
        AddUnit('multiprocessing');
      end;
  T:=P.Targets.AddImplicitUnit('Collections.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('ColorPicker.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('quickdraw');
        AddUnit('events');
      end;
  T:=P.Targets.AddImplicitUnit('Components.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('ConditionalMacros.pas');
  T:=P.Targets.AddImplicitUnit('ControlDefinitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textedit');
        AddUnit('axuielement');
        AddUnit('aedatamodel');
        AddUnit('cfbase');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('icons');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('datetimeutils');
        AddUnit('drag');
        AddUnit('textcommon');
        AddUnit('appearance');
        AddUnit('carbonevents');
        AddUnit('controls');
        AddUnit('lists');
        AddUnit('machelp');
        AddUnit('menus');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('Controls.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('files');
        AddUnit('events');
        AddUnit('cgimage');
        AddUnit('quickdraw');
        AddUnit('menus');
        AddUnit('textedit');
        AddUnit('drag');
        AddUnit('icons');
        AddUnit('collections');
        AddUnit('macerrors');
        AddUnit('appearance');
      end;
  T:=P.Targets.AddImplicitUnit('CoreAudioTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('CoreFoundation.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfbag');
        AddUnit('cfcharacterset');
        AddUnit('cfdata');
        AddUnit('cfdate');
        AddUnit('cfdictionary');
        AddUnit('cfnumber');
        AddUnit('cfpropertylist');
        AddUnit('cfset');
        AddUnit('cfstring');
        AddUnit('cfstringencodingext');
        AddUnit('cftimezone');
        AddUnit('cftree');
        AddUnit('cfurl');
        AddUnit('cfxmlnode');
        AddUnit('cfxmlparser');
        AddUnit('cfmachport');
        AddUnit('cfmessageport');
        AddUnit('cfrunloop');
        AddUnit('cfsocket');
        AddUnit('cfbinaryheap');
        AddUnit('cfbitvector');
        AddUnit('cfbundle');
        AddUnit('cfbyteorders');
        AddUnit('cfplugin');
        AddUnit('cfpreferences');
        AddUnit('cfurlaccess');
        AddUnit('cfuuid');
        AddUnit('cflocale');
        AddUnit('cfstream');
        AddUnit('cfdateformatter');
        AddUnit('cfnumberformatter');
        AddUnit('cfcalendar');
        AddUnit('cfusernotification');
        AddUnit('cfnotificationcenter');
        AddUnit('cfattributedstring');
      end;
  T:=P.Targets.AddImplicitUnit('CoreGraphics.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgaffinetransforms');
        AddUnit('cgbitmapcontext');
        AddUnit('cgcolor');
        AddUnit('cgcolorspace');
        AddUnit('cgcontext');
        AddUnit('cgdataconsumer');
        AddUnit('cgdataprovider');
        AddUnit('cgdirectdisplay');
        AddUnit('cgdirectpalette');
        AddUnit('cgdisplayconfiguration');
        AddUnit('cgdisplayfades');
        AddUnit('cgerrors');
        AddUnit('cgevent');
        AddUnit('cgeventsource');
        AddUnit('cgeventtypes');
        AddUnit('cgfont');
        AddUnit('cgfunction');
        AddUnit('cgglcontext');
        AddUnit('cggeometry');
        AddUnit('cgimage');
        AddUnit('cglayer');
        AddUnit('cgpdfarray');
        AddUnit('cgpdfcontentstream');
        AddUnit('cgpdfcontext');
        AddUnit('cgpdfdictionary');
        AddUnit('cgpdfdocument');
        AddUnit('cgpdfobject');
        AddUnit('cgpdfoperatortable');
        AddUnit('cgpdfpage');
        AddUnit('cgpdfscanner');
        AddUnit('cgpdfstream');
        AddUnit('cgpdfstring');
        AddUnit('cgpsconverter');
        AddUnit('cgpath');
        AddUnit('cgpattern');
        AddUnit('cgremoteoperation');
        AddUnit('cgsession');
        AddUnit('cgshading');
        AddUnit('cgwindowlevels');
      end;
  T:=P.Targets.AddImplicitUnit('DHCPClientPreferences.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('DateTimeUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('Debugging.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('Dialogs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('carboneventscore');
        AddUnit('quickdraw');
        AddUnit('mixedmode');
        AddUnit('events');
        AddUnit('macwindows');
        AddUnit('textedit');
        AddUnit('controls');
        AddUnit('macerrors');
        AddUnit('carbonevents');
      end;
  T:=P.Targets.AddImplicitUnit('Dictionary.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('files');
        AddUnit('aeregistry');
        AddUnit('codefragments');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('Displays.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('aedatamodel');
        AddUnit('conditionalmacros');
        AddUnit('components');
        AddUnit('appleevents');
        AddUnit('events');
        AddUnit('processes');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddImplicitUnit('Drag.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('cgimage');
        AddUnit('cggeometry');
        AddUnit('events');
        AddUnit('files');
        AddUnit('appleevents');
        AddUnit('quickdraw');
        AddUnit('pasteboard');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddImplicitUnit('DrawSprocket.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('qdoffscreen');
        AddUnit('displays');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('DriverSynchronization.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('Endian.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('Events.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('osutils');
        AddUnit('quickdraw');
        AddUnit('endian');
      end;
  T:=P.Targets.AddUnit('MacOSAll.pas');
  T:=P.Targets.AddImplicitUnit('FileTypesAndCreators.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('Files.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('osutils');
        AddUnit('textcommon');
        AddUnit('utcutils');
        AddUnit('finder');
      end;
  T:=P.Targets.AddImplicitUnit('Finder.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('FinderRegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aeregistry');
        AddUnit('osa');
      end;
  T:=P.Targets.AddImplicitUnit('FixMath.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('Folders.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('FontPanel.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('aeregistry');
        AddUnit('atstypes');
        AddUnit('carboneventscore');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddImplicitUnit('FontSync.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atstypes');
        AddUnit('files');
        AddUnit('fonts');
        AddUnit('sfnttypes');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('Fonts.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdrawtext');
        AddUnit('atstypes');
        AddUnit('files');
        AddUnit('macerrors');
        AddUnit('quickdraw');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddImplicitUnit('GPCStrings.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('GestaltEqu.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('HFSVolumes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('finder');
      end;
  T:=P.Targets.AddImplicitUnit('HIArchive.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfnumber');
        AddUnit('hiobject');
      end;
  T:=P.Targets.AddImplicitUnit('HIGeometry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddImplicitUnit('HIMovieView.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('higeometry');
        AddUnit('hiview');
        AddUnit('movies');
      end;
  T:=P.Targets.AddImplicitUnit('HIObject.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfbundle');
        AddUnit('events');
        AddUnit('carbonevents');
        AddUnit('axuielement');
        AddUnit('cfdictionary');
        AddUnit('carboneventscore');
      end;
  T:=P.Targets.AddImplicitUnit('HIShape.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgcontext');
        AddUnit('drag');
        AddUnit('quickdraw');
        AddUnit('carbonevents');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddImplicitUnit('HITextUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('HITheme.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('appearance');
        AddUnit('hishape');
        AddUnit('higeometry');
        AddUnit('drag');
        AddUnit('cfdate');
        AddUnit('cgcontext');
        AddUnit('macwindows');
        AddUnit('controls');
      end;
  T:=P.Targets.AddImplicitUnit('HIToolbar.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cgimage');
        AddUnit('icons');
        AddUnit('menus');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddImplicitUnit('HIToolbox.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('hiobject');
        AddUnit('hiarchive');
        AddUnit('higeometry');
        AddUnit('hitoolbar');
        AddUnit('hiview');
        AddUnit('hitextutils');
        AddUnit('hishape');
        AddUnit('events');
        AddUnit('notification');
        AddUnit('drag');
        AddUnit('controls');
        AddUnit('appearance');
        AddUnit('hitheme');
        AddUnit('macwindows');
        AddUnit('textedit');
        AddUnit('menus');
        AddUnit('dialogs');
        AddUnit('lists');
        AddUnit('carboneventscore');
        AddUnit('carbonevents');
        AddUnit('textservices');
        AddUnit('scrap');
        AddUnit('mactexteditor');
        AddUnit('machelp');
        AddUnit('controldefinitions');
        AddUnit('tsmte');
        AddUnit('translationextensions');
        AddUnit('translation');
        AddUnit('aeinteraction');
        AddUnit('typeselect');
        AddUnit('macapplication');
        AddUnit('keyboards');
        AddUnit('ibcarbonruntime');
      end;
  T:=P.Targets.AddImplicitUnit('HIView.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cgcontext');
        AddUnit('cgimage');
        AddUnit('carboneventscore');
        AddUnit('drag');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('menus');
        AddUnit('appearance');
        AddUnit('controls');
        AddUnit('carbonevents');
        AddUnit('higeometry');
        AddUnit('icons');
        AddUnit('hishape');
      end;
  T:=P.Targets.AddImplicitUnit('HTMLRendering.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('quickdraw');
        AddUnit('events');
        AddUnit('files');
        AddUnit('codefragments');
        AddUnit('controls');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('HostTime.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('coreaudiotypes');
      end;
  T:=P.Targets.AddImplicitUnit('IBCarbonRuntime.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('quickdraw');
        AddUnit('menus');
        AddUnit('cfstring');
        AddUnit('cfbundle');
        AddUnit('macwindows');
        AddUnit('controldefinitions');
      end;
  T:=P.Targets.AddImplicitUnit('ICAApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('files');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('ICACamera.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('ICADevice.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('icaapplication');
      end;
  T:=P.Targets.AddImplicitUnit('Icons.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cggeometry');
        AddUnit('quickdraw');
        AddUnit('files');
        AddUnit('codefragments');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddImplicitUnit('ImageCodec.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('qdoffscreen');
        AddUnit('osutils');
        AddUnit('dialogs');
        AddUnit('quickdraw');
        AddUnit('components');
        AddUnit('imagecompression');
        AddUnit('movies');
      end;
  T:=P.Targets.AddImplicitUnit('ImageCompression.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('osutils');
        AddUnit('quickdraw');
        AddUnit('qdoffscreen');
        AddUnit('components');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddImplicitUnit('InternetConfig.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('aliases');
        AddUnit('components');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddImplicitUnit('IntlResources.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('Keyboards.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('KeychainCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('aliases');
        AddUnit('codefragments');
        AddUnit('macerrors');
        AddUnit('processes');
        AddUnit('events');
      end;
  T:=P.Targets.AddImplicitUnit('KeychainHI.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('keychaincore');
        AddUnit('cfstring');
        AddUnit('cfarray');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddImplicitUnit('LanguageAnalysis.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('files');
        AddUnit('aeregistry');
        AddUnit('dictionary');
        AddUnit('textcommon');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('Lists.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('controls');
      end;
  T:=P.Targets.AddImplicitUnit('LowMem.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('macmemory');
        AddUnit('osutils');
        AddUnit('resources');
        AddUnit('quickdraw');
        AddUnit('controls');
        AddUnit('events');
        AddUnit('menus');
        AddUnit('fonts');
        AddUnit('macwindows');
      end;
  T:=P.Targets.AddImplicitUnit('MacApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgimage');
        AddUnit('menus');
        AddUnit('cgcontext');
        AddUnit('quickdraw');
        AddUnit('textcommon');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('MacErrors.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('MacHelp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('quickdraw');
        AddUnit('textedit');
        AddUnit('controls');
        AddUnit('dialogs');
        AddUnit('events');
        AddUnit('macwindows');
        AddUnit('menus');
      end;
  T:=P.Targets.AddImplicitUnit('MacLocales.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('MacMemory.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('MacOS.pas');
    T.Install:=false;
  T:=P.Targets.AddImplicitUnit('MacOSXPosix.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('MacTextEditor.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfurl');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
        AddUnit('aedatamodel');
        AddUnit('textcommon');
        AddUnit('quickdraw');
        AddUnit('qdoffscreen');
        AddUnit('menus');
        AddUnit('atsunicodetypes');
        AddUnit('conditionalmacros');
        AddUnit('drag');
        AddUnit('macwindows');
        AddUnit('files');
        AddUnit('events');
        AddUnit('macerrors');
        AddUnit('carbonevents');
        AddUnit('hiview');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddImplicitUnit('MacTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('MacWindows.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('files');
        AddUnit('appearance');
        AddUnit('carbonevents');
        AddUnit('hitoolbar');
        AddUnit('aliases');
        AddUnit('appleevents');
        AddUnit('collections');
        AddUnit('drag');
        AddUnit('events');
        AddUnit('menus');
        AddUnit('mixedmode');
        AddUnit('qdoffscreen');
        AddUnit('quickdraw');
        AddUnit('textcommon');
        AddUnit('icons');
        AddUnit('macerrors');
        AddUnit('cfstring');
        AddUnit('cgwindowlevels');
        AddUnit('higeometry');
        AddUnit('carboneventscore');
      end;
  T:=P.Targets.AddImplicitUnit('MachineExceptions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('Math64.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('MediaHandlers.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('events');
        AddUnit('imagecompression');
        AddUnit('conditionalmacros');
        AddUnit('components');
        AddUnit('sound');
        AddUnit('movies');
      end;
  T:=P.Targets.AddImplicitUnit('Menus.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('cfbase');
        AddUnit('cgcontext');
        AddUnit('atstypes');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('fonts');
        AddUnit('textcommon');
        AddUnit('processes');
        AddUnit('appleevents');
        AddUnit('collections');
        AddUnit('macerrors');
        AddUnit('cfstring');
        AddUnit('cfuuid');
        AddUnit('carboneventscore');
      end;
  T:=P.Targets.AddImplicitUnit('MixedMode.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('Movies.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('qdoffscreen');
        AddUnit('textedit');
        AddUnit('controls');
        AddUnit('dialogs');
        AddUnit('quickdraw');
        AddUnit('aliases');
        AddUnit('events');
        AddUnit('menus');
        AddUnit('components');
        AddUnit('sound');
        AddUnit('imagecompression');
      end;
  T:=P.Targets.AddImplicitUnit('MoviesFormat.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('imagecompression');
        AddUnit('components');
        AddUnit('movies');
      end;
  T:=P.Targets.AddImplicitUnit('Multiprocessing.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('NSL.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('nslcore');
      end;
  T:=P.Targets.AddImplicitUnit('NSLCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('codefragments');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('Navigation.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('cfbase');
        AddUnit('quickdraw');
        AddUnit('finder');
        AddUnit('events');
        AddUnit('appleevents');
        AddUnit('translation');
        AddUnit('macwindows');
        AddUnit('codefragments');
        AddUnit('macerrors');
        AddUnit('cfarray');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('Notification.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('osutils');
      end;
  T:=P.Targets.AddImplicitUnit('NumberFormatting.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
        AddUnit('intlresources');
      end;
  T:=P.Targets.AddImplicitUnit('OSA.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('macerrors');
        AddUnit('appleevents');
        AddUnit('aeobjects');
        AddUnit('aeinteraction');
        AddUnit('components');
      end;
  T:=P.Targets.AddImplicitUnit('OSAComp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddImplicitUnit('OSAGeneric.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('components');
        AddUnit('macerrors');
        AddUnit('appleevents');
        AddUnit('osa');
      end;
  T:=P.Targets.AddImplicitUnit('OSUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('mixedmode');
        AddUnit('macmemory');
        AddUnit('datetimeutils');
        AddUnit('cfstring');
        AddUnit('endian');
      end;
  T:=P.Targets.AddImplicitUnit('OpenTransport.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('OpenTransportProtocol.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
        AddUnit('files');
        AddUnit('codefragments');
        AddUnit('opentransport');
      end;
  T:=P.Targets.AddImplicitUnit('OpenTransportProviders.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('opentransport');
      end;
  T:=P.Targets.AddImplicitUnit('PEFBinaryFormat.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('PLStringFuncs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('PMApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('pmdefinitions');
        AddUnit('pmcore');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddImplicitUnit('PMCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('macerrors');
        AddUnit('files');
        AddUnit('cfstring');
        AddUnit('cfurl');
        AddUnit('quickdraw');
        AddUnit('pmdefinitions');
      end;
  T:=P.Targets.AddImplicitUnit('PMDefinitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('Palettes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddImplicitUnit('Pasteboard.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('PictUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('palettes');
      end;
  T:=P.Targets.AddImplicitUnit('Power.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('multiprocessing');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('Processes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('quickdraw');
        AddUnit('aedatamodel');
        AddUnit('events');
        AddUnit('files');
        AddUnit('textcommon');
        AddUnit('cfstring');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddImplicitUnit('QDOffscreen.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddImplicitUnit('QDPictToCGContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgcontext');
        AddUnit('cggeometry');
        AddUnit('cgdataprovider');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('QTML.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('events');
        AddUnit('macmemory');
        AddUnit('macwindows');
        AddUnit('osutils');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('QTSMovie.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('components');
        AddUnit('movies');
        AddUnit('quicktimestreaming');
      end;
  T:=P.Targets.AddImplicitUnit('QTStreamingComponents.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('dialogs');
        AddUnit('components');
        AddUnit('movies');
        AddUnit('quicktimestreaming');
      end;
  T:=P.Targets.AddImplicitUnit('QuickTimeComponents.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('events');
        AddUnit('qdoffscreen');
        AddUnit('menus');
        AddUnit('dialogs');
        AddUnit('aliases');
        AddUnit('mixedmode');
        AddUnit('components');
        AddUnit('quickdraw');
        AddUnit('sound');
        AddUnit('imagecompression');
        AddUnit('movies');
        AddUnit('quicktimemusic');
      end;
  T:=P.Targets.AddImplicitUnit('QuickTimeMusic.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('dialogs');
        AddUnit('files');
        AddUnit('components');
        AddUnit('quickdraw');
        AddUnit('macmemory');
        AddUnit('sound');
        AddUnit('endian');
        AddUnit('imagecompression');
        AddUnit('movies');
      end;
  T:=P.Targets.AddImplicitUnit('QuickTimeStreaming.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('events');
        AddUnit('imagecompression');
        AddUnit('quickdraw');
        AddUnit('components');
        AddUnit('macerrors');
        AddUnit('movies');
        AddUnit('quicktimecomponents');
      end;
  T:=P.Targets.AddImplicitUnit('QuickTimeVR.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('movies');
      end;
  T:=P.Targets.AddImplicitUnit('QuickTimeVRFormat.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('conditionalmacros');
        AddUnit('movies');
        AddUnit('quicktimevr');
      end;
  T:=P.Targets.AddImplicitUnit('Quickdraw.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgdirectdisplay');
        AddUnit('components');
        AddUnit('mixedmode');
        AddUnit('quickdrawtext');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddImplicitUnit('QuickdrawText.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('intlresources');
      end;
  T:=P.Targets.AddImplicitUnit('Resources.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('SCDynamicStore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfrunloop');
        AddUnit('cfpropertylist');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('SCDynamicStoreCopyDHCPInfos.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scdynamicstore');
        AddUnit('cfdictionary');
        AddUnit('cfdata');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddImplicitUnit('SCDynamicStoreCopySpecific.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scdynamicstore');
        AddUnit('macosxposix');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('SCDynamicStoreKey.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('SCNetwork.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddImplicitUnit('SCNetworkConnection.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddImplicitUnit('SCNetworkReachability.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scnetwork');
        AddUnit('macosxposix');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddImplicitUnit('SCPreferences.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scdynamicstore');
        AddUnit('cfdate');
        AddUnit('cfpropertylist');
        AddUnit('cfarray');
        AddUnit('cfdata');
      end;
  T:=P.Targets.AddImplicitUnit('SCPreferencesPath.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scpreferences');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddImplicitUnit('SCPreferencesSetSpecific.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scpreferences');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('SCSI.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('applediskpartitions');
      end;
  T:=P.Targets.AddImplicitUnit('SCSchemaDefinitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('SFNTLayoutTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('SFNTTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('Scrap.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('mixedmode');
        AddUnit('macerrors');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddImplicitUnit('Script.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('intlresources');
      end;
  T:=P.Targets.AddImplicitUnit('Sound.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('components');
        AddUnit('mixedmode');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddImplicitUnit('SpeechRecognition.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('aedatamodel');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('SpeechSynthesis.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('StringCompare.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('textcommon');
        AddUnit('script');
        AddUnit('typeselect');
      end;
  T:=P.Targets.AddImplicitUnit('SystemConfiguration.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('SystemSound.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('cfbase');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddImplicitUnit('TSMTE.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textedit');
        AddUnit('dialogs');
        AddUnit('appleevents');
        AddUnit('textservices');
      end;
  T:=P.Targets.AddImplicitUnit('TextCommon.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddImplicitUnit('TextEdit.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('TextEncodingConverter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddImplicitUnit('TextEncodingPlugin.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('textencodingconverter');
      end;
  T:=P.Targets.AddImplicitUnit('TextServices.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('conditionalmacros');
        AddUnit('cfbase');
        AddUnit('carboneventscore');
        AddUnit('atstypes');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('events');
        AddUnit('menus');
        AddUnit('aedatamodel');
        AddUnit('aeregistry');
        AddUnit('aeinteraction');
        AddUnit('components');
        AddUnit('carbonevents');
      end;
  T:=P.Targets.AddImplicitUnit('TextUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('intlresources');
        AddUnit('numberformatting');
        AddUnit('stringcompare');
        AddUnit('datetimeutils');
      end;
  T:=P.Targets.AddImplicitUnit('Threads.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('Timer.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
        AddUnit('osutils');
      end;
  T:=P.Targets.AddImplicitUnit('ToolUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('fixmath');
        AddUnit('textutils');
        AddUnit('icons');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddImplicitUnit('Translation.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('components');
        AddUnit('translationextensions');
      end;
  T:=P.Targets.AddImplicitUnit('TranslationExtensions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('quickdraw');
        AddUnit('components');
      end;
  T:=P.Targets.AddImplicitUnit('TranslationServices.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
        AddUnit('files');
      end;
  T:=P.Targets.AddImplicitUnit('TypeSelect.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
      end;
  T:=P.Targets.AddImplicitUnit('URLAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('codefragments');
        AddUnit('macerrors');
        AddUnit('events');
      end;
  T:=P.Targets.AddImplicitUnit('UTCUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddImplicitUnit('UTCoreTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddImplicitUnit('UTType.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddImplicitUnit('UnicodeConverter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddImplicitUnit('UnicodeUtilities.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('maclocales');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddImplicitUnit('UniversalAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddImplicitUnit('Video.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddImplicitUnit('fenv.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddImplicitUnit('fp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('vBLAS.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('vDSP.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddImplicitUnit('xattr.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;

    T:=P.Targets.AddImplicitUnit('CoreText.pas');
    T:=P.Targets.AddImplicitUnit('DADisk.pas');
    T:=P.Targets.AddImplicitUnit('DASession.pas');
    T:=P.Targets.AddImplicitUnit('FSEvents.pas');
    T:=P.Targets.AddImplicitUnit('LSInfo.pas');
    T:=P.Targets.AddImplicitUnit('LSOpen.pas');
    T:=P.Targets.AddImplicitUnit('MDItem.pas');
    T:=P.Targets.AddImplicitUnit('SecTrust.pas');
    T:=P.Targets.AddImplicitUnit('WSTypes.pas');
    T:=P.Targets.AddImplicitUnit('cblas.pas');
    T:=P.Targets.AddImplicitUnit('CTFont.pas');
    T:=P.Targets.AddImplicitUnit('CTFontCollection.pas');
    T:=P.Targets.AddImplicitUnit('CTFontDescriptor.pas');
    T:=P.Targets.AddImplicitUnit('CTFontManager.pas');
    T:=P.Targets.AddImplicitUnit('CTFontManagerErrors.pas');
    T:=P.Targets.AddImplicitUnit('CTFontTraits.pas');
    T:=P.Targets.AddImplicitUnit('CTFrame.pas');
    T:=P.Targets.AddImplicitUnit('CTFramesetter.pas');
    T:=P.Targets.AddImplicitUnit('AUComponent.pas');
    T:=P.Targets.AddImplicitUnit('AXConstants.pas');
    T:=P.Targets.AddImplicitUnit('Accessibility.pas');
    T:=P.Targets.AddImplicitUnit('AudioCodecs.pas');
    T:=P.Targets.AddImplicitUnit('AudioComponents.pas');
    T:=P.Targets.AddImplicitUnit('AudioOutputUnit.pas');
    T:=P.Targets.AddImplicitUnit('AudioUnitCarbonViews.pas');
    T:=P.Targets.AddImplicitUnit('AudioUnitParameters.pas');
    T:=P.Targets.AddImplicitUnit('AudioUnitProperties.pas');
    T:=P.Targets.AddImplicitUnit('AuthorizationPlugin.pas');
    T:=P.Targets.AddImplicitUnit('BackupCore.pas');
    T:=P.Targets.AddImplicitUnit('CFError.pas');
    T:=P.Targets.AddImplicitUnit('CFHTTPAuthentication.pas');
    T:=P.Targets.AddImplicitUnit('CFNetDiagnostics.pas');
    T:=P.Targets.AddImplicitUnit('CFNetworkErrorss.pas');
    T:=P.Targets.AddImplicitUnit('CFProxySupport.pas');
    T:=P.Targets.AddImplicitUnit('CFStringTokenizer.pas');
    T:=P.Targets.AddImplicitUnit('CGGradient.pas');
    T:=P.Targets.AddImplicitUnit('CGLCurrent.pas');
    T:=P.Targets.AddImplicitUnit('CGLDevice.pas');
    T:=P.Targets.AddImplicitUnit('CGLProfiler.pas');
    T:=P.Targets.AddImplicitUnit('CGLProfilerFunctionEnums.pas');
    T:=P.Targets.AddImplicitUnit('CGLRenderers.pas');
    T:=P.Targets.AddImplicitUnit('CGLTypes.pas');
    T:=P.Targets.AddImplicitUnit('CGWindow.pas');
    T:=P.Targets.AddImplicitUnit('CSIdentity.pas');
    T:=P.Targets.AddImplicitUnit('CSIdentityAuthority.pas');
    T:=P.Targets.AddImplicitUnit('CSIdentityQuery.pas');
    T:=P.Targets.AddImplicitUnit('CTGlyphInfo.pas');
    T:=P.Targets.AddImplicitUnit('CTLine.pas');
    T:=P.Targets.AddImplicitUnit('CTParagraphStyle.pas');
    T:=P.Targets.AddImplicitUnit('CTRun.pas');
    T:=P.Targets.AddImplicitUnit('CTStringAttributes.pas');
    T:=P.Targets.AddImplicitUnit('CTTextTab.pas');
    T:=P.Targets.AddImplicitUnit('CTTypesetter.pas');
    T:=P.Targets.AddImplicitUnit('CVBase.pas');
    T:=P.Targets.AddImplicitUnit('CVBuffer.pas');
    T:=P.Targets.AddImplicitUnit('CVDisplayLink.pas');
    T:=P.Targets.AddImplicitUnit('CVHostTime.pas');
    T:=P.Targets.AddImplicitUnit('CVImageBuffer.pas');
    T:=P.Targets.AddImplicitUnit('CVOpenGLBuffer.pas');
    T:=P.Targets.AddImplicitUnit('CVOpenGLBufferPool.pas');
    T:=P.Targets.AddImplicitUnit('CVOpenGLTexture.pas');
    T:=P.Targets.AddImplicitUnit('CVOpenGLTextureCache.pas');
    T:=P.Targets.AddImplicitUnit('CVPixelBuffer.pas');
    T:=P.Targets.AddImplicitUnit('CVPixelBufferPool.pas');
    T:=P.Targets.AddImplicitUnit('CVPixelFormatDescription.pas');
    T:=P.Targets.AddImplicitUnit('CVReturns.pas');
    T:=P.Targets.AddImplicitUnit('ColorSyncCMM.pas');
    T:=P.Targets.AddImplicitUnit('ColorSyncDeprecated.pas');
    T:=P.Targets.AddImplicitUnit('ColorSyncDevice.pas');
    T:=P.Targets.AddImplicitUnit('ColorSyncProfile.pas');
    T:=P.Targets.AddImplicitUnit('ColorSyncTransform.pas');
    T:=P.Targets.AddImplicitUnit('DigitalHubRegistry.pas');
    T:=P.Targets.AddImplicitUnit('DriverServices.pas');
    T:=P.Targets.AddImplicitUnit('HIAccessibility.pas');
    T:=P.Targets.AddImplicitUnit('HIButtonViews.pas');
    T:=P.Targets.AddImplicitUnit('HIClockView.pas');
    T:=P.Targets.AddImplicitUnit('HIComboBox.pas');
    T:=P.Targets.AddImplicitUnit('HIContainerViews.pas');
    T:=P.Targets.AddImplicitUnit('HIDataBrowser.pas');
    T:=P.Targets.AddImplicitUnit('HIDisclosureViews.pas');
    T:=P.Targets.AddImplicitUnit('HIImageViews.pas');
    T:=P.Targets.AddImplicitUnit('HILittleArrows.pas');
    T:=P.Targets.AddImplicitUnit('HIMenuView.pas');
    T:=P.Targets.AddImplicitUnit('HIPopupButton.pas');
    T:=P.Targets.AddImplicitUnit('HIProgressViews.pas');
    T:=P.Targets.AddImplicitUnit('HIRelevanceBar.pas');
    T:=P.Targets.AddImplicitUnit('HIScrollView.pas');
    T:=P.Targets.AddImplicitUnit('HISearchField.pas');
    T:=P.Targets.AddImplicitUnit('HISegmentedView.pas');
    T:=P.Targets.AddImplicitUnit('HISeparator.pas');
    T:=P.Targets.AddImplicitUnit('HISlider.pas');
    T:=P.Targets.AddImplicitUnit('HITabbedView.pas');
    T:=P.Targets.AddImplicitUnit('HITextLengthFilter.pas');
    T:=P.Targets.AddImplicitUnit('HITextViews.pas');
    T:=P.Targets.AddImplicitUnit('HIToolboxDebugging.pas');
    T:=P.Targets.AddImplicitUnit('HIWindowViews.pas');
    T:=P.Targets.AddImplicitUnit('IOKitReturn.pas');
    T:=P.Targets.AddImplicitUnit('IOSurfaceAPI.pas');
    T:=P.Targets.AddImplicitUnit('IconStorage.pas');
    T:=P.Targets.AddImplicitUnit('IconsCore.pas');
    T:=P.Targets.AddImplicitUnit('KeyEvents.pas');
    T:=P.Targets.AddImplicitUnit('LSQuarantine.pas');
    T:=P.Targets.AddImplicitUnit('LSSharedFileList.pas');
    T:=P.Targets.AddImplicitUnit('MDExternalDatastore.pas');
    T:=P.Targets.AddImplicitUnit('MDImporter.pas');
    T:=P.Targets.AddImplicitUnit('MDLineage.pas');
    T:=P.Targets.AddImplicitUnit('MDQuery.pas');
    T:=P.Targets.AddImplicitUnit('MDSchema.pas');
    T:=P.Targets.AddImplicitUnit('MIDIDriver.pas');
    T:=P.Targets.AddImplicitUnit('MIDIServices.pas');
    T:=P.Targets.AddImplicitUnit('MIDISetup.pas');
    T:=P.Targets.AddImplicitUnit('MIDIThruConnection.pas');
    T:=P.Targets.AddImplicitUnit('MacOpenGL.pas');
    T:=P.Targets.AddImplicitUnit('MultiProcessingInfo.pas');
    T:=P.Targets.AddImplicitUnit('MusicDevice.pas');
    T:=P.Targets.AddImplicitUnit('ObjCRuntime.pas');
    T:=P.Targets.AddImplicitUnit('PMApplicationDeprecated.pas');
    T:=P.Targets.AddImplicitUnit('PMCoreDeprecated.pas');
    T:=P.Targets.AddImplicitUnit('PMDefinitionsDeprecated.pas');
    T:=P.Targets.AddImplicitUnit('PMErrors.pas');
    T:=P.Targets.AddImplicitUnit('PMPrintAETypes.pas');
    T:=P.Targets.AddImplicitUnit('PMPrintSettingsKeys.pas');
    T:=P.Targets.AddImplicitUnit('PMPrintingDialogExtensions.pas');
    T:=P.Targets.AddImplicitUnit('QDCMCommon.pas');
    T:=P.Targets.AddImplicitUnit('QLBase.pas');
    T:=P.Targets.AddImplicitUnit('QLGenerator.pas');
    T:=P.Targets.AddImplicitUnit('QLThumbnailImage.pas');
    T:=P.Targets.AddImplicitUnit('QuickTimeErrors.pas');
    T:=P.Targets.AddImplicitUnit('QuickdrawTypes.pas');
    T:=P.Targets.AddImplicitUnit('SCNetworkConfiguration.pas');
    T:=P.Targets.AddImplicitUnit('ScalerStreamTypes.pas');
    T:=P.Targets.AddImplicitUnit('SecBase.pas');
    T:=P.Targets.AddImplicitUnit('TextInputSources.pas');
    T:=P.Targets.AddImplicitUnit('WSMethodInvocation.pas');
    T:=P.Targets.AddImplicitUnit('WSProtocolHandler.pas');
    T:=P.Targets.AddImplicitUnit('certextensions.pas');
    T:=P.Targets.AddImplicitUnit('cssmapple.pas');
    T:=P.Targets.AddImplicitUnit('cssmconfig.pas');
    T:=P.Targets.AddImplicitUnit('cssmerr.pas');
    T:=P.Targets.AddImplicitUnit('cssmkrapi.pas');
    T:=P.Targets.AddImplicitUnit('cssmtype.pas');
    T:=P.Targets.AddImplicitUnit('gliContexts.pas');
    T:=P.Targets.AddImplicitUnit('gliDispatch.pas');
    T:=P.Targets.AddImplicitUnit('gluContext.pas');
    T:=P.Targets.AddImplicitUnit('kern_return.pas');
    T:=P.Targets.AddImplicitUnit('macgl.pas');
    T:=P.Targets.AddImplicitUnit('macglext.pas');
    T:=P.Targets.AddImplicitUnit('macglu.pas');
    T:=P.Targets.AddImplicitUnit('mach_error.pas');
    T:=P.Targets.AddImplicitUnit('x509defs.pas');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('controldemo.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
