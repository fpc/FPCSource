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
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

  T:=P.Targets.AddUnit('ABActions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('abaddressbook');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('ABAddressBook.pas');
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
  T:=P.Targets.AddUnit('ABGlobals.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('ABPeoplePicker.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('abaddressbook');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cggeometry');
        AddUnit('drag');
        AddUnit('hiobjectcore');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddUnit('ABTypedefs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AEDataModel.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('AEHelpers.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('appleevents');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddUnit('AEInteraction.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('aedatamodel');
        AddUnit('notification');
        AddUnit('events');
      end;
  T:=P.Targets.AddUnit('AEMach.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddUnit('AEObjects.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('osutils');
        AddUnit('appleevents');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('AEPackObject.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('appleevents');
      end;
  T:=P.Targets.AddUnit('AERegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atstypes');
        AddUnit('macerrors');
        AddUnit('appleevents');
      end;
  T:=P.Targets.AddUnit('AEUserTermTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('AIFF.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('ASDebugging.pas');
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
  T:=P.Targets.AddUnit('ASRegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aeregistry');
        AddUnit('aeobjects');
      end;
  T:=P.Targets.AddUnit('ATSFont.pas');
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
  T:=P.Targets.AddUnit('ATSLayoutTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('sfntlayouttypes');
        AddUnit('atstypes');
      end;
  T:=P.Targets.AddUnit('ATSTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeDirectAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atslayouttypes');
        AddUnit('atsunicodetypes');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeDrawing.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('quickdraw');
        AddUnit('atslayouttypes');
        AddUnit('atsunicodetypes');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeFlattening.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('sfnttypes');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeFonts.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('sfnttypes');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeGlyphs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('textcommon');
        AddUnit('atstypes');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeObjects.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atsunicodetypes');
        AddUnit('textcommon');
        AddUnit('sfntlayouttypes');
      end;
  T:=P.Targets.AddUnit('ATSUnicodeTypes.pas');
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
  T:=P.Targets.AddUnit('AVLTree.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('AXActionConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AXAttributeConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AXErrors.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AXNotificationConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AXRoleConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AXTextAttributedString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('AXUIElement.pas');
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
  T:=P.Targets.AddUnit('AXValue.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('AXValueConstants.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Aliases.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('Appearance.pas');
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
  T:=P.Targets.AddUnit('AppleDiskPartitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AppleEvents.pas');
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
  T:=P.Targets.AddUnit('AppleHelp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('files');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('AppleScript.pas');
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
  T:=P.Targets.AddUnit('AudioHardware.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfrunloop');
        AddUnit('coreaudiotypes');
      end;
  T:=P.Targets.AddUnit('AuthSession.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('authorization');
      end;
  T:=P.Targets.AddUnit('Authorization.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('AuthorizationDB.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('authorization');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cfbundle');
      end;
  T:=P.Targets.AddUnit('AuthorizationTags.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('CFArray.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFAttributedString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('CFBag.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFBase.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('CFBinaryHeap.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFBitVector.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFBundle.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cfstring');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('CFByteOrders.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFCalendar.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cflocale');
        AddUnit('cfdate');
        AddUnit('cftimezone');
      end;
  T:=P.Targets.AddUnit('CFCharacterSet.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
      end;
  T:=P.Targets.AddUnit('CFData.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFDate.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFDateFormatter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cflocale');
      end;
  T:=P.Targets.AddUnit('CFDictionary.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFFTPStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstream');
        AddUnit('cfurl');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('CFHTTPMessage.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfstring');
        AddUnit('cfurl');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('CFHTTPStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfstream');
        AddUnit('cfbase');
        AddUnit('cfhttpmessage');
      end;
  T:=P.Targets.AddUnit('CFHost.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfarray');
        AddUnit('cfrunloop');
        AddUnit('cfstream');
      end;
  T:=P.Targets.AddUnit('CFLocale.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('CFMachPort.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfrunloop');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddUnit('CFMessagePort.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cfrunloop');
        AddUnit('cfdata');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddUnit('CFNetServices.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstream');
        AddUnit('cfarray');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddUnit('CFNotificationCenter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('CFNumber.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFNumberFormatter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfnumber');
        AddUnit('cflocale');
      end;
  T:=P.Targets.AddUnit('CFPlugIn.pas');
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
  T:=P.Targets.AddUnit('CFPlugInCOM.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfplugin');
        AddUnit('cfuuid');
      end;
  T:=P.Targets.AddUnit('CFPreferences.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfdictionary');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfpropertylist');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('CFPropertyList.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('cfstream');
      end;
  T:=P.Targets.AddUnit('CFRunLoop.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdate');
        AddUnit('cfstring');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddUnit('CFSet.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFSocket.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('cfrunloop');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddUnit('CFSocketStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfstream');
        AddUnit('cfbase');
        AddUnit('cfhost');
        AddUnit('cfnetservices');
      end;
  T:=P.Targets.AddUnit('CFStream.pas');
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
  T:=P.Targets.AddUnit('CFString.pas');
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
  T:=P.Targets.AddUnit('CFStringEncodingExt.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFTimeZone.pas');
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
  T:=P.Targets.AddUnit('CFTree.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CFURL.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfstring');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('CFURLAccess.pas');
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
  T:=P.Targets.AddUnit('CFUUID.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('CFUserNotification.pas');
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
  T:=P.Targets.AddUnit('CFXMLNode.pas');
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
  T:=P.Targets.AddUnit('CFXMLParser.pas');
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
  T:=P.Targets.AddUnit('CGAffineTransforms.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddUnit('CGBase.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('CGBitmapContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgcolorspace');
        AddUnit('cgimage');
        AddUnit('cgbase');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddUnit('CGColor.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cgcolorspace');
      end;
  T:=P.Targets.AddUnit('CGColorSpace.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfstring');
        AddUnit('cmtypes');
        AddUnit('cgbase');
        AddUnit('cgdataprovider');
      end;
  T:=P.Targets.AddUnit('CGContext.pas');
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
  T:=P.Targets.AddUnit('CGDataConsumer.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cgbase');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('CGDataProvider.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cgbase');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('CGDirectDisplay.pas');
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
  T:=P.Targets.AddUnit('CGDirectPalette.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgdirectdisplay');
      end;
  T:=P.Targets.AddUnit('CGDisplayConfiguration.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgdirectdisplay');
        AddUnit('cgerrors');
        AddUnit('cfdictionary');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddUnit('CGDisplayFade.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgerrors');
        AddUnit('cgdirectdisplay');
        AddUnit('cgdisplayconfiguration');
      end;
  T:=P.Targets.AddUnit('CGErrors.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGEvent.pas');
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
  T:=P.Targets.AddUnit('CGEventSource.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cgremoteoperation');
        AddUnit('cgeventtypes');
      end;
  T:=P.Targets.AddUnit('CGEventTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macosxposix');
        AddUnit('cgremoteoperation');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGFont.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfdictionary');
        AddUnit('cfarray');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGFunction.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CGGLContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgcontext');
        AddUnit('cggeometry');
        AddUnit('cgcolorspace');
      end;
  T:=P.Targets.AddUnit('CGGeometry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGImage.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgcolorspace');
        AddUnit('cgdataprovider');
      end;
  T:=P.Targets.AddUnit('CGImageDestination.pas');
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
  T:=P.Targets.AddUnit('CGImageProperties.pas');
    with T.Dependencies do
      begin
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CGImageSource.pas');
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
  T:=P.Targets.AddUnit('CGLayer.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddUnit('CGPDFArray.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgpdfobject');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGPDFContentStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cgpdfobject');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGPDFContext.pas');
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
  T:=P.Targets.AddUnit('CGPDFDictionary.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgpdfobject');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGPDFDocument.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cgdataprovider');
        AddUnit('cggeometry');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('CGPDFObject.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGPDFOperatorTable.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGPDFPage.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgaffinetransforms');
        AddUnit('cfbase');
        AddUnit('cgbase');
        AddUnit('cgpdfdocument');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddUnit('CGPDFScanner.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgpdfobject');
      end;
  T:=P.Targets.AddUnit('CGPDFStream.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cfdata');
      end;
  T:=P.Targets.AddUnit('CGPDFString.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdate');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGPSConverter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
        AddUnit('cgdataconsumer');
        AddUnit('cgdataprovider');
      end;
  T:=P.Targets.AddUnit('CGPath.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgaffinetransforms');
        AddUnit('cfbase');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddUnit('CGPattern.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cggeometry');
        AddUnit('cgaffinetransforms');
        AddUnit('cgbase');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddUnit('CGRemoteOperation.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfmachport');
        AddUnit('cgbase');
        AddUnit('cggeometry');
        AddUnit('cgerrors');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddUnit('CGSession.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfdictionary');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CGShading.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
        AddUnit('cgcolorspace');
        AddUnit('cgfunction');
        AddUnit('cggeometry');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CGWindowLevels.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgbase');
      end;
  T:=P.Targets.AddUnit('CMApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('files');
        AddUnit('cmiccprofile');
        AddUnit('macerrors');
        AddUnit('cmtypes');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
        AddUnit('quickdraw');
        AddUnit('printing');
      end;
  T:=P.Targets.AddUnit('CMCalibrator.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cmtypes');
        AddUnit('cmapplication');
        AddUnit('events');
      end;
  T:=P.Targets.AddUnit('CMDeviceIntegration.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cmtypes');
        AddUnit('cmapplication');
        AddUnit('cmiccprofile');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('CMICCProfile.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('CMMComponent.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('cmtypes');
        AddUnit('cmiccprofile');
        AddUnit('cmapplication');
        AddUnit('quickdraw');
        AddUnit('components');
      end;
  T:=P.Targets.AddUnit('CMPRComponent.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cmtypes');
        AddUnit('cmiccprofile');
        AddUnit('quickdraw');
        AddUnit('components');
        AddUnit('cmapplication');
      end;
  T:=P.Targets.AddUnit('CMScriptingPlugin.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cmtypes');
        AddUnit('aedatamodel');
        AddUnit('files');
        AddUnit('cmapplication');
      end;
  T:=P.Targets.AddUnit('CMTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('CarbonEvents.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cgcontext');
        AddUnit('quickdraw');
        AddUnit('axuielement');
        AddUnit('drag');
        AddUnit('cfarray');
        AddUnit('hiobjectcore');
        AddUnit('events');
        AddUnit('menus');
        AddUnit('controls');
        AddUnit('cfstring');
        AddUnit('aeregistry');
        AddUnit('aedatamodel');
        AddUnit('carboneventscore');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddUnit('CarbonEventsCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('CodeFragments.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbundle');
        AddUnit('files');
        AddUnit('multiprocessing');
      end;
  T:=P.Targets.AddUnit('Collections.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('ColorPicker.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cmtypes');
        AddUnit('cmiccprofile');
        AddUnit('mixedmode');
        AddUnit('quickdraw');
        AddUnit('cmapplication');
        AddUnit('events');
      end;
  T:=P.Targets.AddUnit('Components.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('ConditionalMacros.pas');
  T:=P.Targets.AddUnit('ControlDefinitions.pas');
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
  T:=P.Targets.AddUnit('Controls.pas');
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
  T:=P.Targets.AddUnit('CoreAudioTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('CoreFoundation.pas');
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
  T:=P.Targets.AddUnit('CoreGraphics.pas');
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
        AddUnit('cgdisplayfade');
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
  T:=P.Targets.AddUnit('DHCPClientPreferences.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('DateTimeUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('Debugging.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('Devices.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('osutils');
        AddUnit('files');
        AddUnit('quickdraw');
        AddUnit('nameregistry');
        AddUnit('codefragments');
        AddUnit('multiprocessing');
        AddUnit('driverfamilymatching');
      end;
  T:=P.Targets.AddUnit('Dialogs.pas');
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
  T:=P.Targets.AddUnit('Dictionary.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('files');
        AddUnit('aeregistry');
        AddUnit('codefragments');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('Displays.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('cmtypes');
        AddUnit('aedatamodel');
        AddUnit('conditionalmacros');
        AddUnit('components');
        AddUnit('cmapplication');
        AddUnit('appleevents');
        AddUnit('events');
        AddUnit('processes');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddUnit('Drag.pas');
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
  T:=P.Targets.AddUnit('DrawSprocket.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('qdoffscreen');
        AddUnit('displays');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('DriverFamilyMatching.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('nameregistry');
        AddUnit('codefragments');
      end;
  T:=P.Targets.AddUnit('DriverGestalt.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('osutils');
        AddUnit('nameregistry');
        AddUnit('scsi');
        AddUnit('usb');
      end;
  T:=P.Targets.AddUnit('DriverSynchronization.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('Endian.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Events.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('osutils');
        AddUnit('quickdraw');
        AddUnit('endian');
      end;
  T:=P.Targets.AddUnit('FPCMacOSAll.pas');
  T:=P.Targets.AddUnit('FileTypesAndCreators.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('Files.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('osutils');
        AddUnit('textcommon');
        AddUnit('utcutils');
        AddUnit('finder');
      end;
  T:=P.Targets.AddUnit('FindByContent.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('files');
        AddUnit('macerrors');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('Finder.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('FinderRegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aeregistry');
        AddUnit('osa');
      end;
  T:=P.Targets.AddUnit('FixMath.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Folders.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('FontPanel.pas');
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
  T:=P.Targets.AddUnit('FontSync.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('atstypes');
        AddUnit('files');
        AddUnit('fonts');
        AddUnit('sfnttypes');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('Fonts.pas');
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
  T:=P.Targets.AddUnit('GPCStrings.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('GXTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('fixmath');
      end;
  T:=P.Targets.AddUnit('GestaltEqu.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('HFSVolumes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('finder');
      end;
  T:=P.Targets.AddUnit('HIArchive.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdata');
        AddUnit('cfnumber');
        AddUnit('hiobject');
      end;
  T:=P.Targets.AddUnit('HIGeometry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddUnit('HIMovieView.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('higeometry');
        AddUnit('hiview');
        AddUnit('movies');
      end;
  T:=P.Targets.AddUnit('HIObject.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfbundle');
        AddUnit('events');
        AddUnit('carbonevents');
        AddUnit('hiobjectcore');
        AddUnit('axuielement');
        AddUnit('cfdictionary');
        AddUnit('carboneventscore');
      end;
  T:=P.Targets.AddUnit('HIObjectCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('HIShape.pas');
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
  T:=P.Targets.AddUnit('HITextUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('HITheme.pas');
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
  T:=P.Targets.AddUnit('HIToolbar.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfarray');
        AddUnit('cfbase');
        AddUnit('cgimage');
        AddUnit('icons');
        AddUnit('menus');
        AddUnit('hiobjectcore');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddUnit('HIToolbox.pas');
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
  T:=P.Targets.AddUnit('HIView.pas');
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
  T:=P.Targets.AddUnit('HTMLRendering.pas');
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
  T:=P.Targets.AddUnit('HostTime.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('coreaudiotypes');
      end;
  T:=P.Targets.AddUnit('IBCarbonRuntime.pas');
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
  T:=P.Targets.AddUnit('ICAApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('files');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('ICACamera.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('ICADevice.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('icaapplication');
      end;
  T:=P.Targets.AddUnit('Icons.pas');
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
  T:=P.Targets.AddUnit('ImageCodec.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('qdoffscreen');
        AddUnit('osutils');
        AddUnit('dialogs');
        AddUnit('quickdraw');
        AddUnit('components');
        AddUnit('gxtypes');
        AddUnit('imagecompression');
        AddUnit('movies');
      end;
  T:=P.Targets.AddUnit('ImageCompression.pas');
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
  T:=P.Targets.AddUnit('InternetConfig.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('aliases');
        AddUnit('components');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddUnit('IntlResources.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Keyboards.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('KeychainCore.pas');
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
  T:=P.Targets.AddUnit('KeychainHI.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('keychaincore');
        AddUnit('cfstring');
        AddUnit('cfarray');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddUnit('LanguageAnalysis.pas');
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
  T:=P.Targets.AddUnit('LaunchServices.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfstring');
        AddUnit('files');
        AddUnit('cfurl');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddUnit('Lists.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('quickdraw');
        AddUnit('controls');
      end;
  T:=P.Targets.AddUnit('LowMem.pas');
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
  T:=P.Targets.AddUnit('MacApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgimage');
        AddUnit('menus');
        AddUnit('cgcontext');
        AddUnit('quickdraw');
        AddUnit('hiobjectcore');
        AddUnit('textcommon');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('MacErrors.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('MacHelp.pas');
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
  T:=P.Targets.AddUnit('MacLocales.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('MacMemory.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('MacOS.pas');
    with T.Dependencies do
      begin
        AddUnit('abactions');
        AddUnit('abaddressbook');
        AddUnit('abglobals');
        AddUnit('abpeoplepicker');
        AddUnit('abtypedefs');
        AddUnit('aedatamodel');
        AddUnit('aehelpers');
        AddUnit('aeinteraction');
        AddUnit('aemach');
        AddUnit('aeobjects');
        AddUnit('aepackobject');
        AddUnit('aeregistry');
        AddUnit('aeusertermtypes');
        AddUnit('aiff');
        AddUnit('asdebugging');
        AddUnit('asregistry');
        AddUnit('atsfont');
        AddUnit('atslayouttypes');
        AddUnit('atstypes');
        AddUnit('atsunicodedirectaccess');
        AddUnit('atsunicodedrawing');
        AddUnit('atsunicodeflattening');
        AddUnit('atsunicodefonts');
        AddUnit('atsunicodeglyphs');
        AddUnit('atsunicodeobjects');
        AddUnit('atsunicodetypes');
        AddUnit('avltree');
        AddUnit('axactionconstants');
        AddUnit('axattributeconstants');
        AddUnit('axerrors');
        AddUnit('axnotificationconstants');
        AddUnit('axroleconstants');
        AddUnit('axtextattributedstring');
        AddUnit('axuielement');
        AddUnit('axvalue');
        AddUnit('axvalueconstants');
        AddUnit('aliases');
        AddUnit('appearance');
        AddUnit('applediskpartitions');
        AddUnit('appleevents');
        AddUnit('applehelp');
        AddUnit('applescript');
        AddUnit('audiohardware');
        AddUnit('authsession');
        AddUnit('authorization');
        AddUnit('authorizationdb');
        AddUnit('authorizationtags');
        AddUnit('cfarray');
        AddUnit('cfattributedstring');
        AddUnit('cfbag');
        AddUnit('cfbase');
        AddUnit('cfbinaryheap');
        AddUnit('cfbitvector');
        AddUnit('cfbundle');
        AddUnit('cfbyteorders');
        AddUnit('cfcalendar');
        AddUnit('cfcharacterset');
        AddUnit('cfdata');
        AddUnit('cfdate');
        AddUnit('cfdateformatter');
        AddUnit('cfdictionary');
        AddUnit('cfftpstream');
        AddUnit('cfhttpmessage');
        AddUnit('cfhttpstream');
        AddUnit('cfhost');
        AddUnit('cflocale');
        AddUnit('cfmachport');
        AddUnit('cfmessageport');
        AddUnit('cfnetservices');
        AddUnit('cfnotificationcenter');
        AddUnit('cfnumber');
        AddUnit('cfnumberformatter');
        AddUnit('cfplugin');
        AddUnit('cfplugincom');
        AddUnit('cfpreferences');
        AddUnit('cfpropertylist');
        AddUnit('cfrunloop');
        AddUnit('cfset');
        AddUnit('cfsocket');
        AddUnit('cfsocketstream');
        AddUnit('cfstream');
        AddUnit('cfstring');
        AddUnit('cfstringencodingext');
        AddUnit('cftimezone');
        AddUnit('cftree');
        AddUnit('cfurl');
        AddUnit('cfurlaccess');
        AddUnit('cfuuid');
        AddUnit('cfusernotification');
        AddUnit('cfxmlnode');
        AddUnit('cfxmlparser');
        AddUnit('cgaffinetransforms');
        AddUnit('cgbase');
        AddUnit('cgbitmapcontext');
        AddUnit('cgcolor');
        AddUnit('cgcolorspace');
        AddUnit('cgcontext');
        AddUnit('cgdataconsumer');
        AddUnit('cgdataprovider');
        AddUnit('cgdirectdisplay');
        AddUnit('cgdirectpalette');
        AddUnit('cgdisplayconfiguration');
        AddUnit('cgdisplayfade');
        AddUnit('cgerrors');
        AddUnit('cgevent');
        AddUnit('cgeventsource');
        AddUnit('cgeventtypes');
        AddUnit('cgfont');
        AddUnit('cgfunction');
        AddUnit('cgglcontext');
        AddUnit('cggeometry');
        AddUnit('cgimage');
        AddUnit('cgimagedestination');
        AddUnit('cgimageproperties');
        AddUnit('cgimagesource');
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
        AddUnit('cmapplication');
        AddUnit('cmcalibrator');
        AddUnit('cmdeviceintegration');
        AddUnit('cmiccprofile');
        AddUnit('cmmcomponent');
        AddUnit('cmprcomponent');
        AddUnit('cmscriptingplugin');
        AddUnit('cmtypes');
        AddUnit('carbonevents');
        AddUnit('carboneventscore');
        AddUnit('codefragments');
        AddUnit('collections');
        AddUnit('colorpicker');
        AddUnit('components');
        AddUnit('conditionalmacros');
        AddUnit('controldefinitions');
        AddUnit('controls');
        AddUnit('coreaudiotypes');
        AddUnit('corefoundation');
        AddUnit('coregraphics');
        AddUnit('dhcpclientpreferences');
        AddUnit('datetimeutils');
        AddUnit('debugging');
        AddUnit('devices');
        AddUnit('dialogs');
        AddUnit('dictionary');
        AddUnit('displays');
        AddUnit('drag');
        AddUnit('drawsprocket');
        AddUnit('driverfamilymatching');
        AddUnit('drivergestalt');
        AddUnit('driversynchronization');
        AddUnit('endian');
        AddUnit('events');
        AddUnit('filetypesandcreators');
        AddUnit('files');
        AddUnit('findbycontent');
        AddUnit('finder');
        AddUnit('finderregistry');
        AddUnit('fixmath');
        AddUnit('folders');
        AddUnit('fontpanel');
        AddUnit('fontsync');
        AddUnit('fonts');
        AddUnit('gxtypes');
        AddUnit('gestaltequ');
        AddUnit('hfsvolumes');
        AddUnit('hiarchive');
        AddUnit('higeometry');
        AddUnit('himovieview');
        AddUnit('hiobject');
        AddUnit('hiobjectcore');
        AddUnit('hishape');
        AddUnit('hitextutils');
        AddUnit('hitheme');
        AddUnit('hitoolbar');
        AddUnit('hitoolbox');
        AddUnit('hiview');
        AddUnit('htmlrendering');
        AddUnit('hosttime');
        AddUnit('ibcarbonruntime');
        AddUnit('icaapplication');
        AddUnit('icacamera');
        AddUnit('icadevice');
        AddUnit('icons');
        AddUnit('imagecodec');
        AddUnit('imagecompression');
        AddUnit('internetconfig');
        AddUnit('intlresources');
        AddUnit('keyboards');
        AddUnit('keychaincore');
        AddUnit('keychainhi');
        AddUnit('languageanalysis');
        AddUnit('launchservices');
        AddUnit('lists');
        AddUnit('lowmem');
        AddUnit('macapplication');
        AddUnit('macerrors');
        AddUnit('machelp');
        AddUnit('maclocales');
        AddUnit('macmemory');
        AddUnit('macosxposix');
        AddUnit('mactexteditor');
        AddUnit('mactypes');
        AddUnit('macwindows');
        AddUnit('machineexceptions');
        AddUnit('math64');
        AddUnit('mediahandlers');
        AddUnit('menus');
        AddUnit('mixedmode');
        AddUnit('movies');
        AddUnit('moviesformat');
        AddUnit('multiprocessing');
        AddUnit('nsl');
        AddUnit('nslcore');
        AddUnit('nameregistry');
        AddUnit('navigation');
        AddUnit('notification');
        AddUnit('numberformatting');
        AddUnit('osa');
        AddUnit('osacomp');
        AddUnit('osageneric');
        AddUnit('osutils');
        AddUnit('opentransport');
        AddUnit('opentransportprotocol');
        AddUnit('opentransportproviders');
        AddUnit('pefbinaryformat');
        AddUnit('plstringfuncs');
        AddUnit('pmapplication');
        AddUnit('pmcore');
        AddUnit('pmdefinitions');
        AddUnit('palettes');
        AddUnit('pasteboard');
        AddUnit('pictutils');
        AddUnit('power');
        AddUnit('printing');
        AddUnit('processes');
        AddUnit('qdoffscreen');
        AddUnit('qdpicttocgcontext');
        AddUnit('qtml');
        AddUnit('qtsmovie');
        AddUnit('qtstreamingcomponents');
        AddUnit('quicktimecomponents');
        AddUnit('quicktimemusic');
        AddUnit('quicktimestreaming');
        AddUnit('quicktimevr');
        AddUnit('quicktimevrformat');
        AddUnit('quickdraw');
        AddUnit('quickdrawtext');
        AddUnit('resources');
        AddUnit('scdynamicstore');
        AddUnit('scdynamicstorecopydhcpinfos');
        AddUnit('scdynamicstorecopyspecific');
        AddUnit('scdynamicstorekey');
        AddUnit('scnetwork');
        AddUnit('scnetworkconnection');
        AddUnit('scnetworkreachability');
        AddUnit('scpreferences');
        AddUnit('scpreferencespath');
        AddUnit('scpreferencessetspecific');
        AddUnit('scsi');
        AddUnit('scschemadefinitions');
        AddUnit('sfntlayouttypes');
        AddUnit('sfnttypes');
        AddUnit('scrap');
        AddUnit('script');
        AddUnit('sound');
        AddUnit('speechrecognition');
        AddUnit('speechsynthesis');
        AddUnit('stringcompare');
        AddUnit('systemconfiguration');
        AddUnit('systemsound');
        AddUnit('tsmte');
        AddUnit('textcommon');
        AddUnit('textedit');
        AddUnit('textencodingconverter');
        AddUnit('textencodingplugin');
        AddUnit('textservices');
        AddUnit('textutils');
        AddUnit('timer');
        AddUnit('toolutils');
        AddUnit('translation');
        AddUnit('translationextensions');
        AddUnit('translationservices');
        AddUnit('typeselect');
        AddUnit('urlaccess');
        AddUnit('usb');
        AddUnit('utcutils');
        AddUnit('utcoretypes');
        AddUnit('uttype');
        AddUnit('unicodeconverter');
        AddUnit('unicodeutilities');
        AddUnit('universalaccess');
        AddUnit('worldscript');
        AddUnit('fenv');
        AddUnit('vblas');
        AddUnit('vdsp');
        AddUnit('xattr');
      end;
  T:=P.Targets.AddUnit('MacOSXPosix.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('MacTextEditor.pas');
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
        AddUnit('hiobjectcore');
        AddUnit('hiview');
        AddUnit('higeometry');
      end;
  T:=P.Targets.AddUnit('MacTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('MacWindows.pas');
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
  T:=P.Targets.AddUnit('MachineExceptions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('Math64.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('MediaHandlers.pas');
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
  T:=P.Targets.AddUnit('Menus.pas');
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
  T:=P.Targets.AddUnit('MixedMode.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Movies.pas');
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
  T:=P.Targets.AddUnit('MoviesFormat.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('imagecompression');
        AddUnit('components');
        AddUnit('movies');
      end;
  T:=P.Targets.AddUnit('Multiprocessing.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('NSL.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
        AddUnit('nslcore');
      end;
  T:=P.Targets.AddUnit('NSLCore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('codefragments');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('NameRegistry.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Navigation.pas');
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
  T:=P.Targets.AddUnit('Notification.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('osutils');
      end;
  T:=P.Targets.AddUnit('NumberFormatting.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
        AddUnit('intlresources');
      end;
  T:=P.Targets.AddUnit('OSA.pas');
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
  T:=P.Targets.AddUnit('OSAComp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddUnit('OSAGeneric.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
        AddUnit('components');
        AddUnit('macerrors');
        AddUnit('appleevents');
        AddUnit('osa');
      end;
  T:=P.Targets.AddUnit('OSUtils.pas');
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
  T:=P.Targets.AddUnit('OpenTransport.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('OpenTransportProtocol.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
        AddUnit('files');
        AddUnit('codefragments');
        AddUnit('opentransport');
      end;
  T:=P.Targets.AddUnit('OpenTransportProviders.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('opentransport');
      end;
  T:=P.Targets.AddUnit('PEFBinaryFormat.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('PLStringFuncs.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('PMApplication.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('pmdefinitions');
        AddUnit('pmcore');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddUnit('PMCore.pas');
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
        AddUnit('cmapplication');
        AddUnit('pmdefinitions');
      end;
  T:=P.Targets.AddUnit('PMDefinitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('Palettes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddUnit('Pasteboard.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdata');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('PictUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('palettes');
      end;
  T:=P.Targets.AddUnit('Power.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('multiprocessing');
        AddUnit('nameregistry');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('Printing.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('quickdraw');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddUnit('Processes.pas');
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
  T:=P.Targets.AddUnit('QDOffscreen.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddUnit('QDPictToCGContext.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cgcontext');
        AddUnit('cggeometry');
        AddUnit('cgdataprovider');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('QTML.pas');
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
  T:=P.Targets.AddUnit('QTSMovie.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('components');
        AddUnit('movies');
        AddUnit('quicktimestreaming');
      end;
  T:=P.Targets.AddUnit('QTStreamingComponents.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('dialogs');
        AddUnit('components');
        AddUnit('movies');
        AddUnit('quicktimestreaming');
      end;
  T:=P.Targets.AddUnit('QuickTimeComponents.pas');
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
  T:=P.Targets.AddUnit('QuickTimeMusic.pas');
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
  T:=P.Targets.AddUnit('QuickTimeStreaming.pas');
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
  T:=P.Targets.AddUnit('QuickTimeVR.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('movies');
      end;
  T:=P.Targets.AddUnit('QuickTimeVRFormat.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('conditionalmacros');
        AddUnit('movies');
        AddUnit('quicktimevr');
      end;
  T:=P.Targets.AddUnit('Quickdraw.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cmtypes');
        AddUnit('cgdirectdisplay');
        AddUnit('components');
        AddUnit('mixedmode');
        AddUnit('quickdrawtext');
        AddUnit('cgcontext');
      end;
  T:=P.Targets.AddUnit('QuickdrawText.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('intlresources');
      end;
  T:=P.Targets.AddUnit('Resources.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('SCDynamicStore.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfrunloop');
        AddUnit('cfpropertylist');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('SCDynamicStoreCopyDHCPInfos.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scdynamicstore');
        AddUnit('cfdictionary');
        AddUnit('cfdata');
        AddUnit('cfdate');
      end;
  T:=P.Targets.AddUnit('SCDynamicStoreCopySpecific.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scdynamicstore');
        AddUnit('macosxposix');
        AddUnit('cfstring');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('SCDynamicStoreKey.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('SCNetwork.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('macosxposix');
      end;
  T:=P.Targets.AddUnit('SCNetworkConnection.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfdictionary');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddUnit('SCNetworkReachability.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scnetwork');
        AddUnit('macosxposix');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddUnit('SCPreferences.pas');
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
  T:=P.Targets.AddUnit('SCPreferencesPath.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scpreferences');
        AddUnit('cfdictionary');
      end;
  T:=P.Targets.AddUnit('SCPreferencesSetSpecific.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('scpreferences');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('SCSI.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('applediskpartitions');
      end;
  T:=P.Targets.AddUnit('SCSchemaDefinitions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('SFNTLayoutTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('SFNTTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('Scrap.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('mixedmode');
        AddUnit('macerrors');
        AddUnit('cfstring');
      end;
  T:=P.Targets.AddUnit('Script.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('intlresources');
      end;
  T:=P.Targets.AddUnit('Sound.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('components');
        AddUnit('mixedmode');
        AddUnit('dialogs');
      end;
  T:=P.Targets.AddUnit('SpeechRecognition.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('aedatamodel');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('SpeechSynthesis.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('files');
      end;
  T:=P.Targets.AddUnit('StringCompare.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('textcommon');
        AddUnit('script');
        AddUnit('typeselect');
      end;
  T:=P.Targets.AddUnit('SystemConfiguration.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('SystemSound.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('cfbase');
        AddUnit('cfrunloop');
      end;
  T:=P.Targets.AddUnit('TSMTE.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textedit');
        AddUnit('dialogs');
        AddUnit('appleevents');
        AddUnit('textservices');
      end;
  T:=P.Targets.AddUnit('TextCommon.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('aedatamodel');
      end;
  T:=P.Targets.AddUnit('TextEdit.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdraw');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('TextEncodingConverter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddUnit('TextEncodingPlugin.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('textencodingconverter');
      end;
  T:=P.Targets.AddUnit('TextServices.pas');
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
  T:=P.Targets.AddUnit('TextUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('intlresources');
        AddUnit('numberformatting');
        AddUnit('stringcompare');
        AddUnit('datetimeutils');
      end;
  T:=P.Targets.AddUnit('Threads.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('mixedmode');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('Timer.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
        AddUnit('osutils');
      end;
  T:=P.Targets.AddUnit('ToolUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('fixmath');
        AddUnit('textutils');
        AddUnit('icons');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddUnit('Translation.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('components');
        AddUnit('translationextensions');
      end;
  T:=P.Targets.AddUnit('TranslationExtensions.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('quickdraw');
        AddUnit('components');
      end;
  T:=P.Targets.AddUnit('TranslationServices.pas');
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
  T:=P.Targets.AddUnit('TypeSelect.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('events');
      end;
  T:=P.Targets.AddUnit('URLAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('codefragments');
        AddUnit('macerrors');
        AddUnit('events');
      end;
  T:=P.Targets.AddUnit('USB.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('files');
        AddUnit('nameregistry');
        AddUnit('codefragments');
        AddUnit('devices');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('UTCUtils.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('macerrors');
      end;
  T:=P.Targets.AddUnit('UTCoreTypes.pas');
    with T.Dependencies do
      begin
        AddUnit('cfbase');
      end;
  T:=P.Targets.AddUnit('UTType.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cfbase');
        AddUnit('cfarray');
        AddUnit('cfdictionary');
        AddUnit('cfurl');
      end;
  T:=P.Targets.AddUnit('UnicodeConverter.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('textcommon');
        AddUnit('mixedmode');
      end;
  T:=P.Targets.AddUnit('UnicodeUtilities.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('maclocales');
        AddUnit('textcommon');
      end;
  T:=P.Targets.AddUnit('UniversalAccess.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('cggeometry');
      end;
  T:=P.Targets.AddUnit('Video.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('nameregistry');
        AddUnit('quickdraw');
      end;
  T:=P.Targets.AddUnit('WorldScript.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('quickdrawtext');
      end;
  T:=P.Targets.AddUnit('fenv.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;
  T:=P.Targets.AddUnit('fp.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('vBLAS.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('vDSP.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
        AddUnit('conditionalmacros');
      end;
  T:=P.Targets.AddUnit('xattr.pas');
    with T.Dependencies do
      begin
        AddUnit('mactypes');
      end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
