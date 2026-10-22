{ %skiptarget=win16 }
{ System declares the RT_* resource types on all targets; Windows and Types declare aliases of them }
program tresrt1;

{$mode objfpc}

uses
{$if defined(win32) or defined(win64) or defined(wince)}
  Windows,
{$endif}
  Types;

{$R tres4.res}

var
  gErrors: Integer = 0;


procedure CheckEqual(const aName: string; aValue, aExpected: PtrUInt);

begin
  if aValue=aExpected then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR: got ',aValue,', expected ',aExpected);
    Inc(gErrors);
    end;
end;


begin
  CheckEqual('System.RT_CURSOR',PtrUInt(System.RT_CURSOR),1);
  CheckEqual('System.RT_BITMAP',PtrUInt(System.RT_BITMAP),2);
  CheckEqual('System.RT_ICON',PtrUInt(System.RT_ICON),3);
  CheckEqual('System.RT_MENU',PtrUInt(System.RT_MENU),4);
  CheckEqual('System.RT_DIALOG',PtrUInt(System.RT_DIALOG),5);
  CheckEqual('System.RT_STRING',PtrUInt(System.RT_STRING),6);
  CheckEqual('System.RT_FONTDIR',PtrUInt(System.RT_FONTDIR),7);
  CheckEqual('System.RT_FONT',PtrUInt(System.RT_FONT),8);
  CheckEqual('System.RT_ACCELERATOR',PtrUInt(System.RT_ACCELERATOR),9);
  CheckEqual('System.RT_RCDATA',PtrUInt(System.RT_RCDATA),10);
  CheckEqual('System.RT_MESSAGETABLE',PtrUInt(System.RT_MESSAGETABLE),11);
  CheckEqual('System.RT_GROUP_CURSOR',PtrUInt(System.RT_GROUP_CURSOR),12);
  CheckEqual('System.RT_GROUP_ICON',PtrUInt(System.RT_GROUP_ICON),14);
  CheckEqual('System.RT_VERSION',PtrUInt(System.RT_VERSION),16);
  CheckEqual('System.RT_ANICURSOR',PtrUInt(System.RT_ANICURSOR),21);
  CheckEqual('System.RT_ANIICON',PtrUInt(System.RT_ANIICON),22);
  CheckEqual('System.RT_HTML',PtrUInt(System.RT_HTML),23);
  CheckEqual('System.RT_MANIFEST',PtrUInt(System.RT_MANIFEST),24);
  CheckEqual('Types.RT_RCDATA',PtrUInt(Types.RT_RCDATA),10);
{$if defined(win32) or defined(win64) or defined(wince)}
  CheckEqual('Windows.RT_CURSOR',PtrUInt(Windows.RT_CURSOR),1);
  CheckEqual('Windows.RT_BITMAP',PtrUInt(Windows.RT_BITMAP),2);
  CheckEqual('Windows.RT_ICON',PtrUInt(Windows.RT_ICON),3);
  CheckEqual('Windows.RT_MENU',PtrUInt(Windows.RT_MENU),4);
  CheckEqual('Windows.RT_DIALOG',PtrUInt(Windows.RT_DIALOG),5);
  CheckEqual('Windows.RT_STRING',PtrUInt(Windows.RT_STRING),6);
  CheckEqual('Windows.RT_FONTDIR',PtrUInt(Windows.RT_FONTDIR),7);
  CheckEqual('Windows.RT_FONT',PtrUInt(Windows.RT_FONT),8);
  CheckEqual('Windows.RT_ACCELERATOR',PtrUInt(Windows.RT_ACCELERATOR),9);
  CheckEqual('Windows.RT_RCDATA',PtrUInt(Windows.RT_RCDATA),10);
  CheckEqual('Windows.RT_MESSAGETABLE',PtrUInt(Windows.RT_MESSAGETABLE),11);
  CheckEqual('Windows.RT_GROUP_CURSOR',PtrUInt(Windows.RT_GROUP_CURSOR),12);
  CheckEqual('Windows.RT_GROUP_ICON',PtrUInt(Windows.RT_GROUP_ICON),14);
  CheckEqual('Windows.RT_VERSION',PtrUInt(Windows.RT_VERSION),16);
{$endif}
  CheckEqual('Unqualified RT_RCDATA',PtrUInt(RT_RCDATA),10);
  CheckEqual('Resource found with RT_RCDATA',Ord(FindResource(HInstance,'mdtoptyt_png',RT_RCDATA)<>0),1);
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
