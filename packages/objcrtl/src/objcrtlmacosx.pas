{
  objcrtlmacosx.pas

  Copyright (C) 2009 Dmitry Boyarintsev

  This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 2.0
  headers included with XCode 3.1.2
  The original copyright note of is kept on each include file
}

unit objcrtlMacOSX;

{$linkframework CoreServices}
{$mode macpas}

interface

uses
  objcrtl, objcrtl10, objcrtl20;

implementation

type
  SInt16 = Integer;
  SInt32 = LongInt;
  UInt32 = Longword;
  FourCharCode = UInt32;
	OSErr  = SInt16;
  OSType = FourCharCode;

const
  noErr = 0;
	gestaltSystemVersionMinor   = FourCharCode('sys2');                       {  The minor system version number; in 10.4.17 this would be the decimal value 4 }

function Gestalt(selector: OSType; var response: SInt32): OSErr; mwpascal; external name '_Gestalt';

procedure InitObjCRunTime;
var
  MacVersion : SInt32;
begin
  if (Gestalt(gestaltSystemVersionMinor, MacVersion) = noErr) then begin
    if MacVersion >= 5
      then InitializeObjcRtl20(DefaultObjCLibName)
      else InitializeObjcRtl10(DefaultObjCLibName);
  end else
    InitializeObjcRtl20(DefaultObjCLibName);
end;

{initialization}
begin
  InitObjCRuntime;

end.

