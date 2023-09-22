{
  objcrtliPhoneOS.pas

  Copyright (C) 2009 Dmitry Boyarintsev

  This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 2.0
  headers included with XCode 3.1.2
  The original copyright note of is kept on each include file
}

{$IFNDEF FPC_DOTTEDUNITS}
unit objcrtliPhoneOS;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.ObjC.Rtl, Api.ObjC.Rtl20;
{$ELSE FPC_DOTTEDUNITS}
uses
  objcrtl, objcrtl20;
{$ENDIF FPC_DOTTEDUNITS}

implementation

initialization
  InitializeObjcRtl20(DefaultObjCLibName);

end.

