{
  objcrtliPhoneOS.pas

  Copyright (C) 2009 Dmitry Boyarintsev

  This unit is implementation for dynamic Objective-C Run-time Library based on run-time version 2.0
  headers included with XCode 3.1.2
  The original copyright note of is kept on each include file
}

unit objcrtliPhoneOS;

{$mode objfpc}{$H+}

interface

uses
  objcrtl, objcrtl20;

implementation

initialization
  InitializeObjcRtl20(DefaultObjCLibName);

end.

