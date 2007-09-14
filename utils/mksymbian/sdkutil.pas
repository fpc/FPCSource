{
sdkutil.pas

SDK utility methods

Copyright (C) 2006-2007 Felipe Monteiro de Carvalho

This file is part of MkSymbian build tool.

MkSymbian is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

MkSymbian is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating MkSymbian into proprietary programs.
}
unit sdkutil;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, registry;

type

  { TSDKUtil }

  TSDKUtil = class(TObject)
  private
    vSDKFolder, vSDKPartialFolder: string;
  public
    constructor Create;
    procedure LocateSDK;

    property SDKFolder: string read vSDKFolder;
    property SDKPartialFolder: string read vSDKPartialFolder;
  end;

var
  vSDKUtil: TSDKUtil;

implementation

{ TSDKUtil }

procedure TSDKUtil.LocateSDK;
var
  Reg: TRegistry;
  BufferStr: string;
begin
  Reg := TRegistry.Create;
 
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SOFTWARE\Symbian\UIQ\SDK\UIQ3SDK', False) then
    begin
      BufferStr := Reg.ReadString('InstallPath');
      vSDKFolder := IncludeTrailingBackslash(BufferStr);
      vSDKPartialFolder := Copy(vSDKFolder, 3, Length(vSDKFolder) - 2);
    end
    else
    begin
      WriteLn('  ERROR: Could not locate the SDK, using default values');
      vSDKPartialFolder := '\Symbian\UIQ3SDK\';
      vSDKFolder := 'C:' + vSDKPartialFolder;
    end;
  finally
    Reg.Free;
  end;
end;

constructor TSDKUtil.Create;
begin
  LocateSDK;
end;

end.

