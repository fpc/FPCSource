{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2019 by the Free Pascal development team

    This file provides the base for the pluggable sorting algorithm
    support. It also provides a default QuickSort implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utcdevices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, types, system.devices;

type
  TMyDeviceInfo = Class(TBaseDeviceInfo)
  //
  end;

  { TTestDevices }

  TTestDevices= class(TTestCase)
  private
    FDevice: TMyDeviceInfo;
    procedure AddSampleDevice1;
    procedure AddSampleDevice2;
    procedure AssertSampleDevice2(aDevice: TBaseDeviceInfo);
    procedure CreateEmpty;
    procedure AssertSampleDevice(aDevice: TBaseDeviceInfo);
    function CreateSampleDevice: TMyDeviceInfo;
    function CreateSampleDevice2: TMyDeviceInfo;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure AssertEquals(Msg : String; aExpected,aActual : TSize); overload;
    Procedure AssertEquals(Msg : String; aExpected,aActual : TDeviceInfo.TDeviceClass); overload;
    Procedure AssertEquals(Msg : String; aExpected,aActual : TOSVersion.TPlatform);overload;
    // Freed in teardown
    Property Device : TMyDeviceInfo Read FDevice Write FDevice;
  published
    procedure TestHookUp;
    Procedure TestCreate;
    Procedure TestCreateEmpty;
    Procedure TestDiagonal;
    Procedure TestAddDevice;
    Procedure TestAttribute;
    procedure TestSelect;
    procedure TestSelect2;
    procedure TestSelect3;
  end;

implementation

uses typinfo;

procedure TTestDevices.TestHookUp;
begin
  AssertNull('No this device',TDeviceInfo.ThisDevice);
  AssertEquals('No devices',0, TDeviceInfo.DeviceCount);
end;

function TTestDevices.CreateSampleDevice: TMyDeviceInfo;

begin
  Result:=TMyDeviceInfo.Create(TDeviceInfo.TDeviceClass.Desktop,'1',
                               TSize.Create(1200,900),
                               TSize.Create(2400,1800),
                               TSize.Create(1900,1200),
                               TSize.Create(3800,2400),TOSVersion.TPlatform.pfLinux,96,True);
end;

function TTestDevices.CreateSampleDevice2: TMyDeviceInfo;
begin
  Result:=TMyDeviceInfo.Create(TDeviceInfo.TDeviceClass.Tablet,'2',
                        TSize.Create(1600,1024),
                        TSize.Create(3200,2048),
                        TSize.Create(2048,1600),
                        TSize.Create(4096,3200),TOSVersion.TPlatform.pfLinux,128,False);

end;

procedure TTestDevices.TestCreate;

begin
  Device:=CreateSampleDevice;
  AssertSampleDevice(Device);
end;

procedure TTestDevices.AssertSampleDevice(aDevice : TBaseDeviceInfo);

begin
  AssertEquals('MinPhysicalScreenSize',TSize.Create(1200,900),aDevice.MinPhysicalScreenSize);
  AssertEquals('MinLogicalScreenSize',TSize.Create(2400,1800),aDevice.MinLogicalScreenSize);
  AssertEquals('MaxPhysicalScreenSize',TSize.Create(1900,1200),aDevice.MaxPhysicalScreenSize);
  AssertEquals('MaxLogicalScreenSize',TSize.Create(3800,2400),aDevice.MaxLogicalScreenSize);
  AssertEquals('Platform',TOSVersion.TPlatform.pfLinux,aDevice.Platform);
  AssertEquals('DeviceClass',TDeviceInfo.TDeviceClass.Desktop,aDevice.DeviceClass);
  AssertEquals('ID','1',aDevice.ID);
  AssertEquals('PixelsPerInch',96,aDevice.PixelsPerInch);
  AssertEquals('Exclusive',True,aDevice.Exclusive);
end;

procedure TTestDevices.AssertSampleDevice2(aDevice : TBaseDeviceInfo);

begin
  AssertEquals('MinPhysicalScreenSize',TSize.Create(1600,1024),aDevice.MinPhysicalScreenSize);
  AssertEquals('MinLogicalScreenSize',TSize.Create(3200,2048),aDevice.MinLogicalScreenSize);
  AssertEquals('MaxPhysicalScreenSize',TSize.Create(2048,1600),aDevice.MaxPhysicalScreenSize);
  AssertEquals('MaxLogicalScreenSize',TSize.Create(4096,3200),aDevice.MaxLogicalScreenSize);
  AssertEquals('Platform',TOSVersion.TPlatform.pfLinux,aDevice.Platform);
  AssertEquals('DeviceClass',TDeviceInfo.TDeviceClass.Tablet,aDevice.DeviceClass);
  AssertEquals('ID','2',aDevice.ID);
  AssertEquals('PixelsPerInch',128,aDevice.PixelsPerInch);
  AssertEquals('Exclusive',False,aDevice.Exclusive);
end;

procedure TTestDevices.CreateEmpty;

begin
  TDeviceInfo.create;
end;

procedure TTestDevices.TestCreateEmpty;
begin
  AssertException('Cannot create directly',ENoConstructException,@CreateEmpty);
end;

procedure TTestDevices.TestDiagonal;

Var
  D: Single;

begin
  Device:=CreateSampleDevice;
  D:=Sqrt(Sqr(1900)+Sqr(1200))/96;
  AssertEquals('MaxDiagonal',D,Device.MaxDiagonal);
  D:=Sqrt(Sqr(1200)+Sqr(900))/96;
  AssertEquals('MinDiagonal',D,Device.MinDiagonal);
end;

procedure TTestDevices.AddSampleDevice1;
begin
  TDeviceInfo.AddDevice(TDeviceInfo.TDeviceClass.Desktop,'1',
                        TSize.Create(1200,900),
                        TSize.Create(2400,1800),
                        TSize.Create(1900,1200),
                        TSize.Create(3800,2400),TOSVersion.TPlatform.pfLinux,96,True);
end;

procedure TTestDevices.AddSampleDevice2;
begin
  TDeviceInfo.AddDevice(TDeviceInfo.TDeviceClass.Tablet,'2',
                        TSize.Create(1600,1024),
                        TSize.Create(3200,2048),
                        TSize.Create(2048,1600),
                        TSize.Create(4096,3200),TOSVersion.TPlatform.pfLinux,128,False);
end;

procedure TTestDevices.TestAddDevice;
begin
  AddSampleDevice1;
  AssertEquals('Count correct',1,TDeviceInfo.DeviceCount);
  AssertSampleDevice(TDeviceInfo.Devices[0]);
  AddSampleDevice2;
  AssertEquals('Count correct',2,TDeviceInfo.DeviceCount);
  AssertSampleDevice2(TDeviceInfo.Devices[1]);
end;

procedure TTestDevices.TestAttribute;
begin
  Device:=CreateSampleDevice;
  Device.AddAttribute('tutu','toto');
  AssertEquals('Has existing attribute',True,Device.HasAttribute('tutu'));
  AssertEquals('Does not have non-existing attribute',False,Device.HasAttribute('titi'));
  AssertEquals('Value of existing attribute','toto',Device.Attributes['tutu']);
  AssertEquals('Value of non-existing attribute','',Device.Attributes['titi']);
end;

procedure TTestDevices.TestSelect;

Var
  Arr : TDeviceInfoArray;

begin
  AddSampleDevice1;
  AddSampleDevice2;
  Arr:=TDeviceInfo.SelectDevices(TDeviceInfo.TDeviceClass.Automotive,TSize.Create(100,100),TSize.Create(200,200),TOSVersion.TPlatform.pfAndroid,96,False);
  AssertEquals('Empty, platform does not match',0,Length(Arr));
end;

procedure TTestDevices.TestSelect2;

Var
  Arr : TDeviceInfoArray;

begin
  AddSampleDevice1;
  AddSampleDevice2;
  Arr:=TDeviceInfo.SelectDevices(TDeviceInfo.TDeviceClass.Desktop,TSize.Create(100,100),TSize.Create(200,200),TOSVersion.TPlatform.pfLinux,96,False);
  AssertEquals('platform matches',2,Length(Arr));
  AssertSame('El 1',TDeviceInfo.Devices[0],Arr[1]);
  AssertSame('El 2',TDeviceInfo.Devices[1],Arr[0]);
end;

procedure TTestDevices.TestSelect3;
Var
  Arr : TDeviceInfoArray;

begin
  AddSampleDevice1;
  AddSampleDevice2;
  Arr:=TDeviceInfo.SelectDevices(TDeviceInfo.TDeviceClass.Desktop,TSize.Create(1200,900),TSize.Create(2400,1800),TOSVersion.TPlatform.pfLinux,96,False);
  AssertEquals('platform matches',2,Length(Arr));
  AssertSame('El 1',TDeviceInfo.Devices[0],Arr[0]);
  AssertSame('El 2',TDeviceInfo.Devices[1],Arr[1]);
end;

procedure TTestDevices.SetUp;
begin
  TDeviceInfo.ClearDevices;
end;

procedure TTestDevices.TearDown;

begin
  TDeviceInfo.ClearDevices;
  FreeAndNil(FDevice);
end;

procedure TTestDevices.AssertEquals(Msg: String; aExpected, aActual: TSize);
begin
  AssertEquals(Msg+': cx',aExpected.cx,aActual.cx);
  AssertEquals(Msg+': cy',aExpected.cy,aActual.cy);
end;

procedure TTestDevices.AssertEquals(Msg: String; aExpected,
  aActual: TDeviceInfo.TDeviceClass);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TDeviceInfo.TDeviceClass),Ord(aExpected)),
                   GetEnumName(TypeInfo(TDeviceInfo.TDeviceClass),Ord(aActual)));
end;

procedure TTestDevices.AssertEquals(Msg: String; aExpected,
  aActual: TOSVersion.TPlatform);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TOSVersion.TPlatform),Ord(aExpected)),
                   GetEnumName(TypeInfo(TOSVersion.TPlatform),Ord(aActual)));
end;


initialization
  RegisterTest(TTestDevices);
end.

