{ **********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    File/Directory watch component: generic implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
  { TGenericDriver }

  TGenericDriver = Class(TDirListDriver)
  Public
    Destructor destroy; Override;
    Procedure Init; override;
    function DoCheck : cardinal; override;
    Procedure Done; override;
  end;


procedure TGenericDriver.Done;

var
  I : Integer;
  D : TObject;

begin
  if not Assigned(Watch) then
    exit;
  For I:=0 to Watch.Watches.Count-1 do
    begin
    D:=Watch.Watches[i].DriverData;
    Watch.Watches[i].DriverData:=Nil;
    FreeAndNil(D);
    end;

end;

destructor TGenericDriver.destroy;
begin
  Done;
  inherited destroy;
end;

procedure TGenericDriver.Init;

Var
  I : Integer;
  lData : TDirData;
begin
  For I:=0 to Watch.Watches.Count-1 do
    begin
    lData:=TDirData.Create(Watch,Watch.Watches[i]);
    Watch.Watches[i].DriverData:=lData;
    lData.InitWatch;
    end;
end;


function TGenericDriver.DoCheck: cardinal;

var
  I : integer;
  lData : TDirData;

begin
  Result:=0;
  I:=0;
  While (Not Terminated) and (I<Watch.Watches.Count) do
    begin
    lData:=TDirData(Watch.Watches[i].DriverData);
    Result:=Result+LData.Check;
    Inc(I);
    end;
end;


