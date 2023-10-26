{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2023 the Free Pascal development team

   Generic permissions service class.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit System.Permissions;

{$MODE OBJFPC}
{$H+}
{$SCOPEDENUMS ON}
{$modeswitch functionreferences}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Types;
{$ELSE}
  SysUtils, Types;
{$ENDIF}

type
  EPermissionException = class(Exception);
  TProc = reference to procedure;
    
  TPermissionStatus = (Granted, Denied, PermanentlyDenied);
  TPermissionStatusDynArray = array of TPermissionStatus;
  TClassicPermissionStatusDynArray = TPermissionStatusDynArray;
  
  
  TRequestPermissionsResultEvent = 
      procedure(Sender: TObject; 
                const aPermissions: TStringDynArray;
                const aGrantResults: TPermissionStatusDynArray) of object;
                                             
  TRequestPermissionsResultProc = 
    reference to procedure(const aPermissions: TStringDynArray;
                           const aGrantResults: TPermissionStatusDynArray);

  TDisplayRationaleEvent = 
     procedure(Sender: TObject; 
               const aPermissions: TStringDynArray; 
               const aPostRationaleProc: TProc) of object;
  TDisplayRationaleProc = 
     reference to procedure(const aPermissions: TStringDynArray; 
                            const aPostRationaleProc: TProc);

  TPermissionsService = class abstract
  private
    class function GetDefaultService: TPermissionsService; static;
  protected
    class var FDefaultService: TPermissionsService;
    constructor Create; virtual;
  public
    class destructor Done;
    function IsPermissionGranted(const aPermission: string): Boolean; virtual;
    function IsEveryPermissionGranted(const aPermissions: TStringDynArray): Boolean; virtual;
    procedure RequestPermissions(const aPermissions: TStringDynArray;
                                 const aOnRequestPermissionsResult: TRequestPermissionsResultEvent; 
                                 aOnDisplayRationale: TDisplayRationaleEvent = nil); overload; virtual;
    procedure RequestPermissions(const aPermissions: TStringDynArray;
                                 const aOnRequestPermissionsResult: TRequestPermissionsResultProc; 
                                 aOnDisplayRationale: TDisplayRationaleProc = nil); overload; virtual;
    class property DefaultService: TPermissionsService read GetDefaultService;
  end;

  TPermissionsServiceClass = class of TPermissionsService;

var
  PermissionsServiceClass: TPermissionsServiceClass = TPermissionsService;

function PermissionsService: TPermissionsService; inline;

implementation

function PermissionsService: TPermissionsService;

begin
  Result:=TPermissionsService.DefaultService;
end;

{ ---------------------------------------------------------------------
  TPermissionsService 
  ---------------------------------------------------------------------}

class function TPermissionsService.GetDefaultService: TPermissionsService;
begin
  if Not Assigned(FDefaultService) then
    if Assigned(PermissionsServiceClass) then
      FDefaultService:=PermissionsServiceClass.Create;
  Result:=FDefaultService;
end;


constructor TPermissionsService.Create;
begin
  // Nothing to do
end;


function TPermissionsService.IsPermissionGranted(const aPermission: string): Boolean;
begin
  Result:=True;
end;


function TPermissionsService.IsEveryPermissionGranted(const aPermissions: TStringDynArray): Boolean;
begin
  Result:=True;
end;

Function InitResults(aLen : Integer) : TPermissionStatusDynArray; inline;

var
  I : Integer;

begin
  Result:=[];
  SetLength(Result,aLen);
  for I:=0 to Length(Result)-1 do
    Result[I]:=TPermissionStatus.Granted;
end;

procedure TPermissionsService.RequestPermissions(const aPermissions: TStringDynArray;
                                                 const aOnRequestPermissionsResult: TRequestPermissionsResultEvent; 
                                                 aOnDisplayRationale: TDisplayRationaleEvent);
                                                 
var
  Res: TPermissionStatusDynArray;
  I: Integer;
  
begin
  Res:=InitResults(Length(aPermissions));
  AOnRequestPermissionsResult(Self,aPermissions,Res)
end;

procedure TPermissionsService.RequestPermissions(const aPermissions: TStringDynArray;
                                                 const aOnRequestPermissionsResult: TRequestPermissionsResultProc; 
                                                 aOnDisplayRationale: TDisplayRationaleProc);
var
  Res: TPermissionStatusDynArray;
  I: Integer;
  
begin
  Res:=InitResults(Length(aPermissions));
  AOnRequestPermissionsResult(aPermissions,TPermissionStatusDynArray(res))
end;

class destructor TPermissionsService.Done;
begin
  FreeAndNil(FDefaultService);
end;


end.
