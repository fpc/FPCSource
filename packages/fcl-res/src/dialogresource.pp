{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024by the Free Pascal development team

    Dialog resource type

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit dialogresource;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE OBJFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Resources.Resource;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, resource;
{$ENDIF FPC_DOTTEDUNITS}

type

  { TDialogResource }

  TDialogResource = class(TAbstractResource)
  private
    fType : TResourceDesc;
    fName : TResourceDesc;
  protected
    function GetType : TResourceDesc; override;
    function GetName : TResourceDesc; override;
    function ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean; override;
    function ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean; override;
    procedure NotifyResourcesLoaded; override;
    procedure UpdateRawData; override;
  public
    constructor Create; override;
    constructor Create(aType, aName : TResourceDesc); override;
    destructor Destroy; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Resources.Factory;
{$ELSE FPC_DOTTEDUNITS}
uses
  resfactory;
{$ENDIF FPC_DOTTEDUNITS}

{ TDialogResource }

constructor TDialogResource.Create;
begin
  inherited Create;
  fType := TResourceDesc.Create(RT_DIALOG);
  fName := TResourceDesc.Create(0);
  SetDescOwner(fType);
  SetDescOwner(fName);
end;

constructor TDialogResource.Create(aType, aName : TResourceDesc);
begin
  Create;
  fName.Assign(aName);
end;

destructor TDialogResource.Destroy;
begin
  inherited Destroy;
end;

function TDialogResource.GetType : TResourceDesc;
begin
  Result := fType;
end;

function TDialogResource.GetName : TResourceDesc;
begin
  Result := fName;
end;

function TDialogResource.ChangeDescTypeAllowed(aDesc : TResourceDesc) : boolean;
begin
  Result := (aDesc = fType) or (aDesc = fName);
end;

function TDialogResource.ChangeDescValueAllowed(aDesc : TResourceDesc) : boolean;
begin
  Result := true;
end;

procedure TDialogResource.NotifyResourcesLoaded;
begin
  // Raw data is set directly by the parser.
end;

procedure TDialogResource.UpdateRawData;
begin
  // Raw data is set directly by the parser.
end;

initialization
  TResourceFactory.RegisterResourceClass(RT_DIALOG, TDialogResource);
end.
