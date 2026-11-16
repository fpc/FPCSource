{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Change tracker that keeps a memento as the reference state of an object

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.MementoTracker;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  Opf.Fields, Opf.ChangeTracking, Opf.Memento;

type

  { TOPMementoTracker:
    Change tracker keeping one memento per tracked object.
    The memento comes from:
    - The object itself through IOPMementoSource
    - OPDefaultMementoFactory. An object that supplies neither is not tracked and is saved with every field.

    An object the tracker cannot observe must be purged before it is freed,
    otherwise its entry outlives it and the memento it holds is never freed. }

  TOPMementoTracker = class(TOPObjectStateTracker)
  private
    FUseDefaultFactory: Boolean;
  protected
    // Memento of aObject, or Nil when no memento can be made for it.
    function CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject; override;
    // Add the name of every property the memento reports as different from aObject.
    procedure CompareState(aState: TObject; aObject: TObject;
      aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames); override;
  public
    // Create the tracker and attach it to the default manager.
    constructor Create(AOwner: TComponent); override;
    // Ask OPDefaultMementoFactory for a memento when the class has no IOPMementoSource.
    property UseDefaultFactory: Boolean read FUseDefaultFactory write FUseDefaultFactory;
  end;

implementation

uses
  Opf.Manager;

{ TOPMementoTracker }

constructor TOPMementoTracker.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FUseDefaultFactory:=True;
  Manager:=OPManager;
end;


{ A class handing out Nil has opted out, so the factory is not tried after it. }

function TOPMementoTracker.CreateState(aObject: TObject; aFieldMap: TOPFieldMapList): TObject;

var
  lSource: IOPMementoSource;

begin
  Result:=Nil;
  if aObject.GetInterface(SGUIDIOPMementoSource,lSource) then
    Result:=lSource.CreateMemento
  else if FUseDefaultFactory and Assigned(OPDefaultMementoFactory) then
    Result:=OPDefaultMementoFactory(aObject,aFieldMap);
end;


{ The field map is ignored: a memento captured what it wanted at capture time
  and needs nothing else to compare. }

{$push}
{$warn 5024 off}
procedure TOPMementoTracker.CompareState(aState: TObject; aObject: TObject;
  aFieldMap: TOPFieldMapList; var aNames: TOPChangeNames);

var
  lNames: TUTF8StringArray;
  lIndex: Integer;

begin
  lNames:=TOPMemento(aState).GetChanges(aObject);
  for lIndex:=0 to Length(lNames)-1 do
    aNames.Add(lNames[lIndex]);
end;
{$pop}


end.
