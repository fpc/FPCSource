{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Memento contract: capture, compare and restore the state of an object

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Memento;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
{$ELSE}
  Classes, SysUtils,
{$ENDIF}
  Opf.Types, Opf.Fields, Opf.ChangeTracking;

const
  SGUIDIOPMementoSource   = '{5B9F2D63-4C18-47AE-91D0-8E7A3C62B145}';
  SGUIDIOPMementoRestored = '{C4A70E15-2B69-4D83-A5F2-9017E6B4C238}';

type
  TOPMemento = class;

  // Objects that can hand out a snapshot of their own state.
  IOPMementoSource = interface [SGUIDIOPMementoSource]
    // Create a memento holding the current state of this object; the caller owns it.
    function CreateMemento: TOPMemento;
  end;

  // Objects that want a notification after a memento was restored into them.
  IOPMementoRestored = interface [SGUIDIOPMementoRestored]
    // Called after the stored state was written back into this object.
    procedure MementoRestored;
  end;

  { TOPMemento: snapshot of the state of one object at one moment. }

  TOPMemento = class(TObject)
  protected
    // Raise EOPFMemento for aOperation with aReason as the message.
    procedure RaiseError(const aOperation, aReason: string); overload;
    // Raise EOPFMemento for aOperation with aFmt formatted over aArgs as the message.
    procedure RaiseError(const aOperation, aFmt: string; const aArgs: array of const); overload;
    // The class this memento captures. Subjects must be of exactly this class.
    function GetSubjectClass: TClass; virtual; abstract;
    // Copy the state of aSubject into this memento.
    procedure Capture(aSubject: TObject); virtual; abstract;
    // Write the stored state back into aSubject.
    procedure DoRestore(aSubject: TObject); virtual; abstract;
    // Add the name of every property of aSubject that differs to aNames.
    procedure DoCompare(aSubject: TObject; var aNames: TOPChangeNames); virtual; abstract;
    // Raise an error when aSubject is Nil or not of the class GetSubjectClass returns.
    procedure CheckSubject(aSubject: TObject; const aOperation: string);
  public
    // Check aSubject and capture its state.
    constructor Create(aSubject: TObject);
    // True when aSubject no longer matches the stored state.
    function IsChanged(aSubject: TObject): Boolean; virtual;
    // Names of the properties of aSubject that differ from the stored state.
    function GetChanges(aSubject: TObject): TUTF8StringArray;
    // Write the stored state back into aSubject and call MementoRestored when it has one.
    procedure RestoreTo(aSubject: TObject);
    // Class of the object this memento captures.
    property SubjectClass: TClass read GetSubjectClass;
  end;

  // Builds a memento for a class that has no hand-written one.
  TOPMementoFactory = function(aSubject: TObject; aFieldMap: TOPFieldMapList): TOPMemento;

var
  OPDefaultMementoFactory: TOPMementoFactory = Nil;

implementation

uses
  Opf.Consts;

{ TOPMemento }

procedure TOPMemento.RaiseError(const aOperation, aReason: string);

begin
  OPFRaiseError(EOPFMemento,ClassName,aOperation,aReason);
end;


procedure TOPMemento.RaiseError(const aOperation, aFmt: string; const aArgs: array of const);

begin
  OPFRaiseError(EOPFMemento,ClassName,aOperation,aFmt,aArgs);
end;


procedure TOPMemento.CheckSubject(aSubject: TObject; const aOperation: string);

var
  lClass: TClass;

begin
  if aSubject=Nil then
    RaiseError(aOperation,SErrMementoNilSubject);
  lClass:=GetSubjectClass;
  if lClass=Nil then
    RaiseError(aOperation,SErrMementoNoClass,[ClassName]);
  if aSubject.ClassType<>lClass then
    RaiseError(aOperation,SErrMementoClassMismatch,[lClass.ClassName,aSubject.ClassName]);
end;


constructor TOPMemento.Create(aSubject: TObject);

begin
  Inherited Create;
  CheckSubject(aSubject,'Create');
  Capture(aSubject);
end;


function TOPMemento.IsChanged(aSubject: TObject): Boolean;

var
  lNames: TOPChangeNames;

begin
  CheckSubject(aSubject,'IsChanged');
  lNames:=Default(TOPChangeNames);
  DoCompare(aSubject,lNames);
  Result:=lNames.Count>0;
end;


function TOPMemento.GetChanges(aSubject: TObject): TUTF8StringArray;

var
  lNames: TOPChangeNames;

begin
  CheckSubject(aSubject,'GetChanges');
  lNames:=Default(TOPChangeNames);
  DoCompare(aSubject,lNames);
  Result:=lNames.ToArray;
end;


procedure TOPMemento.RestoreTo(aSubject: TObject);

var
  lRestored: IOPMementoRestored;

begin
  CheckSubject(aSubject,'RestoreTo');
  DoRestore(aSubject);
  if aSubject.GetInterface(SGUIDIOPMementoRestored,lRestored) then
    lRestored.MementoRestored;
end;


end.
