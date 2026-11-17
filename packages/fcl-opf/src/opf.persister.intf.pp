{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Interface based persisting

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Persister.Intf;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, db,
{$ENDIF}
  Opf.Types, Opf.Registry, Opf.Fields, Opf.Consts;

Const
  SGUIDIOPFieldProvider = '{A7E3B1C4-5D9F-4A82-B6E1-3C8D2F7A9E50}';

Type

  IOPFieldProvider = Interface ['{A7E3B1C4-5D9F-4A82-B6E1-3C8D2F7A9E50}']
    // Adds the object's persisted fields to the map
    Procedure GetFieldDefs(aMap: TOPFieldMapList);
    // Copies a database field value into the object; true if handled
    Function ApplyField(aFieldMap: TOPFieldMap; aField: TField): Boolean;
    // Copies an object value into a query parameter; true if handled
    Function PersistField(aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
  end;

  { TInterfacePersister }

  TInterfacePersister = class(TOPPersister)
  public
    // Fills the map with the fields reported by the class itself
    procedure PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList); override;
    // Lets the object copy a database field value into itself
    function ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean; override;
    // Lets the object copy one of its values into a query parameter
    function PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean; override;
    // Raises a persister error for the given operation and reason
    procedure RaiseError(const aOperation, aReason: string);
  end;

implementation

uses Opf.Factory;

{ TInterfacePersister }

procedure TInterfacePersister.PopulateFieldMap(aClass: TClass; aMap: TOPFieldMapList);
var
  Tmp: TObject;
  Provider: IOPFieldProvider;
begin
  // Through the factory: TObject.Create would skip the class's own constructor
  Tmp := OPFactory.CreateInstance(aClass, nil);
  try
    if not Tmp.GetInterface(SGUIDIOPFieldProvider, Provider) then
      RaiseError('PopulateFieldMap', Format(SErrClassDoesNotSupportIOPFieldProvider, [aClass.ClassName]));
    Provider.GetFieldDefs(aMap);
  finally
    Tmp.Free;
  end;
end;

function TInterfacePersister.ApplyProperty(Instance: TObject; aFieldMap: TOPFieldMap; aField: TField): Boolean;
var
  Provider: IOPFieldProvider;
begin
  if not Instance.GetInterface(SGUIDIOPFieldProvider, Provider) then
    RaiseError('ApplyProperty', Format(SErrClassDoesNotSupportIOPFieldProvider, [Instance.ClassName]));
  Result := Provider.ApplyField(aFieldMap, aField);
end;

function TInterfacePersister.PersistProperty(Instance: TObject; aFieldMap: TOPFieldMap; aParam: TParam): Boolean;
var
  Provider: IOPFieldProvider;
begin
  if not Instance.GetInterface(SGUIDIOPFieldProvider, Provider) then
    RaiseError('PersistProperty', Format(SErrClassDoesNotSupportIOPFieldProvider, [Instance.ClassName]));
  Result := Provider.PersistField(aFieldMap, aParam);
end;

procedure TInterfacePersister.RaiseError(const aOperation, aReason: string);
begin
  OPFRaiseError(EOPFPersisterIntf, ClassName, aOperation, aReason);
end;

end.
