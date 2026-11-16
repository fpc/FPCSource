unit opfdatamodule;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,
  Opf.Manager, Opf.Registry, Opf.Sql;

type

  { TOPFDataModule }

  TOPFDataModule = class(TDataModule)
    OPManager1: TOPManager;
    OPRegistry1: TOPRegistry;
    OPSQLBuilder1: TOPSQLBuilder;
  end;

var
  OPFDataModuleInstance: TOPFDataModule;

implementation

{$R *.lfm}

end.
