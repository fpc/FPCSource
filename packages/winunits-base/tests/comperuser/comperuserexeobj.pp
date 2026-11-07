{
    COM class served by comperuserexe; its factory is created before ComServer handles the command line.
}
unit comperuserexeobj;

{$mode objfpc}{$H+}

interface

uses
  ComObj, ComServ, comperuserintf;

type
  TPerUserExeObject = class(TComObject)
  end;

implementation

initialization
  TComObjectFactory.Create(ComServer,TPerUserExeObject,CLASS_PerUserExe,'PerUserExe',
    'FPC per-user registration test (exe)',ciMultiInstance,tmApartment);
end.
