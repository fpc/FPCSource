{ %fail }

{ Source provided for Free Pascal Bug Report 3716 }
{ Submitted by "Marc Weustink" on  2005-03-01 }
{ e-mail: marc@freepascal.org }
program protect;

{$mode objfpc}{$H+}

uses
  Classes;

type
  TMyClass = class(TObject)
  public
    procedure p;
  end;

procedure TMyClass.p;
var
  C: TCollection;
begin
  C.PropName;
end;

begin
end.
