{ Source provided for Free Pascal Bug Report 2702 }
{ Submitted by "Johannes Berg" on  2003-10-01 }
{ e-mail: johannes -at- sipsolutions -dot- de }
{$mode delphi}

uses
  Classes;

type
  TDummy = class(TObject)
  end;
  TF = class(TObject)
    function GetDummy: TDummy; virtual; abstract;
    property dummy: TDummy read GetDummy;
    procedure dada;
  end;

procedure TF.dada;
begin
  if Assigned(dummy) then begin
  end;
end;


begin
end.
