{ Source provided for Free Pascal Bug Report 2708 }
{ Submitted by "Johannes Berg" on  2003-10-02 }
{ e-mail: johannes -at- sipsolutions -dot- de }
program k;
{$mode delphi}
type
  TA = class
    procedure a; overload; virtual; abstract;
    procedure a(const s:string); overload;
  end;

procedure TA.a(const s:string);
begin
end;

begin
end.
