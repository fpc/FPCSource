{ %fail }

{ Source provided for Free Pascal Bug Report 3000 }
{ Submitted by "Vincent Snijders" on  2004-03-03 }
{ e-mail: vslist@zonnet.nl }
{$mode objfpc}{$H+}

uses
  Classes;

type
  TA=class
    procedure bla(const a: string); virtual; abstract;
  end;
  TB=class(TA)
    procedure bla(a: string); override;
  end;

procedure TB.bla(a: string);
begin

end;

begin
end.
