{ %OPT=-Sew -vw }

{ Source provided for Free Pascal Bug Report 2710 }
{ Submitted by "Micha Nelissen" on  2003-10-04 }
{ e-mail: M.Nelissen@student.tue.nl }
unit tw2710;

{$mode delphi}

interface

type

TAbstract = class(TObject)
public
  constructor Create;

  procedure AbstractMethod; virtual; abstract;
end;

type

TDerived = class(TAbstract)
public
  constructor Create;

  procedure AbstractMethod; override;
end;

implementation

constructor TAbstract.Create;
begin
  inherited;
end;

constructor TDerived.Create;
begin
  inherited;
end;

procedure TDerived.AbstractMethod;
begin
end;

end.
