{ %fail }

{ $mode delphi}
{$mode objfpc}
type
  TA = class
    procedure douseful; virtual; abstract;
  end;

  TB = class(TA)
  public
    procedure callabs;
    procedure douseful; override;
  end;

procedure TB.douseful;
begin
  { This should give an error, comaptible with Kylix

    Delphi eats this
    FP shouldn't by default
    (FK) }
  inherited;
end;

procedure TB.Callabs;
begin
end;

begin
end.
