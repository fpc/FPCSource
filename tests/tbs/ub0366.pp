{$ifdef fpc}{$mode objfpc}{$endif}
unit ub0366;
interface

type
  tc1=class
  private
    FHeight : integer;
  public
    constructor Create;
    property Height : integer read FHeight write FHeight;
  end;

implementation

constructor tc1.Create;
begin
  FHeight:=0;
end;

end.
