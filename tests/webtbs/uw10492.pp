unit uw10492;

{$mode objfpc}

interface

const
  ISSTORED = false;

type
  TTest = class
  private
    Faaaa: String;
  published
    property AAAA: String read Faaaa write Faaaa stored ISSTORED;
  end;

implementation

begin
end.
