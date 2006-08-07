unit uw6922;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface

type

  { TA }

  TA = class
  private
    FT: string;
  protected
    property T: string read FT write FT;
  end;

  TB = class(TA)
  end;

implementation

end.
