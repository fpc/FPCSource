{ %norun }
{ %OPT=-Sen -vn }

unit tw5100; 

{$mode objfpc}{$H+}

interface

type

  { TA }

  TA = class
  private
    FT: string;
  protected
    property T: string read FT write FT;
  end;

  TB = class
  private
    FT: string;
    property T: string read FT write FT;
  protected
    procedure test;
  end;
  
implementation

procedure tb.test;
begin
  writeln(t);
end;

end.
      
