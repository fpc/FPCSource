{ %norun }
{ %OPT=-Sen }
{ %fail }

unit tw5100a; 

{$mode objfpc}{$H+}

interface

type

  { TB }

  TB = class
  private
    FT: string;
    property T: string read FT write FT;
  end;
  
implementation


end.
      
