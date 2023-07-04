{ %CPU=wasm32 }
Unit uthintfr;

{$mode objfpc}
{$h+}
{$interfaces corba}

interface


type
  {$M+}
  TMyInterface = Interface ['{76DC0D03-376C-45AA-9E0C-B3546B0C7208}']
    Procedure DoA(a : Integer);
    Procedure DoA;
    function doB : Integer;
    function doc(a : integer) : integer;
  end;

 
implementation

end.  
  
  
  