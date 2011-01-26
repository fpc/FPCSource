unit tw9673;

interface
{$mode objfpc}

type
  generic Testclass<T> = class
  
  type
    TList = array of byte;
	
  end;
	    
var
  b : Testclass<T>.TList;
	      
	    
implementation
begin
end.