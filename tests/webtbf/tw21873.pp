{ %fail }
{$mode objfpc}{$H+}

uses
  Classes, sysutils;     
var
    i: integer;
begin
      try
	i:=StrToInt('abc');
      except
	    try
	      writeln('First Exception');
              // allowed in 'except' block, but not in 'try' part of nested try..except
	      raise;
	    except
	      on econv: EConvertError do
		  writeln('EConvertError');
  
	      on e: Exception do
		  writeln('Exception');
  
	      else
		  writeln('The Else Block');  
	    end;
      end;
      writeln('After "Try" blocks');
end.
