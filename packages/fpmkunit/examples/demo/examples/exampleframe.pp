{
  Example unit of the fpmkunit demo package, compiled only with fpmake -be.
}
unit ExampleFrame;

{$mode objfpc}{$H+}

interface

// aText surrounded by a frame of '*' characters, as three lines.
function Framed(const aText : String) : String;

implementation

function Framed(const aText : String) : String;

var
  lBorder : String;

begin
  lBorder:=StringOfChar('*',Length(aText)+4);
  Result:=lBorder+LineEnding+'* '+aText+' *'+LineEnding+lBorder;
end;


end.
