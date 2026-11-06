{
  Unit of the fpmkunit demo package.
}
unit DemoGreet;

{$mode objfpc}{$H+}

interface

// Greeting for aName, "Hello, World!" when aName is empty.
function Greeting(const aName : String) : String;

implementation

function Greeting(const aName : String) : String;

begin
  if aName='' then
    Result:='Hello, World!'
  else
    Result:='Hello, '+aName+'!';
end;


end.
