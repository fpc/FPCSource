Library Greeting;

{$Mode objfpc}

Procedure Hello;

begin
  Writeln ('Hello, World !');
end;

Function GetNumber (Max : longint) : Longint;

begin
  Repeat
    Write ('Please enter a nuber between 0 and ',max,' : ');
    ReadLn (Result);
  Until (Result>=0) and (Result<=Max);
end;

exports
  Hello,
  GetNumber;

end. 