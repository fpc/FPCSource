Program Example14;
uses Dos;

{ Program to demonstrate the GetEnv function. }

begin
  WriteLn('Current PATH is ',GetEnv('PATH'));
end.
