{ %FAIL }
{ Old file: tbf0301.pp }
{ crash if destructor without object name is parsed    OK 0.99.13 (PFV) }

Program bug0301;

destructor done;
begin
end;

begin
end.
