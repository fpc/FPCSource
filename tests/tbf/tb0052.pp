{ %FAIL }
{ Old file: tbf0269.pp }
{ wrong linenumber for repeat until when type mismatch OK 0.99.12b (PM) }

{ No idea how I could test this !! PM }
{ we should parse the compiler output !! }
{ Wrong line number for error message }
begin
  repeat
   writeln('test');
  until sptr;
end.
