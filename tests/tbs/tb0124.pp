{ Old file: tbs0143.pp }
{ cannot concat string and array of char in $X+ mode    OK 0.99.7 (PFV) }



const
  string1 : string = 'hello ';
  string2 : array[1..5] of char = 'there';
var
  s : string;
begin
  s:=string1+string2;
  writeln(string1+string2);
end.
