{ %FAIL }
{ Old file: tbf0205.pp }
{ and parsing bugs, generates wrong code (tp7 gives parser error) OK 0.99.11 (PM) }

program bug_show;
{ By PAV (pavsoft@usa.net) }

function bad_uppercase(s:string):string;
var i:integer;
begin
  for i:=1 to length(s) do
    if (ord(s[i])>=97 and ord(s[i])<=122) then s[i]:=chr(ord(s[i])-97+65);
  bad_uppercase:=s;
end;

function good_uppercase(s:string):string;
var i:integer;
begin
  for i:=1 to length(s) do
    if (ord(s[i])>=97) and (ord(s[i])<=122) then s[i]:=chr(ord(s[i])-97+65);
  good_uppercase:=s;
end;

const cadena='Free Paskal Compiler 0.99.8  !!! (bug)';
begin
  writeln('This is the original string before convert it');
  writeln(cadena);
  writeln();
  writeln('This is a bad result, using "if (  and  )"');
  writeln(bad_uppercase(cadena));
  writeln();
  writeln('This is a good result, using "if () and ()"');
  writeln(good_uppercase(cadena));
  writeln();
end.
