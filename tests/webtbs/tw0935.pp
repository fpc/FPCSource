{$inline on}

procedure test(v:boolean);

 procedure notice(s:string);inline;
 begin
 writeln(s);
 end;

begin
if v then notice('this string vanishes.');
writeln('"test" main body executed.');
end;



begin
writeln('testing with True...');
test(true);
writeln;
writeln('testing with False...');
test(false);
end.
