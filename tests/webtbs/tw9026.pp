operator := (input:extended) output: string;
begin
   str(round(input),output);
end;

operator + (const s: string; input:extended) output: string;
begin
   str(round(input),output);
   output:=s+output;
end;

procedure test(a:string);
begin
   writeln(a);
   if (a <> 'help1') then
     halt(1);
end;

var
s: string;
begin
   s:='help';
   test('help'+1);
   test(s+1);
   test(s+1.2);
   test(s+extended(1.2));
   test(s+string(1.2));
end.
