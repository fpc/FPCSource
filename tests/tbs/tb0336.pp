var
   l : longint;
   d : dword;
   s : string;
   code : integer;

procedure do_error(l : longint);

  begin
     writeln('Error near number ',l);
     halt(1);
  end;

begin
   s:='4294967295';
   val(s,d,code);
   if code<>0 then
     do_error(1);
   s:='4294967296';
   val(s,d,code);
{$ifdef CPU64}
   if code<>0 then
{$else CPU64}
   if code=0 then
{$endif CPU64}
     do_error(1);

   s:='2147483647';
   val(s,l,code);
   if code<>0 then
     do_error(3);
   s:='2147483648';
   val(s,l,code);
{$ifdef CPU64}
   if code<>0 then
{$else CPU64}
   if code=0 then
{$endif CPU64}
     do_error(4);
   s:='-2147483648';
   val(s,l,code);
   if code<>0 then
     do_error(5);
   s:='-2147483649';
   val(s,l,code);
{$ifdef CPU64}
   if code<>0 then
{$else CPU64}
   if code=0 then
{$endif CPU64}
     do_error(6);
end.
