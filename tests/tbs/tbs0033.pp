var
   p1 : pchar;
   p2 : array[0..10] of char;
   s : string;
   c : char;

begin
   p1:='c';
   s:='c';
   { this isn't allowed
   p1:=c;
   }
end.
