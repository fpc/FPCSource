{ %VERSION=1.1 }
var
   w : widechar;

begin
   case w of
      'A' : ;
      'B' : ;
      #1234: ;
      #8888: ;
      #8889..#9999: ;
      'Z'..'a': ;
   end;
end.
