program test_set_subrange;

 type
   enum = (zero,one,two,three);

   sub_enum = one..three;
   prec = ^trec;
   
   trec = record
     dummy : longint;
     en : enum;
     next : prec;
   end;

 const
   str : array[sub_enum] of string = ('one','two','three');

procedure test;

 var hp : prec;
    t : sub_enum;
 
 begin
   new(hp);
   hp^.en:=two;
   new(hp^.next);
   hp^.next^.en:=three;
   t:=hp^.en;
   Writeln('hp^.en = ',str[hp^.en]);
   Writeln('hp^.next^.en = ',str[hp^.next^.en]);
 end;

begin
  test;
end.