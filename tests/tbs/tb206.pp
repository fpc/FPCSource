{ Old file: tbs0236.pp }
{ Problem with range check of subsets !! compile with -Cr OK 0.99.11 (PFV) }

{$R+}
program test_set_subrange;

uses
  erroru;

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
   hp^.en:=zero;
   new(hp^.next);
   hp^.next^.en:=three;
   t:=hp^.en;
   Writeln('hp^.en = ',str[hp^.en]);
   Writeln('hp^.next^.en = ',str[hp^.next^.en]);
 end;

begin
  require_error(201);
  test;
end.
