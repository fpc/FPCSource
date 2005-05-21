{$H+}

type
  Tsome = Record
    One,Two,Three:String;
  end;

Procedure passhere(Some:TSome;onemore:String);
Begin
end;

procedure fromhere;
Var
  me:Tsome;
Begin
 me.one:='blah';
 me.two:='';
 me.three:='';
 passhere(Me,'text some');
end;

begin
  fromhere;
end.
