{ Old file: tbs0191.pp }
{ missing vecn constant evaluation                      OK 0.99.11 (PM) }

type
  trec=record
   a,b : longint;
  end;
  prec=^trec;

const
  s  : string = 'test';

  cfg : array[1..2] of trec=(
   (a:1;b:2),
   (a:3;b:4)
  );
  pcfg : prec = @cfg[2];

  l : ^longint = @cfg[1].b; { l^ should be 2 }

  pc : pchar = @s[1];

begin
  Writeln(' l^ = ',l^);
  Writeln('pc[0] = ',pc[0]);
  if (l^<>2) or (pc[0]<>'t') then
    Begin
       Writeln('Wrong code generated');
       RunError(1);
    End;
end.
