type
  trec=record
   a,b : longint;
  end;
  prec=^trec;

const
  s  : string = 'test';
  pc : pchar = @s[1];

  cfg : array[1..2] of trec=(
   (a:1;b:2),
   (a:3;b:4)
  );
  pcfg : prec = @cfg[2];

  l : ^longint = @cfg[1].b; { l^ should be 2 }

begin
end.
