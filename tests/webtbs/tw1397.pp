uses
  Objects;

var
  Coll  : PCollection;
  Thing : PObject;

  Line1 : String;                            {*** This is a global variable ***}


procedure Zero;

  var
    Line2 : String;                           {*** This is a local variable ***}

  procedure Two (Thing: PObject);
  begin
    Line1 := 'BBB';
    Line2 := 'BBB';

    WriteLn('2: ', Line1, ' * ', Line2);                 {*** Output line 2 ***}
    if Line2<>'BBB' then
     begin
       writeln('ERROR!');
       halt(1);
     end;
  end;

  procedure One (Thing: PObject);

    procedure LocalTwo (Thing: PObject);
    begin
      Line1 := 'BBB';
      Line2 := 'BBB';

      WriteLn('2: ', Line1, ' * ', Line2);                 {*** Output line 2 ***}
      if Line2<>'BBB' then
       begin
         writeln('ERROR!');
         halt(1);
       end;
    end;

  begin
    Line1 := 'AAA';
    Line2 := 'AAA';

    WriteLn('1: ', Line1, ' * ', Line2);                 {*** Output line 1 ***}

    Coll^.ForEach(@LocalTwo);

    WriteLn('3: ', Line1, ' * ', Line2);                 {*** Output line 3 ***}
    if Line2<>'BBB' then
     begin
       writeln('ERROR!');
       halt(1);
     end;
  end;
                                         {*** I expected that output line 3 ***}
begin                                    {*** would be the same as output   ***}
  Coll^.ForEach(@One);                   {*** line 2. It is not.            ***}
end;


begin
  New(Coll, Init(1, 1));

  New(Thing, Init);
  Coll^.Insert(Thing);

  Zero;

  Dispose(Coll, Done);
end.
