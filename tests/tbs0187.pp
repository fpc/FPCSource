program test;

type
        Tbaseclass = object
                constructor     Init;
                destructor      Done;
                procedure       Run;                            virtual;

        end;
        Totherclass = object(Tbaseclass)
                procedure       Run;                            virtual;

        end;

constructor Tbaseclass.Init;

begin
  writeln('Init');
  Run;
end;

destructor Tbaseclass.Done;

begin
  writeln('Done');
end;

procedure Tbaseclass.Run;

begin
  writeln('Base method');
end;


procedure Totherclass.Run;

begin
  writeln('Inherited method');
end;

var     base            : Tbaseclass;
        other           : Totherclass;
//        asmrec          : Tasmrec;
        testfield       : longint;

begin
// Uncommenting here and commenting the init in the WIth solves it.
//  Base.Init;
  with base do
  begin
    Init;
    Run;
    Done;
  end;
// Uncommenting here and commenting the init in the WIth solves it.
//  Other.init;
  with other do
  begin
    Init;
    Run;
    Done;
  end;

{ Calls Tbaseclass.Run when it should call Totherclass.Run }

end.
