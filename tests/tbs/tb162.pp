{ %OPT=-St -Cr }

{ Old file: tbs0187.pp }
{ constructor in a WIth statement isn't called correct. (works at lest in the case stated)                    OK 0.99.11 (PM) }

{$static on}

type
        Tbaseclass = object
                base_arg : longint;
                st_count : longint;static;
                constructor     Init;
                destructor      Done;
                procedure       Run;                            virtual;

        end;
        Totherclass = object(Tbaseclass)
                other_arg : longint;
                procedure       Run;                            virtual;

        end;

const
    BaseRunCount : integer = 0;
    OtherRunCount : integer = 0;

constructor Tbaseclass.Init;

begin
  writeln('Init');
  Inc(st_count);
  Run;
end;

destructor Tbaseclass.Done;

begin
  writeln('Done');
  dec(st_count);
end;

procedure Tbaseclass.Run;

begin
  writeln('Base method');
  inc(BaseRunCount);
end;


procedure Totherclass.Run;

begin
  writeln('Inherited method');
  inc(OtherRunCount);
end;

 { try this as local vars }

 procedure test_local_class_init;
  var base1 : TbaseClass;
  var other1 : TOtherClass;
  begin
     with other1 do
          Init;
     with base1 do
          Init;
     with other1 do
        begin
           Writeln('number of objects = ',st_count);
           base_arg:=2;
           other_arg:=6;
           Run;
        end;
     { test if changed !! }

     if (other1.base_arg<>2) or (other1.other_arg<>6) then
       Halt(1);

     with base1 do
        begin
           Run;
           Done;
        end;
     other1.done;
   end;

var     base            : Tbaseclass;
        other           : Totherclass;
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

 test_local_class_init;
{ Calls Tbaseclass.Run when it should call Totherclass.Run }
  If (BaseRunCount<>4) or (OtherRunCount<>4) then
    Begin
       Writeln('Error in tb162');
       Halt(1);
    End;
end.
