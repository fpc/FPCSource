{ $OPT=-Un }
{ this name should be accepted with -Un option !! }
UNIT bug0180;
INTERFACE
  uses
     tbs0180a;

  procedure dummy;
IMPLEMENTATION
  procedure dummy;
    begin
      { Unit_with_strange_name.dummy; should this work ?? }
      tbs0180a.dummy;
    end;
END.
