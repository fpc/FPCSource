{ this name should be accepted with -Un option !! }
UNIT bug0180;
INTERFACE
  uses 
     bug0180a;

  procedure dummy;
IMPLEMENTATION
  procedure dummy;
    begin
      { Unit_with_strange_name.dummy; should this work ?? }
      bug0180a.dummy; 
    end;
END.
