{ %OPT=-Un }

{ Old file: tbs0180a.pp }

{ this name should be accepted with -Un option !! }
UNIT Unit_with_strange_name;
INTERFACE
  procedure dummy;
IMPLEMENTATION
  procedure dummy;
    begin
    end;

begin
   Unit_with_strange_name.dummy;
END.
