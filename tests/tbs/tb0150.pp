{ %OPT=-Un }
{ %RECOMPILE }

{ Old file: tbs0180.pp }
{ problem for units with names different from file name should be accepted with -Un !! Solved, but you still need to use the file name from other units                                                 OK 0.99.9 (PM) }

{ this name should be accepted with -Un option !! }
UNIT tb154_wrong;
INTERFACE
  uses
     ub0150;

  procedure dummy;
IMPLEMENTATION
  procedure dummy;
    begin
      { Unit_with_strange_name.dummy; should this work ?? }
      ub0150.dummy;
    end;
END.
