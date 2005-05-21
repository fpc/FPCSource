{ Old file: tbs0232.pp }
{ const. procedure variables need a special syntax if they use calling specification modifiers }

const
   p1 : procedure;stdcall=nil;   { <----- this doesn't what you expect !!!!}
   p2 : procedure stdcall=nil;   { so delphi supports also this way of }
                                  { declaration                         }

begin
end.
