const
   p1 : procedure;stdcall=nil;   { <----- this doesn't what you expect !!!!}
   p2 : procedure stdcall=nil;   { so delphi supports also this way of }
                                { declaration                         }

begin
end.

