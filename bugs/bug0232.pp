const
   p : procedure;stdcall=nil;   { <----- this doesn't what you expect !!!!}
   p : procedure stdcall=nil;   { so delphi supports also this way of }
                                { declaration                         }

begin
end.

