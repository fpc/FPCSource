const
   p : procedure a;stdcall=nil;   { <----- this doesn't what you expect !!!!}
   p : procedure a stdcall=nil;   { so delphi supports also this way of }
                                  { declaration                         }

begin
end.

