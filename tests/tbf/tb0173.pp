{ %fail }

{ use a structured type used to keep the namings clean }
type
   tLoopParm =
   record
      Min,
      Max,
      Cur : word;
   end {tLoopParm};
var
   One,
   Two,
   Three,
   Four : tLoopParm;
begin
   One.Min := 42;
   One.Max := 42;
   for One.Cur := One.Min to One.Max do
   begin
   end {for One};
end {Bug}.
