{$mode objfpc}
{$M+}

type
   tenum = (te_first,te_second,te_third,te_fourth,te_fifth);

   tenumrange = te_second..te_fourth;

   tc1 = class
      public
        fe : tenumrange;
      published
        property enumrange : tenumrange read fe write fe;
   end;

begin
end.
