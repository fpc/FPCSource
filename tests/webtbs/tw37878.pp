{$mode objfpc}
 var i64: int64; w: word;
begin
{$Q+}
w := 4096;
i64 := 8191;
i64 := i64 - 2*int64(w);
end.
