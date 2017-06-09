uses
  math;
const
{$if defined(CPU8) or defined(CPU16)}
  lg2upperlimit = 14;
{$else}
  lg2upperlimit = 22;
{$endif}
var
  a : array of word;
  i,j : longint;
  upperlimit : longint;
  histogram : array[0.. 1 shl lg2upperlimit] of longint;
  entropy : double;
begin
  randomize;
  for i:=1 to lg2upperlimit do
    begin
      upperlimit:=1 shl i;
      setlength(a,upperlimit);
      for j:=0 to upperlimit-1 do
        a[j]:=random(1 shl lg2upperlimit)+ 1);
      FillChar(histogram,sizeof(histogram),0);
      for j:=0 to upperlimit-1 do
        inc(histogram[a[j]]);
      entropy:=0;
      for j:=low(histogram) to high(histogram) do
        if histogram[j]/upperlimit>0 then
          entropy:=entropy-histogram[j]/upperlimit*log2(histogram[j]/upperlimit);

      write(entropy);
      if entropy<0.9*min(i,16) then
        begin
          writeln(' Entropy for ',upperlimit,' numbers too low, this could be a spurious result, but if it is happening regularily, random is broken!');
          halt(1);
        end
      else
        writeln;
    end;
  writeln('ok');
end.
