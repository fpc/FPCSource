uses garrayutils, gutil, gvector;

type vectorlli = specialize TVector<longint>;
     lesslli = specialize TLess<longint>;
     sortlli = specialize 
         TOrderingArrayUtils<vectorlli, longint, lesslli>;

var data:vectorlli; n,i:longint;

begin
  randomize;
  data:=vectorlli.Create;
  read(n);
  for i:=1 to n do
    data.pushback(random(1000000000));
  sortlli.sort(data, data.size());
  for i:=1 to n do
    writeln(data[i-1]);

  data.Destroy;
end.
