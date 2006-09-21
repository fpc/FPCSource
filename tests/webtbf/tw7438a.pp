{ %fail }

{$mode macpas}

uses
  tw7438;

var
  t: tr;
begin
  writeln(ptruint(@t.l2) - ptruint(@t.l1));
end.
