uses uw9113a, uw9113b;

var
  v1: smallint; cvar; external;
  myv2: smallint; external name '_v2';
  myv3: smallint; external name '_v3';
  v4: smallint; cvar; external;
  myv5: smallint; external name '_v5';
  myv6: smallint; external name '_v6';

begin
  if (v1 <> 1) or
     (myv2 <> 2) or
     (myv3 <> 3) or
     (v4 <> 4) or
     (myv5 <> 5) or
     (myv6 <> 6) then
    halt(1);
end.

