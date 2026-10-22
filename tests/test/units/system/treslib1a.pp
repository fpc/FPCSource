{ %TARGET=linux,freebsd,netbsd,openbsd,dragonfly }
{ %norun }
{ Library with resources that exports its resource header as rsrc }
library treslib1a;

{$R tres1.res}

exports
  rsrc;

begin
end.
