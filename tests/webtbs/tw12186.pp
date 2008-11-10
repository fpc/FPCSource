{$mode objfpc}
begin
writeln(13); // ok
writeln($d); // ok
writeln(&15); // ok
writeln(%1101); // ok
writeln(0000098); // ok
writeln(#$62); // ok
writeln(#&142); // error!
writeln(#%1100010); // error!
end.