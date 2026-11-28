{ %fail }
{ %cpu=jvm }

{ the JVM target keeps the limit of 256 set elements also in
  FPC/ObjFPC mode }
{$mode objfpc}
program tset256;

type
  t = set of 0..256;

begin
end.
