{$mode objfpc}

uses generic_indirectuses_ant;

var
  Ant: TAntBirdCat;
begin
  Ant:=TAntBirdCat.Create;
  writeln(Ant.Crawl(2));
end.
