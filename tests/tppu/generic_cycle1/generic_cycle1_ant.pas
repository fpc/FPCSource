unit generic_cycle1_ant;

{$mode objfpc}

interface

uses generic_cycle1_bird;

type

  { TAnt }

  generic TAnt<T> = class
  public
    type TMyBird = specialize TBird<T>;
    var
    Bird: TMyBird;
    constructor Create;
    function Run(p: T): word;
  end;

  generic TRunner<T> = record
    Value: T;
  end;

const
  Factor = 7;

function Crawl(w : word): word;

implementation

type
  TAntWord = specialize TAnt<Word>;

function Crawl(w : word): word;
var
  Ant: TAntWord;
begin
  Ant:=TAntWord.Create;
  Result:=Ant.Run(w);
  Ant.Free;
end;

{ TAnt }

constructor TAnt.Create;
begin
  Bird:=TMyBird.Create;
end;

function TAnt.Run(p: T): word;
begin
  Result:=Bird.Fly(p)*3;
end;

end.
