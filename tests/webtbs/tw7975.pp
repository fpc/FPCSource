{$mode objfpc}
{$inline on}

unit tw7975;

interface

type
  tc = class
    function t(const s: string): string; virtual;
    pref: string;
    parent: tc;
  end;

function test(c: tc): string; inline;

implementation

function tc.t(const s: string): string;
begin
  result := s + ' -- passed t';
end;

function test(c: tc): string; inline;
begin
  c.pref := 'bla';
  c.parent := c;
  result := c.parent.t('a'+c.pref);
end;

end.
