{$mode delphi}

type
  TGuidHelper = record helper for TGUID
    Class Function Create(const Data): TGUID; overload; static; inline;
    Class Function Create(const S: string): TGUID; overload; static;
  end;

class function TGuidHelper.Create(const Data): TGUID;
begin
  halt(1);
end;

class function TGuidHelper.Create(const S: string): TGUID;
begin
  writeln('B');
end;

var
  c: PChar;
  g: TGUID;
begin
  g.Create(utf8string(c)); // will print 'A'
  g.Create(unicodestring(c)); // will print 'A'
  g.Create(shortstring(c)); // will print 'A'
end.
