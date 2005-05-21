type
  G = object
    public
      B:procedure;
    { the 1.1 compiler parses the next "public" as a procdirective of the preceding procedure }
    public
      constructor init;
  end;

  constructor G.init;
    begin
      B:=nil;
    end;

begin
end.
