
TYPE
  tmyexample =object
  public
   constructor init;
   destructor done; virtual;
  private
   procedure mytest;virtual;       { syntax error --> should give only a 
warning ? }
  end;

  constructor tmyexample.init;
  begin
  end;

  destructor tmyexample.done;
  Begin
  end;

  procedure tmyexample.mytest;
  begin
  end;

Begin
end.
    
