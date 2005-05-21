{ Old file: tbs0045.pp }
{  shows problem with virtual private methods (might not be a true bugs but more of an incompatiblity?) the compiler warns now if there is a private and virtual method }


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
