{ %NORUN }

program tw26599;

{$mode delphi}

type
  TSomeList<T : TObject> = Class
  End; { Class }

  TSomeClass = Class;
  TSomeClassList = TSomeList<TSomeClass>;

  TSomeClass = Class(TObject)
    SomeList : TSomeClassList;
  End;

begin
end.
