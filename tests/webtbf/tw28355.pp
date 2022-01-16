{ %fail }

Program FPC30821_Bug;
{$mode objfpc}
{$modeswitch advancedrecords}

type
  TStringWithCounter = record
     Data : String;
     Counter : Integer;
     function Create( Const s : String; i : Integer) : TStringWithCounter;
  end;

  ttest = class
    public
      procedure test();
  end;

function TStringWithCounter.Create( Const s : String; i : Integer) : TStringWithCounter;
  begin
    Result.Data := s;
    Result.Counter := i;
  end;

procedure ttest.test();
  begin
    TStringWithCounter.Create('aa', 5); // => no error, but I think should be an error
  end;


begin
end.
