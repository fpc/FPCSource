unit ujsetter;

{$namespace org.freepascal.test.jsetter}
{$mode delphi}

interface

type
  tjsetterbase = class
   protected
    fval: longint;
    procedure SetVal(v: longint); virtual;
   public
    function get: longint;
  end;

  tjsetterchild = class(tjsetterbase)
   public
    property Val: longint read fval write SetVal;
  end;

  tjsetterchild2 = class(tjsetterchild)
   protected
    procedure SetVal(v: longint); override;
   public
    property Val: longint read fval write SetVal;
  end;

implementation

function tjsetterbase.get: longint;
begin
  result:=fval;
end;

procedure tjsetterbase.SetVal(v: longint);
begin
  fval:=v;
end;

procedure tjsetterchild2.SetVal(v: longint);
begin
  fval:=v-1;
end;


end.

