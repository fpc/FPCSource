{ Old file: tbs0082.pp }
{  Shows incompatibility with BP : Multiple destructors. OK 0.99.1 (FK) }

Unit tbs0082;

interface

Type T = OBject
      Constructor Init;
      Destructor Free; virtual;
      Destructor Destroy; virtual;
      end;

implementation

constructor T.INit;

begin
end;

Destructor t.Free;

begin
end;

Destructor t.Destroy;

begin
end;


end.