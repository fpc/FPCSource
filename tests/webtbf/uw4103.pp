unit uw4103;

interface

type    junk=record
            data:string;
        end;

operator :=(const s:string) result:junk;

implementation

operator :=(const s:string) result:junk;

begin
  result.data:=s;
end;

operator :=(const n:longint) result:junk;

begin
  str(n,result.data);
end;

operator :=(const n:cardinal) result:junk;

begin
  str(n,result.data);
end;

end.
