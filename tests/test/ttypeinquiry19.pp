{ "type of PT^" with PT a generic type parameter: the type is resolved on specialization }
program ttypeinquiry19;

{$mode objfpc}

type
  generic TGiraffe<PT> = class
  public
    type
      T = type of PT^;
    function Size: SizeInt;
    function Info: Pointer;
  end;

function TGiraffe.Size: SizeInt;
begin
  Result:=SizeOf(T);
end;

function TGiraffe.Info: Pointer;
begin
  Result:=TypeInfo(T);
end;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

var
  Giraffe: specialize TGiraffe<PWord>;
  Giraffe4: specialize TGiraffe<PLongint>;
begin
  Giraffe:=specialize TGiraffe<PWord>.Create;
  Check(Giraffe.Size=SizeOf(word),1);
  Check(Giraffe.Info=TypeInfo(word),2);
  Giraffe.Free;

  Giraffe4:=specialize TGiraffe<PLongint>.Create;
  Check(Giraffe4.Size=SizeOf(longint),3);
  Check(Giraffe4.Info=TypeInfo(longint),4);
  Giraffe4.Free;

  writeln('ok');
end.
