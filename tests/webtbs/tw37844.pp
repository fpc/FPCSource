program tw37844;
{$mode objfpc}

type
    trec = record
      value: longint;
    end;
    {generic grec<T> = record
        value: T;
    end;}

    tmytype = class
    public
    generic function func1<T>( const v: longint ): trec;//specialize grec<T>;
    end;

generic function tmytype.func1<T>( const v: longint ): trec;//specialize grec<T>;
begin
    result.value := v;
    //result.value := t(v);
end;

var
    tmp: tmytype;
    gr: trec;//specialize grec<string>;
    vr: longint;//variant;

begin
    tmp := tmytype.Create;
    vr := 123;
    gr := Default(trec);
    with tmp do
        gr := specialize func1<string>( vr ); // <--!!!!!!!!!!!!!!!!!!!
    //gr := tmp.specialize func1<string>(vr);
    //writeln(gr.value);
    tmp.Free;
    if gr.value<>vr then
      halt(1);
    //readln;
end.
