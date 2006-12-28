const
  types: array[1..19] of string[20] =
          ('formal','comp','int64','currency','longint','cardinal','word','smallint',
           'byte','shortint',
           'shortstring','ansistring','single','double','extended','char','boolean',
           'widestring','widechar');
  compidx=2;
  int64idx=3;
  doubleidx=14;
  extendedidx=15;
  curridx=4;

function tostr(i: longint): string;
begin
  str(i,tostr);
end;

{$i+}
var
  i,j,k: byte;
  t: text;
begin
  k := 1;
  for i := low(types) to high(types)-1 do
    for j := succ(i) to high(types) do
      begin
        assign(t,'tvarol'+tostr(k)+'.pp');
        rewrite(t);
        writeln(t,'{$ifdef fpc}');
        writeln(t,'{$mode delphi}');
        writeln(t,'{$endif fpc}');
        writeln(t);

        writeln(t,'{$ifdef FPC_COMP_IS_INT64}');
        writeln(t,'type ');
        if not(j in [doubleidx,extendedidx]) then
          writeln(t,'  comp = double;')
        else
          writeln(t,'  comp = currency;');
        writeln(t,'{$endif FPC_COMP_IS_INT64}');

{
        if (i=compidx) and
           ((j=int64idx) or
            (j=doubleidx)) then
          writeln(t,'{$ifndef FPC_COMP_IS_INT64}');
}
        if (i in [curridx,compidx,doubleidx]) and
           (j=extendedidx) then
          writeln(t,'{$ifdef FPC_HAS_TYPE_EXTENDED}');

        if (i <> low(types)) then
          writeln(t,'procedure test(a: ',types[i],'); overload;')
        else
          writeln(t,'procedure test(var a); overload;');
        writeln(t,'  begin');
{ 
       if (i=compidx) then
          begin
             writeln(t,'{$ifdef FPC_COMP_IS_INT64}');
             writeln(t,'    writeln(''COMPFAILQ'');');
             writeln(t,'{$endif FPC_COMP_IS_INT64}')
          end;
} 
       writeln(t,'    writeln(''',types[i],' called instead of ',types[j],''');');
        writeln(t,'    writeln(''XXX'')');
        writeln(t,'  end;');
        writeln(t);
        writeln(t,'procedure test(a: ',types[j],'); overload;');
        writeln(t,'  begin');
{ 
       if (i=compidx) then
          begin
             writeln(t,'{$ifdef FPC_COMP_IS_INT64}');
             writeln(t,'    writeln(''COMPFAILV'');');
             writeln(t,'{$endif FPC_COMP_IS_INT64}')
          end;
}
        writeln(t,'    writeln(''',types[j],' called instead of ',types[i],''');');
        writeln(t,'    writeln(''YYY'')');
        writeln(t,'  end;');
        writeln(t);
        writeln(t,'var');
        writeln(t,'  v: variant;');
        if (i <> low(types)) then
          writeln(t,'  x: ',types[i],';')
        else
          writeln(t,'  x: longint;');
        writeln(t,'  y: ',types[j],';');
        writeln(t);
        writeln(t,'begin');
        writeln(t,'  try');
        writeln(t,'    v := x;');
        writeln(t,'    test(v);');
        writeln(t,'  except');
        writeln(t,'    on E : TObject do');
        writeln(t,'      writeln(''QQQ'');');
        writeln(t,'  end;');
        writeln(t);
        writeln(t,'  try');
        writeln(t,'    v := y;');
        writeln(t,'    test(v);');
        writeln(t,'  except');
        writeln(t,'    on E : TObject do');
        writeln(t,'      writeln(''VVV'');');
        writeln(t,'  end;');
 
       if ({(i=compidx) and
            ((j=int64idx) or
             (j=doubleidx)) or
}
           ((i in [curridx,compidx,doubleidx]) and
            (j=extendedidx))) then
          begin
            writeln(t,'{$else}');
            writeln(t,'begin');

            if (i=doubleidx) and
               (j=curridx) then
              { compilation has to fail }
              writeln(t,'  abc');

//            writeln(t,'  halt(COMPFAIL);');
            writeln(t,'{$endif}');
          end;

        writeln(t,'end.');
        close(t);

        inc(k);
      end;
end.
