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
  k: longint;
  i,j: byte;
  t: text;
begin
  k := 1;
  for i := low(types) to high(types)-1 do
    for j := succ(i) to high(types) do
      begin
        assign(t,'tvarol'+tostr(k)+'.pp');
        rewrite(t);
        writeln(t,'{$ifndef bigfile}');
        writeln(t,'{$ifdef fpc}');
        writeln(t,'{$mode delphi}');
        writeln(t,'{$else fpc}');
        writeln(t,'{$define FPC_HAS_TYPE_EXTENDED}');
        writeln(t,'{$endif fpc}');
        writeln(t,'{$endif bigfile}');
        writeln(t);

        types[compidx]:='comp'+tostr(k);
        writeln(t,'type ');
        writeln(t,'{$ifdef FPC_COMP_IS_INT64}');
        if not(j in [doubleidx,extendedidx]) then
          writeln(t,'  ',types[compidx],' = double;')
        else
          writeln(t,'  ',types[compidx],' = currency;');
        writeln(t,'{$else FPC_COMP_IS_INT64}');
        writeln(t,'  ',types[compidx],' = comp;');
        writeln(t,'{$endif FPC_COMP_IS_INT64}');

        if (i in [curridx,compidx,doubleidx]) and
           (j=extendedidx) then
          writeln(t,'{$ifdef FPC_HAS_TYPE_EXTENDED}');

        if (i <> low(types)) then
          writeln(t,'procedure test',tostr(k),'(a: ',types[i],'); overload;')
        else
          writeln(t,'procedure test',tostr(k),'(var a); overload;');
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
        writeln(t,'procedure test',tostr(k),'(a: ',types[j],'); overload;');
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
        { global to avoid problems with invalid floats }
        { due to uninitialised variables, and to avoid }
        { having to generate type-specific init code   }
        writeln(t,'var');
        if (i <> low(types)) then
          writeln(t,'  x',tostr(k),': ',types[i],';')
        else
          writeln(t,'  x',tostr(k),': longint;');
        writeln(t);
        writeln(t,'  y',tostr(k),': ',types[j],';');
        writeln(t,'procedure dotest',tostr(k),';');
        writeln(t,'var');
        writeln(t,'  v: variant;');
        writeln(t);
        writeln(t,'begin');
        writeln(t,'  try');
        writeln(t,'    v := x',tostr(k),';');
        writeln(t,'    test',tostr(k),'(v);');
        writeln(t,'  except');
        writeln(t,'    on E : TObject do');
        writeln(t,'      writeln(''QQQ'');');
        writeln(t,'  end;');
        writeln(t);
        writeln(t,'  try');
        writeln(t,'    v := y',tostr(k),';');
        writeln(t,'    test',tostr(k),'(v);');
        writeln(t,'  except');
        writeln(t,'    on E : TObject do');
        writeln(t,'      writeln(''VVV'');');
        writeln(t,'  end;');
        writeln(t,'end;');
        writeln(t);

        writeln(t,'{$ifndef bigfile} begin');
        writeln(t,'  dotest',tostr(k),';');
        writeln(t,'end. {$endif not bigfile}');
       if (((i in [curridx,compidx,doubleidx]) and
            (j=extendedidx))) then
          begin
            writeln(t,'{$else FPC_HAS_TYPE_EXTENDED}');
            writeln(t,'begin');
            if (i=doubleidx) and
               (j=curridx) then
              { compilation has to fail }
              writeln(t,'  abc');
            writeln(t,'end.');
            writeln(t,'{$endif FPC_HAS_TYPE_EXTENDED}');
          end;

(*
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
*)
        close(t);

        inc(k);
      end;
end.
