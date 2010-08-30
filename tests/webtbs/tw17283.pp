{$mode objfpc}

  program test;

  type
    tr_32=packed record
      case integer of
      1: (words: array [0..1] of word);
      2: (low,high: word);
      end;
(*
  procedure f_ref(var l,h:word);
  begin
    l:=1;
    h:=2;
    end;

  function f_test1:longint;
  begin
    result:=$12345678;
    f_ref(tr_32(result).words[0],tr_32(result).words[1]);
    end;

  function f_test2:longint;
  begin
    result:=$12345678;
    f_ref(tr_32(result).low,tr_32(result).high);
    end;

  function f_test3:longint;
  var
    q: longint;
  begin
    q:=$12345678;
    f_ref(tr_32(q).words[0],tr_32(q).words[1]);
    result:=q;
    end;
*)
  function f_test4:longint;
  var
    q: longint;
  begin
    q:=$12345678;
    tr_32(q).words[0]:=1;
    tr_32(q).words[1]:=2;
    result:=q;
    end;
    
  var
    l,q: longint;
    
  begin
(*
    l:=f_test1;
    if (tr_32(l).low<>1) or
       (tr_32(l).high<>2) then
      halt(1);

    l:=f_test2;
    if (tr_32(l).low<>1) or
       (tr_32(l).high<>2) then
      halt(2);

    q:=$12345678;
    f_ref(tr_32(q).words[0],tr_32(q).words[1]);
    if (tr_32(q).low<>1) or
       (tr_32(q).high<>2) then
      halt(3);
    
    q:=$12345678;
    f_ref(tr_32(q).low,tr_32(q).high);
    if (tr_32(q).low<>1) or
       (tr_32(q).high<>2) then
      halt(4);
    
    l:=f_test3;
    if (tr_32(l).low<>1) or
       (tr_32(l).high<>2) then
      halt(5);
*)
    l:=f_test4;
    if (tr_32(l).low<>1) or
       (tr_32(l).high<>2) then
      halt(6);
end.

