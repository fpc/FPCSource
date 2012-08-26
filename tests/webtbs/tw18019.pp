{ %cpu=i386 }
{ %opt=-Cg- }

  (*$ifdef FPC *)
    (*$z1*)
    (*$mode delphi *)
    (*$packset 1 *)
    (*$asmmode intel *)
  (*$endif *)
  (*$apptype console *)

  program asm_test;
  type
    tr=packed record
      case integer of
      1: (bytes: array [0..31] of byte);
      2: (a,b,c,d,e,f,g,h: byte);
      3: (aa,bb,cc,dd: word);
    end;

  var
    r: tr;

  function check_byte:boolean;
  asm
    cmp [r.a],0
    setnz al
  end;

  function check_byte_as_dword:boolean;
  asm
    // the dword ptr has to override the size of the field declaration
    cmp dword ptr [r.a],0
    setnz al
  end;


  function check_word:boolean;
  asm
    cmp [r.aa],0
    setnz al
  end;


  function check_word_as_dword:boolean;
  asm
    // the dword ptr has to override the size of the field declaration
    cmp dword ptr [r.aa],0
    setnz al
  end;


  begin
    fillchar(r,sizeof(r),#$ff);
    r.a:=0;
    if check_byte then
      halt(1);
    if not check_byte_as_dword then
      halt(2);
    r.aa:=0;
    if check_word then
      halt(3);
    if not check_word_as_dword then
      halt(4);
    fillchar(r,sizeof(r),#$ff);
    r.a:=1;
    if not check_byte then
      halt(5);
    r.aa:=1;
    if not check_word then
      halt(6);
  end.

