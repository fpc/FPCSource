{$mode objfpc}

type
  TAsmList = ptruint;
  tdef = ptruint;
  tregister = ptruint;

  treference = record
    one,two,three,four: ptruint;
  end;

  tcg = class
  public
    function getintregister(list:TAsmList;size:tdef):Tregister;virtual;
    procedure a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);virtual;
    procedure a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);virtual;
    procedure a_load_ref_ref(list : TAsmList;fromsize, tosize : tdef;const sref : treference;const dref : treference);virtual;
  end;

procedure error(code: integer);
begin
  writeln('ERROR: ', code);
  halt(code);
end;

function references_equal(const sref,dref : treference):boolean; inline;
  begin
    references_equal:=CompareByte(sref,dref,sizeof(treference))=0;
  end;

function tcg.getintregister(list:TAsmList;size:tdef):Tregister;
begin
  if list<>40 then
    error(1);
  result:=10;
end;

procedure tcg.a_load_ref_reg(list : TAsmList;fromsize, tosize : tdef;const ref : treference;register : tregister);
begin
  if list<>40 then
    error(2);
  if fromsize<>50 then
    error(3);
  if tosize<>60 then
    error(4);
  if ref.one<>20 then
    error(5);
  if register<>10 then
    error(6);
end;

procedure tcg.a_load_reg_ref(list : TAsmList;fromsize, tosize : tdef;register : tregister;const ref : treference);
begin
  if list<>40 then
    error(10);
  if fromsize<>60 then
    error(11);
  if tosize<>60 then
    error(12);
  if ref.one<>30 then
    error(13);
  if register<>10 then
    error(14);
end;

procedure tcg.a_load_ref_ref(list: TAsmList; fromsize, tosize: tdef; const sref: treference; const dref: treference);
  var
    tmpreg: tregister;
  begin
    if references_equal(sref,dref) then
      exit;
    tmpreg:=getintregister(list,tosize);
    a_load_ref_reg(list,fromsize,tosize,sref,tmpreg);
    a_load_reg_ref(list,tosize,tosize,tmpreg,dref);
  end;

var
  cg: tcg;
  ref1: treference;
  ref2: treference;
begin
  cg:=tcg.create;
  ref1.one:=20;
  ref2.one:=30;
  cg.a_load_ref_ref(40,50,60,ref1,ref2);
  cg.Free;
  writeln('OK');
end.
