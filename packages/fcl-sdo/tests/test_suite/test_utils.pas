{$INCLUDE sdo_global.inc}
unit test_utils;

interface
uses SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , sdo_types, sdo_linked_list ;

type

  TDoubleLinkedList_Test = class(TTestCase)
  published
    procedure test_Create();
    procedure All_1();
    procedure All_2();
  end;

  TLinkedListIterator_Test = class(TTestCase)
  published
    procedure All_1();
    procedure All_2();
    procedure Bookmark;
    procedure MoveTo();
    procedure GetPosition();
    procedure Eof();
  end;

  TUtilsProc_Test = class(TTestCase)
  published
    procedure IsStrEmpty_test();
    procedure GetNextToken_test();
    procedure ExtractLocalName_test();
    procedure IsValidName_test();
    procedure CopySimpleList_integer_test();
  end;

implementation
uses
  Contnrs, sdo_imp_utils, sdo, sdo_datafactory, sdo_dataobject,
  Math, test_suite_utils;


const
  SIZE_A : PtrInt = 1;
  SIZE_B : PtrInt = 123;
  VAL_A = 12;
  VAL_B = -34;

{ TDoubleLinkedList_Test }

procedure TDoubleLinkedList_Test.All_1();
var
  ll : TDoubleLinkedList;
  p0, p01 : PLinkedNode;
  p0_val, p1_val : PPtrInt;
begin
  ll := TDoubleLinkedList.Create(SizeOf(PtrInt));
  try
    CheckEquals(True,ll.IsEmpty(),'IsEmpty()');
    CheckEquals(Integer(0),ll.GetLength(),'GetLength()');
    p0 := ll.InsertFirst();
    Check(Assigned(p0),'InsertFirst()');
    Check(p0 = ll.GetFirst(),'GetFirst()');
    Check(p0 = ll.GetLast(),'GetLast()');
    Check(nil=p0^.Previous,'p0^.Previous = nil');
    Check(nil=p0^.Next,'p0^.Next = nil');
    CheckEquals(1,ll.GetLength(),'GetLength()');
    CheckEquals(False,ll.IsEmpty(),'IsEmpty()');

    p0_val := PPtrInt(@(p0^.Data[0]));
    CheckEquals(PtrInt(0),p0_val^);

    p0_val^ := VAL_A;
    CheckEquals(PtrInt(VAL_A),p0_val^);

    p01 := ll.InsertFirst();
    Check(Assigned(p01),'InsertFirst()');
    Check(p01 = ll.GetFirst(),'GetFirst()');
    Check(p0 = ll.GetLast());
    Check(p01^.Previous=nil,'p1^.Previous=nil');
    Check(p01^.Next=p0,'p1^.Next=p0');
    Check(p01=p0^.Previous,'p1=p0^.Previous');
    Check(p0^.Next=nil,'p0^.Next = nil');
    Check(p0 <> p01,'p0 <> p1');
    CheckEquals(2,ll.GetLength(),'GetLength()');
    CheckEquals(PtrInt(VAL_A),p0_val^);
    p1_val := PPtrInt(@(p01^.Data[0]));
    CheckEquals(PtrInt(0),p1_val^);


    ll.Remove(p01);
    Check(ll.GetFirst() = p0);
    Check(p0 = ll.GetFirst(),'GetFirst()');
    Check(p0 = ll.GetLast(),'GetLast()');
    Check(nil=p0^.Previous,'p0^.Previous = nil');
    Check(nil=p0^.Next,'p0^.Next = nil');
    CheckEquals(1,ll.GetLength(),'GetLength()');
    CheckEquals(False,ll.IsEmpty(),'IsEmpty()');
    ll.FreeBuffer(p01);

    p01 := ll.InsertBefore(p0);
    Check(Assigned(p01),'InsertFirst()');
    Check(p01 = ll.GetFirst(),'GetFirst()');
    Check(p0 = ll.GetLast());
    Check(p01^.Previous=nil,'p1^.Previous=nil');
    Check(p01^.Next=p0,'p1^.Next=p0');
    Check(p01=p0^.Previous,'p1=p0^.Previous');
    Check(p0^.Next=nil,'p0^.Next = nil');
    Check(p0 <> p01,'p0 <> p1');
    CheckEquals(2,ll.GetLength(),'GetLength()');
    CheckEquals(PtrInt(VAL_A),p0_val^);
    p1_val := PPtrInt(@(p01^.Data[0]));
    CheckEquals(PtrInt(0),p1_val^);

    ll.Delete(p0);
    ll.Delete(p01);
    CheckEquals(True,ll.IsEmpty(),'IsEmpty()');
    CheckEquals(Integer(0),ll.GetLength(),'GetLength()');
  finally
    FreeAndNil(ll);
  end;
end;

procedure TDoubleLinkedList_Test.All_2();
var
  ll : TDoubleLinkedList;
  p0, p1, p2 : PLinkedNode;
begin
  ll := TDoubleLinkedList.Create(SizeOf(PtrInt));
  try
    p1 := ll.InsertFirst();
    p0 := ll.InsertBefore(p1);
    p2 := ll.InsertAfter(p1);

    CheckEquals(False,ll.IsEmpty());
    CheckEquals(3,ll.GetLength());
    Check(p0 = ll.GetFirst());
    Check(p2 = ll.GetLast());

    Check(p0^.Previous = nil);
    Check(p0^.Next = p1);

    Check(p1^.Previous = p0);
    Check(p1^.Next = p2);

    Check(p2^.Previous = p1);
    Check(p2^.Next = nil);

    ll.Delete(p1);
      CheckEquals(2,ll.GetLength());
      Check(p0^.Previous = nil);
      Check(p0^.Next = p2);
      Check(p2^.Previous = p0);
      Check(p2^.Next = nil);

      ll.Delete(p2);
        CheckEquals(1,ll.GetLength());
        Check(p0^.Previous = nil);
        Check(p0^.Next = nil);

        ll.Delete(p0);
        CheckEquals(True,ll.IsEmpty());
        CheckEquals(0,ll.GetLength());
  finally
    FreeAndNil(ll);
  end;
end;

procedure TDoubleLinkedList_Test.test_Create();
var
  ok : Boolean;
  ll : TDoubleLinkedList;
begin
  ok := False;
  try
    TDoubleLinkedList.Create(-1);
  except
    on e : EListException do begin
      ok := True;
    end;
  end;
  Check(ok,'Create(-1);');

  ll := TDoubleLinkedList.Create(SIZE_A);
  try
    CheckEquals(SIZE_A,ll.DataSize);
  finally
    FreeAndNil(ll);
  end;
  ll := TDoubleLinkedList.Create(SIZE_B);
  try
    CheckEquals(SIZE_B,ll.DataSize);
  finally
    FreeAndNil(ll);
  end;
end;

{ TLinkedListIterator_Test }

procedure TLinkedListIterator_Test.All_2();
var
  ll : TDoubleLinkedList;
  c : ILinkedListCursor;
  p0, p1, p2 : PLinkedNode;
begin
  ll := TDoubleLinkedList.Create(SIZE_A);
  try
    c := CreateIterator(ll);
    CheckEquals(False,c.IsPosValid());
    c.Reset();
    CheckEquals(False,c.IsPosValid());
    CheckEquals(False,c.MoveNext());
    CheckEquals(False,c.IsPosValid());
    CheckEquals(False,c.MovePrevious());
    CheckEquals(False,c.IsPosValid());
    CheckEquals(False,c.MoveFirst());
    CheckEquals(False,c.IsPosValid());
    CheckEquals(False,c.MoveLast());
    CheckEquals(False,c.IsPosValid());
    Check(nil = c.GetCurrent());

    ll.Clear();
    p0 := ll.InsertFirst();
      CheckEquals(False,c.IsPosValid());
      c.Reset();
      CheckEquals(True,c.MoveNext());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(False,c.MoveNext());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(False,c.MoveNext());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(False,c.MovePrevious());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(False,c.MovePrevious());
        CheckEquals(True,c.IsPosValid());
      Check(p0 = c.GetCurrent());

      CheckEquals(True,c.MoveFirst());
      Check(p0 = c.GetCurrent());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(True,c.MoveLast());
      Check(p0 = c.GetCurrent());
        CheckEquals(True,c.IsPosValid());

    ll.Clear();
    p0 := ll.InsertFirst();
    p1 := ll.InsertAfter(p0);
      c.Reset();
      CheckEquals(False,c.IsPosValid());
      CheckEquals(True,c.MoveNext());
        Check(p0 = c.GetCurrent());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(True,c.MoveNext());
        Check(p1 = c.GetCurrent());
        CheckEquals(True,c.IsPosValid());
      CheckEquals(False,c.MoveNext());
        Check(p1 = c.GetCurrent());
      CheckEquals(True,c.MovePrevious());
        Check(p0 = c.GetCurrent());
      CheckEquals(False,c.MovePrevious());
        Check(p0 = c.GetCurrent());
      Check(p0 = c.GetCurrent());

      CheckEquals(True,c.MoveFirst());
      Check(p0 = c.GetCurrent());
      CheckEquals(True,c.MoveLast());
      Check(p1 = c.GetCurrent());

    ll.Clear();
    CheckEquals(False,c.IsPosValid());
    p0 := ll.InsertFirst();
    p1 := ll.InsertAfter(p0);
    p2 := ll.InsertAfter(p1);
      c.Reset();
      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.MovePrevious());
      Check(p0 = c.GetCurrent());

      CheckEquals(True,c.MoveNext());
      Check(p1 = c.GetCurrent());
      CheckEquals(True,c.MovePrevious());
      Check(p0 = c.GetCurrent());
      CheckEquals(True,c.MoveNext());
      Check(p1 = c.GetCurrent());
      CheckEquals(True,c.MoveNext());
      Check(p2 = c.GetCurrent());
      CheckEquals(False,c.MoveNext());
      Check(p2 = c.GetCurrent());

      CheckEquals(True,c.MovePrevious()); // p2 > p1
      CheckEquals(True,c.MovePrevious()); // p1 > p0
      CheckEquals(False,c.MovePrevious()); // p0 = p0

      CheckEquals(True,c.MoveFirst());
      Check(p0 = c.GetCurrent());
      CheckEquals(True,c.MoveLast());
      Check(p2 = c.GetCurrent());

    ll.Clear();
    c.Reset();
    CheckEquals(False,c.MoveNext());
    CheckEquals(False,c.MovePrevious());
    Check(nil = c.GetCurrent());
  finally
    ll.Free();
  end;
end;

procedure TLinkedListIterator_Test.All_1();
var
  lls : TObjectList;
  ll : TDoubleLinkedList;
  c : ILinkedListCursor;
  p0, p1, p2 : PLinkedNode;
begin
  lls := TObjectList.Create(True);
  try
    ll := TDoubleLinkedList.Create(SIZE_A);
    lls.Add(ll);
    c := CreateIterator(ll);
    CheckEquals(True,c.Bof());
    CheckEquals(True,c.Eof());
    CheckEquals(False,c.MoveFirst());
    CheckEquals(False,c.MoveLast());
    c.Reset();
    CheckEquals(True,c.Bof());
    CheckEquals(True,c.Eof());
    CheckEquals(False,c.MoveFirst());
    CheckEquals(False,c.MoveLast());

    ll := TDoubleLinkedList.Create(SIZE_A);
    lls.Add(ll);
    c := CreateIterator(ll);
    p0 := ll.InsertFirst();
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());
      c.Reset();
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveNext());
      CheckEquals(True,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(False,c.MoveNext());
      CheckEquals(False,c.Bof());
      CheckEquals(True,c.Eof());

    ll := TDoubleLinkedList.Create(SIZE_A);
    lls.Add(ll);
    c := CreateIterator(ll);
    p0 := ll.InsertFirst();
    p1 := ll.InsertAfter(p0);
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());
      c.Reset();
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveNext());
      CheckEquals(True,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveFirst());
      Check(c.GetCurrent() = ll.GetFirst());
      CheckEquals(True,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveLast());
      Check(c.GetCurrent() = ll.GetLast());
      CheckEquals(False,c.Bof());
      CheckEquals(True,c.Eof());

    ll := TDoubleLinkedList.Create(SIZE_A);
    lls.Add(ll);
    c := CreateIterator(ll);
    CheckEquals(False,c.IsPosValid());
    p0 := ll.InsertFirst();
    p1 := ll.InsertAfter(p0);
    p2 := ll.InsertAfter(p1);
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());
      c.Reset();
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveNext());
      CheckEquals(True,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveFirst());
      Check(c.GetCurrent() = ll.GetFirst());
      CheckEquals(True,c.Bof());
      CheckEquals(False,c.Eof());

      CheckEquals(True,c.MoveLast());
      Check(c.GetCurrent() = ll.GetLast());
      CheckEquals(False,c.Bof());
      CheckEquals(True,c.Eof());
  finally
    lls.Free();
  end;
end;

procedure TLinkedListIterator_Test.Bookmark();
var
  ll : TDoubleLinkedList;
  c : ILinkedListCursor;
  bmk : TLinkedListBookmark;
  oldPos : PtrInt;
  p : PLinkedNode;
begin
  ll := TDoubleLinkedList.Create(SizeOf(Integer));
  try
    c := CreateIterator(ll);
    p := c.GetCurrent();
    bmk := c.GetBookmark(); oldPos := c.GetPosition();
    CheckEquals(True,c.GotoBookmark(bmk));
    CheckEquals(True,c.GotoBookmark(bmk));
    CheckEquals(PtrInt(p),PtrInt(c.GetCurrent()));
    CheckEquals(oldPos,c.GetPosition());

    ll.Append();
    ll.Append();
    ll.Append();
    ll.Append();
    ll.Append();

    c.MoveFirst();
    p := c.GetCurrent();
    bmk := c.GetBookmark(); oldPos := c.GetPosition();
    c.MoveNext();
    c.MoveNext();
    CheckEquals(True,c.GotoBookmark(bmk));
    CheckEquals(PtrInt(p),PtrInt(c.GetCurrent()));
    CheckEquals(oldPos,c.GetPosition());

    c.MoveFirst();
    c.MoveNext();
    c.MoveNext();
    p := c.GetCurrent();
    bmk := c.GetBookmark(); oldPos := c.GetPosition();
    c.MoveLast();
    CheckEquals(True,c.GotoBookmark(bmk));
    CheckEquals(PtrInt(p),PtrInt(c.GetCurrent()));
    CheckEquals(oldPos,c.GetPosition());

    c.MoveLast();
    p := c.GetCurrent();
    bmk := c.GetBookmark(); oldPos := c.GetPosition();
    c.MoveFirst();
    CheckEquals(True,c.GotoBookmark(bmk));
    CheckEquals(PtrInt(p),PtrInt(c.GetCurrent()));
    CheckEquals(oldPos,c.GetPosition());
  finally
    ll.Free();
  end;
end;

procedure TLinkedListIterator_Test.MoveTo();
var
  ll : TDoubleLinkedList;
  c : ILinkedListCursor;
  i : PtrInt;
begin
  ll := TDoubleLinkedList.Create(SizeOf(PtrInt));
  try
    c := CreateIterator(ll);
    CheckEquals(False, c.MoveTo(0));
    CheckEquals(False, c.MoveTo(-1));
    CheckEquals(False, c.MoveTo(1));

    PPtrInt(@((ll.Append()^.Data[0])))^ := 0;
    PPtrInt(@((ll.Append()^.Data[0])))^ := 1;
    PPtrInt(@((ll.Append()^.Data[0])))^ := 2;
    PPtrInt(@((ll.Append()^.Data[0])))^ := 3;

    c.MoveFirst();
      CheckEquals(0,PPtrInt(@((c.GetCurrent()^.Data[0])))^);
    for i := 0 to 3 do begin
      CheckEquals(True, c.MoveTo(i));
      CheckEquals(i,PPtrInt(@((c.GetCurrent()^.Data[0])))^);

      CheckEquals(True, c.MoveFirst());
        CheckEquals(True, c.MoveTo(i));
        CheckEquals(i,PPtrInt(@((c.GetCurrent()^.Data[0])))^);
      CheckEquals(True, c.MoveLast());
        CheckEquals(True, c.MoveTo(i));
        CheckEquals(i,PPtrInt(@((c.GetCurrent()^.Data[0])))^);
    end;

    CheckEquals(True, c.MoveTo(1));
      CheckEquals(1,PPtrInt(@((c.GetCurrent()^.Data[0])))^);
    c.MoveFirst();
    CheckEquals(True, c.MoveTo(3));
      CheckEquals(3,PPtrInt(@((c.GetCurrent()^.Data[0])))^);

    CheckEquals(True, c.MoveLast());
    CheckEquals(True, c.MoveTo(2));
      CheckEquals(2,PPtrInt(@((c.GetCurrent()^.Data[0])))^);

    CheckEquals(True, c.MoveTo(1));
    CheckEquals(True, c.MoveTo(2));
      CheckEquals(2,PPtrInt(@((c.GetCurrent()^.Data[0])))^);

  finally
    ll.Free();
  end;
end;

procedure TLinkedListIterator_Test.GetPosition();
var
  ll : TDoubleLinkedList;
  c : ILinkedListCursor;
  i : PtrInt;
begin
  ll := TDoubleLinkedList.Create(SizeOf(PtrInt));
  try
    c := CreateIterator(ll);
    CheckEquals(-1,c.GetPosition());
    CheckEquals(False, c.MoveTo(0));
      CheckEquals(-1,c.GetPosition());
    CheckEquals(False, c.MoveTo(-1));
      CheckEquals(-1,c.GetPosition());
    CheckEquals(False, c.MoveTo(1));
      CheckEquals(-1,c.GetPosition());

    PPtrInt(@((ll.Append()^.Data[0])))^ := 0;
    PPtrInt(@((ll.Append()^.Data[0])))^ := 1;
    PPtrInt(@((ll.Append()^.Data[0])))^ := 2;
    PPtrInt(@((ll.Append()^.Data[0])))^ := 3;

    c.Reset();
      CheckEquals(-1,c.GetPosition());
    c.MoveFirst();
      CheckEquals(0,c.GetPosition());
    for i := 0 to 3 do begin
      CheckEquals(True, c.MoveTo(i));
      CheckEquals(i,c.GetPosition());
    end;

    CheckEquals(True, c.MoveLast());
      CheckEquals(Pred(ll.GetLength()),c.GetPosition());
    c.MoveFirst();
      CheckEquals(0,c.GetPosition());

  finally
    ll.Free();
  end;
end;

procedure TLinkedListIterator_Test.Eof();
var
  lls : TObjectList;
  ll : TDoubleLinkedList;
  c : ILinkedListCursor;
  p0, p1, p2 : PLinkedNode;
begin
  lls := TObjectList.Create(True);
  try
    ll := TDoubleLinkedList.Create(SIZE_A);
    lls.Add(ll);
    c := CreateIterator(ll);
    CheckEquals(True,c.Eof());

    p0 := ll.InsertFirst();
      c.Reset();
      CheckEquals(False,c.Eof());
      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Eof());
      CheckEquals(False,c.MoveNext());
      CheckEquals(True,c.Eof());

      c.Reset();
      CheckEquals(False,c.Eof());
      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Eof());
      CheckEquals(False,c.MoveNext());
      CheckEquals(True,c.Eof());

      c.Reset();
      CheckEquals(False,c.Eof());
      CheckEquals(True,c.MoveLast());
      CheckEquals(True,c.Eof());

    ll := TDoubleLinkedList.Create(SIZE_A);
    lls.Add(ll);
    c := CreateIterator(ll);
    p0 := ll.InsertFirst();
    p1 := ll.InsertAfter(p0);
      c.Reset();
      CheckEquals(False,c.Eof());
      c.Reset();
      CheckEquals(False,c.Eof());
      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Eof());
      CheckEquals(True,c.MoveNext());
      CheckEquals(False,c.Eof());
      CheckEquals(False,c.MoveNext());
      CheckEquals(True,c.Eof());

      c.Reset();
      CheckEquals(False,c.Eof());
      CheckEquals(True,c.MoveLast());
      CheckEquals(True,c.Eof());
  finally
    lls.Free();
  end;
end;

{ TUtilsProc_Test }

procedure TUtilsProc_Test.CopySimpleList_integer_test();

  procedure check_list(const x, y : ISDODataObjectList);
  var
    cx, cy : ILinkedListCursor;
  begin
    CheckEquals(x.size(), y.size(), 'size');
    if ( x.size() > 0 ) then begin
      cx := x.getCursor();
      cx.Reset();
      cy := y.getCursor();
      cy.Reset();
      while cx.MoveNext() do begin
        Check(cy.MoveNext(), 'MoveNext()');
        CheckEquals(x.getInteger(), y.getInteger(), Format('Item #%d',[cx.GetPosition()]));
      end;
    end;
  end;

var
  f : ISDODataFactory;
  a, b : ISDODataObjectList;
  t : ISDOType;
  i, c : PtrInt;
begin
  Randomize();
  f := TSDODataFactory.Create();
  t := f.getType(sdo_namespace,SDOTypeDefaultTypeNames[IntegerType]);
  a := TSDODataObjectList.Create(t);
  b := TSDODataObjectList.Create(t);

  CopySimpleList(a, b, t.getTypeEnum());
    check_list(a, b);

  c := RandomRange(1,100);
  for i := 0 to Pred(c) do
    a.append(RandomRange(-1234, 56789));
  CopySimpleList(a, b, t.getTypeEnum());
    check_list(a, b);
end;

procedure TUtilsProc_Test.ExtractLocalName_test();
begin
  CheckEquals('',ExtractLocalName(''));
  CheckEquals('a',ExtractLocalName('a'));
  CheckEquals('azerty',ExtractLocalName('azerty'));
  CheckEquals('azerty',ExtractLocalName('ns:azerty'));
  CheckEquals('azerty',ExtractLocalName('n:azerty'));
  CheckEquals('azerty',ExtractLocalName(':azerty'));
  CheckEquals('',ExtractLocalName('azerty:'));
end;

procedure TUtilsProc_Test.GetNextToken_test();
var
  buffer, x : string;
  sep : Char;
begin
  sep := ';';
  buffer := '';
    x := GetNextToken(buffer,sep);
    CheckEquals('', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := ';';
    x := GetNextToken(buffer,sep);
    CheckEquals('', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := ';;;';
    x := GetNextToken(buffer,sep);
    CheckEquals('', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := 'abc';
    x := GetNextToken(buffer,sep);
    CheckEquals('abc', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := 'ab;cd';
    x := GetNextToken(buffer,sep);
    CheckEquals('ab', x);
    CheckEquals('cd', buffer);
    x := GetNextToken(buffer,sep);
    CheckEquals('cd', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := ';ab;cd';
    x := GetNextToken(buffer,sep);
    CheckEquals('ab', x);
    CheckEquals('cd', buffer);
    x := GetNextToken(buffer,sep);
    CheckEquals('cd', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := ';ab;;cd';
    x := GetNextToken(buffer,sep);
    CheckEquals('ab', x);
    CheckEquals(';cd', buffer);
    x := GetNextToken(buffer,sep);
    CheckEquals('cd', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := 'ab;cd;';
    x := GetNextToken(buffer,sep);
    CheckEquals('ab', x);
    CheckEquals('cd;', buffer);
    x := GetNextToken(buffer,sep);
    CheckEquals('cd', x);
    CheckEquals('', buffer);

  sep := ';';
  buffer := 'ab;cd;de';
    x := GetNextToken(buffer,sep);
    CheckEquals('ab', x);
    CheckEquals('cd;de', buffer);
    x := GetNextToken(buffer,sep);
    CheckEquals('cd', x);
    CheckEquals('de', buffer);
    x := GetNextToken(buffer,sep);
    CheckEquals('de', x);
    CheckEquals('', buffer);

end;

procedure TUtilsProc_Test.IsStrEmpty_test;
begin
  CheckEquals(True,IsStrEmpty(''));
  CheckEquals(True,IsStrEmpty('  '));

  CheckEquals(False,IsStrEmpty('a'));
  CheckEquals(False,IsStrEmpty('  a'));
  CheckEquals(False,IsStrEmpty('afghjk'));
  CheckEquals(False,IsStrEmpty(' jhhfjjsd '));
  CheckEquals(False,IsStrEmpty(':6789^;:,?'));
end;

procedure TUtilsProc_Test.IsValidName_test();
begin
  CheckEquals(True, IsValidName('_'));
  CheckEquals(True, IsValidName('_a'));
  CheckEquals(True, IsValidName('_1'));
  CheckEquals(True, IsValidName('_azerty'));
  CheckEquals(True, IsValidName('a'));
  CheckEquals(True, IsValidName('azerty'));
  CheckEquals(True, IsValidName('_a_z'));
  CheckEquals(True, IsValidName('a1210'));

  CheckEquals(False, IsValidName(''));
  CheckEquals(False, IsValidName('-'));
  CheckEquals(False, IsValidName('-a'));
  CheckEquals(False, IsValidName('4'));
  CheckEquals(False, IsValidName('1s'));
  CheckEquals(False, IsValidName('-4'));
  CheckEquals(False, IsValidName('aad-ddd'));
  CheckEquals(False, IsValidName('zz_-'));
  CheckEquals(False, IsValidName('['));
  CheckEquals(False, IsValidName(']'));
  CheckEquals(False, IsValidName('az[]'));
  CheckEquals(False, IsValidName('l[1]'));
  CheckEquals(False, IsValidName('dd['));
  CheckEquals(False, IsValidName('dd]'));
end;

initialization
  RegisterTest('utils',TDoubleLinkedList_Test.Suite);
  RegisterTest('utils',TLinkedListIterator_Test.Suite);
  RegisterTest('utils',TUtilsProc_Test.Suite);

end.
