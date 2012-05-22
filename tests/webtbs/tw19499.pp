{$MODE OBJFPC} { -*- text -*- }
program tw19499;

{$apptype console}

type
   generic THashTable <TKey, TValue> = class
     type
      THashTableEnumerator = class
       private
         FOwner: THashTable;
         function GetCurrent(): TValue;
       public
         constructor Create(Owner: THashTable);
         function MoveNext(): Boolean;
         property Current: TValue read GetCurrent;
      end;
      function GetEnumerator(): THashTableEnumerator;
     // note that this works:
     var
      Foo: THashTable;
      procedure Bar(Arg: THashTable);
   end;

function THashTable.THashTableEnumerator.GetCurrent(): TValue;
begin
// this did not compile in the original test
//   Result := TValue(nil);
   Result := Default(TValue);
end;

constructor THashTable.THashTableEnumerator.Create(Owner: THashTable);
begin
   FOwner := Owner;
end;

function THashTable.THashTableEnumerator.MoveNext(): Boolean;
begin
   Result := False;
end;

function THashTable.GetEnumerator(): THashTableEnumerator;
begin
   Result := THashTableEnumerator.Create(Self);
end;

procedure THashTable.Bar(Arg: THashTable);
var
   Quux: THashTable; // this works also
begin
end;

type
   TIntegerToStringHashTable = specialize THashTable<Integer, AnsiString>;

var
   Test: TIntegerToStringHashTable;
   S: AnsiString;

begin
   // this was incorrect in the original test
   //Test.Create();
   Test := TIntegerToStringHashTable.Create();
   try
      for S in Test do
         Writeln(S);
   finally
      Test.Destroy();
   end;
   Writeln('PASS');
end.
