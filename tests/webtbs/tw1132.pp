program BugDemo2;

type
   MyRecordType =
   record
      RecordElement1 : word;
      RecordElement2 : word;
   end;

var
   MyRecord : MyRecordType;
   MyPointer1,MyPointer2 : pointer;

begin
   with MyRecord do
   begin
      { next statement crashes the compiler }
      MyPointer1 := addr(RecordElement2);

      { next statement is OK }
      MyPointer2 := addr(MyRecord.RecordElement2);
   end;
  if MyPointer1<>MyPointer2 then
   begin
     Writeln('Error with addr() and with statement');
     halt(1);
   end;
end.
