type
  test_one = class
     protected
       fTest : String;
     public
       property Test: String READ fTest WRITE fTest;
       procedure Testen(Test: BOolean);
                        { ^ duplicate identifier? }
  end;


procedure test_one.testen(test: boolean);
begin
end;

begin
end.
