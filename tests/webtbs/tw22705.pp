{ %norun }

program badclass;

{$mode objfpc}

type
  tMyClass=class
   end;
  tMyFormType= class of tMyClass;

  prec = ^trec;
  trec = record
  end;

  TForm1 = class
  public
    procedure myFNC(obj:tMyFormType);  overload;
    procedure myFNC(obj:tClass);       overload;
    procedure myFNC2(obj:prec);       overload;
    procedure myFNC2(obj:pchar);       overload;
  end;

  TForm2 = class
  public
    procedure myFNC(obj:tClass);       overload;
    procedure myFNC(obj:tMyFormType);  overload;
    procedure myFNC2(obj:pchar);       overload;
    procedure myFNC2(obj:prec);       overload;
  end;

procedure TForm1.myFNC(obj:tClass);
begin
end;

procedure TForm1.myFNC(obj:tMyFormType);
begin
end;

procedure TForm1.myFNC2(obj:prec);
begin
end;

procedure TForm1.myFNC2(obj:pchar);
begin
end;


procedure TForm2.myFNC(obj:tClass);
begin
end;

procedure TForm2.myFNC(obj:tMyFormType);
begin
end;

procedure TForm2.myFNC2(obj:prec);
begin
end;

procedure TForm2.myFNC2(obj:pchar);
begin
end;


begin
end.


