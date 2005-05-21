{ Source provided for Free Pascal Bug Report 3786 }
{ Submitted by "drf" on  2005-03-14 }
{ e-mail: drfuchs@yahoo.com }

{$mode delphi}

type
  funky_class=class;
  base_class=class
    procedure proc(f:funky_class);virtual;
  end;
  subclass2=class(base_class)
    procedure proc(f:funky_class);override;
  end;
  subclass3=class(base_class)
    procedure proc(f:funky_class);override;
  end;
  funky_class=class
    procedure proc(p:base_class);overload;virtual;
    procedure proc(p:subclass2);overload;virtual;
    procedure proc(p:subclass3);overload;virtual;
  end;
  funky_subclass=class(funky_class)
    procedure proc(p:subclass3);override;
    procedure proc(p:subclass2); override;
  end;

procedure base_class.proc(f:funky_class); begin end;
procedure subclass2.proc(f:funky_class); begin end;
procedure subclass3.proc(f:funky_class); begin end;

procedure funky_class.proc(p:base_class); begin end;
procedure funky_class.proc(p:subclass2); begin end;
procedure funky_class.proc(p:subclass3); begin end;

procedure funky_subclass.proc(p:subclass2); begin end;
procedure funky_subclass.proc(p:subclass3); begin end;

begin
end.
