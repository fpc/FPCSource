{ %fail }

uses
  Classes;

type
  TForm1 = class
  private
    { private declarations }
    fScript:TStringlist.Create; <--- erroneous decl.
  public
    { public declarations }
  end; 

begin
end.
