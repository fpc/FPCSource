program testthreads;

{$mode objfpc}

uses
  sysutils,
  classes;

type
  TMyThread=class(TThread)
  private
    ch : char;
  protected
    procedure Execute; override;
  public
    constructor Create(c:char);
  end;

procedure TMyThread.Execute;
begin
  repeat
    write(ch);
  until false;
end;


constructor TMyThread.Create(c:char);
begin
  ch:=c;
  inherited Create(false);
end;

var
  t1,t2 : TMyThread;
begin
  t1:=TMyThread.Create('a');
  t2:=TMyThread.Create('b');
  readln;
  t2.Terminate;
  readln;
  t1.Terminate;
  readln;
  t2.Destroy;
  t1.Destroy;
end.
  $Log$
  Revision 1.3  2002-09-07 15:15:28  peter
    * old logs removed and tabs fixed

}
