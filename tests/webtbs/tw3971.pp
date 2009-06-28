{ Source provided for Free Pascal Bug Report 3971 }
{ Submitted by "Thomas Schatzl" on  2005-05-16 }
{ e-mail:  }
type
        TDemo1 = object
                member1 : byte;
                member2 : longint;
                
                member7 : ^longint;
                member3 : int64;
                
                member4 : byte;
                member5 : longint;
                //x : boolean;
                //
                member6 : int64;
                x : boolean;
        end;
        
        TDemo = object
                member1 : byte;
                member5 : longint;
                member6 : int64;
                y : array[0..2] of TDemo1;

                constructor init;
                destructor Destroy;
                procedure doSomething;
                procedure doSomething2;
        end;
var
        x : array[0..2] of TDemo;
        
        z : TDemo;
        w : TDemo1;

constructor TDemo.init();
begin
        WriteLn('Create start');
        inherited;
        WriteLn('Create end');
end;

destructor TDemo.Destroy();
begin
        WriteLn('Destroy start');
        inherited;
        WriteLn('Destroy end');
end;

procedure TDemo.doSomething;
begin
        WriteLn('doSomething');
end;

procedure TDemo.doSomething2;
begin
        WriteLn('doSomething');
end;


begin
        z.init;
        if ((ptrint(@z.y)-ptrint(@z)) mod sizeof(ptrint))<>0 then
          halt(1);
        if ((ptrint(@z.y[0].member7)-ptrint(@z)) mod sizeof(ptrint))<>0 then
          halt(1);
        z.destroy;
        WriteLn(sizeof(TDemo), ' ', sizeof(TDemo1));
end.
