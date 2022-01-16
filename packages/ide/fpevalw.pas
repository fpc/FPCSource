{
    Copyright (c) 1997-2008 by the Daniel Mantione
   
    Debug expression evaluator dialog ? 
   
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpevalw;

{****************************************************************************}
                                  interface
{****************************************************************************}

uses fpdebug,dialogs,views,objects,fpconst,drivers;

type  Pevaluate_dialog=^Tevaluate_dialog;
      Tevaluate_dialog=object(Tdialog)
        watch:Pwatch;
        expr_input,expr_output:Pinputline;
        constructor init(var bounds:Trect);
        procedure evaluate;
        procedure handleevent(var event:Tevent);virtual;
        destructor done;
      end;

{****************************************************************************}
                                implementation
{****************************************************************************}

constructor Tevaluate_dialog.init(var bounds:Trect);

var r:Trect;
    l:Plabel;
    b:Pbutton;

begin
  inherited init(bounds,'Evaluate expression');
  options:=options or ofcentered;
  {watch is auto initialized to nil.}

  r.assign(2,3,size.x-20,4);
  new(expr_input,init(r,255));
  insert(expr_input);

  r.assign(size.x-20,3,size.x-18,4);
  insert(new(Phistory,init(r,expr_input,hidEvaluate)));

  r.assign(2,2,size.x-20,3);
  new(l,init(r,'E~x~pression:',expr_input));
  insert(l);

  r.assign(2,6,size.x-20,7);
  new(expr_output,init(r,255));
  insert(expr_output);

  r.assign(2,5,size.x-20,6);
  new(l,init(r,'~R~esult:',expr_output));
  insert(l);

  r.assign(size.x-14,3,size.x-3,5);
  new(b,init(r,'~E~valuate',cmEvaluate,bfDefault));
  insert(b);

  //r.assign(size.x-14,6,size.x-3,8);
  //new(b,init(r,'Help',cmHelp,bfNormal));
  //insert(b);

  expr_input^.select;
end;

procedure Tevaluate_dialog.evaluate;

begin
  if watch<>nil then
    dispose(watch,done);
  new(watch,init(expr_input^.data^));
  expr_output^.data^:=strpas(watch^.current_value);
  expr_output^.drawview;
end;

procedure Tevaluate_dialog.handleevent(var event:Tevent);

begin
  inherited handleevent(event);
  if event.what=evCommand then
    case event.command of
      cmEvaluate:
        evaluate;
    end;
end;

destructor Tevaluate_dialog.done;

begin
  if watch<>nil then
    dispose(watch,done);
end;

end.
