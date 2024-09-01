{
    Copyright (c) 1997-2008 by the Daniel Mantione

    Debug expression evaluator dialog

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpevalw;

{$H-}

{****************************************************************************}
                                  interface
{****************************************************************************}

uses fpdebug,dialogs,WViews,objects,fpconst,drivers,WEditor;

type  Pevaluate_dialog=^Tevaluate_dialog;
      Tevaluate_dialog=object(TCenterDialog)
        watch : PWatch;
        expr_input : PEditorInputLine;
        expr_output : PEditorInputLine;
        constructor init(var bounds:Trect);
        procedure evaluate;
        procedure HandleEvent(var Event:TEvent);virtual;
        destructor done;
      end;

{****************************************************************************}
                                implementation
{****************************************************************************}
uses FPViews;

constructor Tevaluate_dialog.init(var bounds:Trect);
var r:Trect;
    l:Plabel;
    b:Pbutton;
    EditorWindow : PSourceWindow;
    S : String;
begin
  inherited init(bounds,'Evaluate expression');
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
  EditorWindow:=FirstEditorWindow;
  If assigned(EditorWindow) then
    S:=EditorWindow^.Editor^.GetCurrentWord
  else
    S:='';
  expr_input^.SetData(S);
  expr_input^.Select;
  if s <>'' then
    evaluate;
  { for right arrow to give a char from EditorWindow }
  if assigned(EditorWindow) then
    FindReplaceEditor:=EditorWindow^.Editor;

end;

procedure Tevaluate_dialog.Evaluate;
begin
  if watch<>nil then
    dispose(watch,done);
  new(watch,init(expr_input^.data^));
  expr_output^.data^:=strpas(watch^.current_value);
  expr_output^.drawview;
end;

procedure Tevaluate_dialog.HandleEvent(var Event:TEvent);
begin
  inherited HandleEvent(Event);
  if Event.what=evCommand then
    case Event.command of
      cmEvaluate:
        evaluate;
    end;
end;

destructor Tevaluate_dialog.done;
begin
  FindReplaceEditor:=nil;
  if watch<>nil then
    dispose(watch,done);
end;

end.
