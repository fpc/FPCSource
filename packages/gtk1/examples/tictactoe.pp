{

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
unit tictactoe;
interface

uses
  glib,gdk,gtk;

type
  PTictactoe      = ^TTictactoe;
  TTictactoe      = record
                      vbox    : TGtkVBox ;
                      buttons : array [0..2 , 0..2] of pGtkWidget;
                    end;
  PTictactoeClass = ^TTictactoeClass;
  TTictactoeClass = record
                      parent_class: TGtkVBoxClass ;
                      tictactoe: Procedure (ttt : pTictactoe); cdecl;
                    end;

Function tictactoe_get_type : guint;
Function tictactoe_new : pGtkWidget;
procedure tictactoe_clear (ttt : pTictactoe);

implementation

const
  ANZ_SIGNAL = 1;
type
  TTT_Signals = (TICTACTOE_SIGNAL);
const
  tictactoe_signals: array[TTT_Signals] of guint = (0);

Procedure tictactoe_toggle (widget : pGtkWidget ; ttt: pTictactoe); cdecl;
const rwins: array[0..7,0..2] of integer =
  ( ( 0, 0, 0 ), ( 1, 1, 1 ), ( 2, 2, 2 ),
   ( 0, 1, 2 ), ( 0, 1, 2 ), ( 0, 1, 2 ),
   ( 0, 1, 2 ), ( 0, 1, 2 ) );
  cwins:array [0..7,0..2] of integer =
  ( ( 0, 1, 2 ), ( 0, 1, 2 ), ( 0, 1, 2 ),
   ( 0, 0, 0 ), ( 1, 1, 1 ), ( 2, 2, 2 ),
   ( 0, 1, 2 ), ( 2, 1, 0 ) );
var i, k         : integer;
  success, found : boolean;
begin
  for k:=0 to 7 do
  begin
    success := TRUE;
    found := FALSE;

    for i:=0 to 2 do
    begin
      success := success and
      boolean(active(pGTKTOGGLEBUTTON(ttt^.buttons[rwins[k,i],cwins[k,i]])^));
      found := found or
      (ttt^.buttons[rwins[k,i],cwins[k,i]] = widget);
    end;

    if (success and found) then
    begin
      gtk_signal_emit (pGTKOBJECT (ttt),
                        tictactoe_signals[TICTACTOE_SIGNAL]);
      break;
    end;
  end;
end;

Procedure gtk_signal_default_marshallerT(theobject : pGtkObject;
                                         func      : GTK_SIGNAL_FUNC;
                                         func_data : gpointer;
                                         args      : pGtkArg); cdecl;
begin
  gtk_marshal_NONE__NONE (theobject,func,func_data,args);
end;

Procedure tictactoe_class_init (theclass : pTictactoeClass );
var object_class : pGtkObjectClass ;
begin
  object_class := pGtkObjectClass (theclass);

  tictactoe_signals[TICTACTOE_SIGNAL] :=gtk_signal_new ('tictactoe',
                      GTK_RUN_FIRST,
                      object_class^.thetype,
                      @theclass^.tictactoe - pointer(theclass),
                      @gtk_signal_default_marshallerT, GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, pguint(@tictactoe_signals), ANZ_SIGNAL);

  theclass^.tictactoe := NIL;
end;

Procedure tictactoe_init (ttt : pTictactoe );
var table : pGtkWidget ;
  i,j     : gint;
begin
  table := gtk_table_new (3, 3, true);
  gtk_container_add (pGTKCONTAINER(ttt), table);
  gtk_widget_show (table);

  for i:=0 to 2 do
    for j:=0 to 2 do
    begin
      ttt^.buttons[i][j] := gtk_toggle_button_new ();
      gtk_table_attach_defaults (pGTKTABLE(table), ttt^.buttons[i][j],
                                 i, i+1, j, j+1);
      gtk_signal_connect (pGTKOBJECT (ttt^.buttons[i][j]), 'toggled',
                          GTK_SIGNAL_FUNC (@tictactoe_toggle), ttt);
      gtk_widget_set_usize (ttt^.buttons[i][j], 20, 20);
      gtk_widget_show (ttt^.buttons[i][j]);
    end;
end;

Procedure tictactoe_class_init2 (theclass : gpointer ); cdecl;
begin
  tictactoe_class_init (theclass);
end;

Procedure tictactoe_init2 (ttt : gpointer; klass:gpointer); cdecl;
begin
  tictactoe_init (ttt);
end;

Function tictactoe_get_type:guint;
const ttt_type : guint  = 0;
  ttt_info: TGtkTypeInfo = (
                                                        type_name : 'Tictactoe';
                                                        object_size : sizeof (TTictactoe);
                                                        class_size : sizeof (TTictactoeClass);
                                                        class_init_func : @tictactoe_class_init2;
                                                        object_init_func : @tictactoe_init2;
                                                        );
begin
  if (ttt_type = 0) then
    ttt_type := gtk_type_unique (gtk_vbox_get_type (), @ttt_info);

  tictactoe_get_type:= ttt_type;
end;

Function tictactoe_new:pGtkWidget;
begin
  tictactoe_new:= pGTKWIDGET ( gtk_type_new (tictactoe_get_type ()));
end;

Procedure tictactoe_clear (ttt :  pTictactoe );
var i,j : integer;
begin
  for i:=0 to 2 do
    for j:=0 to 2 do
    begin
      gtk_signal_handler_block_by_data (pGTKOBJECT(ttt^.buttons[i][j]), ttt);
      gtk_toggle_button_set_active (pGTKTOGGLEBUTTON (ttt^.buttons[i][j]),
                                   false);
      gtk_signal_handler_unblock_by_data (pGTKOBJECT(ttt^.buttons[i][j]), ttt);
    end;
end;

end.
