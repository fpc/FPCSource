Unit fxbitmaps;

{$mode objfpc}

Interface

uses glib,gdk,gtk;

Const
  DeleteXpmHeight=16;
  DeleteXpmColors=2;
  DeleteXpmArraySize=DeleteXpmHeight+DeleteXpmColors+1;
  DeleteXpm : Array[1..DeleteXpmArraySize] of Pchar = (
    '16 16 2 1',        { 16x16 bitmap using 2 colors, 1 char per color}
    '. c #000000',      { First color: Black  }
    '# c None',         { Second color : Transparent}
    '################', { The bitmap }
    '################',
    '##...#########.#',
    '##....######..##',
    '###....####..###',
    '#####...##..####',
    '######.....#####',
    '#######...######',
    '######.....#####',
    '#####...##..####',
    '####...####..###',
    '###...######.###',
    '##....#######.##',
    '##...###########',
    '###.##########.#',
    '################'
    );

  PropertiesXpmHeight = 16;
  PropertiesXpmColors = 4;
  PropertiesXpmArraySize = PropertiesXpmHeight+PropertiesXpmColors+1;
  PropertiesXpm : Array [1..PropertiesXpmArraySize] of PChar = (
    '16 16 4 1',        { 16x16 bitmap using 2 colors, 1 char per color}
    '. c #000000',      { First color : Black }
    '# c #000080',      { Second color : Light Blue }
    'a c None',         { Third color : Transparent }
    'b c #f8fcf8',      { Last color : greyish }
    'aaaaaaaaaaaaaaaa',
    'aaaaaaa......a##',
    'aaaaaa.aaaaaa.##',
    'aaaaa.a.aaaaaa##',
    '.....a.a.aaaaa##',
    '.bb.a.a.a.aaa.##',
    '.b.a.b.a.a...a##',
    '.b..bbb.a.b.aaaa',
    '.bbbbbbb.bb.aaaa',
    '.bbbbbbbbbb.aaaa',
    '.b..b.....b.aaaa',
    '.bbbbbbbbbb.aaaa',
    '.b..b.....b.aaaa',
    '.bbbbbbbbbb.aaaa',
    '............aaaa',
    'aaaaaaaaaaaaaaaa'
);

function CreateWidgetFromXPM (Window : PGtkWidget; Data : PPChar) : PgtkWidget;

Implementation

function CreateWidgetFromXPM (Window : PGtkWidget; Data : PPChar) : PGtkWidget;

Var
  mask   : PGdkBitmap;
  pixmap : PGdkPixMap;

begin
  pixmap:=gdk_pixmap_create_from_xpm_d(window^.window,@mask,nil,ppgchar(Data));
  Result:=gtk_pixmap_new(Pixmap,Mask);
  gtk_widget_show(Result);
end;

end.
