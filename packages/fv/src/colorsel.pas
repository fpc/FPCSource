{

   (Still unused) skeleton for Colorsel replacement, based on mostly the 
     use by the fpmopts.inc file, to be added on as details emerge.

   Copyright 2008 by Marco van de Voort and Andreas Jakobsche

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.

 ****************************************************************************}
}
unit ColorSel;

interface

uses Objects, Dialogs, Views;

type

  PColorItem = ^TColorItem;
  TColorItem = record
    Name: PString;
    Index: Byte;
    Next: PColorItem;
  end;

  PColorGroup = ^TColorGroup;
  TColorGroup = record
    Name: PString;
    Index: Byte;
    Items: PColorItem;
    Next: PColorGroup;
  end;

  PColorGroupList = ^TColorGroupList;
  TColorGroupList = object(TListViewer)
    Groups: PColorGroup;
    constructor Init(var Bounds: TRect; AScrollBar: PScrollBar; AGroups: PColorGroup);
  end;

  PColorDialog = ^TColorDialog;
  TColorDialog = object(TDialog)
    Groups: PColorGroupList;
    Pal: TPalette;
    constructor Init(APalette: TPalette; AGroups: PColorGroup);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
  end;

function ColorGroup(Name: string; Items: PColorItem; Next: PColorGroup): PColorGroup;

function ColorItem(Name: string; Index: Byte; Next: PColorItem): PColorItem;

implementation

uses App;

const
  RColorDialog: TStreamRec = (
     ObjType: idColorDialog;
     VmtLink: Ofs(TypeOf(TColorDialog)^);
     Load:    @TColorDialog.Load;
     Store:   @TColorDialog.Store
  );

procedure RegisterColorsel;
begin
 // according to help should register TColorSelector,     TMonoSelector, TColorDisplay, TColorGroupList, TColorItemList,     TColorDialog
 // probably don't bother with the mono variants. Except for (P/T)colordialog, these don't grep in FV/IDE src.

 // TColorSelector -> the square colorselection widget (instantiated twice once for front, once for back?)
 // TColorGrouplist-> the selection of the color group (left list)  (TListbox or whatever the TV eq is?)
 // TColorItemList -> the selection of the color identifier (right list)  (TListbox or whatever the TV eq is?)

 RegisterType(RColorDialog);
end ;


function ColorGroup(Name: string; Items: PColorItem; Next: PColorGroup): PColorGroup;
var
  R: PColorGroup;
begin
  New(R);
  R^.Name := NewStr(Name);
  R^.Items := Items;
  R^.Next := Next;
  ColorGroup := R;
end;


function ColorItem(Name: string; Index: Byte; Next: PColorItem): PColorItem;
var R: PColorItem;
begin
  New(R);
  R^.Name := NewStr(Name);
  R^.Index := Index;
  R^.Next := Next;
  ColorItem := R
end;


constructor TColorGroupList.Init(var Bounds: TRect; AScrollBar: PScrollBar; AGroups: PColorGroup);
var
  x: PColorGroup;
begin
  inherited Init(Bounds, 1, nil, AScrollBar);
  Range := 0;
  Groups := AGroups;
  x := AGroups;
  while Assigned(x) do begin
    x^.Index := Range;
    Inc(Range);
    x := x^.Next
  end;
end;


constructor TColorDialog.Init(APalette: TPalette; AGroups: PColorGroup);
var
  Bounds: TRect;
begin
  Bounds.Assign(0, 0, 62, 19);
  inherited Init(Bounds, 'Colors');
  Options := Options or ofCentered;
  Pal := APalette;
  Bounds.Grow(-1, -1);
  Groups := New(PColorGroupList, Init(Bounds, nil, AGroups));
end;


constructor TColorDialog.Load(var S: TStream);
begin
end;


procedure TColorDialog.Store(var S: TStream);
begin
end;

end.

{


 ColorGroup(label_colors_grp_menus,   MenuColorItems(nil),
    ColorGroup(label_colors_grp_desktop, DesktopColorItems(nil),
    ColorGroup(label_colors_grp_dialogs, DialogColorItems(dpGrayDialog,nil),


 from fpmopts.inc
procedure TIDEApp.Colors;

var D: PColorDialog;
begin
  New(D, Init(AppPalette,
    ColorGroup(label_colors_grp_browser,
      ColorItem(label_colors_framepassive   , 215,
      ColorItem(label_colors_frameactive    , 216,
      ColorItem(label_colors_frameicon      , 217,
      ColorItem(label_colors_scrollbarpage  , 218,
      ColorItem(label_colors_scrollbaricons , 219,
      ColorItem(label_colors_normaltext     , 220,
      ColorItem(label_colors_selectedtext   , 221,
      ColorItem(label_colors_activeitem     , 222,
      ColorItem(label_colors_inactiveitem   , 223,
      ColorItem(label_colors_focuseditem    , 224,
      ColorItem(label_colors_selecteditem   , 225,
      ColorItem(label_colors_divider        , 226,
      nil)))))))))))),
    ColorGroup(label_colors_grp_clock,
      ColorItem(label_colors_clockview      , 227,
      nil),
    ColorGroup(label_colors_grp_menus,   MenuColorItems(nil),
    ColorGroup(label_colors_grp_desktop, DesktopColorItems(nil),
    ColorGroup(label_colors_grp_dialogs, DialogColorItems(dpGrayDialog,nil),
    ColorGroup(label_colors_grp_editor,
      ColorItem(label_colors_framepassive   , 167,
      ColorItem(label_colors_frameactive    , 168,
      ColorItem(label_colors_frameicon      , 169,
      ColorItem(label_colors_scrollbarpage  , 170,
      ColorItem(label_colors_scrollbaricons , 171,
      ColorItem(label_colors_normaltext     , 199,
      ColorItem(label_colors_selectedtext   , 208,
      ColorItem(label_colors_highlighcolumn , 209,
      ColorItem(label_colors_highlightrow   , 210,
      ColorItem(label_colors_errormessages  , 214,
      nil)))))))))),
    ColorGroup(label_colors_grp_help,
      ColorItem(label_colors_framepassive   , 128,
      ColorItem(label_colors_frameactive    , 129,
      ColorItem(label_colors_frameicon      , 130,
      ColorItem(label_colors_scrollbarpage  , 131,
      ColorItem(label_colors_scrollbaricons , 132,
      ColorItem(label_colors_helptext       , 160,
      ColorItem(label_colors_helplinks      , 161,
      ColorItem(label_colors_selectedlink   , 162,
      ColorItem(label_colors_selectedtext   , 163,
      ColorItem(label_colors_html_heading1  , 229,
      ColorItem(label_colors_html_heading2  , 230,
      ColorItem(label_colors_html_heading3  , 231,
      ColorItem(label_colors_html_heading4  , 232,
      ColorItem(label_colors_html_heading5  , 233,
      ColorItem(label_colors_html_heading6  , 234,
      nil))))))))))))))),
    ColorGroup(label_colors_grp_menus,   MenuColorItems(nil),
    ColorGroup(label_colors_grp_syntax,
      ColorItem(label_colors_whitespace      , 200,
      ColorItem(label_colors_comments        , 201,
      ColorItem(label_colors_reservedwords   , 202,
      ColorItem(label_colors_identifiers     , 203,
      ColorItem(label_colors_strings         , 204,
      ColorItem(label_colors_numbers         , 205,
      ColorItem(label_colors_hexnumbers      , 212,
      ColorItem(label_colors_assembler       , 206,
      ColorItem(label_colors_symbols         , 207,
      ColorItem(label_colors_directives      , 211,
      ColorItem(label_colors_tabs            , 213,
      nil))))))))))),
    nil))))))))));
end;

fvconsts.pas:  idColorSelector = 92;
fvconsts.pas:  idMonoSelector = 93;

}
