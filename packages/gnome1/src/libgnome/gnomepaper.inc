{$IfDef read_interface}

type
   PGnomePaper = ^TGnomePaper;
   TGnomePaper = Record
   end;

   PGnomeUnit = ^TGnomeUnit;
   TGnomeUnit = Record
   end;

function gnome_paper_name_list:PGList;cdecl;external libgnomedll name 'gnome_paper_name_list';
function gnome_paper_with_name(papername:Pgchar):PGnomePaper;cdecl;external libgnomedll name 'gnome_paper_with_name';
function gnome_paper_with_size(pswidth:double; psheight:double):PGnomePaper;cdecl;external libgnomedll name 'gnome_paper_with_size';
function gnome_paper_name(paper:PGnomePaper):Pgchar;cdecl;external libgnomedll name 'gnome_paper_name';
function gnome_paper_pswidth(paper:PGnomePaper):gdouble;cdecl;external libgnomedll name 'gnome_paper_pswidth';
function gnome_paper_psheight(paper:PGnomePaper):gdouble;cdecl;external libgnomedll name 'gnome_paper_psheight';
function gnome_paper_lmargin(paper:PGnomePaper):gdouble;cdecl;external libgnomedll name 'gnome_paper_lmargin';
function gnome_paper_tmargin(paper:PGnomePaper):gdouble;cdecl;external libgnomedll name 'gnome_paper_tmargin';
function gnome_paper_rmargin(paper:PGnomePaper):gdouble;cdecl;external libgnomedll name 'gnome_paper_rmargin';
function gnome_paper_bmargin(paper:PGnomePaper):gdouble;cdecl;external libgnomedll name 'gnome_paper_bmargin';
function gnome_paper_name_default:Pgchar;cdecl;external libgnomedll name 'gnome_paper_name_default';
function gnome_unit_name_list:PGList;cdecl;external libgnomedll name 'gnome_unit_name_list';
function gnome_unit_with_name(unitname:Pgchar):PGnomeUnit;cdecl;external libgnomedll name 'gnome_unit_with_name';
function gnome_paper_convert(psvalue:double; aunit:PGnomeUnit):gdouble;cdecl;external libgnomedll name 'gnome_paper_convert';
function gnome_paper_convert_to_points(othervalue:double; aunit:PGnomeUnit):double;cdecl;external libgnomedll name 'gnome_paper_convert_to_points';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
