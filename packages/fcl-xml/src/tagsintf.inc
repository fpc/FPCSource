{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  THTML_a = class (THTMLAttrsElement)
  private
    function Getshape: THTMLshape;
    procedure Setshape(const AValue: THTMLshape);
  public
    constructor create (AOwner: TDOMDocument); override;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
    property charset : DOMString index atcharset read GetAttribute write SetAttribute;
    property coords : DOMString index atcoords read GetAttribute write SetAttribute;
    property href : DOMString index athref read GetAttribute write SetAttribute;
    property hreflang : DOMString index athreflang read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
    property rel : DOMString index atrel read GetAttribute write SetAttribute;
    property rev : DOMString index atrev read GetAttribute write SetAttribute;
    property shape : THTMLshape read Getshape write Setshape;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
    property thetype : DOMString index attype read GetAttribute write SetAttribute;
  end;

  THTML_abbr = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_acronym = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_address = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_applet = class (THTMLCoreattrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property alt : DOMString index atalt read GetAttribute write SetAttribute;
    property archive : DOMString index atarchive read GetAttribute write SetAttribute;
    property codebase : DOMString index atcodebase read GetAttribute write SetAttribute;
    property height : DOMString index atheight read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_area = class (THTMLAttrsElement)
  private
    function Getnohref: boolean;
    procedure Setnohref(const AValue: boolean);
    function Getshape: THTMLshape;
    procedure Setshape(const AValue: THTMLshape);
  public
    constructor create (AOwner: TDOMDocument); override;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
    property alt : DOMString index atalt read GetAttribute write SetAttribute;
    property coords : DOMString index atcoords read GetAttribute write SetAttribute;
    property href : DOMString index athref read GetAttribute write SetAttribute;
    property nohref : boolean read Getnohref write Setnohref;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
    property shape : THTMLshape read Getshape write Setshape;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
  end;

  THTML_b = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_base = class (THTMLCustomElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property href : DOMString index athref read GetAttribute write SetAttribute;
  end;

  THTML_basefont = class (THTMLIDElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_bdo = class (THTMLCores18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_big = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_blockquote = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property cite : DOMString index atcite read GetAttribute write SetAttribute;
  end;

  THTML_body = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property onload : DOMString index atonload read GetAttribute write SetAttribute;
    property onunload : DOMString index atonunload read GetAttribute write SetAttribute;
  end;

  THTML_br = class (THTMLCoreattrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_button = class (THTMLAttrsElement)
  private
    function Getdisabled: boolean;
    procedure Setdisabled(const AValue: boolean);
    function Gettype: THTMLbuttontype;
    procedure Settype(const AValue: THTMLbuttontype);
  public
    constructor create (AOwner: TDOMDocument); override;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
    property disabled : boolean read Getdisabled write Setdisabled;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
    property thetype : THTMLbuttontype read Gettype write Settype;
    property value : DOMString index atvalue read GetAttribute write SetAttribute;
  end;

  THTML_caption = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_center = class (THTMLCustomElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_cite = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_code = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_col = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property span : DOMString index atspan read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_colgroup = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property span : DOMString index atspan read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_dd = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_del = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property cite : DOMString index atcite read GetAttribute write SetAttribute;
    property datetime : DOMString index atdatetime read GetAttribute write SetAttribute;
  end;

  THTML_dfn = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_dir = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_div = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_dl = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_dt = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_em = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_fieldset = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_font = class (THTMLCores18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_form = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property acceptcharset : DOMString index atacceptcharset read GetAttribute write SetAttribute;
    property action : DOMString index ataction read GetAttribute write SetAttribute;
    property enctype : DOMString index atenctype read GetAttribute write SetAttribute;
    property method : DOMString index atmethod read GetAttribute write SetAttribute;
    property onreset : DOMString index atonreset read GetAttribute write SetAttribute;
    property onsubmit : DOMString index atonsubmit read GetAttribute write SetAttribute;
  end;

  THTML_frame = class (THTMLCoreattrsElement)
  private
    function Getnoresize: boolean;
    procedure Setnoresize(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property frameborder : DOMString index atframeborder read GetAttribute write SetAttribute;
    property longdesc : DOMString index atlongdesc read GetAttribute write SetAttribute;
    property marginheight : DOMString index atmarginheight read GetAttribute write SetAttribute;
    property marginwidth : DOMString index atmarginwidth read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property noresize : boolean read Getnoresize write Setnoresize;
    property scrolling : DOMString index atscrolling read GetAttribute write SetAttribute;
    property src : DOMString index atsrc read GetAttribute write SetAttribute;
  end;

  THTML_frameset = class (THTMLCoreattrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property cols : DOMString index atcols read GetAttribute write SetAttribute;
    property onload : DOMString index atonload read GetAttribute write SetAttribute;
    property onunload : DOMString index atonunload read GetAttribute write SetAttribute;
    property rows : DOMString index atrows read GetAttribute write SetAttribute;
  end;

  THTML_h1 = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_h2 = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_h3 = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_h4 = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_h5 = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_h6 = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_head = class (THTMLs18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property profile : DOMString index atprofile read GetAttribute write SetAttribute;
  end;

  THTML_hr = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_html = class (THTMLs18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_i = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_iframe = class (THTMLCoreattrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property frameborder : DOMString index atframeborder read GetAttribute write SetAttribute;
    property height : DOMString index atheight read GetAttribute write SetAttribute;
    property longdesc : DOMString index atlongdesc read GetAttribute write SetAttribute;
    property marginheight : DOMString index atmarginheight read GetAttribute write SetAttribute;
    property marginwidth : DOMString index atmarginwidth read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property scrolling : DOMString index atscrolling read GetAttribute write SetAttribute;
    property src : DOMString index atsrc read GetAttribute write SetAttribute;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_img = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property alt : DOMString index atalt read GetAttribute write SetAttribute;
    property height : DOMString index atheight read GetAttribute write SetAttribute;
    property ismap : DOMString index atismap read GetAttribute write SetAttribute;
    property longdesc : DOMString index atlongdesc read GetAttribute write SetAttribute;
    property src : DOMString index atsrc read GetAttribute write SetAttribute;
    property usemap : DOMString index atusemap read GetAttribute write SetAttribute;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_input = class (THTMLAttrsElement)
  private
    function Getchecked: boolean;
    procedure Setchecked(const AValue: boolean);
    function Getdisabled: boolean;
    procedure Setdisabled(const AValue: boolean);
    function Getreadonly: boolean;
    procedure Setreadonly(const AValue: boolean);
    function Gettype: THTMLinputtype;
    procedure Settype(const AValue: THTMLinputtype);
  public
    constructor create (AOwner: TDOMDocument); override;
    property accept : DOMString index ataccept read GetAttribute write SetAttribute;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
    property alt : DOMString index atalt read GetAttribute write SetAttribute;
    property checked : boolean read Getchecked write Setchecked;
    property disabled : boolean read Getdisabled write Setdisabled;
    property maxlength : DOMString index atmaxlength read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onchange : DOMString index atonchange read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
    property onselect : DOMString index atonselect read GetAttribute write SetAttribute;
    property readonly : boolean read Getreadonly write Setreadonly;
    property size : DOMString index atsize read GetAttribute write SetAttribute;
    property src : DOMString index atsrc read GetAttribute write SetAttribute;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
    property thetype : THTMLinputtype read Gettype write Settype;
    property usemap : DOMString index atusemap read GetAttribute write SetAttribute;
    property value : DOMString index atvalue read GetAttribute write SetAttribute;
  end;

  THTML_ins = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property cite : DOMString index atcite read GetAttribute write SetAttribute;
    property datetime : DOMString index atdatetime read GetAttribute write SetAttribute;
  end;

  THTML_isindex = class (THTMLCores18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_kbd = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_label = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
    property forinput : DOMString index atfor read GetAttribute write SetAttribute;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
  end;

  THTML_legend = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
  end;

  THTML_li = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_link = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property charset : DOMString index atcharset read GetAttribute write SetAttribute;
    property href : DOMString index athref read GetAttribute write SetAttribute;
    property hreflang : DOMString index athreflang read GetAttribute write SetAttribute;
    property media : DOMString index atmedia read GetAttribute write SetAttribute;
    property rel : DOMString index atrel read GetAttribute write SetAttribute;
    property rev : DOMString index atrev read GetAttribute write SetAttribute;
    property thetype : DOMString index attype read GetAttribute write SetAttribute;
  end;

  THTML_map = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property name : DOMString index atname read GetAttribute write SetAttribute;
  end;

  THTML_menu = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_meta = class (THTMLs18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property content : DOMString index atcontent read GetAttribute write SetAttribute;
    property httpequiv : DOMString index athttpequiv read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property scheme : DOMString index atscheme read GetAttribute write SetAttribute;
  end;

  THTML_noframes = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_noscript = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_object = class (THTMLAttrsElement)
  private
    function Getdeclare: boolean;
    procedure Setdeclare(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property archive : DOMString index atarchive read GetAttribute write SetAttribute;
    property classid : DOMString index atclassid read GetAttribute write SetAttribute;
    property codebase : DOMString index atcodebase read GetAttribute write SetAttribute;
    property codetype : DOMString index atcodetype read GetAttribute write SetAttribute;
    property data : DOMString index atdata read GetAttribute write SetAttribute;
    property declare : boolean read Getdeclare write Setdeclare;
    property height : DOMString index atheight read GetAttribute write SetAttribute;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property standby : DOMString index atstandby read GetAttribute write SetAttribute;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
    property thetype : DOMString index attype read GetAttribute write SetAttribute;
    property usemap : DOMString index atusemap read GetAttribute write SetAttribute;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_ol = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_optgroup = class (THTMLAttrsElement)
  private
    function Getdisabled: boolean;
    procedure Setdisabled(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property disabled : boolean read Getdisabled write Setdisabled;
    property thelabel : DOMString index atlabel read GetAttribute write SetAttribute;
  end;

  THTML_option = class (THTMLAttrsElement)
  private
    function Getdisabled: boolean;
    procedure Setdisabled(const AValue: boolean);
    function Getselected: boolean;
    procedure Setselected(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property disabled : boolean read Getdisabled write Setdisabled;
    property thelabel : DOMString index atlabel read GetAttribute write SetAttribute;
    property selected : boolean read Getselected write Setselected;
    property value : DOMString index atvalue read GetAttribute write SetAttribute;
  end;

  THTML_p = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_param = class (THTMLIDElement)
  private
    function Getvaluetype: THTMLvaluetype;
    procedure Setvaluetype(const AValue: THTMLvaluetype);
  public
    constructor create (AOwner: TDOMDocument); override;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property thetype : DOMString index attype read GetAttribute write SetAttribute;
    property value : DOMString index atvalue read GetAttribute write SetAttribute;
    property valuetype : THTMLvaluetype read Getvaluetype write Setvaluetype;
  end;

  THTML_pre = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_q = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property cite : DOMString index atcite read GetAttribute write SetAttribute;
  end;

  THTML_s = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_samp = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_script = class (THTMLCustomElement)
  private
    function Getdefer: boolean;
    procedure Setdefer(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property charset : DOMString index atcharset read GetAttribute write SetAttribute;
    property defer : boolean read Getdefer write Setdefer;
    property src : DOMString index atsrc read GetAttribute write SetAttribute;
    property thetype : DOMString index attype read GetAttribute write SetAttribute;
  end;

  THTML_select = class (THTMLAttrsElement)
  private
    function Getdisabled: boolean;
    procedure Setdisabled(const AValue: boolean);
    function Getmultiple: boolean;
    procedure Setmultiple(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property disabled : boolean read Getdisabled write Setdisabled;
    property multiple : boolean read Getmultiple write Setmultiple;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onchange : DOMString index atonchange read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
    property size : DOMString index atsize read GetAttribute write SetAttribute;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
  end;

  THTML_small = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_span = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_strike = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_strong = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_style = class (THTMLs18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
    property media : DOMString index atmedia read GetAttribute write SetAttribute;
    property thetype : DOMString index attype read GetAttribute write SetAttribute;
  end;

  THTML_sub = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_sup = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_table = class (THTMLAttrsElement)
  private
    function Getframe: THTMLframe;
    procedure Setframe(const AValue: THTMLframe);
    function Getrules: THTMLrules;
    procedure Setrules(const AValue: THTMLrules);
  public
    constructor create (AOwner: TDOMDocument); override;
    property border : DOMString index atborder read GetAttribute write SetAttribute;
    property cellpadding : DOMString index atcellpadding read GetAttribute write SetAttribute;
    property cellspacing : DOMString index atcellspacing read GetAttribute write SetAttribute;
    property frame : THTMLframe read Getframe write Setframe;
    property rules : THTMLrules read Getrules write Setrules;
    property summary : DOMString index atsummary read GetAttribute write SetAttribute;
    property width : DOMString index atwidth read GetAttribute write SetAttribute;
  end;

  THTML_tbody = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
  end;

  THTML_td = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property abbr : DOMString index atabbr read GetAttribute write SetAttribute;
    property align : THTMLalign read Getalign write Setalign;
    property axis : DOMString index ataxis read GetAttribute write SetAttribute;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property colspan : DOMString index atcolspan read GetAttribute write SetAttribute;
    property headers : DOMString index atheaders read GetAttribute write SetAttribute;
    property rowspan : DOMString index atrowspan read GetAttribute write SetAttribute;
    property scope : DOMString index atscope read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
  end;

  THTML_textarea = class (THTMLAttrsElement)
  private
    function Getdisabled: boolean;
    procedure Setdisabled(const AValue: boolean);
    function Getreadonly: boolean;
    procedure Setreadonly(const AValue: boolean);
  public
    constructor create (AOwner: TDOMDocument); override;
    property accesskey : DOMString index ataccesskey read GetAttribute write SetAttribute;
    property cols : DOMString index atcols read GetAttribute write SetAttribute;
    property disabled : boolean read Getdisabled write Setdisabled;
    property name : DOMString index atname read GetAttribute write SetAttribute;
    property onblur : DOMString index atonblur read GetAttribute write SetAttribute;
    property onchange : DOMString index atonchange read GetAttribute write SetAttribute;
    property onfocus : DOMString index atonfocus read GetAttribute write SetAttribute;
    property onselect : DOMString index atonselect read GetAttribute write SetAttribute;
    property readonly : boolean read Getreadonly write Setreadonly;
    property rows : DOMString index atrows read GetAttribute write SetAttribute;
    property tabindex : DOMString index attabindex read GetAttribute write SetAttribute;
  end;

  THTML_tfoot = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
  end;

  THTML_th = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property abbr : DOMString index atabbr read GetAttribute write SetAttribute;
    property align : THTMLalign read Getalign write Setalign;
    property axis : DOMString index ataxis read GetAttribute write SetAttribute;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property colspan : DOMString index atcolspan read GetAttribute write SetAttribute;
    property headers : DOMString index atheaders read GetAttribute write SetAttribute;
    property rowspan : DOMString index atrowspan read GetAttribute write SetAttribute;
    property scope : DOMString index atscope read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
  end;

  THTML_thead = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
  end;

  THTML_title = class (THTMLs18nElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_tr = class (THTMLAttrsElement)
  private
    function Getalign: THTMLalign;
    procedure Setalign(const AValue: THTMLalign);
    function Getvalign: THTMLvalign;
    procedure Setvalign(const AValue: THTMLvalign);
  public
    constructor create (AOwner: TDOMDocument); override;
    property align : THTMLalign read Getalign write Setalign;
    property char : DOMString index atchar read GetAttribute write SetAttribute;
    property charoff : DOMString index atcharoff read GetAttribute write SetAttribute;
    property valign : THTMLvalign read Getvalign write Setvalign;
  end;

  THTML_tt = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_u = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_ul = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

  THTML_var = class (THTMLAttrsElement)
  public
    constructor create (AOwner: TDOMDocument); override;
  end;

