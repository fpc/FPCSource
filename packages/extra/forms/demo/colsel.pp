program colsel;

uses xforms;

begin
   fl_initialize(@argc, argv, 'FormDemo', nil, 0);
   fl_show_colormap(0);
end.
{
  $Log$
  Revision 1.3  2003-10-27 15:48:13  peter
    * renamed forms unit to xforms to prevent conflict with Forms
      from the LCL

}
