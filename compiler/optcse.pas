{
    Common subexpression elimination on base blocks

    Copyright (c) 2005 by Florian Klaempfl

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit optcse;

{$i fpcdefs.inc}

  interface

    procedure docse(rootnode : tnode);

  implementation

    procedure docse(rootnode : tnode);
      begin
        { create a linear list of nodes }

        { create hash values }

        { sort by hash values, taking care of nf_csebarrier and keeping the
          original order of the nodes }

        { compare nodes with equal hash values }

        { search barrier }
        for i:=0 to nodelist.length-1 do
          begin
            { and then search backward so we get always the largest equal trees }
            j:=i+1;
            { collect equal nodes }
            while (j<=nodelist.length-1) and
              nodelist[i].docompare(nodelist[j]) do
              inc(j);
            dec(j);
            if j>i then
              begin
                { cse found }

                { create temp. location }

                { replace first node by
                  - temp. creation
                  - expression calculation
                  - assignment of expression to temp. }
                tempnode:=ctempcreatenode.create(nodelist[i].resultdef,nodelist[i].resultdef.size,tt_persistent,
                  nodelist[i].resultdef.is_intregable or nodelist[i].resultdef.is_fpuregable);
                addstatement(createstatement,tempnode);
                addstatement(createstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                      caddrnode.create_internal(para.left)));
                    para.left := ctypeconvnode.create_internal(cderefnode.create(ctemprefnode.create(tempnode)),para.left.resultdef);
                    addstatement(deletestatement,ctempdeletenode.create(tempnode));

                { replace next nodes by loading the temp. reference }

                { replace last node by loading the temp. reference and
                  delete the temp. }
              end;
          end;
      end;

end.
