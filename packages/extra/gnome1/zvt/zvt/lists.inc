{$IfDef read_interface}

type
   Pvt_listnode = ^Tvt_listnode;
   Tvt_listnode = record
        next : Pvt_listnode;
        prev : Pvt_listnode;
     end;

   Pvt_list = ^Tvt_list;
   Tvt_list = record
        head : Pvt_listnode;
        tail : Pvt_listnode;
        tailpred : Pvt_listnode;
     end;

procedure vt_list_new(List:Pvt_list);cdecl;external libzvtdll name 'vt_list_new';
function vt_list_addhead(list:Pvt_list; node:Pvt_listnode):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_addhead';
function vt_list_addtail(list:Pvt_list; node:Pvt_listnode):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_addtail';
function vt_list_empty(list:Pvt_list):longint;cdecl;external libzvtdll name 'vt_list_empty';
function vt_list_remhead(list:Pvt_list):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_remhead';
function vt_list_remtail(list:Pvt_list):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_remtail';
function vt_list_remove(node:Pvt_listnode):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_remove';
function vt_list_insert(list:Pvt_list; p:Pvt_listnode; node:Pvt_listnode):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_insert';
function vt_list_index(list:Pvt_list; index:longint):Pvt_listnode;cdecl;external libzvtdll name 'vt_list_index';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
