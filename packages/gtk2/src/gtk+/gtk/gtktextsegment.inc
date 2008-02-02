// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTextLineSegment = ^TGtkTextLineSegment;
   PGtkTextLineSegmentClass = ^TGtkTextLineSegmentClass;

{
   Segments: each line is divided into one or more segments, where each
   segment is one of several things, such as a group of characters, a
   tag toggle, a mark, or an embedded widget.  Each segment starts with
   a standard header followed by a body that varies from type to type.
  }
{ This header has the segment type, and two specific segments
   (character and toggle segments)  }
{ Information a BTree stores about a tag.  }
{ highest-level node containing the tag  }
{ total toggles of this tag below tag_root  }
   PGtkTextTagInfo = ^TGtkTextTagInfo;
   TGtkTextTagInfo = record
        tag : PGtkTextTag;
        tag_root : PGtkTextBTreeNode;
        toggle_count : gint;
     end;

{ Body of a segment that toggles a tag on or off  }
{ Tag that starts or ends here.  }
{ TRUE means this toggle has been
                                        accounted for in node toggle
                                        counts; FALSE means it hasn't, yet.  }
   PGtkTextToggleBody = ^TGtkTextToggleBody;
   TGtkTextToggleBody = record
        info : PGtkTextTagInfo;
        inNodeCounts : gboolean;
     end;

{
   The data structure below defines line segments.
  }
{ Pointer to record describing
                                                   segment's type.  }
{ Next in list of segments for this
                                              line, or NULL for theEnd of list.  }
{ # of chars of index space occupied  }
{ Size of this segment (# of bytes
                                           of index space it occupies).  }
{ Characters that make up character
                                           info.  Actual length varies to
                                           hold as many characters as needed. }
{ Information about tag toggle.  }
{ Information about mark.  }
{ Child pixbuf  }
{ Child widget  }
   TGtkTextLineSegment = record
        _type : PGtkTextLineSegmentClass;
        next : PGtkTextLineSegment;
        char_count : longint;
        byte_count : longint;
        body : record
            case longint of
               0 : ( chars : array[0..3] of char );
               1 : ( toggle : TGtkTextToggleBody );
               2 : ( mark : TGtkTextMarkBody );
               3 : ( pixbuf : TGtkTextPixbuf );
               4 : ( child : TGtkTextChildBody );
            end;
     end;


{ Class struct for segments  }
{ Split seg at index, returning list of two new segments, and freeing seg  }

   PGtkTextSegSplitFunc = ^TGtkTextSegSplitFunc;
   TGtkTextSegSplitFunc = TGtkTextLineSegment;
{ Delete seg which is contained in line; if tree_gone, the tree is being
   freed in its entirety, which may matter for some reason (?)
   Return TRUE if the segment is not deleteable, e.g. a mark.
  }

   TGtkTextSegDeleteFunc = function (seg:PGtkTextLineSegment; line:PGtkTextLine; tree_gone:gboolean):gboolean; cdecl;
{ Called after segment structure of line changes, so segments can
   cleanup (e.g. merge with adjacent segments). Returns a segment list
   to replace the original segment list with. The line argument is
   the current line.
  }

   PGtkTextSegCleanupFunc = ^TGtkTextSegCleanupFunc;
   TGtkTextSegCleanupFunc = TGtkTextLineSegment;
{ Called when a segment moves from one line to another. CleanupFunc is also
   called in that case, so many segments just use CleanupFunc, I'm not sure
   what's up with that (this function may not be needed...)
  }

   TGtkTextSegLineChangeFunc = procedure (seg:PGtkTextLineSegment; line:PGtkTextLine); cdecl;
{ Called to do debug checks on the segment.  }

   TGtkTextSegCheckFunc = procedure (seg:PGtkTextLineSegment; line:PGtkTextLine); cdecl;
{ Name of this kind of segment.  }
{ If a segment has zero size (e.g. a
                                           mark or tag toggle), does it
                                           attach to character to its left
                                           or right?  1 means left, 0 means
                                           right.  }
{ Procedure to split large segment
                                           into two smaller ones.  }
{ Procedure to call to delete
                                           segment.  }
{ After any change to a line, this
                                          procedure is invoked for all
                                          segments left in the line to
                                          perform any cleanup they wish
                                          (e.g. joining neighboring
                                          segments).  }
{ Invoked when a segment is about
     to be moved from its current line
     to an earlier line because of
     a deletion.  The line is that
     for the segment's old line.
     CleanupFunc will be invoked after
     the deletion is finished.  }
{ Called during consistency checks
                                          to check internal consistency of
                                          segment.  }
   TGtkTextLineSegmentClass = record
        name : Pchar;
        leftGravity : gboolean;
        splitFunc : TGtkTextSegSplitFunc;
        deleteFunc : TGtkTextSegDeleteFunc;
        cleanupFunc : TGtkTextSegCleanupFunc;
        lineChangeFunc : TGtkTextSegLineChangeFunc;
        checkFunc : TGtkTextSegCheckFunc;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function gtk_text_line_segment_split(iter:PGtkTextIter):PGtkTextLineSegment; cdecl; external gtklib;
function _gtk_char_segment_new(text:Pgchar; len:guint):PGtkTextLineSegment; cdecl; external gtklib;

function _gtk_char_segment_new_from_two_strings(text1:Pgchar; len1:guint; text2:Pgchar; len2:guint):PGtkTextLineSegment; cdecl; external gtklib;
function _gtk_toggle_segment_new(info:PGtkTextTagInfo; StateOn:gboolean):PGtkTextLineSegment; cdecl; external gtklib;
{$ENDIF read_interface_rest}

