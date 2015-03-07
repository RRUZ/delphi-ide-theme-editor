//**************************************************************************************************
//
// Unit Colorizer.VirtualTrees
// unit Colorizer.VirtualTrees for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.VirtualTrees.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.VirtualTrees;

interface

uses
  Windows,
  Types,
  ImgList,
  Classes,
  Graphics;

type
  TVirtualNodeState = (
    vsInitialized,       // Set after the node has been initialized.
    vsChecking,          // Node's check state is changing, avoid propagation.
    vsCutOrCopy,         // Node is selected as cut or copy and paste source.
    vsDisabled,          // Set if node is disabled.
    vsDeleting,          // Set when the node is about to be freed.
    vsExpanded,          // Set if the node is expanded.
    vsHasChildren,       // Indicates the presence of child nodes without actually setting them.
    vsVisible,           // Indicate whether the node is visible or not (independant of the expand states of its parents).
    vsSelected,          // Set if the node is in the current selection.
    vsOnFreeNodeCallRequired,   // Set if user data has been set which requires OnFreeNode.
    vsAllChildrenHidden, // Set if vsHasChildren is set and no child node has the vsVisible flag set.
    vsClearing,          // A node's children are being deleted. Don't register structure change event.
    vsMultiline,         // Node text is wrapped at the cell boundaries instead of being shorted.
    vsHeightMeasured,    // Node height has been determined and does not need a recalculation.
    vsToggling,          // Set when a node is expanded/collapsed to prevent recursive calls.
    vsFiltered           // Indicates that the node should not be painted (without effecting its children).
  );
  TVirtualNodeStates = set of TVirtualNodeState;

  TCheckState = (
    csUncheckedNormal,  // unchecked and not pressed
    csUncheckedPressed, // unchecked and pressed
    csCheckedNormal,    // checked and not pressed
    csCheckedPressed,   // checked and pressed
    csMixedNormal,      // 3-state check box and not pressed
    csMixedPressed      // 3-state check box and pressed
  );

  TCheckType = (
    ctNone,
    ctTriStateCheckBox,
    ctCheckBox,
    ctRadioButton,
    ctButton
  );

  TVTInternalPaintOption = (
    poBackground,       // draw background image if there is any and it is enabled
    poColumnColor,      // erase node's background with the column's color
    poDrawFocusRect,    // draw focus rectangle around the focused node
    poDrawSelection,    // draw selected nodes with the normal selection color
    poDrawDropMark,     // draw drop mark if a node is currently the drop target
    poGridLines,        // draw grid lines if enabled
    poMainOnly,         // draw only the main column
    poSelectedOnly,     // draw only selected nodes
    poUnbuffered        // draw directly onto the target canvas; especially useful when printing
  );
  TVTInternalPaintOptions = set of TVTInternalPaintOption;

  TColumnIndex = type Integer;
  TColumnPosition = type Cardinal;


  TVTImageInfoIndex = (
    iiNormal,
    iiState,
    iiCheck,
    iiOverlay
  );

  TVTImageInfo = record
    Index: Integer;           // Index in the associated image list.
    XPos,                     // Horizontal position in the current target canvas.
    YPos: Integer;            // Vertical position in the current target canvas.
    Ghosted: Boolean;         // Flag to indicate that the image must be drawn slightly lighter.
    Images: TCustomImageList; // The image list to be used for painting.
  end;

  TVTPaintInfo = record
    Canvas: TCanvas;              // the canvas to paint on
    PaintOptions: TVTInternalPaintOptions;  // a copy of the paint options passed to PaintTree
    Node: Pointer;           // the node to paint
    Column: TColumnIndex;         // the node's column index to paint
    Position: TColumnPosition;    // the column position of the node
    CellRect,                     // the node cell
    ContentRect: TRect;           // the area of the cell used for the node's content
    NodeWidth: Integer;           // the actual node width
    Alignment: TAlignment;        // how to align within the node rectangle
    CaptionAlignment: TAlignment; // how to align text within the caption rectangle
    BidiMode: TBidiMode;          // directionality to be used for painting
    BrushOrigin: TPoint;          // the alignment for the brush used to draw dotted lines
    ImageInfo: array[TVTImageInfoIndex] of TVTImageInfo; // info about each possible node image
  end;

  TVirtualNode = packed record
    Index,                   // index of node with regard to its parent
    ChildCount: Cardinal;    // number of child nodes
    NodeHeight: Word;        // height in pixels
    States: TVirtualNodeStates; // states describing various properties of the node (expanded, initialized etc.)
    Align: Byte;             // line/button alignment
    CheckState: TCheckState; // indicates the current check state (e.g. checked, pressed etc.)
    CheckType: TCheckType;   // indicates which check type shall be used for this node
    Dummy: Byte;             // dummy value to fill DWORD boundary
    TotalCount,              // sum of this node, all of its child nodes and their child nodes etc.
    TotalHeight: Cardinal;   // height in pixels this node covers on screen including the height of all of its
                             // children
    // Note: Some copy routines require that all pointers (as well as the data area) in a node are
    //       located at the end of the node! Hence if you want to add new member fields (except pointers to internal
    //       data) then put them before field Parent.
    Parent,                  // reference to the node's parent (for the root this contains the treeview)
    PrevSibling,             // link to the node's previous sibling or nil if it is the first node
    NextSibling,             // link to the node's next sibling or nil if it is the last node
    FirstChild,              // link to the node's first child...
    LastChild: Pointer; // link to the node's last child...
    Data: record end;        // this is a placeholder, each node gets extra data determined by NodeDataSize
  end;

implementation

end.
