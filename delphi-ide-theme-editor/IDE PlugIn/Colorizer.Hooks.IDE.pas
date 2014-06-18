//**************************************************************************************************
//
// Unit Colorizer.Hooks.IDE
// unit Colorizer.Hooks.IDE for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Hooks.IDE.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Hooks.IDE;

interface
{$I ..\Common\Jedi.inc}

var
  DrawNamePair          : Boolean     = False;
  {$IFDEF DELPHIXE5_UP}
  {$ELSE}
  DrawItemIDEInsight    : Boolean     = False;
  DrawItemIDEInsightSel : Boolean     = False;
  {$ENDIF}

const
{$IFDEF DELPHIXE}  sVclIDEModule =  'vclide150.bpl';{$ENDIF}
{$IFDEF DELPHIXE2} sVclIDEModule =  'vclide160.bpl';{$ENDIF}
{$IFDEF DELPHIXE3} sVclIDEModule =  'vclide170.bpl';{$ENDIF}
{$IFDEF DELPHIXE4} sVclIDEModule =  'vclide180.bpl';{$ENDIF}
{$IFDEF DELPHIXE5} sVclIDEModule =  'vclide190.bpl';{$ENDIF}
{$IFDEF DELPHIXE6} sVclIDEModule =  'vclide200.bpl';{$ENDIF}

{$IFDEF DELPHIXE}  sCoreIDEModule =  'coreide150.bpl';{$ENDIF}
{$IFDEF DELPHIXE2} sCoreIDEModule =  'coreide160.bpl';{$ENDIF}
{$IFDEF DELPHIXE3} sCoreIDEModule =  'coreide170.bpl';{$ENDIF}
{$IFDEF DELPHIXE4} sCoreIDEModule =  'coreide180.bpl';{$ENDIF}
{$IFDEF DELPHIXE5} sCoreIDEModule =  'coreide190.bpl';{$ENDIF}
{$IFDEF DELPHIXE6} sCoreIDEModule =  'coreide200.bpl';{$ENDIF}

{$IFDEF DELPHIXE6} sModernThemeModule =  'ModernTheme200.bpl';{$ENDIF}


implementation

uses
  Colorizer.VirtualTrees,
  Colorizer.Wrappers,
  Colorizer.Utils,
  Generics.Collections,
  Classes,
  Windows,
  DDetours,
  uMisc,
  Forms,
  Controls,
  PngImage,
  GraphUtil,
  CaptionedDockTree,
  Graphics;

type
 TCustomControlClass     = class(TCustomControl);
 TDockCaptionDrawerClass = class(TDockCaptionDrawer);

var
   ListControlWrappers  : TObjectDictionary<TCustomControl, TRttiWrapper>;

  Trampoline_ProjectTree2PaintText         : procedure (Self : TObject; Sender: TObject{TBaseVirtualTree}; const TargetCanvas: TCanvas; Node: {PVirtualNode}Pointer; Column: Integer{TColumnIndex}; TextType: Byte {TVSTTextType})=nil;
  {$IFDEF DELPHIXE6_UP}
  Trampoline_TModernTheme_SetHotSingleColor : procedure(Self : TObject;Color : TColor) = nil;
  {$ENDIF}
  //002B7ADC 10611 2EDD __fastcall Msglines::TCompilerMsgLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
  Trampoline_CompilerMsgLine_Draw          : procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean) = nil;
  //002B7360 10632 2F54 __fastcall Msglines::TTitleLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
  Trampoline_TitleLine_Draw                : procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean) = nil;
  Trampoline_TFileFindLine_Draw            : procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean) = nil;
  //002B7E78 10606 2F00 __fastcall Msglines::TFileFindLine::InternalCalcDraw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool, bool)
  Trampoline_TFileFindLine_InternalCalcDraw: procedure (Self : TObject;Canvas : TCanvas; Rect : TRect; Flag, Flag2 : Boolean) = nil;

  Trampoline_TPropertyInspector_DrawNamePair:procedure(Self : TObject;Canvas : TCanvas; Rect : TRect; Flag, Flag2 : boolean; const Str1, Str2 : String) = nil;

  //  001D755C 6558 374D __fastcall Popupsrchfrm::TPopupSearchForm::DrawTreeDrawNode(Idevirtualtrees::TBaseVirtualTree *, Idevirtualtrees::TVTPaintInfo&)
  //  001D813C 6536 3763 __fastcall Popupsrchfrm::TPopupSearchForm::PaintCategoryNode(Idevirtualtrees::TVirtualNode *, Vcl::Graphics::TCanvas *, System::Types::TRect&, Ideinsightmgr::TIDEInsightCategory *, bool)
  //  001D8268 6535 3764 __fastcall Popupsrchfrm::TPopupSearchForm::PaintItemNode(Idevirtualtrees::TVirtualNode *, Vcl::Graphics::TCanvas *, System::Types::TRect&, Ideinsightmgr::TIDEInsightItem *, bool)

  {$IFDEF DELPHIXE5_UP}
  {$ELSE}
  Trampoline_TPopupSearchForm_DrawTreeDrawNode : procedure (Self: TObject;BaseVirtualTree: TCustomControl;const PaintInfo: TVTPaintInfo) = nil;
  Trampoline_TPopupSearchForm_PaintCategoryNode: procedure (Self: TObject;VirtualNode : TVirtualNode; Canvas : TCanvas; Rect:TRect; Category : TObject; Flag: Boolean) = nil;
  Trampoline_TPopupSearchForm_PaintItemNode    : procedure (Self: TObject;VirtualNode : TVirtualNode; Canvas : TCanvas; Rect:TRect; Item : TObject; Flag: Boolean) = nil;
  {$ENDIF}

  //  0022C1A0 7413 2BF5 __fastcall Ideinsight::TIDEInsightForm::DrawTreeDrawNode(Idevirtualtrees::TBaseVirtualTree *, Idevirtualtrees::TVTPaintInfo&)
  //  0022CE84 7395 2C05 __fastcall Ideinsight::TIDEInsightForm::PaintCategoryNode(Idevirtualtrees::TVirtualNode *, Vcl::Graphics::TCanvas *, System::Types::TRect&, Ideinsightmgr::TIDEInsightCategory *, bool)
  //  0022D020 7393 2C06 __fastcall Ideinsight::TIDEInsightForm::PaintItemNode(Idevirtualtrees::TVirtualNode *, Vcl::Graphics::TCanvas *, System::Types::TRect&, Ideinsightmgr::TIDEInsightItem *, bool)
  //000E6D74 4639 1D48 __fastcall Idevirtualtrees::TBaseVirtualTree::PrepareBitmaps(bool, bool)
  Trampoline_TBaseVirtualTree_PrepareBitmaps : procedure (Self : TCustomControl;NeedButtons, NeedLines: Boolean) = nil;

  //0005B7F4 1682 0BCA __fastcall Idelistbtns::TListButton::Paint()
  Trampoline_TListButton_Paint : procedure (Self : TCustomControl) = nil;
  //000F1DD0 4400 1CD1 __fastcall Idevirtualtrees::TBaseVirtualTree::GetHintWindowClass()
  Trampoline_TBaseVirtual_GetHintWindowClass : function (Self : TCustomControl) : THintWindowClass = nil;
  Trampoline_TExpandableEvalView_FormCreate : procedure(Self: TForm; Sender: TObject);

  Trampoline_Gradientdrawer_GetOutlineColor : function() : TColor = nil;

  {$IFDEF DELPHIXE6_UP}
  Trampoline_ModernDockCaptionDrawer_DrawDockCaption : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;
  {$ENDIF}
  Trampoline_TDockCaptionDrawer_DrawDockCaption      : function (Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest =nil;

procedure CustomProjectTree2PaintText(Self : TObject; Sender: TObject{TBaseVirtualTree}; const TargetCanvas: TCanvas; Node: {PVirtualNode}Pointer; Column: Integer{TColumnIndex}; TextType: Byte {TVSTTextType});
begin
  //TargetCanvas.Font.Color:=clRed;
  Trampoline_ProjectTree2PaintText(Self, Sender, TargetCanvas, Node, Column, TextType);
end;

{$IFDEF DELPHIXE6_UP}
procedure Detour_TModernTheme_SetHotSingleColor(Self : TObject;Color : TColor);
begin
 //AddLog('Detour_TModernTheme_SetHotSingleColor');
 Trampoline_TModernTheme_SetHotSingleColor(Self, clYellow);
end;
{$ENDIF}

procedure Detour_TExpandableEvalView_FormCreate(Self: TForm; Sender: TObject);
begin
  //AddLog('Detour_TExpandableEvalView_FormCreate', 'Foo');
  Trampoline_TExpandableEvalView_FormCreate(Self, Sender);
end;

//Detour for TListButton.Paint
procedure Detour_TListButton_Paint(Self : TCustomControlClass);
var
  ArrowSize, i  : integer;
  LPoint     : TPoint;
  ListButton : TRttiListButton;
  LRect      : TRect;
  LParentForm : TCustomForm;
begin
  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
    Trampoline_TListButton_Paint(Self);
    exit;
  end;

  LParentForm:= GetParentForm(Self);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    Trampoline_TListButton_Paint(Self);
    exit;
  end;

  if not ListControlWrappers.ContainsKey(Self) then
   ListControlWrappers.Add(Self, TRttiListButton.Create(Self));
  ListButton := TRttiListButton(ListControlWrappers.Items[Self]);

  ArrowSize := 3;
  if Self.Width>16 then ArrowSize := 4;
  LPoint    := Point(Self.ClientRect.Left+4, Self.ClientRect.Top+6);

  Self.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.Color;
  Self.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
  Self.Canvas.Rectangle(Self.ClientRect);

  ListButton.LoadValues();
//    AddLog('Detour_TListButton_Paint', Format('Self %p', [@Self]));
//    AddLog('Detour_TListButton_Paint', 'Name '+ ListButton.ListButton.Name);
//    AddLog('Detour_TListButton_Paint', 'MaxListWidth '+ IntToStr(ListButton.MaxListWidth));
//    AddLog('Detour_TListButton_Paint', 'MinListWidth '+ IntToStr(ListButton.MinListWidth));
//    AddLog('Detour_TListButton_Paint', 'ItemIndex    '+ IntToStr(ListButton.ItemIndex));
//    AddLog('Detour_TListButton_Paint', 'ItemCount    '+ IntToStr(ListButton.ItemCount));
//    AddLog('Detour_TListButton_Paint', 'SelectString '+ ListButton.SelectString);
//    AddLog('Detour_TListButton_Paint', ListButton.Items.Text);
//    if  ListButton.PopupPanel<>nil then
//    AddLog('Detour_TListButton_Paint', 'PopupPanel ' + ListButton.PopupPanel.ClassName);
//    if  ListButton.ListBox<>nil then
//    AddLog('Detour_TListButton_Paint', 'ListBox ' + ListButton.ListBox.Items.Text);
//    if  ListButton.Items<>nil then
//    AddLog('Detour_TListButton_Paint', 'Items ' + ListButton.Items.Text);

  if (ListButton.Items.Count>0) or (ListButton.PopupPanel<>nil) then
  begin
    Self.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
    Self.Canvas.Pen.Color  :=TColorizerLocalSettings.ColorMap.FontColor;
    DrawArrow(Self.Canvas, TScrollDirection.sdDown, LPoint, ArrowSize);
  end
  else
  begin
    LPoint    := Point(Self.ClientRect.Left+4, Self.ClientRect.Top+6);
    for i := 0 to 2 do
    begin
     LRect := Rect(LPoint.X, LPoint.Y, LPoint.X+2, LPoint.Y+2);
     Self.Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.FontColor;
     Self.Canvas.FillRect(LRect);
     LPoint.X:=LPoint.X+3;
    end;
  end;
end;

//Detour for TBaseVirtualTree.PrepareBitmaps
procedure Detour_TBaseVirtualTree_PrepareBitmaps(Self : TCustomControl;NeedButtons, NeedLines: Boolean);
const
  LineBitsDotted: array [0..8] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
  PatternBitmap: HBITMAP;
  Bits: Pointer;
  Size: TSize;

  procedure FillBitmap(ABitmap: TBitmap);
  begin
    if Assigned(ABitmap) then
    with ABitmap, Canvas do
    begin
      Width := Size.cx;
      Height := Size.cy;
      Brush.Color := TColorizerLocalSettings.ColorMap.Color;
      FillRect(Rect(0, 0, Width, Height));
    end;
  end;

var
  LRttiBaseVirtualTree : TRttiBaseVirtualTree;
  LParentForm          : TCustomForm;
begin

  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (csDesigning in Self.ComponentState) or (not Assigned(TColorizerLocalSettings.ColorMap)) then
  begin
    Trampoline_TBaseVirtualTree_PrepareBitmaps(Self, NeedButtons, NeedLines);
    exit;
  end;

  LParentForm:= GetParentForm(Self);
  if not (Assigned(LParentForm) and Assigned(TColorizerLocalSettings.HookedWindows) and (TColorizerLocalSettings.HookedWindows.IndexOf(LParentForm.ClassName)>=0)) then
  begin
    Trampoline_TBaseVirtualTree_PrepareBitmaps(Self, NeedButtons, NeedLines);
    exit;
  end;

    if not ListControlWrappers.ContainsKey(Self) then
     ListControlWrappers.Add(Self, TRttiBaseVirtualTree.Create(Self));
    LRttiBaseVirtualTree := TRttiBaseVirtualTree(ListControlWrappers.Items[Self]);

    Size.cx := 9;
    Size.cy := 9;

    if NeedButtons then
    begin
       if Assigned(LRttiBaseVirtualTree.MinusBM) then
       with LRttiBaseVirtualTree.MinusBM, Canvas do
       begin
        FillBitmap(LRttiBaseVirtualTree.MinusBM);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        Rectangle(0, 0, Width, Height);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        MoveTo(2, Width div 2);
        LineTo(Width - 2, Width div 2);
       end;

      if Assigned(LRttiBaseVirtualTree.PlusBM) then
      with LRttiBaseVirtualTree.PlusBM, Canvas do
      begin
        FillBitmap(LRttiBaseVirtualTree.PlusBM);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        Rectangle(0, 0, Width, Height);
        Pen.Color := TColorizerLocalSettings.ColorMap.FontColor;
        MoveTo(2, Width div 2);
        LineTo(Width - 2, Width div 2);
        MoveTo(Width div 2, 2);
        LineTo(Width div 2, Width - 2);
      end;
    end;

    if NeedLines then
    begin
      if LRttiBaseVirtualTree.DottedBrush <> 0 then
        DeleteObject(LRttiBaseVirtualTree.DottedBrush);

      Bits := @LineBitsDotted;
      PatternBitmap := CreateBitmap(8, 8, 1, 1, Bits);
      LRttiBaseVirtualTree.DottedBrush := CreatePatternBrush(PatternBitmap);
      DeleteObject(PatternBitmap);
    end;
end;


{
    002B4B34 10661 2EDB Msglines::TCompilerMsgLine::
    002B7A68 10612 2EDC __fastcall Msglines::TCompilerMsgLine::TCompilerMsgLine(Compintf::TMsgKind, int, const System::UnicodeString, int, int, const System::UnicodeString, const System::UnicodeString, bool, const System::DelphiInterface<Msglinesintf::IMessageGroup>)
    002B7ADC 10611 2EDD __fastcall Msglines::TCompilerMsgLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
    002B7BBC 10610 2EDE __fastcall Msglines::TCompilerMsgLine::GetLineText()

@Msglines@TCompilerMsgLine@
@Msglines@TCompilerMsgLine@$bctr$qqr17Compintf@TMsgKindix20System@UnicodeStringiit3t3ox54System@%DelphiInterface$t26Msglinesintf@IMessageGroup%
@Msglines@TCompilerMsgLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto
@Msglines@TCompilerMsgLine@GetLineText$qqrv

}
procedure Detour_TCompilerMsgLine_Draw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean);
var
 SavedIndex : Integer;
begin
 SavedIndex := SaveDC(Canvas.Handle);
 try
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
   if Canvas.Brush.Color=clWindow then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor
   else
   if Canvas.Brush.Color=clBtnFace then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.DisabledColor
   else
   if Canvas.Brush.Color=clHighlight then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor;

   if Canvas.Font.Color=clWindowText then
    Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.FontColor
   else
   if Canvas.Font.Color=clHighlightText then
    Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.SelectedFontColor;

   if Canvas.Brush.Color=clWhite then //Show hint in white background
   begin
    Canvas.Brush.Color := TColorizerLocalSettings.ColorMap.WindowColor;
    Canvas.Font.Color  := TColorizerLocalSettings.ColorMap.FontColor;
   end;
  end;

  //  Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.MenuColor;
  //  Canvas.Font.Color :=clBlack;
  //AddLog('Detour_TCompilerMsgLine_Draw', Format('Canvas.Brush.Color %s Canvas.Font.Color %s Flag %s', [ColorToString(Canvas.Brush.Color), ColorToString(Canvas.Font.Color), BoolToStr(Flag, True)]));
  //AddLog('Detour_TCompilerMsgLine_Draw',);
  Trampoline_CompilerMsgLine_Draw(Self, Canvas, Rect, Flag);
 finally
  if SavedIndex<>0 then
    RestoreDC(Canvas.Handle, SavedIndex);
 end;
end;


{
    002B7360 10632 2F54 __fastcall Msglines::TTitleLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
    @Msglines@TTitleLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto
}

procedure Detour_TTitleLine_Draw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean);
var
 SavedIndex : Integer;
begin
 SavedIndex := SaveDC(Canvas.Handle);
 try
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
   if Canvas.Brush.Color=clWindow then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor
   else
   if Canvas.Brush.Color=clBtnFace then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.DisabledColor
   else
   if Canvas.Brush.Color=clHighlight then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor;

   if Canvas.Font.Color=clWindowText then
    Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.FontColor
   else
   if Canvas.Font.Color=clHighlightText then
    Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.SelectedFontColor;
  end;

  Trampoline_TitleLine_Draw(Self, Canvas, Rect, Flag);
 finally
  if SavedIndex<>0 then
    RestoreDC(Canvas.Handle, SavedIndex);
 end;
end;

{
    002B7E58 10607 2EFE __fastcall Msglines::TFileFindLine::Draw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool)
    @Msglines@TFileFindLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto
}
procedure Detour_TFileFindLine_Draw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag : Boolean);
var
 SavedIndex : Integer;
begin
 SavedIndex := SaveDC(Canvas.Handle);
 try
{
Detour_TFileFindLine_Draw : Flag False Canvas.Brush.Color clWindow Canvas.Font.Color clWindowText
Detour_TFileFindLine_Draw : Flag False Canvas.Brush.Color clBtnFace Canvas.Font.Color clWindowText
Detour_TFileFindLine_Draw : Flag False Canvas.Brush.Color clHighlight Canvas.Font.Color clHighlightText
}
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
   if Canvas.Brush.Color=clWindow then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor
   else
   if Canvas.Brush.Color=clBtnFace then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.DisabledColor
   else
   if Canvas.Brush.Color=clHighlight then
    Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.SelectedColor;

   if Canvas.Font.Color=clWindowText then
    Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.FontColor
   else
   if Canvas.Font.Color=clHighlightText then
    Canvas.Font.Color :=TColorizerLocalSettings.ColorMap.SelectedFontColor;
  end;

  Trampoline_TFileFindLine_Draw(Self, Canvas, Rect, Flag);
 finally
  if SavedIndex<>0 then
    RestoreDC(Canvas.Handle, SavedIndex);
 end;
end;

{
    002B7E78 10606 2F00 __fastcall Msglines::TFileFindLine::InternalCalcDraw(Vcl::Graphics::TCanvas *, System::Types::TRect&, bool, bool)
    @Msglines@TFileFindLine@InternalCalcDraw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRectoo
}
procedure Detour_TFileFindLine_InternalCalcDraw(Self: TObject;Canvas : TCanvas; Rect : TRect; Flag, Flag2 : Boolean);
var
 OldFontColor : TColor;
begin
 OldFontColor := Canvas.Font.Color;
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled and Assigned(TColorizerLocalSettings.ColorMap) then
  begin
   Canvas.Brush.Color:=TColorizerLocalSettings.ColorMap.WindowColor;
   Canvas.Font.Color:=TColorizerLocalSettings.ColorMap.FontColor;
  end;
  Trampoline_TFileFindLine_InternalCalcDraw(Self, Canvas, Rect, Flag, Flag2);
 Canvas.Font.Color:=OldFontColor;
end;

procedure Detour_TPropertyInspector_DrawNamePair(Self : TObject;Canvas : TCanvas; Rect : TRect; Flag, Flag2 : Boolean; const Str1, Str2 : String);
begin
 //AddLog('Detour_TPropertyInspector_DrawNamePair', 'Hooked');
 DrawNamePair:=True;
 Trampoline_TPropertyInspector_DrawNamePair(Self, Canvas, Rect, Flag, Flag2, Str1, Str2);
end;

{$IFDEF DELPHIXE5_UP}
{$ELSE}
procedure Detour_TPopupSearchForm_DrawTreeDrawNode(Self: TObject;BaseVirtualTree: TCustomControl;const PaintInfo: TVTPaintInfo);
begin
  //AddLog('Detour_TPopupSearchForm_DrawTreeDrawNode', 'Hooked');
  DrawItemIDEInsight:=True;
  DrawItemIDEInsightSel:=False;
  Trampoline_TPopupSearchForm_DrawTreeDrawNode(Self, BaseVirtualTree, PaintInfo);
end;

procedure Detour_TPopupSearchForm_PaintCategoryNode(Self: TObject;VirtualNode : TVirtualNode; Canvas : TCanvas; Rect:TRect; Category : TObject; Flag: Boolean);
begin
  //AddLog('Detour_TPopupSearchForm_PaintCategoryNode', 'Hooked');
  DrawItemIDEInsight:=True;
  DrawItemIDEInsightSel:= vsSelected in VirtualNode.States;
  Trampoline_TPopupSearchForm_PaintCategoryNode(Self, VirtualNode, Canvas, Rect, Category, Flag);
end;

procedure Detour_TPopupSearchForm_PaintItemNode(Self: TObject;VirtualNode : TVirtualNode; Canvas : TCanvas; Rect:TRect; Item : TObject; Flag: Boolean);
begin
  //AddLog('Detour_TPopupSearchForm_PaintItemNode', 'Hooked');
  DrawItemIDEInsight:=True;
  DrawItemIDEInsightSel:= vsSelected in VirtualNode.States;
  Trampoline_TPopupSearchForm_PaintItemNode(Self, VirtualNode, Canvas, Rect, Item, Flag);
end;
{$ENDIF}

function Detour_Gradientdrawer_GetOutlineColor : TColor;
begin
  if Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.Enabled  then
  begin
    Result := TColorizerLocalSettings.ColorMap.FrameTopLeftOuter;
    if TColorizerLocalSettings.Settings.TabIDECustom then
      Result := TryStrToColor(TColorizerLocalSettings.Settings.TabIDEOutLineColor, TColorizerLocalSettings.ColorMap.FrameTopLeftOuter);
    Exit;
  end;
  Exit(Trampoline_Gradientdrawer_GetOutlineColor);
end;

//Hook for the docked IDE windows.
function Detour_TDockCaptionDrawer_DrawDockCaption(Self : TDockCaptionDrawerClass;const Canvas: TCanvas; CaptionRect: TRect; State: TParentFormState): TDockCaptionHitTest;
var
  LColorStart, LColorEnd : TColor;

  procedure DrawIcon;
  var
    FormBitmap: TBitmap;
    DestBitmap: TBitmap;
    ImageSize: Integer;
    X, Y: Integer;
  begin
    if (State.Icon <> nil) and (State.Icon.HandleAllocated) then
    begin
      if Self.DockCaptionOrientation = dcoHorizontal then
      begin
        ImageSize := CaptionRect.Bottom - CaptionRect.Top - 3;
        X := CaptionRect.Left;
        Y := CaptionRect.Top + 2;
      end
      else
      begin
        ImageSize := CaptionRect.Right - CaptionRect.Left - 3;
        X := CaptionRect.Left + 1;
        Y := CaptionRect.Top;
      end;

      FormBitmap := nil;
      DestBitmap := TBitmap.Create;
      try
        FormBitmap := TBitmap.Create;
        DestBitmap.Width :=  ImageSize;
        DestBitmap.Height := ImageSize;
        DestBitmap.Canvas.Brush.Color := clFuchsia;
        DestBitmap.Canvas.FillRect(Rect(0, 0, DestBitmap.Width, DestBitmap.Height));
        FormBitmap.Width := State.Icon.Width;
        FormBitmap.Height := State.Icon.Height;
        FormBitmap.Canvas.Draw(0, 0, State.Icon);
        ScaleImage(FormBitmap, DestBitmap, DestBitmap.Width / FormBitmap.Width);

        DestBitmap.TransparentColor := DestBitmap.Canvas.Pixels[0, DestBitmap.Height - 1];
        DestBitmap.Transparent := True;

        Canvas.Draw(X, Y, DestBitmap);
      finally
        FormBitmap.Free;
        DestBitmap.Free;
      end;

      if Self.DockCaptionOrientation = dcoHorizontal then
        CaptionRect.Left := CaptionRect.Left + 6 + ImageSize
      else
        CaptionRect.Top := CaptionRect.Top + 6 + ImageSize;
    end;
  end;

  function CalcButtonSize(
    const CaptionRect: TRect): Integer;
  const
    cButtonBuffer = 8;
  begin
    if Self.DockCaptionOrientation = dcoHorizontal then
      Result := CaptionRect.Bottom - CaptionRect.Top - cButtonBuffer
    else
      Result := CaptionRect.Right - CaptionRect.Left - cButtonBuffer;
  end;

  function GetCloseRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    CloseSize: Integer;
  begin
    CloseSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - CloseSize - cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - CloseSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - CloseSize) div 2;
      Result.Top := CaptionRect.Top + 2 * cSideBuffer;
    end;
    Result.Right := Result.Left + CloseSize;
    Result.Bottom := Result.Top + CloseSize;
  end;

  function GetPinRect(const CaptionRect: TRect): TRect;
  const
    cSideBuffer = 4;
  var
    PinSize: Integer;
  begin
    PinSize := CalcButtonSize(CaptionRect);
    if Self.DockCaptionOrientation = dcoHorizontal then
    begin
      Result.Left := CaptionRect.Right - 2*PinSize - 2*cSideBuffer;
      Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - PinSize) div 2;
    end
    else
    begin
      Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - PinSize) div 2;
      Result.Top := CaptionRect.Top + 2*cSideBuffer + 2*PinSize;
    end;
    Result.Right := Result.Left + PinSize + 2;
    Result.Bottom := Result.Top + PinSize;
  end;

var
  ShouldDrawClose: Boolean;
  CloseRect, PinRect: TRect;
  LPngImage : TPngImage;
begin

  if (Assigned(TColorizerLocalSettings.Settings) and not TColorizerLocalSettings.Settings.Enabled) or (not TColorizerLocalSettings.Settings.DockCustom) or (not Assigned(TColorizerLocalSettings.ColorMap)) or (Assigned(TColorizerLocalSettings.Settings) and TColorizerLocalSettings.Settings.UseVCLStyles) then
  begin
    {$IFDEF DELPHIXE6_UP}
      if Assigned(Trampoline_ModernDockCaptionDrawer_DrawDockCaption) then
       Exit(Trampoline_ModernDockCaptionDrawer_DrawDockCaption(Self, Canvas, CaptionRect, State))
      else
    {$ENDIF}
    Exit(Trampoline_TDockCaptionDrawer_DrawDockCaption(Self, Canvas, CaptionRect, State));
  end;

  Canvas.Font.Color :=  TColorizerLocalSettings.ColorMap.FontColor;
  if Self.DockCaptionOrientation = dcoHorizontal then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := TColorizerLocalSettings.ColorMap.FrameTopLeftInner;

    CaptionRect.Top := CaptionRect.Top + 1;

    if State.Focused then
    begin
      if not TColorizerLocalSettings.Settings.DockCustomColors then
      begin
        LColorStart := TColorizerLocalSettings.ColorMap.Color;
        LColorEnd   := TColorizerLocalSettings.ColorMap.HighlightColor;
      end
      else
      begin
        try LColorStart := StringToColor(TColorizerLocalSettings.Settings.DockStartGradActive); except LColorStart := TColorizerLocalSettings.ColorMap.Color; end;
        try LColorEnd   := StringToColor(TColorizerLocalSettings.Settings.DockEndGradActive);   except LColorEnd   := TColorizerLocalSettings.ColorMap.HighlightColor; end;
      end;

    end
    else
    begin
      if not TColorizerLocalSettings.Settings.DockCustomColors then
      begin
        LColorStart := TColorizerLocalSettings.ColorMap.DisabledColor;
        LColorEnd   := TColorizerLocalSettings.ColorMap.DisabledColor;//GetHighLightColor(TColorizerLocalSettings.ColorMap.DisabledColor);
      end
      else
      begin
        try LColorStart := StringToColor(TColorizerLocalSettings.Settings.DockStartGradInActive); except LColorStart := TColorizerLocalSettings.ColorMap.DisabledColor; end;
        try LColorEnd   := StringToColor(TColorizerLocalSettings.Settings.DockEndGradInActive); except LColorEnd   := TColorizerLocalSettings.ColorMap.DisabledColor; end;
      end;
    end;

    //Canvas.Brush.Color := LColor;

    if TColorizerLocalSettings.Settings.DockGradientHor then
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom), gdHorizontal)
    else
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left + 1, CaptionRect.Top + 1, CaptionRect.Right, CaptionRect.Bottom), gdVertical);


     if TColorizerLocalSettings.Settings.DockCustomColors then
     begin
        if State.Focused then
          try Canvas.Pen.Color:=StringToColor(TColorizerLocalSettings.Settings.DockActiveBorderColor) except Canvas.Pen.Color:=clBlack end
        else
          try Canvas.Pen.Color:=StringToColor(TColorizerLocalSettings.Settings.DockInActiveBorderColor) except Canvas.Pen.Color:=clBlack end;
     end
     else
      Canvas.Pen.Color :=  TColorizerLocalSettings.ColorMap.FrameTopLeftOuter; //GetShadowColor(Canvas.Pen.Color, -20);

    if TColorizerLocalSettings.Settings.DockBorderRounded then
      with CaptionRect do
        Canvas.Polyline([Point(Left + 2, Top),
          Point(Right - 2, Top),
          Point(Right, Top + 2),
          Point(Right, Bottom - 2),
          Point(Right - 2, Bottom),
          Point(Left + 2, Bottom),
          Point(Left, Bottom - 2),
          Point(Left, Top + 2),
          Point(Left + 3, Top)])
    else
    begin
      Canvas.Brush.Style:=bsClear;
      Canvas.Rectangle(CaptionRect);
    end;

    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);

        if Self.DockCaptionPinButton = dcpbUp then
        begin
          CropPNG(TColorizerLocalSettings.DockImages, 32, 0, 16, 16, LPngImage);
          try
            Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
          finally
            LPngImage.free;
          end;
        end
        else
        begin
            CropPNG(TColorizerLocalSettings.DockImages, 16, 0, 16, 16, LPngImage);
            try
              Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
            finally
              LPngImage.free;
            end;
        end;


      CaptionRect.Right := PinRect.Right - 2;
    end
    else
      CaptionRect.Right := CloseRect.Right - 2;

    CaptionRect.Left := CaptionRect.Left + 6;
    DrawIcon;
    ShouldDrawClose := CloseRect.Left >= CaptionRect.Left;
  end
  else
  begin
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Top + 1);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Top + 1);


    if State.Focused then
    begin
      LColorStart := TColorizerLocalSettings.ColorMap.Color;
      LColorEnd   := TColorizerLocalSettings.ColorMap.HighlightColor;
    end
    else
    begin
      LColorStart := TColorizerLocalSettings.ColorMap.DisabledColor;
      LColorEnd   := GetHighLightColor(TColorizerLocalSettings.ColorMap.DisabledColor);
    end;

    //Canvas.Brush.Color := LColor;

    //Canvas.FillRect(Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom));
    if TColorizerLocalSettings.Settings.DockGradientHor then
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom), gdHorizontal)
    else
      GradientFillCanvas(Canvas, LColorStart, LColorEnd, Rect(CaptionRect.Left, CaptionRect.Top + 2, CaptionRect.Right, CaptionRect.Bottom), gdVertical);

    Canvas.Pen.Color := State.EndColor;
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Bottom);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Bottom);

    Canvas.Font.Orientation := 900;
    CloseRect := GetCloseRect(CaptionRect);

    if Self.DockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);
      LPngImage:=TPNGImage.Create;
      try
        if Self.DockCaptionPinButton = dcpbUp then
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock_left')
        else
         LPngImage.LoadFromResourceName(HInstance, 'pin_dock');

        Canvas.Draw(PinRect.Left, PinRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;
      CaptionRect.Top := PinRect.Bottom + 2;
    end
    else
      CaptionRect.Top := CloseRect.Bottom + 2;

    ShouldDrawClose   := CaptionRect.Top < CaptionRect.Bottom;
    CaptionRect.Right := CaptionRect.Left + (CaptionRect.Bottom - CaptionRect.Top - 2);
    CaptionRect.Top   := CaptionRect.Top + Canvas.TextWidth(State.Caption) + 2;

    if CaptionRect.Top > CaptionRect.Bottom then
      CaptionRect.Top := CaptionRect.Bottom;
  end;

  Canvas.Brush.Style := bsClear;
  if State.Caption <> '' then
  begin
    if State.Focused then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];

   if ShouldDrawClose then
     CaptionRect.Right := CaptionRect.Right - (CloseRect.Right - CloseRect.Left) - 4;

     if TColorizerLocalSettings.Settings.DockCustomColors then
     begin
        if State.Focused then
          try Canvas.Font.Color:=StringToColor(TColorizerLocalSettings.Settings.DockActiveFontColor) except Canvas.Font.Color:=clBlack end
        else
          try Canvas.Font.Color:=StringToColor(TColorizerLocalSettings.Settings.DockInActiveFontColor) except Canvas.Font.Color:=clBlack end;
     end;

    Canvas.TextRect(CaptionRect, State.Caption,
      [tfEndEllipsis, tfVerticalCenter, tfSingleLine]);
  end;

  if ShouldDrawClose then
  begin
      CropPNG(TColorizerLocalSettings.DockImages, 0, 0, 16, 16, LPngImage);
      try
        Canvas.Draw(CloseRect.Left, CloseRect.Top, LPngImage);
      finally
        LPngImage.free;
      end;
  end;

  Exit(0);
end;

const
{$IFDEF DELPHIXE}
  sCompilerMsgLineDraw           = '@Msglines@TCompilerMsgLine@Draw$qqrp16Graphics@TCanvasrx11Types@TRecto';
  sTitleLineDraw                 = '@Msglines@TTitleLine@Draw$qqrp16Graphics@TCanvasrx11Types@TRecto';
  sFileFindLineDraw              = '@Msglines@TFileFindLine@Draw$qqrp16Graphics@TCanvasrx11Types@TRecto';
  sGetOutlineColor               = '@Gdiplus@Gradientdrawer@GetOutlineColor$qqrv'; //Don't supported in XE
  sPropertyInspectorDrawNamePair = '@Propinsp@TPropertyInspector@DrawNamePair$qqrp16Graphics@TCanvasrx11Types@TRectoox17System@WideStringt5';
{$ELSE}
  sCompilerMsgLineDraw           = '@Msglines@TCompilerMsgLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto';
  sTitleLineDraw                 = '@Msglines@TTitleLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto';
  sFileFindLineDraw              = '@Msglines@TFileFindLine@Draw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRecto';
  sFileFindLineInternalCalcDraw  = '@Msglines@TFileFindLine@InternalCalcDraw$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRectoo';
  sBaseVirtualTreeGetHintWindowClass = '@Idevirtualtrees@TBaseVirtualTree@GetHintWindowClass$qqrv';
  sGetOutlineColor               = '@Gdiplus@Gradientdrawer@GetOutlineColor$qqrv';
  sExpandableEvalViewFormCreate  = '@Expandableevaltree@TExpandableEvalView@FormCreate$qqrp14System@TObject';
  sPropertyInspectorDrawNamePair = '@Propinsp@TPropertyInspector@DrawNamePair$qqrp20Vcl@Graphics@TCanvasrx18System@Types@TRectoox17System@WideStringt5';
{$ENDIF}

  sProjectTree2PaintText         = '@Projectfrm@TProjectManagerForm@ProjectTree2PaintText$qqrp32Idevirtualtrees@TBaseVirtualTreexp20Vcl@Graphics@TCanvasp28Idevirtualtrees@TVirtualNodei28Idevirtualtrees@TVSTTextType';
{$IFDEF DELPHIXE6_UP}
  sModernThemeDrawDockCaption    = '@Moderntheme@TModernDockCaptionDrawer@DrawDockCaption$qqrxp20Vcl@Graphics@TCanvasrx18System@Types@TRectrx38Vcl@Captioneddocktree@TParentFormState';
  sModernThemeSetHotSingleColor  = '@Moderntheme@TModernTheme@SetHotSingleColor$qqrx21System@Uitypes@TColor';
{$ENDIF}
  sBaseVirtualTreePrepareBitmaps = '@Idevirtualtrees@TBaseVirtualTree@PrepareBitmaps$qqroo';
  sListButtonPaint               = '@Idelistbtns@TListButton@Paint$qqrv';

{$IFDEF DELPHIXE5_UP}
 sIDEInsight_DrawTreeDrawNode  = '@Ideinsight@TIDEInsightForm@DrawTreeDrawNode$qqrp32Idevirtualtrees@TBaseVirtualTreerx28Idevirtualtrees@TVTPaintInfo';
 sIDEInsight_PaintCategoryNode = '@Ideinsight@TIDEInsightForm@PaintCategoryNode$qqrp28Idevirtualtrees@TVirtualNodep20Vcl@Graphics@TCanvasr18System@Types@TRectp33Ideinsightmgr@TIDEInsightCategoryo';
 sIDEInsight_PaintItemNode     = '@Ideinsight@TIDEInsightForm@PaintItemNode$qqrp28Idevirtualtrees@TVirtualNodep20Vcl@Graphics@TCanvasr18System@Types@TRectp29Ideinsightmgr@TIDEInsightItemo';
// sIDEInsight_DrawTreeDrawNode  = '@Popupsrchfrm@TPopupSearchForm@DrawTreeDrawNode$qqrp32Idevirtualtrees@TBaseVirtualTreerx28Idevirtualtrees@TVTPaintInfo';
// sIDEInsight_PaintCategoryNode = '@Popupsrchfrm@TPopupSearchForm@PaintCategoryNode$qqrp28Idevirtualtrees@TVirtualNodep20Vcl@Graphics@TCanvasr18System@Types@TRectp33Ideinsightmgr@TIDEInsightCategoryo';
// sIDEInsight_PaintItemNode     = '@Popupsrchfrm@TPopupSearchForm@PaintItemNode$qqrp28Idevirtualtrees@TVirtualNodep20Vcl@Graphics@TCanvasr18System@Types@TRectp29Ideinsightmgr@TIDEInsightItemo';
{$ELSE}
 sIDEInsight_DrawTreeDrawNode  = '@Popupsrchfrm@TPopupSearchForm@DrawTreeDrawNode$qqrp32Idevirtualtrees@TBaseVirtualTreerx28Idevirtualtrees@TVTPaintInfo';
 sIDEInsight_PaintCategoryNode = '@Popupsrchfrm@TPopupSearchForm@PaintCategoryNode$qqrp28Idevirtualtrees@TVirtualNodep20Vcl@Graphics@TCanvasr18System@Types@TRectp33Ideinsightmgr@TIDEInsightCategoryo';
 sIDEInsight_PaintItemNode     = '@Popupsrchfrm@TPopupSearchForm@PaintItemNode$qqrp28Idevirtualtrees@TVirtualNodep20Vcl@Graphics@TCanvasr18System@Types@TRectp29Ideinsightmgr@TIDEInsightItemo';
{$ENDIF}


var
  pOrgAddress : Pointer;
{$IFDEF DELPHIXE6_UP}
  ModernThemeModule           : HMODULE;
  Modules                     : TStrings;
  ModernThemeLoaded           : Boolean;
{$ENDIF}
 CoreIDEModule, VclIDEModule  : HMODULE;

initialization

 ListControlWrappers := TObjectDictionary<TCustomControl, TRttiWrapper>.Create([doOwnsValues]);
  CoreIDEModule := LoadLibrary(sCoreIDEModule);
  if CoreIDEModule<>0 then
  begin
   pOrgAddress := GetProcAddress(CoreIDEModule, sCompilerMsgLineDraw);
   if Assigned(pOrgAddress) then
    Trampoline_CompilerMsgLine_Draw := InterceptCreate(pOrgAddress, @Detour_TCompilerMsgLine_Draw);

   pOrgAddress := GetProcAddress(CoreIDEModule, sTitleLineDraw);
   if Assigned(pOrgAddress) then
     Trampoline_TitleLine_Draw   := InterceptCreate(pOrgAddress, @Detour_TTitleLine_Draw);

   pOrgAddress := GetProcAddress(CoreIDEModule, sFileFindLineDraw);
   if Assigned(pOrgAddress) then
     Trampoline_TFileFindLine_Draw   := InterceptCreate(pOrgAddress, @Detour_TFileFindLine_Draw);

   pOrgAddress := GetProcAddress(CoreIDEModule, sPropertyInspectorDrawNamePair);
   if Assigned(pOrgAddress) then
     Trampoline_TPropertyInspector_DrawNamePair  := InterceptCreate(pOrgAddress, @Detour_TPropertyInspector_DrawNamePair);

{$IFDEF DELPHIXE5_UP}
{$ELSE}
   pOrgAddress := GetProcAddress(CoreIDEModule, sIDEInsight_DrawTreeDrawNode);
   if Assigned(pOrgAddress) then
   Trampoline_TPopupSearchForm_DrawTreeDrawNode  := InterceptCreate(pOrgAddress, @Detour_TPopupSearchForm_DrawTreeDrawNode);

   pOrgAddress := GetProcAddress(CoreIDEModule, sIDEInsight_PaintCategoryNode);
   if Assigned(pOrgAddress) then
   Trampoline_TPopupSearchForm_PaintCategoryNode:= InterceptCreate(pOrgAddress, @Detour_TPopupSearchForm_PaintCategoryNode);

   pOrgAddress := GetProcAddress(CoreIDEModule, sIDEInsight_PaintItemNode);
   if Assigned(pOrgAddress) then
   Trampoline_TPopupSearchForm_PaintItemNode    := InterceptCreate(pOrgAddress, @Detour_TPopupSearchForm_PaintItemNode);
{$ENDIF}
  end;

  VclIDEModule := LoadLibrary(sVclIDEModule);
  if VclIDEModule<>0 then
  begin
 //  pOrgAddress := GetProcAddress(VclIDEModule, sBaseVirtualTreePrepareBitmaps);
//   if Assigned(pOrgAddress) then
 //   Trampoline_TBaseVirtualTree_PrepareBitmaps := InterceptCreate(pOrgAddress, @Detour_TBaseVirtualTree_PrepareBitmaps);

   pOrgAddress := GetProcAddress(VclIDEModule, sListButtonPaint);
   if Assigned(pOrgAddress) then
    Trampoline_TListButton_Paint := InterceptCreate(pOrgAddress, @Detour_TListButton_Paint);

   pOrgAddress := GetProcAddress(VclIDEModule, sGetOutlineColor);
   if Assigned(pOrgAddress) then
    Trampoline_Gradientdrawer_GetOutlineColor := InterceptCreate(pOrgAddress, @Detour_Gradientdrawer_GetOutlineColor);


//   pOrgAddress := GetProcAddress(VclIDEModule, sBaseVirtualTreeGetHintWindowClass);
//   if Assigned(pOrgAddress) then
//    Trampoline_TBaseVirtual_GetHintWindowClass := InterceptCreate(pOrgAddress, @Detour_TBaseVirtual_GetHintWindowClass);
  end;
//   *******************************************

  Trampoline_TDockCaptionDrawer_DrawDockCaption  := InterceptCreate(@TDockCaptionDrawer.DrawDockCaption,   @Detour_TDockCaptionDrawer_DrawDockCaption);


{$IFDEF DELPHIXE6_UP}
  Modules:=TStringList.Create;
  try
    GetLoadedModules(Modules, True);
    ModernThemeLoaded:=Modules.IndexOf(sModernThemeModule)>=0;
  finally
    Modules.Free;
  end;

  if ModernThemeLoaded then //avoid to load the ModernTheme module
  begin
    //AddLog('ModernThemeLoaded');
    ModernThemeModule := LoadLibrary(sModernThemeModule);
    if ModernThemeModule<>0 then
    begin
     pOrgAddress := GetProcAddress(ModernThemeModule, sModernThemeDrawDockCaption);
     if Assigned(pOrgAddress) then
       Trampoline_ModernDockCaptionDrawer_DrawDockCaption:= InterceptCreate(pOrgAddress, @Detour_TDockCaptionDrawer_DrawDockCaption);

//     pOrgAddress := GetProcAddress(ModernThemeModule, sModernThemeSetHotSingleColor);
//     if Assigned(pOrgAddress) then
//       Trampoline_TModernTheme_SetHotSingleColor:= InterceptCreate(pOrgAddress, @Detour_TModernTheme_SetHotSingleColor);
    end;

  end;
{$ENDIF}

finalization

  if Assigned(Trampoline_CompilerMsgLine_Draw) then
    InterceptRemove(@Trampoline_CompilerMsgLine_Draw);

  if Assigned(Trampoline_TitleLine_Draw) then
    InterceptRemove(@Trampoline_TitleLine_Draw);

  if Assigned(Trampoline_TFileFindLine_Draw) then
    InterceptRemove(@Trampoline_TFileFindLine_Draw);

  if Assigned(Trampoline_TPropertyInspector_DrawNamePair) then
    InterceptRemove(@Trampoline_TPropertyInspector_DrawNamePair);

{$IFDEF DELPHIXE5_UP}
{$ELSE}
  if Assigned(Trampoline_TPopupSearchForm_DrawTreeDrawNode) then
    InterceptRemove(@Trampoline_TPopupSearchForm_DrawTreeDrawNode);

  if Assigned(Trampoline_TPopupSearchForm_PaintCategoryNode) then
    InterceptRemove(@Trampoline_TPopupSearchForm_PaintCategoryNode);

  if Assigned(Trampoline_TPopupSearchForm_PaintItemNode) then
    InterceptRemove(@Trampoline_TPopupSearchForm_PaintItemNode);
{$ENDIF}

  if Assigned(Trampoline_TFileFindLine_InternalCalcDraw) then
    InterceptRemove(@Trampoline_TFileFindLine_InternalCalcDraw);

  if Assigned(Trampoline_TExpandableEvalView_FormCreate) then
    InterceptRemove(@Trampoline_TExpandableEvalView_FormCreate);

  if Assigned(Trampoline_TBaseVirtualTree_PrepareBitmaps) then
    InterceptRemove(@Trampoline_TBaseVirtualTree_PrepareBitmaps);

  if Assigned(Trampoline_TListButton_Paint) then
    InterceptRemove(@Trampoline_TListButton_Paint);

  if Assigned(Trampoline_Gradientdrawer_GetOutlineColor) then
    InterceptRemove(@Trampoline_Gradientdrawer_GetOutlineColor);

  if Assigned(Trampoline_TBaseVirtual_GetHintWindowClass) then
    InterceptRemove(@Trampoline_TBaseVirtual_GetHintWindowClass);

  if Assigned(Trampoline_ProjectTree2PaintText) then
    InterceptRemove(@Trampoline_ProjectTree2PaintText);

  if Assigned(Trampoline_TDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_TDockCaptionDrawer_DrawDockCaption);

{$IFDEF DELPHIXE6_UP}
  if Assigned(Trampoline_ModernDockCaptionDrawer_DrawDockCaption) then
    InterceptRemove(@Trampoline_ModernDockCaptionDrawer_DrawDockCaption);

  if Assigned(Trampoline_TModernTheme_SetHotSingleColor) then
    InterceptRemove(@Trampoline_TModernTheme_SetHotSingleColor);
{$ENDIF}

   ListControlWrappers.Free;

end.
