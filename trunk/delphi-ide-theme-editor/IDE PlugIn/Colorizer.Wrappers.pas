//**************************************************************************************************
//
// Unit Colorizer.Wrappers
// unit Colorizer.Wrappers for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Wrappers.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Wrappers;

interface

{$I ..\Common\Jedi.inc}

uses
  ActnMan,
  Classes;

   function RunWrapper(AComponent : TComponent; AColorMap:TCustomActionBarColorMap; Invalidate : Boolean = False) : Boolean;

implementation

uses
  Rtti,
  uRttiHelper,
  Forms,
  Windows,
  StdCtrls,
  SysUtils,
  ExtCtrls,
  Controls,
  ComCtrls,
  Graphics,
  ImgList,
  Tabs,
  CategoryButtons,
  Generics.Collections,
  ActnColorMaps,
  ActnPopup,
  ActnMenus,
  UxTheme,
  ActnCtrls,
  {$IFDEF DELPHIXE2_UP}
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.Ext,
  {$ENDIF}
  {$IFDEF DELPHI2009_UP}
  PngImage,
  {$ENDIF}
  Colorizer.Utils;

type
   TBaseWrapper = class (TComponent)
   private
    procedure SetFlatParent(AComponent: TComponent);
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); virtual;
   end;

   TWrapperVirtualStringTree = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperIDECategoryButtons = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperDisassemblerView = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperTDStringGrid = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperDeguggerWindows = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperIDEComboBox = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperSimpleControl = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperPanel = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperInspListBox = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperStringGrid = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperRefactoringTree = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperCodeEditorTabControl = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperEditControl = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperToolBar = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperTabSet = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperClosableTabScroller = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperGradientTabSet = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperIDEGradientTabSet = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperTabSheet = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperActionMainMenuBar = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperPopupActionBar = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperActionToolBar = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperControlBar = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperStatusBar = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperLists = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperGroupComponents  = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperFontComponents  = class(TBaseWrapper)
   protected
    procedure SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;


  TBaseWrapperClass = class of TBaseWrapper;
  TRegisteredWrappers = class
  public
    class var Wrappers          : TDictionary<string, TBaseWrapperClass>;
    class var WrappersInstances : TObjectDictionary<string, TBaseWrapper>;
  end;

procedure RegisterColorizerWrapper(const ComponentClass : string; ClassWrapper : TBaseWrapperClass);
var
   LInstance : TBaseWrapperClass;
begin
  if not TRegisteredWrappers.Wrappers.ContainsKey(ComponentClass) then
  begin
    //AddLog('Registred '+ClassWrapper.ClassName+' for '+ComponentClass);
    TRegisteredWrappers.Wrappers.Add(ComponentClass, ClassWrapper);
    LInstance:= TRegisteredWrappers.Wrappers.Items[ComponentClass];
    TRegisteredWrappers.WrappersInstances.Add(ComponentClass, LInstance.Create(nil));
  end;
end;

function RunWrapper(AComponent : TComponent; AColorMap:TCustomActionBarColorMap; Invalidate : Boolean = False) : Boolean;
begin
  Result:=False;
  if TRegisteredWrappers.WrappersInstances.ContainsKey(AComponent.ClassName) then
  begin
    //AddLog('RunWrapper '+AComponent.ClassName);
    TRegisteredWrappers.WrappersInstances.Items[AComponent.ClassName].SetColors(AComponent, AColorMap);
//    if AComponent is TWinControl then
//      TWinControl(AComponent).Invalidate();
    Result:=True;
  end;
end;

{ TWrapperVirtualStringTree }
procedure TWrapperVirtualStringTree.SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap);
begin
  inherited;
  //AddLog(Self.ClassName+' SetColors '+AComponent.ClassName);

  //BorderStyle


  TRttiUtils.SetRttiPropertyValue(AComponent,'BevelKind', TValue.From(TBevelKind.bkFlat));
  TRttiUtils.SetRttiPropertyValue(AComponent,'BorderStyle', TValue.From(TFormBorderStyle.bsNone));
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);  //ok
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor); //ok
  TRttiUtils.SetRttiPropertyValue(AComponent,'Header.Font.Color', AColorMap.FontColor); //ok
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False); //ok

  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.TreeLineColor', AColorMap.FontColor); //ok

  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.SelectionRectangleBlendColor', AColorMap.SelectedColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.SelectionRectangleBorderColor', AColorMap.FrameTopLeftInner);

  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.FocusedSelectionColor', AColorMap.SelectedColor);   //ok
  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.FocusedSelectionBorderColor', AColorMap.FrameTopLeftInner);  //ok

  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.UnfocusedSelectionColor', AColorMap.DisabledColor);   //ok
  TRttiUtils.SetRttiPropertyValue(AComponent,'Colors.UnfocusedSelectionBorderColor', AColorMap.FrameTopLeftInner);  //ok

        //  TVTColors
        //	__fastcall TVTColors(TBaseVirtualTree* AOwner);
        //	virtual void __fastcall Assign(Classes::TPersistent* Source);
        //
        //__published:
        //	__property Graphics::TColor BorderColor = {read=GetColor, write=SetColor, index=7, default=-16777201};
        //	__property Graphics::TColor DisabledColor = {read=GetColor, write=SetColor, index=0, default=-16777200};
        //	__property Graphics::TColor DropMarkColor = {read=GetColor, write=SetColor, index=1, default=-16777203};
        //	__property Graphics::TColor DropTargetColor = {read=GetColor, write=SetColor, index=2, default=-16777203};
        //	__property Graphics::TColor DropTargetBorderColor = {read=GetColor, write=SetColor, index=11, default=-16777203};
        //	__property Graphics::TColor FocusedSelectionColor = {read=GetColor, write=SetColor, index=3, default=-16777203};
        //	__property Graphics::TColor FocusedSelectionBorderColor = {read=GetColor, write=SetColor, index=9, default=-16777203};
        //	__property Graphics::TColor GridLineColor = {read=GetColor, write=SetColor, index=4, default=-16777201};
        //	__property Graphics::TColor HeaderHotColor = {read=GetColor, write=SetColor, index=14, default=-16777200};
        //	__property Graphics::TColor HotColor = {read=GetColor, write=SetColor, index=8, default=-16777208};
        //	__property Graphics::TColor SelectionRectangleBlendColor = {read=GetColor, write=SetColor, index=12, default=-16777203};
        //	__property Graphics::TColor SelectionRectangleBorderColor = {read=GetColor, write=SetColor, index=13, default=-16777203};
        //	__property Graphics::TColor TreeLineColor = {read=GetColor, write=SetColor, index=5, default=-16777200};
        //	__property Graphics::TColor UnfocusedSelectionColor = {read=GetColor, write=SetColor, index=6, default=-16777201};
        //	__property Graphics::TColor UnfocusedSelectionBorderColor = {read=GetColor, write=SetColor, index=10, default=-16777201};

//  TVTColors = class(TPersistent)
//  private
//    FOwner: TBaseVirtualTree;
//    FColors: array[0..15] of TColor;
//    function GetColor(const Index: Integer): TColor;
//    procedure SetColor(const Index: Integer; const Value: TColor);
//    function GetBackgroundColor: TColor;
//    function GetHeaderFontColor: TColor;
//    function GetNodeFontColor: TColor;
//  public
//    constructor Create(AOwner: TBaseVirtualTree);
//
//    procedure Assign(Source: TPersistent); override;
//    property BackGroundColor: TColor read GetBackgroundColor;
//    property HeaderFontColor: TColor read  GetHeaderFontColor;
//    property NodeFontColor: TColor read GetNodeFontColor;
//  published
//    property BorderColor: TColor index 7 read GetColor write SetColor default clBtnFace;
//    property DisabledColor: TColor index 0 read GetColor write SetColor default clBtnShadow;
//    property DropMarkColor: TColor index 1 read GetColor write SetColor default clHighlight;
//    property DropTargetColor: TColor index 2 read GetColor write SetColor default clHighLight;
//    property DropTargetBorderColor: TColor index 11 read GetColor write SetColor default clHighLight;
//    property FocusedSelectionColor: TColor index 3 read GetColor write SetColor default clHighLight;
//    property FocusedSelectionBorderColor: TColor index 9 read GetColor write SetColor default clHighLight;
//    property GridLineColor: TColor index 4 read GetColor write SetColor default clBtnFace;
//    property HeaderHotColor: TColor index 14 read GetColor write SetColor default clBtnShadow;
//    property HotColor: TColor index 8 read GetColor write SetColor default clWindowText;
//    property SelectionRectangleBlendColor: TColor index 12 read GetColor write SetColor default clHighlight;
//    property SelectionRectangleBorderColor: TColor index 13 read GetColor write SetColor default clHighlight;
//    property SelectionTextColor: TColor index 15 read GetColor write SetColor default clHighlightText;
//    property TreeLineColor: TColor index 5 read GetColor write SetColor default clBtnShadow;
//    property UnfocusedSelectionColor: TColor index 6 read GetColor write SetColor default clBtnFace;
//    property UnfocusedSelectionBorderColor: TColor index 10 read GetColor write SetColor default clBtnFace;
//  end;

//       {$IFDEF DELPHIXE2_UP}
//        if TColorizerLocalSettings.Settings.UseVCLStyles then
//        begin
//          if not IsStyleHookRegistered(AComponent.ClassType, TTreeViewStyleHook) then
//           TStyleEngine.RegisterStyleHook(AComponent.ClassType, TTreeViewStyleHook);
//        end;
//       {$ENDIF}
end;

{ TBaseWrapper }

procedure TBaseWrapper.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin

end;

procedure TBaseWrapper.SetFlatParent(AComponent: TComponent);
var
  LParent : TComponent;
begin
  LParent:=AComponent;
  if (LParent<>nil) then
  repeat
    LParent:=LParent.GetParentComponent;
    if (LParent<>nil) and (LParent is TPanel) then
    begin
     TPanel(LParent).BevelInner  :=TBevelCut.bvNone;
     TPanel(LParent).BevelOuter  :=TBevelCut.bvNone;
     TPanel(LParent).Color       := TColorizerLocalSettings.ColorMap.Color;
     TPanel(LParent).Ctl3D       := False;
     TPanel(LParent).BevelKind   := TBevelKind.bkNone;
     TPanel(LParent).BorderStyle := bsNone;
    end;
  until (LParent=nil) or not (LParent Is TPanel);
end;

{ TWrapperIDECategoryButtons }

procedure TWrapperIDECategoryButtons.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  LCategoryButtons : TCategoryButtons;
  i :   Integer;
begin
  inherited;
  LCategoryButtons:= TCategoryButtons(AComponent);
  LCategoryButtons.BorderStyle:=bsNone;
  LCategoryButtons.ButtonOptions      := LCategoryButtons.ButtonOptions + [boGradientFill];
  LCategoryButtons.Color              := AColorMap.MenuColor;
  LCategoryButtons.BackgroundGradientColor:= AColorMap.MenuColor;
  LCategoryButtons.HotButtonColor     := AColorMap.HighlightColor;
  LCategoryButtons.RegularButtonColor := AColorMap.MenuColor;
  LCategoryButtons.SelectedButtonColor:= AColorMap.SelectedColor;
  for i := 0 to LCategoryButtons.Categories.Count-1 do
   begin
     LCategoryButtons.Categories[i].GradientColor := AColorMap.MenuColor;
    LCategoryButtons.Categories[i].Color := AColorMap.Color;
    LCategoryButtons.Categories[i].TextColor := AColorMap.FontColor;
   end;

  LCategoryButtons.Font.Color:=AColorMap.FontColor;
  LCategoryButtons.Invalidate;
end;

{ TWrapperDisassemblerView }

procedure TWrapperDisassemblerView.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'BreakpointColor', AColorMap.SelectedColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'BreakpointTextColor', AColorMap.SelectedFontColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);

  {
     property BreakpointColor: TColor;
     property BreakpointTextColor: TColor;
     property Color: TColor;
     property Ctl3D: Boolean;
     property Enabled: Boolean;
     property Font: TFont;
  }
end;

{ TWrapperTDStringGrid }

procedure TWrapperTDStringGrid.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'GradientStartColor', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'GradientEndColor', AColorMap.Color);
end;

{ TWrapperDeguggerWindows }

procedure TWrapperDeguggerWindows.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color',AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
end;

{ TWrapperComboBox }

type
 TCustomComboBoxClass = class(TCustomComboBox);

procedure TWrapperIDEComboBox.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  LCustomComboBox : TCustomComboBoxClass;
begin
  inherited;

  LCustomComboBox:=TCustomComboBoxClass(AComponent);
  if LCustomComboBox.Handle<>0 then
   SetWindowTheme(LCustomComboBox.Handle, '', '');

  LCustomComboBox.Color      := AColorMap.MenuColor;
  LCustomComboBox.Font.Color := AColorMap.FontColor;
  LCustomComboBox.BevelKind  := bkFlat;
  LCustomComboBox.BevelInner := bvNone;
  LCustomComboBox.Ctl3D      := False;
end;

{ TWrapperStandardControl }

procedure TWrapperSimpleControl.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperPanel }

procedure TWrapperPanel.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  LPanel : TPanel;
begin
  inherited;
  LPanel        := TPanel(AComponent);
  LPanel.Color  := AColorMap.Color;
end;

{ TWrapperInspListBox }

procedure TWrapperInspListBox.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
    {
     property BackgroundColor: TColor;
     property PropNameColor: TColor;
     property PropValueColor: TColor;
     property EditBackgroundColor: TColor;
     property EditValueColor: TColor;
     property CategoryColor: TColor;
     property GutterColor: TColor;
     property GutterEdgeColor: TColor;
     property ReferenceColor: TColor;
     property SubPropColor: TColor;
     property ReadOnlyColor: TColor;
     property NonDefaultColor: TColor;
     property HighlightColor: TColor;
     property HighlightFontColor: TColor;
    }
    TRttiUtils.SetRttiPropertyValue(AComponent,'BackgroundColor', AColorMap.Color);
    TRttiUtils.SetRttiPropertyValue(AComponent,'PropNameColor', AColorMap.FontColor);   //*
    TRttiUtils.SetRttiPropertyValue(AComponent,'PropValueColor', AColorMap.FontColor);  //*
    TRttiUtils.SetRttiPropertyValue(AComponent,'EditBackgroundColor', AColorMap.MenuColor);
    TRttiUtils.SetRttiPropertyValue(AComponent,'EditValueColor', AColorMap.FontColor); //*
    TRttiUtils.SetRttiPropertyValue(AComponent,'CategoryColor', AColorMap.FontColor); //*
    TRttiUtils.SetRttiPropertyValue(AComponent,'GutterColor', AColorMap.Color);
    TRttiUtils.SetRttiPropertyValue(AComponent,'GutterEdgeColor', AColorMap.FrameTopLeftInner); //*
    TRttiUtils.SetRttiPropertyValue(AComponent,'ReferenceColor', AColorMap.FontColor);//*

    TRttiUtils.SetRttiPropertyValue(AComponent,'SubPropColor', AColorMap.FontColor);//*
    TRttiUtils.SetRttiPropertyValue(AComponent,'ReadOnlyColor', AColorMap.FontColor);//*
    TRttiUtils.SetRttiPropertyValue(AComponent,'NonDefaultColor', AColorMap.FontColor);//*


    TRttiUtils.SetRttiPropertyValue(AComponent,'HighlightColor', AColorMap.SelectedColor);
    TRttiUtils.SetRttiPropertyValue(AComponent,'HighlightFontColor', AColorMap.SelectedFontColor);
    TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperStringGrid }

procedure TWrapperStringGrid.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperRefactoringTree }

procedure TWrapperRefactoringTree.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
end;

{ TWrapperCodeEditorTabControl }

procedure TWrapperCodeEditorTabControl.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'UnselectedColor',AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'SelectedColor',AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'BackgroundColor',AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
end;

{ TWrapperEditControl }

procedure TWrapperEditControl.SetColors(AComponent: TComponent; AColorMap: TCustomActionBarColorMap);
begin
  inherited;
   SetFlatParent(AComponent);
   TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
   //TRttiUtils.SetRttiPropertyValue(AComponent, 'BorderStyle',  Ord(bsNone));
   {$IFDEF DELPHIXE2_UP}
//    if TColorizerLocalSettings.Settings.UseVCLStyles then
//    begin
//      if not IsStyleHookRegistered(AComponent.ClassType, TMemoStyleHook) then
//       TStyleEngine.RegisterStyleHook(AComponent.ClassType, TMemoStyleHook);
//    end;
   {$ENDIF}
   // Setting these properties and Fields has not effect in the gutter color.
   //SetRttiFieldValue(AComponent,'GutterBrush.Color',  clYellow);
   //TRttiUtils.SetRttiPropertyValue(AComponent,'Brush.Color',  clRed);
   //SetRttiFieldValue(AComponent,'FParentColor',  False);
   //SetRttiFieldValue(AComponent,'FColor',  clYellow);
   //SetRttiFieldValue(AComponent,'CurForeColor',  clYellow);
   //SetRttiFieldValue(AComponent,'CurBackColor',  clRed);
   //ExecMethodRtti(AComponent, 'Invalidate');
   //DumpParentClass(AComponent);
end;

{ TWrapperToolBar }

procedure TWrapperToolBar.SetColors(AComponent: TComponent;  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  SetFlatParent(AComponent);
  with TToolBar(AComponent) do
  begin
    Color              := AColorMap.Color;
//    if Restore then
//      DrawingStyle     := TTBDrawingStyle(dsNormal)
//    else
    //don't change this value (dsNormal), because prevents which the toolbars of the IDE continue using the themes color even if the Wizard is not running
    DrawingStyle       := TTBDrawingStyle(dsNormal); //dsGradient
    GradientStartColor := AColorMap.MenuColor;
    GradientEndColor   := AColorMap.Color;
    HotTrackColor      := AColorMap.SelectedColor;
    Font.Color         := AColorMap.FontColor;
    Ctl3D:=False;
  end;

end;

{ TWrapperTabSet }

procedure TWrapperTabSet.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  with TTabSet(AComponent) do
  begin
    BackgroundColor:= AColorMap.MenuColor;
    SelectedColor  := AColorMap.Color;
    UnselectedColor:= AColorMap.MenuColor;
    Font.Color     := AColorMap.FontColor;
    Style          := tsModernTabs; //necessary for allow paint the background color
  end

end;

{ TWrapperClosableTabScroller }

procedure TWrapperClosableTabScroller.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
   inherited;
   //SetBkColor
   //Canvas.Fillrect
   //SetDCBrushColor
   //CreateSolidBrush
   TRttiUtils.SetRttiPropertyValue(AComponent,'CloseButton.BackgroundColor', AColorMap.MenuColor);
   TRttiUtils.SetRttiPropertyValue(AComponent,'CloseButton.Transparent', False);
   TRttiUtils.SetRttiPropertyValue(AComponent,'DropDownButton.BackgroundColor', AColorMap.MenuColor);
   TRttiUtils.SetRttiPropertyValue(AComponent,'DropDownButton.Transparent', False);

   TRttiUtils.SetRttiPropertyValue(AComponent,'LeftButton.BackgroundColor', AColorMap.MenuColor);
   TRttiUtils.SetRttiPropertyValue(AComponent,'LeftButton.Transparent', False);
   TRttiUtils.SetRttiPropertyValue(AComponent,'RightButton.BackgroundColor', AColorMap.MenuColor);
   TRttiUtils.SetRttiPropertyValue(AComponent,'RightButton.Transparent', False);

   TRttiUtils.SetRttiPropertyValue(AComponent,'Brush.Color', AColorMap.MenuColor);
end;

{ TWrapperGradientTabSet }

procedure TWrapperGradientTabSet.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveStart', AColorMap.Color);//AColorMap.HighlightColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveEnd', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveStart', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveEnd', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);

  TRttiUtils.SetRttiPropertyValue(AComponent,'FScroller.FLeftButton.BackgroundColor', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FScroller.FLeftButton.Transparent', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FScroller.FRightButton.BackgroundColor', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FScroller.FRightButton.Transparent', False);
end;

{ TWrapperIDEGradientTabSet }

procedure TWrapperIDEGradientTabSet.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveStart', AColorMap.Color);//AColorMap.HighlightColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveEnd', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveStart', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveEnd', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperTabSheet }

procedure TWrapperTabSheet.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  with TTabSheet(AComponent) do
  begin
     //Color:=AColorMap.Color;
     Font.Color:=AColorMap.FontColor;
  end
end;

{ TWrapperActionMainMenuBar }

procedure TWrapperActionMainMenuBar.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  LColorMap      : TCustomActionBarColorMap;
begin
  inherited;
  with TActionMainMenuBar(AComponent) do
  begin
    LColorMap:=TXPColorMap.Create(AComponent);
    LColorMap.Assign(AColorMap);
    LColorMap.OnColorChange:=nil;
    ColorMap:=LColorMap;
    AnimationStyle  := asFade;
    AnimateDuration := 1200;
    Shadows         := True;
    Font.Color      := AColorMap.FontColor;
  end

end;

{ TWrapperControlBar }

procedure TWrapperControlBar.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  with TControlBar(AComponent) do
  begin
    Color := AColorMap.Color;
    DrawingStyle := TBandDrawingStyle.dsGradient;
    GradientStartColor :=  AColorMap.Color;
    GradientEndColor   :=  AColorMap.Color;
  end;
end;

{ TWrapperPopupActionBar }

procedure TWrapperPopupActionBar.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);

    procedure ImageListAdd(LImages : TImageList;Index: Integer;const ResourceName: String);
    {$IFDEF DELPHI2009_UP}
    var
     LPngImage: TPngImage;
     LBitMap: TBitmap;
    {$ENDIF}
    begin
    {$IFDEF DELPHI2009_UP}
      LPngImage:=TPNGImage.Create;
      try
        LPngImage.LoadFromResourceName(HInstance, ResourceName);
        LBitMap:=TBitmap.Create;
        try
          LPngImage.AssignTo(LBitMap);
          LBitMap.AlphaFormat:=afDefined;
          LImages.Add(LBitMap, nil);
        finally
          LBitMap.Free;
        end;
      finally
        LPngImage.free;
      end;
    {$ENDIF}
    end;

    procedure ImageListReplace(LImages : TImageList;Index: Integer;const ResourceName: String);
    {$IFDEF DELPHI2009_UP}
    var
     LPngImage: TPngImage;
     LBitMap: TBitmap;
    {$ENDIF}
    begin
    {$IFDEF DELPHI2009_UP}
      LPngImage:=TPNGImage.Create;
      try
        LPngImage.LoadFromResourceName(HInstance, ResourceName);
        LBitMap:=TBitmap.Create;
        try
          LPngImage.AssignTo(LBitMap);
          LBitMap.AlphaFormat:=afDefined;
          LImages.Replace(Index, LBitMap, nil);
        finally
          LBitMap.Free;
        end;
      finally
        LPngImage.free;
      end;
    {$ENDIF}
    end;
var
  LImages        : TImageList;
  i              : Integer;
begin
  inherited;
  if (TColorizerLocalSettings.Settings.ChangeIconsGutter) and not (TColorizerLocalSettings.ImagesGutterChanged) and SameText('ModuleMenu', AComponent.Name) then
  begin
    LImages:=TImageList(TPopupActionBar(AComponent).Images);
    if (LImages<>nil) and (LImages.Count>=27) then
     begin
      LImages.Clear;
      {$IF CompilerVersion > 20}
      LImages.ColorDepth:=TColorDepth.cd32Bit;
      {$IFEND}
      LImages.DrawingStyle:=dsNormal;
      LImages.Width:=15;
      LImages.Height:=15;

      for i:= 0 to 26 do
       ImageListAdd(LImages, i, Format('p%.2d',[i]));

       TPopupActionBar(AComponent).Images:=LImages;
       TColorizerLocalSettings.ImagesGutterChanged:=True;
     end;
  end;
end;

{ TWrapperActionToolBar }

procedure TWrapperActionToolBar.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  LColorMap      : TCustomActionBarColorMap;
begin
  inherited;
  with TActionToolBar(AComponent) do
  begin
    LColorMap:=TXPColorMap.Create(AComponent);
    LColorMap.Assign(AColorMap);
    LColorMap.OnColorChange:=nil;
    ColorMap:=LColorMap;
  end;
end;


{ TWrapperStatusBar }

procedure TWrapperStatusBar.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  i : Integer;
begin
  inherited;
  with TStatusBar(AComponent) do
  begin
     //SizeGrip is removed because can't be painted
     SizeGrip:=False;
     Color   := AColorMap.Color;
     //remove the bevels
     for i := 0 to TStatusBar(AComponent).Panels.Count-1 do
      TStatusBar(AComponent).Panels[i].Bevel:=pbNone;

     Font.Color:=AColorMap.FontColor;
  end;

end;

{ TWrapperLists }

procedure TWrapperLists.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperGroupComponents }

procedure TWrapperGroupComponents.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'ParentBackGround', True);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperFontComponents }

procedure TWrapperFontComponents.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

initialization
  TRegisteredWrappers.Wrappers:=TDictionary<string, TBaseWrapperClass>.Create;
  TRegisteredWrappers.WrappersInstances:=TObjectDictionary<string, TBaseWrapper>.Create([doOwnsValues]);

  RegisterColorizerWrapper('TVirtualStringTree',  TWrapperVirtualStringTree);
  RegisterColorizerWrapper('TBetterHintWindowVirtualDrawTree',  TWrapperVirtualStringTree);

  RegisterColorizerWrapper('TIDECategoryButtons',  TWrapperIDECategoryButtons);
  RegisterColorizerWrapper('TDisassemblerView',  TWrapperDisassemblerView);
  RegisterColorizerWrapper('TTDStringGrid',  TWrapperTDStringGrid);

  RegisterColorizerWrapper('TRegisterView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TFlagsView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TDumpView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TFPURegisterView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TXMMRegisterView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TCPUStackView',  TWrapperDeguggerWindows);

  RegisterColorizerWrapper('TDesktopComboBox',  TWrapperIDEComboBox);
  RegisterColorizerWrapper('THistoryPropComboBox',  TWrapperIDEComboBox);

  //RegisterColorizerWrapper('TComboBox',  TWrapperComboBox);  // TODO : Add own wrapper
  RegisterColorizerWrapper('TEdit',  TWrapperSimpleControl);
  RegisterColorizerWrapper('TPropCheckBox',  TWrapperSimpleControl);
  RegisterColorizerWrapper('TEditorDockPanel',  TWrapperSimpleControl);
  RegisterColorizerWrapper('TPanel',  TWrapperPanel);
  RegisterColorizerWrapper('TInspListBox',  TWrapperInspListBox);
  RegisterColorizerWrapper('TStringGrid',  TWrapperStringGrid);
  RegisterColorizerWrapper('TRefactoringTree',  TWrapperRefactoringTree);
  RegisterColorizerWrapper('TCodeEditorTabControl',  TWrapperCodeEditorTabControl);
  RegisterColorizerWrapper('TEditControl',  TWrapperEditControl);

  RegisterColorizerWrapper('TToolBar',  TWrapperToolBar);
  RegisterColorizerWrapper('TDockToolBar',  TWrapperToolBar);
  RegisterColorizerWrapper('TCnSrcEditorToolBar',  TWrapperToolBar);//cnwizards toolbar

  RegisterColorizerWrapper('TTabSet',  TWrapperTabSet);
  RegisterColorizerWrapper('TIDEDockTabSet',  TWrapperTabSet); //TIDEDockTabSet->TDockTabSet->TTabSet


  RegisterColorizerWrapper('TClosableTabScroller',  TWrapperClosableTabScroller);
  RegisterColorizerWrapper('TGradientTabSet',  TWrapperGradientTabSet);
  RegisterColorizerWrapper('TIDEGradientTabSet',  TWrapperIDEGradientTabSet);
  RegisterColorizerWrapper('TTabSheet',  TWrapperTabSheet);
  RegisterColorizerWrapper('TActionMainMenuBar',  TWrapperActionMainMenuBar);
  RegisterColorizerWrapper('TControlBar',  TWrapperControlBar);
  RegisterColorizerWrapper('TPopupActionBar',  TWrapperPopupActionBar);
  //RegisterColorizerWrapper('TActionToolBar',  TWrapperActionToolBar); //not need it
  RegisterColorizerWrapper('TStatusBar',  TWrapperStatusBar);

  RegisterColorizerWrapper('TListView',  TWrapperLists);
  RegisterColorizerWrapper('TTreeView',  TWrapperLists);
  RegisterColorizerWrapper('TListBox',  TWrapperLists);
  RegisterColorizerWrapper('TCheckListBox',  TWrapperLists);
  RegisterColorizerWrapper('TExplorerCheckListBox',  TWrapperLists);
  RegisterColorizerWrapper('THintListView',  TWrapperLists);

  RegisterColorizerWrapper('TGroupBox',  TWrapperGroupComponents);
  RegisterColorizerWrapper('TRadioGroup',  TWrapperGroupComponents);
  RegisterColorizerWrapper('TPropRadioGroup',  TWrapperGroupComponents);

  RegisterColorizerWrapper('TLabel',  TWrapperFontComponents);
  RegisterColorizerWrapper('TCheckBox',  TWrapperFontComponents);

finalization
  TRegisteredWrappers.Wrappers.Free;
  TRegisteredWrappers.WrappersInstances.Free;
end.
