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
  Rtti,
  Forms,
  ActnMan,
  Generics.Collections,
  StdCtrls,
  Controls,
  Windows,
  Graphics,
  Classes;

   function RunWrapper(AComponent : TComponent; AColorMap:TCustomActionBarColorMap; Invalidate : Boolean = False; Restore : Boolean = False) : Boolean;

type
   TBaseWrapper = class (TComponent)
   private
    FRestore: Boolean;
    procedure SetFlatParent(AComponent: TComponent);
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); virtual;
    property  Restore : Boolean read FRestore Write FRestore;
   public
    constructor Create(AOwner: TComponent); override;
   end;

  TBaseWrapperClass = class of TBaseWrapper;
  TRegisteredWrappers = class
  public
    class var Wrappers          : TDictionary<string, TBaseWrapperClass>;
    class var WrappersInstances : TObjectDictionary<string, TBaseWrapper>;
  end;

  TRttiWrapper = class
  private
    LContext : TRttiContext;
    RootType : TRttiType;
   public
    constructor Create(AObject : TObject);
    destructor Destroy; override;
  end;

  TRttiBaseVirtualTree  = class(TRttiWrapper)
   private
    FVirtualTree : TCustomControl;
    FMinusBM     : TBitmap;
    FPlusBM      : TBitmap;
    function GetDottedBrush: HBRUSH;
    procedure SetDottedBrush(const Value: HBRUSH);
   public
    constructor Create(BaseVirtualTree : TCustomControl); reintroduce;
    property VirtualTree : TCustomControl read FVirtualTree;
    property MinusBM: TBitmap read FMinusBM;
    property PlusBM: TBitmap read FPlusBM;
    property DottedBrush : HBRUSH read GetDottedBrush write SetDottedBrush;
  end;

  TRttiListButton  = class(TRttiWrapper)
   private
    FListButton  : TCustomControl;
    FPopupPanel: TCustomForm;
    FItems: TStrings;
    FListBox: TListBox;
    FMaxListWidth: Integer;
    FMinListWidth: Integer;
    FItemIndex: Integer;
    FListWidth: Integer;
    FItemCount: Integer;
    FSelectString: string;
   public
    procedure LoadValues;
    constructor Create(ListButton : TCustomControl); reintroduce;
    property ListButton : TCustomControl read FListButton;
    property ListBox : TListBox read FListBox;
    property PopupPanel: TCustomForm read FPopupPanel;
    property Items: TStrings read FItems;
    property MaxListWidth: Integer read FMaxListWidth;
    property MinListWidth: Integer read FMinListWidth;
    property ItemIndex: Integer read FItemIndex;
    property ListWidth: Integer read FListWidth;
    property ItemCount: Integer read FItemCount;
    property SelectString : string read FSelectString;
  end;

implementation

uses
  uRttiHelper,
  SysUtils,
  ExtCtrls,
  Buttons,
  ComCtrls,
  ImgList,
  Tabs,
  CategoryButtons,
  ActnColorMaps,
  ActnPopup,
  ActnMenus,
  UxTheme,
  GraphUtil,
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

   TWrapperVirtualStringTree = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperIDECategoryButtons = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperDisassemblerView = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperTDStringGrid = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperDeguggerWindows = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperIDEComboBox = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperSimpleControl = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperPanel = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperInspListBox = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperStringGrid = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperRefactoringTree = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperCodeEditorTabControl = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperEditControl = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperToolBar = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperComponentToolbarFrame = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperTabSet = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperClosableTabScroller = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperGradientTabSet = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperIDEGradientTabSet = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperTabSheet = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperActionMainMenuBar = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperPopupActionBar = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperActionToolBar = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperControlBar = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperStatusBar = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperLists = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperGroupComponents  = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperFontComponents  = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperDescriptionPane = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperHotCommands = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;


   TWrapperGradientButton = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperCategoriesPopUp = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperPropCheckBox = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
   end;

   TWrapperListButton = class(TBaseWrapper)
   protected
    procedure SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap); override;
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

function RunWrapper(AComponent : TComponent; AColorMap:TCustomActionBarColorMap; Invalidate : Boolean = False; Restore : Boolean = False) : Boolean;
var
  LBaseWrapper : TBaseWrapper;
begin
  Result:=False;
  if TRegisteredWrappers.WrappersInstances.ContainsKey(AComponent.ClassName) then
  begin
//    if Restore then
//      AddLog('RunWrapper', AComponent.ClassName);
    LBaseWrapper:= TRegisteredWrappers.WrappersInstances.Items[AComponent.ClassName];
    LBaseWrapper.Restore:=Restore;
    LBaseWrapper.SetProperties(AComponent, AColorMap);
//    if AComponent is TWinControl then
//      TWinControl(AComponent).Invalidate();
    Result:=True;
  end
  else
   ;// AddLog('RunWrapper Igonored', AComponent.ClassName);
//  if SameText(AComponent.ClassName, 'TListButton') then
//     TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'.pas');
end;

{ TWrapperVirtualStringTree }
procedure TWrapperVirtualStringTree.SetProperties(AComponent : TComponent; AColorMap:TCustomActionBarColorMap);
begin
  inherited;
  //TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\'+AComponent.ClassName+'.pas');
  //AddLog('TWrapperVirtualStringTree', AComponent.ClassName);

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

constructor TBaseWrapper.Create(AOwner: TComponent);
begin
  inherited;
  FRestore:=False;
end;

procedure TBaseWrapper.SetProperties(AComponent: TComponent;
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
     //TPanel(LParent).BevelInner  :=TBevelCut.bvNone;
     //TPanel(LParent).BevelOuter  :=TBevelCut.bvNone;
     TPanel(LParent).Color       := TColorizerLocalSettings.ColorMap.Color;
     //TPanel(LParent).Ctl3D       := False;
     //TPanel(LParent).BevelKind   := TBevelKind.bkNone; //works for TEditControl
     //TPanel(LParent).BorderStyle := bsNone;   //works for tookbar
    end;
  until (LParent=nil) or not (LParent Is TPanel);
end;

{ TWrapperIDECategoryButtons }

procedure TWrapperIDECategoryButtons.SetProperties(AComponent: TComponent;
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

procedure TWrapperDisassemblerView.SetProperties(AComponent: TComponent;
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

procedure TWrapperTDStringGrid.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  //TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'GradientStartColor', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'GradientEndColor', AColorMap.Color);
end;

{ TWrapperDeguggerWindows }

procedure TWrapperDeguggerWindows.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  //SetFlatParent(AComponent);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color',AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
end;

{ TWrapperComboBox }

type
 TCustomComboBoxClass = class(TCustomComboBox);

procedure TWrapperIDEComboBox.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
//var
//  LCustomComboBox : TCustomComboBoxClass;
begin
  inherited;
//  LCustomComboBox:=TCustomComboBoxClass(AComponent);
//  if GetWindowTheme(TWinControl(AComponent).Handle) <>0 then
//    SetWindowTheme(TWinControl(AComponent).Handle, '', '');

//  LCustomComboBox.Color      := AColorMap.MenuColor;
//  LCustomComboBox.Font.Color := AColorMap.FontColor;

  //LCustomComboBox.BevelKind  := bkFlat;
  //LCustomComboBox.BevelInner := bvNone;
  //LCustomComboBox.Ctl3D      := False;
end;

{ TWrapperStandardControl }

procedure TWrapperSimpleControl.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  //AddLog('TWrapperSimpleControl', AComponent.ClassName);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperPanel }

procedure TWrapperPanel.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
  LPanel : TPanel;
begin
  inherited;
  LPanel        := TPanel(AComponent);
  LPanel.Color  := AColorMap.Color;
  LPanel.Font.Color   := AColorMap.FontColor;
end;

{ TWrapperInspListBox }

procedure TWrapperInspListBox.SetProperties(AComponent: TComponent;
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

procedure TWrapperStringGrid.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperRefactoringTree }

procedure TWrapperRefactoringTree.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
end;

{ TWrapperCodeEditorTabControl }

procedure TWrapperCodeEditorTabControl.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'UnselectedColor',AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'SelectedColor',AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'BackgroundColor',AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
end;

{ TWrapperEditControl }

procedure TWrapperEditControl.SetProperties(AComponent: TComponent; AColorMap: TCustomActionBarColorMap);
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

procedure TWrapperToolBar.SetProperties(AComponent: TComponent;  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  SetFlatParent(AComponent);
  with TToolBar(AComponent) do
  begin
    Color              := AColorMap.Color;
    if Restore then
      DrawingStyle       := TTBDrawingStyle(dsNormal)
    else
     DrawingStyle       := TTBDrawingStyle.dsGradient;

    if TColorizerLocalSettings.Settings.ToolbarGradientHor then
      GradientDirection:=TGradientDirection.gdHorizontal
    else
      GradientDirection:=TGradientDirection.gdVertical;

      if TColorizerLocalSettings.Settings.ToolbarCustomColors and not Restore then
      begin
         try  GradientStartColor := StringToColor(TColorizerLocalSettings.Settings.ToolbarStartGrad) except GradientStartColor :=AColorMap.Color end;
         try  GradientEndColor := StringToColor(TColorizerLocalSettings.Settings.ToolbarEndGrad) except GradientEndColor :=AColorMap.Color end;
      end
      else
      begin
        GradientStartColor := AColorMap.Color;
        GradientEndColor   := AColorMap.Color;
      end;

    HotTrackColor      := AColorMap.SelectedColor;
    Font.Color         := AColorMap.FontColor;
    EdgeBorders        := [];
    Ctl3D:=False;
    //Flat:=True;
  end;

end;

{ TWrapperTabSet }

procedure TWrapperTabSet.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  with TTabSet(AComponent) do
  begin
    BackgroundColor:= AColorMap.MenuColor;
    SelectedColor  := AColorMap.Color;
    UnselectedColor:= AColorMap.MenuColor;
    Font.Color     := AColorMap.FontColor;
    Style          := tsModernTabs; //necessary for allow use the TTabset hook
  end

end;

{ TWrapperClosableTabScroller }

procedure TWrapperClosableTabScroller.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
   inherited;
   //SetBkColor
   //Canvas.Fillrect
   //SetDCBrushColor
   //CreateSolidBrush

   //TRttiUtils.DumpObject(TRttiUtils.GetRttiPropertyValue(AComponent,'CloseButton').AsObject, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\TGradientButton.pas');

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

procedure TWrapperGradientTabSet.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
//var
//  LCanvas : TCanvas;
//  LRect   : TRect;
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveStart', AColorMap.Color);//AColorMap.HighlightColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.ActiveEnd', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveStart', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'TabColors.InActiveEnd', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);

{
    00107A70 5253 0800 __fastcall Gdiplus::Gradienttabs::TTabInfo::TTabInfo(int, int, int, int)
    00105898 5279 0801 Gdiplus::Gradienttabs::TTabScroller::
    0010768C 5258 0802 __fastcall Gdiplus::Gradienttabs::TTabScroller::TTabScroller(System::Classes::TComponent *)
    001078C4 5256 0803 __fastcall Gdiplus::Gradienttabs::TTabScroller::GetSuggestedWidth(bool)
    001078E4 5255 0804 __fastcall Gdiplus::Gradienttabs::TTabScroller::HideWhenNoScrollbarsShown()
    001078E8 5254 0805 __fastcall Gdiplus::Gradienttabs::TTabScroller::Paint()
    00107878 5257 0806 __fastcall Gdiplus::Gradienttabs::TTabScroller::SetShowScrollers(const const bool)

    @Gdiplus@Gradienttabs@TTabScroller@
    @Gdiplus@Gradienttabs@TTabScroller@$bctr$qqrp25System@Classes@TComponent
    @Gdiplus@Gradienttabs@TTabScroller@GetSuggestedWidth$qqro
    @Gdiplus@Gradienttabs@TTabScroller@HideWhenNoScrollbarsShown$qqrv
    @Gdiplus@Gradienttabs@TTabScroller@Paint$qqrv
    @Gdiplus@Gradienttabs@TTabScroller@SetShowScrollers$qqrxo
}
  //TRttiUtils.SetRttiFieldValue(AComponent,'Scroller.FColor', AColorMap.Color);
  //TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.Color', AColorMap.Color);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.ParentBackground', True);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.LeftButton.BackgroundColor', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.LeftButton.Transparent', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.RightButton.BackgroundColor', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.RightButton.Transparent', False);

  TRttiUtils.SetRttiPropertyValue(AComponent,'Scroller.Brush.Color', AColorMap.Color);

//  LCanvas:= TCanvas(TRttiUtils.GetRttiFieldValue(AComponent, 'Scroller.FCanvas').AsObject);
//  if LCanvas<>nil then
//  begin
//     LRect:=TRttiUtils.GetRttiPropertyValue(AComponent, 'Scroller.ClientRect').AsType<TRect>;
//     LCanvas.Brush.Color:=AColorMap.Color;
//     AddLog('Foo', 'Foo');
//     LCanvas.FillRect(LRect);
//     AddLog('Bar', 'Bar');
//  end;


end;

{ TWrapperIDEGradientTabSet }

procedure TWrapperIDEGradientTabSet.SetProperties(AComponent: TComponent;
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

procedure TWrapperTabSheet.SetProperties(AComponent: TComponent;
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

procedure TWrapperActionMainMenuBar.SetProperties(AComponent: TComponent;
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

procedure TWrapperControlBar.SetProperties(AComponent: TComponent;
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

procedure TWrapperPopupActionBar.SetProperties(AComponent: TComponent;
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
        LPngImage.Free;
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

procedure TWrapperActionToolBar.SetProperties(AComponent: TComponent;
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

procedure TWrapperStatusBar.SetProperties(AComponent: TComponent;
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

procedure TWrapperLists.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperGroupComponents }

procedure TWrapperGroupComponents.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'ParentBackGround', True);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperFontComponents }

procedure TWrapperFontComponents.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperDescriptionPane }

procedure TWrapperDescriptionPane.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  //TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\TDescriptionPane.pas');

  //TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  //TRttiUtils.SetRttiPropertyValue(AComponent,'Ctl3D', False);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'DescriptionFont.Color', AColorMap.FontColor);

//     TPanel(AComponent).BevelInner  := TBevelCut.bvNone;
//     TPanel(AComponent).BevelOuter  := TBevelCut.bvNone;
     TPanel(AComponent).Color       := TColorizerLocalSettings.ColorMap.Color;
     TPanel(AComponent).Ctl3D       := False;
//     TPanel(AComponent).BevelKind   := TBevelKind.bkNone;
//     TPanel(AComponent).BorderStyle := bsNone;

  //TRttiUtils.SetRttiPropertyValue(AComponent,'BevelKind', TValue.From(TBevelKind.bkNone));
  //TRttiUtils.SetRttiPropertyValue(AComponent,'BorderStyle', TValue.From(TFormBorderStyle.bsNone));

end;

{ TWrapperHotCommands }

procedure TWrapperHotCommands.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  //TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\THotCommands.pas');
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperGradientButton }

procedure TWrapperGradientButton.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
//var
// lBitMap : TBitmap;
begin
{
    00106EF0 5272 07C3 __fastcall Gdiplus::Gradienttabs::TGradientButton::TGradientButton(System::Classes::TComponent *)
    00106FFC 5268 07C4 __fastcall Gdiplus::Gradienttabs::TGradientButton::PaintOutline()
    00106F50 5271 07C5 __fastcall Gdiplus::Gradienttabs::TGradientButton::SetBackgroundColor(System::Uitypes::TColor)
    00106F58 5270 07C6 __fastcall Gdiplus::Gradienttabs::TGradientButton::SetFillColor()
    00106FAC 5269 07C7 __fastcall Gdiplus::Gradienttabs::TGradientButton::SetPenColor()


    001B23C8 5292 09E3 Idegradientspeedbuttons::SpeedButtonData
    00108FCC 5300 09E4 Idegradientspeedbuttons::TCloseButton::
    00109514 5291 09E5 __fastcall Idegradientspeedbuttons::TCloseButton::PaintSymbol()
    00108E58 5302 09E6 Idegradientspeedbuttons::TCustomGradientButton::
    00109A64 5288 09E7 __fastcall Idegradientspeedbuttons::TCustomGradientButton::Paint()
    001093E8 5294 09E8 Idegradientspeedbuttons::TCycleButton::
    00109A7C 5287 09E9 __fastcall Idegradientspeedbuttons::TCycleButton::PaintSymbol()
    0010912C 5298 09EA Idegradientspeedbuttons::TDownButton::
    0010973C 5290 09EB __fastcall Idegradientspeedbuttons::TDownButton::PaintSymbol()
    00108D18 5304 09EC Idegradientspeedbuttons::TSpeedButtonData::
    0010928C 5296 09ED Idegradientspeedbuttons::TUpButton::
    001098BC 5289 09EE __fastcall Idegradientspeedbuttons::TUpButton::PaintSymbol()
    001AEA00 5285 09EF __fastcall Idegradientspeedbuttons::initialization()
    00084DE0 2596 09F0 __fastcall Idegraphimpl::Finalization()
}
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'BackgroundColor', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Transparent', False);

  //AddLog('FGlyph',TRttiUtils.GetRttiFieldValue(AComponent,'FGlyph').AsObject.ClassName);  //TButtonGlyph


//  lBitMap := TBitmap(TRttiUtils.GetRttiPropertyValue(AComponent,'Glyph').AsObject);
//  if lBitMap<>nil then
//   lBitMap.LoadFromFile('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Test.bmp');
//
//  TRttiUtils.SetRttiPropertyValue(AComponent,'NumGlyphs', 1);

   //lBitMap.SaveToFile('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Glyph'+AComponent.Name+'.bmp');

end;



{ TWrapperCategoriesPopUp }

procedure TWrapperCategoriesPopUp.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
var
 LComponent : TComponent;
begin
  inherited;
  LComponent:=TComponent(TRttiUtils.GetRttiPropertyValue(AComponent, 'CategoryButtons').AsObject);
  if (LComponent<>nil) then
    TRegisteredWrappers.WrappersInstances['TIDECategoryButtons'].SetProperties(LComponent, AColorMap);

  //CategoryButtons
  //TRttiUtils.DumpObject(AComponent, 'C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\Galileo\TCategoriesPopup.pas');
end;

{ TWrapperPropCheckBox }

procedure TWrapperPropCheckBox.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  //necesary to allow use a hook with the DrawFrameControl and set color of font
  if GetWindowTheme(TWinControl(AComponent).Handle) <>0 then
    SetWindowTheme(TWinControl(AComponent).Handle, '', '');

  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  TRttiUtils.SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
end;

{ TWrapperListButton }

procedure TWrapperListButton.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
//var
//  ListBox: TListBox;
//  PopupPanel: TCustomForm;
begin
  inherited;
  //TRttiUtils.SetRttiPropertyValue(AComponent,'PopupPanel.Color', AColorMap.Color);
  //TRttiUtils.SetRttiPropertyValue(AComponent,'ListBox.Color', AColorMap.MenuColor);
{
   property ListBox: TListBox;
   property PopupPanel: TCustomForm;
   property Items: TStrings;
}
//   ListBox:=TRttiUtils.GetRttiPropertyValue(AComponent,'ListBox').AsType<TListBox>;
//   PopupPanel:=TRttiUtils.GetRttiPropertyValue(AComponent,'PopupPanel').AsType<TCustomForm>;
//   AddLog('TWrapperListButton', 'PopupPanel nil = '+BoolToStr(PopupPanel=nil, True));
//   AddLog('TWrapperListButton', 'ListBox    nil = '+BoolToStr(ListBox=nil, True));
//
//   AddLog('TWrapperListButton', IntToHex(TCustomControl(AComponent).Handle, 8));
//   AddLog('TWrapperListButton', TRttiUtils.GetRttiFieldValue(AComponent,'FSelectString').AsString);       //FSelectString
end;

{ TWrapperComponentToolbarFrame }

procedure TWrapperComponentToolbarFrame.SetProperties(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  TRttiUtils.SetRttiPropertyValue(AComponent,'Color', AColorMap.Color);
  //TRttiUtils.SetRttiPropertyValue(AComponent,'TabControl.Scroller.', AColorMap.Color);
end;

{ TRttiWrapper }

constructor TRttiWrapper.Create(AObject : TObject);
begin
  inherited Create;
  LContext := TRttiContext.Create;
  RootType := LContext.GetType(AObject.ClassInfo);
end;

destructor TRttiWrapper.Destroy;
begin
  LContext.Free;
  inherited;
end;

{ TRttiBaseVirtualTree }

constructor TRttiBaseVirtualTree.Create(BaseVirtualTree: TCustomControl);
begin
  inherited Create(BaseVirtualTree);
  FVirtualTree:=BaseVirtualTree;
//  FHotMinusBM :=TBitmap(RootType.GetProperty('HotMinusBM').GetValue(FVirtualTree).AsObject);
//  FHotPlusBM  :=RootType.GetProperty('HotPlusBM').GetValue(FVirtualTree).AsType<TBitmap>;
  FMinusBM    :=RootType.GetField('FMinusBM').GetValue(FVirtualTree).AsType<TBitmap>;
  FPlusBM     :=RootType.GetField('FPlusBM').GetValue(FVirtualTree).AsType<TBitmap>;
end;

function TRttiBaseVirtualTree.GetDottedBrush: HBRUSH;
begin
  Result:=RootType.GetField('FDottedBrush').GetValue(FVirtualTree).AsType<HBRUSH>;
end;

procedure TRttiBaseVirtualTree.SetDottedBrush(const Value: HBRUSH);
begin
 RootType.GetField('FDottedBrush').SetValue(FVirtualTree, TValue.From<HBRUSH>(Value));
end;

{ TRttiListButton }

constructor TRttiListButton.Create(ListButton: TCustomControl);
begin
  inherited Create(ListButton);
  FListButton   := ListButton;
  LoadValues();
end;

procedure TRttiListButton.LoadValues;
begin
  FPopupPanel   := RootType.GetProperty('PopupPanel').GetValue(FListButton).AsType<TCustomForm>;
  FItems        := RootType.GetProperty('Items').GetValue(FListButton).AsType<TStrings>;
  FListBox      := RootType.GetProperty('ListBox').GetValue(FListButton).AsType<TListBox>;
  FMinListWidth := RootType.GetProperty('MinListWidth').GetValue(FListButton).AsInteger;
  FMaxListWidth := RootType.GetProperty('MaxListWidth').GetValue(FListButton).AsInteger;
  FItemIndex    := RootType.GetProperty('ItemIndex').GetValue(FListButton).AsInteger;
  FListWidth    := RootType.GetProperty('ListWidth').GetValue(FListButton).AsInteger;
  FItemCount    := RootType.GetProperty('ItemCount').GetValue(FListButton).AsInteger;
  FSelectString := RootType.GetField('FSelectString').GetValue(FListButton).AsString;
end;

initialization
  TRegisteredWrappers.Wrappers:=TDictionary<string, TBaseWrapperClass>.Create;
  TRegisteredWrappers.WrappersInstances:=TObjectDictionary<string, TBaseWrapper>.Create([doOwnsValues]);


  RegisterColorizerWrapper('TComponentToolbarFrame',  TWrapperComponentToolbarFrame);


  RegisterColorizerWrapper('TVirtualStringTree',  TWrapperVirtualStringTree);
  RegisterColorizerWrapper('TBetterHintWindowVirtualDrawTree',  TWrapperVirtualStringTree);

  RegisterColorizerWrapper('TIDECategoryButtons',  TWrapperIDECategoryButtons);
  RegisterColorizerWrapper('TDisassemblerView',  TWrapperDisassemblerView);
  RegisterColorizerWrapper('TTDStringGrid',  TWrapperTDStringGrid);

  RegisterColorizerWrapper('TCategoriesPopup',  TWrapperCategoriesPopUp);

  RegisterColorizerWrapper('TRegisterView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TFlagsView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TDumpView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TFPURegisterView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TXMMRegisterView',  TWrapperDeguggerWindows);
  RegisterColorizerWrapper('TCPUStackView',  TWrapperDeguggerWindows);

  RegisterColorizerWrapper('TDescriptionPane',  TWrapperDescriptionPane);
  RegisterColorizerWrapper('THotCommands',  TWrapperHotCommands);

  RegisterColorizerWrapper('TComboBox',  TWrapperIDEComboBox);
  RegisterColorizerWrapper('TDesktopComboBox',  TWrapperIDEComboBox);
  RegisterColorizerWrapper('THistoryPropComboBox',  TWrapperIDEComboBox);
  RegisterColorizerWrapper('TCnToolBarComboBox',  TWrapperIDEComboBox);//cnwizards combobox
  //RegisterColorizerWrapper('TCnProcListComboBox',  TWrapperIDEComboBox);//cnwizards combobox

  RegisterColorizerWrapper('TCloseButton',  TWrapperGradientButton);
  RegisterColorizerWrapper('TGradientButton',  TWrapperGradientButton);

  RegisterColorizerWrapper('TPropCheckBox',  TWrapperPropCheckBox);

  RegisterColorizerWrapper('TEdit',  TWrapperSimpleControl);
  RegisterColorizerWrapper('TButtonedEdit',  TWrapperSimpleControl);
  RegisterColorizerWrapper('TEditorDockPanel',  TWrapperSimpleControl);
  RegisterColorizerWrapper('TPopupListBox',  TWrapperSimpleControl);

  RegisterColorizerWrapper('TPanel',  TWrapperPanel);
  RegisterColorizerWrapper('TInspListBox',  TWrapperInspListBox);
  RegisterColorizerWrapper('TStringGrid',  TWrapperStringGrid);
  RegisterColorizerWrapper('TRefactoringTree',  TWrapperRefactoringTree);
  RegisterColorizerWrapper('TCodeEditorTabControl',  TWrapperCodeEditorTabControl);
  RegisterColorizerWrapper('TEditControl',  TWrapperEditControl);

  RegisterColorizerWrapper('TToolBar',  TWrapperToolBar);
  RegisterColorizerWrapper('TDockToolBar',  TWrapperToolBar);
  RegisterColorizerWrapper('TCnSrcEditorToolBar',  TWrapperToolBar);//cnwizards toolbar
  RegisterColorizerWrapper('TCnExternalSrcEditorToolBar',  TWrapperToolBar);//cnwizards toolbar
  RegisterColorizerWrapper('TGXToolBar',  TWrapperToolBar);//gexperts toolbar


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
  RegisterColorizerWrapper('TListButton',  TWrapperListButton);

finalization
  TRegisteredWrappers.Wrappers.Free;
  TRegisteredWrappers.WrappersInstances.Free;
end.
