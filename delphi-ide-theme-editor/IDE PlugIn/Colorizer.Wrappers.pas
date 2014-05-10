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

uses
  ActnMan,
  System.Classes;


   procedure RunWrapper(AComponent : TComponent; AColorMap:TCustomActionBarColorMap);

implementation

uses
  uRttiHelper,
  CategoryButtons,
  System.Generics.Collections,
  Colorizer.Utils;

type
   TBaseWrapper = class (TComponent)
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
    AddLog('Registred '+ClassWrapper.ClassName+' for '+ComponentClass);
    TRegisteredWrappers.Wrappers.Add(ComponentClass, ClassWrapper);
    LInstance:= TRegisteredWrappers.Wrappers.Items[ComponentClass];
    TRegisteredWrappers.WrappersInstances.Add(ComponentClass, LInstance.Create(nil));
  end;
end;

procedure RunWrapper(AComponent : TComponent; AColorMap:TCustomActionBarColorMap);
begin
  if TRegisteredWrappers.WrappersInstances.ContainsKey(AComponent.ClassName) then
  begin
    AddLog('RunWrapper '+AComponent.ClassName);
    TRegisteredWrappers.WrappersInstances.Items[AComponent.ClassName].SetColors(AComponent, AColorMap);
  end;
end;


{ TWrapperVirtualStringTree }
procedure TWrapperVirtualStringTree.SetColors(AComponent : TComponent; AColorMap:TCustomActionBarColorMap);
begin
  inherited;
        AddLog(Self.ClassName+' SetColors '+AComponent.ClassName);

        SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);  //ok
        SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor); //ok
        SetRttiPropertyValue(AComponent,'Header.Font.Color', AColorMap.FontColor); //ok
        SetRttiPropertyValue(AComponent,'Ctl3D', False); //ok

        SetRttiPropertyValue(AComponent,'Colors.TreeLineColor', AColorMap.FontColor); //ok

        SetRttiPropertyValue(AComponent,'Colors.SelectionRectangleBlendColor', AColorMap.SelectedColor);
        SetRttiPropertyValue(AComponent,'Colors.SelectionRectangleBorderColor', AColorMap.FrameTopLeftInner);

        SetRttiPropertyValue(AComponent,'Colors.FocusedSelectionColor', AColorMap.SelectedColor);   //ok
        SetRttiPropertyValue(AComponent,'Colors.FocusedSelectionBorderColor', AColorMap.FrameTopLeftInner);  //ok

        SetRttiPropertyValue(AComponent,'Colors.UnfocusedSelectionColor', AColorMap.DisabledColor);   //ok
        SetRttiPropertyValue(AComponent,'Colors.UnfocusedSelectionBorderColor', AColorMap.FrameTopLeftInner);  //ok

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

       {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles then
        begin
          if not IsStyleHookRegistered(AComponent.ClassType, TTreeViewStyleHook) then
           TStyleEngine.RegisterStyleHook(AComponent.ClassType, TTreeViewStyleHook);
        end;
       {$ENDIF}
end;

{ TBaseWrapper }

procedure TBaseWrapper.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin

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
  LCategoryButtons.Color:= AColorMap.MenuColor;
  LCategoryButtons.BackgroundGradientColor:= AColorMap.MenuColor;
  LCategoryButtons.ButtonOptions:= LCategoryButtons.ButtonOptions + [boGradientFill];
  LCategoryButtons.HotButtonColor:= AColorMap.HighlightColor;
  LCategoryButtons.SelectedButtonColor:= AColorMap.SelectedColor;
  for i := 0 to LCategoryButtons.Categories.Count-1 do
   begin
    LCategoryButtons.Categories[i].Color := AColorMap.Color;
    LCategoryButtons.Categories[i].TextColor := AColorMap.FontColor;
   end;

  LCategoryButtons.Font.Color:=AColorMap.FontColor;
end;

{ TWrapperDisassemblerView }

procedure TWrapperDisassemblerView.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;

  SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  SetRttiPropertyValue(AComponent,'BreakpointColor', AColorMap.SelectedColor);
  SetRttiPropertyValue(AComponent,'BreakpointTextColor', AColorMap.SelectedFontColor);
  SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);

  {
     property BreakpointColor: TColor;
     property BreakpointTextColor: TColor;
     property Color: TColor;
     property Constraints: TSizeConstraints;
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
  SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
  SetRttiPropertyValue(AComponent,'Ctl3D', False);
  SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
  SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
  SetRttiPropertyValue(AComponent,'GradientStartColor', AColorMap.Color);
  SetRttiPropertyValue(AComponent,'GradientEndColor', AColorMap.Color);
end;

{ TWrapperDeguggerWindows }

procedure TWrapperDeguggerWindows.SetColors(AComponent: TComponent;
  AColorMap: TCustomActionBarColorMap);
begin
  inherited;
  SetRttiPropertyValue(AComponent,'Color',AColorMap.MenuColor);
  SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
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

finalization
  TRegisteredWrappers.Wrappers.Free;
  TRegisteredWrappers.WrappersInstances.Free;
end.
