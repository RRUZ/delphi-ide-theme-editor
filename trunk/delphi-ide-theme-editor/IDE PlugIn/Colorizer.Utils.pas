//**************************************************************************************************
//
// Unit Colorizer.Utils
// unit Colorizer.Utils for the Delphi IDE Colorizer
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Colorizer.Utils.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2011-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************

unit Colorizer.Utils;

interface

{$I ..\Common\Jedi.inc}

uses
 {$IFDEF DELPHIXE2_UP}
 VCL.Themes,
 VCL.Styles,
 {$ENDIF}
 Classes,
 {$IFDEF DELPHI2009_UP}
 System.Generics.Collections,
 {$ENDIF}
 ActnMan,
 uDelphiVersions,
 ActnColorMaps,
 Windows,
 Graphics,
 Colorizer.Settings,
 ColorXPStyleActnCtrls;

{.$DEFINE ENABLELOG}

procedure AddLog(const msg : string);

procedure RefreshIDETheme(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;Restore : Boolean = False); overload;
procedure RefreshIDETheme; overload;
procedure RestoreIDESettings();

procedure LoadSettings(AColorMap:TCustomActionBarColorMap;ActionBarStyle : TActionBarStyle;Settings : TSettings);
procedure ProcessComponent(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;AComponent: TComponent;Restore : Boolean = False);
procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Color, FontColor:TColor);{$IF CompilerVersion >= 23}overload;{$IFEND}
{$IFDEF DELPHIXE2_UP}
procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Style:TCustomStyleServices);overload;
procedure RegisterVClStylesFiles;
{$ENDIF}


 type
   TColorizerLocalSettings = class
   public
      {$IFDEF DELPHI2009_UP}
      class var ActnStyleList : TList<TActionManager>;
      {$ENDIF}
      class var ColorMap       : TCustomActionBarColorMap;
      class var ActionBarStyle : TActionBarStyle;
      class var HookedWindows  : TStringList;
      class var VCLStylesPath  : string;
      class var Settings       : TSettings;
      class var ImagesGutterChanged : Boolean;
      class var IDEData        : TDelphiVersionData;
    end;

implementation

{.$DEFINE DEBUG_MODE}
{.$DEFINE DEBUG_PROFILER}

uses
 {$IFDEF DELPHIXE_UP}
 PlatformDefaultStyleActnCtrls,
 {$ELSE}
 XPStyleActnCtrls,
 {$ENDIF}
 {$IFDEF DELPHIXE2_UP}
 Vcl.Styles.Ext,
 {$ENDIF}
 {$IFDEF DELPHI2009_UP}
 PngImage,
 {$ENDIF}
 {$IFDEF DELPHI2010_UP}
 IOUtils,
 Rtti,
 {$ELSE}
 Variants,
 TypInfo,
 {$ENDIF}
 Types,
 Forms,
 Menus,
 Tabs,
 ComCtrls,
 Controls,
 StdCtrls,
 SysUtils,
 ExtCtrls,
 GraphUtil,
 UxTheme,
 CategoryButtons,
 ImgList,
 ActnCtrls,
 ActnPopup,
 ActnMenus,
 Colorizer.StoreColorMap,
 Dialogs,
 uMisc,
 uRttiHelper;


{$IFDEF DEBUG_MODE}
var
  lcomp         : TStringList;
{$ENDIF}


{$IFDEF DEBUG_PROFILER}
var
  lprofiler     : TStringList;
  lpignored     : TStringList;
  lDumped       : TStringList;
{$ENDIF}


{$IFDEF DELPHIXE2_UP}
procedure RegisterVClStylesFiles;
var
 sPath, FileName : string;
begin
  sPath:=TColorizerLocalSettings.VCLStylesPath;
  if SysUtils.DirectoryExists(sPath) then
  for FileName in TDirectory.GetFiles(sPath, '*.vsf') do
   if TStyleManager.IsValidStyle(FileName) then
    begin
       try
         TStyleManager.LoadFromFile(FileName);
       except
         on EDuplicateStyleException do
       end;
    end;
end;
{$ENDIF}

procedure AddLog(const msg : string);
begin
{$IFDEF ENABLELOG}
   TFile.AppendAllText('C:\Delphi\google-code\DITE\delphi-ide-theme-editor\IDE PlugIn\log.txt',Format('%s %s %s',[FormatDateTime('hh:nn:ss.zzz', Now),  msg, sLineBreak]));
{$ENDIF}
end;

{$IFDEF DEBUG_PROFILER}
//DumpType('GDIPlus.GradientDrawer.TGradientTabDrawer');
procedure DumpType(const QualifiedName:string);
var
  l2 : TStrings;
begin
  l2 := TStringList.Create;
  try
    l2.Text:=DumpTypeDefinition(TRttiContext.Create.FindType(QualifiedName).Handle);
    l2.SaveToFile(ExtractFilePath(GetBplLocation())+'Galileo\'+QualifiedName+'.pas');
  finally
   l2.Free;
  end;
end;

procedure DumpAllTypes;
var
l2 : TStrings;
t  : TRttiType;
begin
  l2 := TStringList.Create;
  try
    for t in TRttiContext.Create.GetTypes do
    if t.IsInstance then
     l2.Add(t.AsInstance.DeclaringUnitName +' '+t.Name);
   l2.SaveToFile(ExtractFilePath(GetBplLocation())+'Galileo\Types.txt');
  finally
   l2.Free;
  end;
end;


{$ENDIF}

procedure DumpObject(AObject: TObject);
var
 LDumpInfo : TStrings;
begin
   LDumpInfo := TStringList.Create;
  try
   LDumpInfo.Text:=DumpTypeDefinition(AObject.ClassInfo);
   LDumpInfo.SaveToFile(ExtractFilePath(GetModuleLocation())+'Galileo\'+AObject.ClassName+'.pas');
  finally
   LDumpInfo.Free;
  end;
end;

procedure RefreshIDETheme;
begin
   RefreshIDETheme(TColorizerLocalSettings.ColorMap, TColorizerLocalSettings.ActionBarStyle);
end;

procedure RefreshIDETheme(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;Restore : Boolean = False);
var
  index     : Integer;
begin
 {
  if GlobalSettings.EnableDWMColorization and DwmIsEnabled then
   SetCompositionColor(AColorMap.Color);
 }
  for Index := 0 to Screen.FormCount-1 do
  if TColorizerLocalSettings.HookedWindows.IndexOf(Screen.Forms[Index].ClassName)<>-1 then
  begin
   if not (csDesigning in Screen.Forms[Index].ComponentState) then
     ProcessComponent(AColorMap, AStyle, Screen.Forms[Index], Restore);
  end
//  {$IFDEF DELPHIXE2_UP}
//  else
//  if (TColorizerLocalSettings.Settings<>nil) and (TColorizerLocalSettings.Settings.UseVCLStyles) and (csDesigning in Screen.Forms[index].ComponentState) then
//    ApplyEmptyVCLStyleHook(Screen.Forms[index].ClassType);
//  {$ENDIF}
end;


procedure LoadSettings(AColorMap:TCustomActionBarColorMap;ActionBarStyle : TActionBarStyle;Settings : TSettings);
Var
 ThemeFileName : string;
begin
  if Settings=nil then exit;
  ReadSettings(Settings, ExtractFilePath(GetModuleLocation()));
  ThemeFileName:=IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleLocation()))+'Themes\'+Settings.ThemeName+'.idetheme';
  if FileExists(ThemeFileName) then
   LoadColorMapFromXmlFile(AColorMap, ThemeFileName);

//  if ActionBarStyles.IndexOf(Settings.StyleBarName)>=0 then
//    ActionBarStyle:= TActionBarStyle(ActionBarStyles.Objects[ActionBarStyles.IndexOf(Settings.StyleBarName)]);
end;


procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Color, FontColor:TColor);
begin
  AColorMap.Color                 :=Color;
  AColorMap.ShadowColor           :=GetShadowColor(Color);
  AColorMap.FontColor             :=FontColor;
  AColorMap.MenuColor             :=GetHighLightColor(Color);
  AColorMap.HighlightColor        :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.BtnSelectedColor      :=GetHighLightColor(AColorMap.MenuColor);
  AColorMap.BtnSelectedFont       :=AColorMap.FontColor;

  AColorMap.SelectedColor         :=GetHighLightColor(Color, 50);
  AColorMap.SelectedFontColor     :=AColorMap.FontColor;

  AColorMap.BtnFrameColor         :=GetShadowColor(Color);
  AColorMap.FrameTopLeftInner     :=GetShadowColor(Color);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;

{$IFDEF DELPHIXE2_UP}
procedure GenerateColorMap(AColorMap:TCustomActionBarColorMap;Style:TCustomStyleServices);
begin
  AColorMap.Color                 :=Style.GetStyleColor(scPanel);
  AColorMap.ShadowColor           :=StyleServices.GetSystemColor(clBtnShadow);
  AColorMap.FontColor             :=Style.GetStyleFontColor(sfButtonTextNormal);

  AColorMap.MenuColor             :=Style.GetStyleColor(scWindow);
  AColorMap.HighlightColor        :=StyleServices.GetSystemColor(clHighlight);
  AColorMap.BtnSelectedColor      :=Style.GetStyleColor(scButtonHot);

  AColorMap.BtnSelectedFont       :=StyleServices.GetSystemColor(clHighlightText);

  AColorMap.SelectedColor         :=StyleServices.GetSystemColor(clHighlight);
  AColorMap.SelectedFontColor     :=StyleServices.GetSystemColor(clHighlightText);

  AColorMap.BtnFrameColor         :=StyleServices.GetSystemColor(clBtnShadow);

  AColorMap.FrameTopLeftInner     :=StyleServices.GetSystemColor(clBtnShadow);
  AColorMap.FrameTopLeftOuter     :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightInner :=AColorMap.FrameTopLeftInner;
  AColorMap.FrameBottomRightOuter :=AColorMap.FrameTopLeftInner;
end;
{$ENDIF}

function ProcessDebuggerWindows(AColorMap:TCustomActionBarColorMap; AComponent: TComponent) : Boolean;
const
  FormsCount  = 6;
  DebuggerForms : array  [0..FormsCount-1] of string = ({'TDisassemblerView',} 'TRegisterView', 'TFlagsView', 'TDumpView', 'TFPURegisterView', 'TXMMRegisterView', 'TCPUStackView');
var
  LIndex : Integer;
begin
   Result:=False;
  for LIndex := 0 to FormsCount-1 do
    if SameText(AComponent.ClassName, DebuggerForms[LIndex]) then
    begin
      SetRttiPropertyValue(AComponent,'Color',AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
      Result:=True;
      Break;
    end
end;

function ProcessVclControls(AColorMap:TCustomActionBarColorMap;AComponent: TComponent) : Boolean;
const
  NumWin  = 7;
  CompList : array  [0..NumWin-1] of string = ('TMemo', 'TListView', 'TTreeView', 'TListBox', 'TCheckListBox', 'TExplorerCheckListBox', 'THintListView');
var
  Index : Integer;
begin
   Result:=False;
  for Index := 0 to NumWin-1 do
    if CompareText(AComponent.ClassName,CompList[Index])=0 then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'Ctl3D', False);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
      Result:=True;
      Break;
    end
end;

//todo color themed tcheckbox , tradiobutton
function ProcessStdVclControls(AColorMap:TCustomActionBarColorMap;AComponent: TComponent) : Boolean;
const
  NumWin  = 2;
  CompList : array  [0..NumWin-1] of string = ('TLabel','TCheckBox');
var
  Index : Integer;
begin
   Result:=False;
  for Index := 0 to NumWin-1 do
    if CompareText(AComponent.ClassName,CompList[Index])=0 then
    begin
      SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
      Result:=True;
      Break;
    end
end;

function ProcessVclGroupControls(AColorMap:TCustomActionBarColorMap;AComponent: TComponent) : Boolean;
const
  NumWin  = 3;
  CompList : array  [0..NumWin-1] of string = ('TGroupBox','TRadioGroup','TPropRadioGroup');
var
  Index : Integer;
begin
   Result:=False;
  for Index := 0 to NumWin-1 do
    if CompareText(AComponent.ClassName,CompList[Index])=0 then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
      SetRttiPropertyValue(AComponent,'Ctl3D', False);
      SetRttiPropertyValue(AComponent,'ParentBackGround', True);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
      Result:=True;
      Break;
    end
end;


Procedure GenIntransparentBitmap(bmp, Intrans: TBitmap);
begin
  Intrans.Assign(bmp);
  Intrans.PixelFormat := pf24bit;
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



procedure ProcessComponent(AColorMap:TCustomActionBarColorMap;AStyle: TActionBarStyle;AComponent: TComponent;Restore : Boolean = False);
var
  I, Index       : Integer;
  LPanel         : TPanel;
  LColorMap      : TCustomActionBarColorMap;
  LActionManager : TActionManager;
  LCategoryButtons : TCategoryButtons;
  LImages        : TImageList;
  LForm          : TForm;
//  LVirtualTreeState : TVirtualTreeStates;
//  LVirtualTreeStatei : Int64;
//  LVirtual : TVirtualStringTree;
begin

 if not Assigned(AComponent) then  exit;
 if not Assigned(AColorMap) then  exit;


{$IFDEF DEBUG_PROFILER}
 lprofiler.Add(Format('%s Processing component %s:%s',[formatdatetime('hh:nn:ss.zzz',Now) ,AComponent.Name,AComponent.ClassName]));
  if lDumped.IndexOf(AComponent.ClassName)=-1 then
  begin
    lDumped.Add(AComponent.ClassName);
    DumpComponent(AComponent);
  end;
{$ENDIF}


{$IFDEF DEBUG_MODE}
 //lcomp.Add(Format('%s : %s',[AComponent.Name,AComponent.ClassName]));
{$ENDIF}


   //todo fix colors
    if AComponent is TCategoryButtons then //Name='TIDECategoryButtons' then
    begin
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
    end
    else
//    if SameText(AComponent.ClassName, 'TFPURegisterView') then
//    begin
//      SetRttiPropertyValue(AComponent,'Color',AColorMap.MenuColor);
//      SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
//    end
//    else
    if SameText(AComponent.ClassName, 'TDisassemblerView') then
    begin
       //DumpComponent(AComponent);
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
    end
    else
    if SameText(AComponent.ClassName, 'TTDStringGrid') then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'Ctl3D', False);
      SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
      SetRttiPropertyValue(AComponent,'GradientStartColor', AColorMap.Color);
      SetRttiPropertyValue(AComponent,'GradientEndColor', AColorMap.Color);
    end
    else
    if AComponent is TPopupActionBar then
    begin
      {
      if not Assigned(TPopupActionBar(AComponent).OnGetControlClass) then
      begin
        LObjectList.Add(THelperClass.Create);
        LObjectList[LObjectList.Count-1].PopupMenu:=TPopupActionBar(AComponent).PopupMenu;
        LObjectList[LObjectList.Count-1].ColorMap:=AColorMap;
        TPopupActionBar(AComponent).OnGetControlClass:=LObjectList[LObjectList.Count-1].PopupActionBar1GetControlClass;
        TPopupActionBar(AComponent).OnPopup          :=LObjectList[LObjectList.Count-1].PopupActionBar1Popup;
      end;
      }
          if (TColorizerLocalSettings.Settings.ChangeIconsGutter) and not (TColorizerLocalSettings.ImagesGutterChanged) and SameText('ModuleMenu', AComponent.Name) then
          begin
            LImages:=TImageList(TPopupActionBar(AComponent).Images);
            if (LImages<>nil) and (LImages.Count>=27) then
             begin
              //LImages:=TImageList.Create(nil);
              LImages.Clear;
              {$IF CompilerVersion > 20}
              LImages.ColorDepth:=TColorDepth.cd32Bit;
              {$IFEND}
              LImages.DrawingStyle:=dsNormal;
              LImages.Width:=15;
              LImages.Height:=15;

              for i:= 0 to 26 do
              ImageListAdd(LImages, i, Format('p%.2d',[i]));
                 //  ImageListReplace(LImages, i, Format('p%.2d',[i]));

               TPopupActionBar(AComponent).Images:=LImages;
               TColorizerLocalSettings.ImagesGutterChanged:=True;
             end;
          end;
    end
    else
    if SameText(AComponent.ClassName, 'TEdit') then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
    end
    else
    if SameText(AComponent.ClassName, 'TPropCheckBox') then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
    end
    else
    if SameText(AComponent.ClassName ,'TDesktopComboBox') or  SameText(AComponent.ClassName ,'THistoryPropComboBox') then
    begin
//      if not TColorizerLocalSettings.Settings.UseVCLStyles then
//      SetWindowTheme(TWinControl(AComponent).Handle,'','');
      SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
      SetRttiPropertyValue(AComponent,'BevelKind', Integer(bkFlat));
      SetRttiPropertyValue(AComponent,'BevelInner', Integer(bvNone));
    end
    else
    if SameText(AComponent.ClassName, 'TComboBox') then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.HighlightColor);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
    end
    else
    if AComponent is TForm then
    begin
      LForm:=TForm(AComponent);
      LForm.Color := AColorMap.Color;
      LForm.Font.Color:=AColorMap.FontColor;
      //DumpObject(AComponent);
    end
    else
    if SameText(AComponent.ClassName, 'TPanel') then
    begin
      LPanel        := TPanel(AComponent);
      LPanel.Color  := AColorMap.Color;
      //LPanel.Invalidate;
    end
    else
    if ProcessStdVclControls(AColorMap, AComponent) then
    else
    if ProcessVclControls(AColorMap, AComponent) then
    else
    if ProcessDebuggerWindows(AColorMap, AComponent) then
    else
    if ProcessVclGroupControls(AColorMap, AComponent) then
    else
    if SameText(AComponent.ClassName, 'TInspListBox') then
    begin
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
      SetRttiPropertyValue(AComponent,'EditBackgroundColor', AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'HighlightColor', AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'BackgroundColor', AColorMap.Color);
      SetRttiPropertyValue(AComponent,'GutterColor', AColorMap.Color);
      SetRttiPropertyValue(AComponent,'HighlightFontColor', AColorMap.FontColor);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
    end
    else
    if SameText(AComponent.ClassName, 'TStringGrid') then
    begin
      SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'FixedColor', AColorMap.Color);
      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
    end
    else
//    if SameText(AComponent.ClassName, 'TBetterHintWindowVirtualDrawTree') then
//    begin
//      SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
//      SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
//    end
//    else
    if  SameText(AComponent.ClassName, 'TRefactoringTree') then
    begin
        SetRttiPropertyValue(AComponent,'Color', AColorMap.MenuColor);
        SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
        SetRttiPropertyValue(AComponent,'Ctl3D', False);
    end
    else
    if SameText(AComponent.ClassName, 'TVirtualStringTree') or SameText(AComponent.ClassName, 'TBetterHintWindowVirtualDrawTree') then
    begin
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
    end
    else
    if SameText(AComponent.ClassName, 'TCodeEditorTabControl') then
    begin
      SetRttiPropertyValue(AComponent,'UnselectedColor',AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'SelectedColor',AColorMap.Color);
      SetRttiPropertyValue(AComponent,'BackgroundColor',AColorMap.MenuColor);
      SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
    end
    else
    if SameText(AComponent.ClassName, 'TEditorDockPanel') then
    begin
      SetRttiPropertyValue(AComponent,'Color',AColorMap.Color);
      SetRttiPropertyValue(AComponent,'Font.Color',AColorMap.FontColor);
    end
    else
    if SameText(AComponent.ClassName, 'TEditControl') then   //TODO
    begin
       //SetRttiPropertyValue(AComponent, 'BorderStyle',  Ord(bsNone));
       {$IFDEF DELPHIXE2_UP}
        if TColorizerLocalSettings.Settings.UseVCLStyles then
        begin
          if not IsStyleHookRegistered(AComponent.ClassType, TMemoStyleHook) then
           TStyleEngine.RegisterStyleHook(AComponent.ClassType, TMemoStyleHook);
        end;
       {$ENDIF}
       // Setting these properties and Fields has not effect in the gutter color.
       //SetRttiFieldValue(AComponent,'GutterBrush.Color',  clYellow);
       //SetRttiPropertyValue(AComponent,'Brush.Color',  clRed);
       //SetRttiFieldValue(AComponent,'FParentColor',  False);
       //SetRttiFieldValue(AComponent,'FColor',  clYellow);
       //SetRttiFieldValue(AComponent,'CurForeColor',  clYellow);
       //SetRttiFieldValue(AComponent,'CurBackColor',  clRed);
       //ExecMethodRtti(AComponent, 'Invalidate');
       //DumpParentClass(AComponent);
    end
    else
    if AComponent is TActionToolBar then
    with TActionToolBar(AComponent) do
    begin
      LColorMap:=TXPColorMap.Create(AComponent);
      LColorMap.Assign(AColorMap);
      LColorMap.OnColorChange:=nil;
      ColorMap:=LColorMap;
    end
    else
    if AComponent is TControlBar then
    with TControlBar(AComponent) do
    begin
      Color := AColorMap.Color;
      DrawingStyle := dsGradient;
      GradientStartColor :=  AColorMap.Color;
      GradientEndColor   :=  AColorMap.Color;
    end
    else
    if SameText(AComponent.ClassName, 'TToolBar') then
    begin
      with TToolBar(AComponent) do
        Color := AColorMap.Color;
    end
    else
    if SameText(AComponent.ClassName, 'TDockToolBar') then
    begin
      with TToolBar(AComponent) do
      begin
        Color              := AColorMap.Color;
        if Restore then
          DrawingStyle       := TTBDrawingStyle(dsNormal)
        else
          DrawingStyle       := TTBDrawingStyle(dsGradient);
        GradientStartColor := AColorMap.MenuColor;
        GradientEndColor   := AColorMap.Color;//$00D1B499;
        HotTrackColor      := AColorMap.SelectedColor;
        Font.Color         := AColorMap.FontColor;
      end;
    end
    else
    if AComponent is TActionMainMenuBar then
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
    else
    if AComponent is TActionManager then
    begin
      LActionManager:=TActionManager(AComponent);
      {$IFDEF DELPHI2009_UP}
//      if not ActnStyleList.ContainsKey(LActionManager) then
//          ActnStyleList.Add(LActionManager, LActionManager.Style);
      if TColorizerLocalSettings.ActnStyleList.IndexOf(LActionManager)=-1 then
          TColorizerLocalSettings.ActnStyleList.Add(LActionManager);
      {$ENDIF}
      LActionManager.Style := AStyle;
    end
    else
    if SameText(AComponent.ClassName, 'TTabSet') then
    with TTabSet(AComponent) do
    begin
      BackgroundColor:=AColorMap.MenuColor;
      SelectedColor  :=AColorMap.Color;
      UnselectedColor:=AColorMap.MenuColor;
      Font.Color    := AColorMap.FontColor;
      Style :=tsModernTabs; //necessary for allow paint background color
    end
    else
    if SameText(AComponent.ClassName, 'TClosableTabScroller') then
    begin

      //SetBkColor
      //Canvas.Fillrect
      //SetDCBrushColor
      //CreateSolidBrush
       SetRttiPropertyValue(AComponent,'CloseButton.BackgroundColor', AColorMap.MenuColor);
       SetRttiPropertyValue(AComponent,'CloseButton.Transparent', False);
       SetRttiPropertyValue(AComponent,'DropDownButton.BackgroundColor', AColorMap.MenuColor);
       SetRttiPropertyValue(AComponent,'DropDownButton.Transparent', False);

       SetRttiPropertyValue(AComponent,'LeftButton.BackgroundColor', AColorMap.MenuColor);
       SetRttiPropertyValue(AComponent,'LeftButton.Transparent', False);
       SetRttiPropertyValue(AComponent,'RightButton.BackgroundColor', AColorMap.MenuColor);
       SetRttiPropertyValue(AComponent,'RightButton.Transparent', False);

       SetRttiPropertyValue(AComponent,'Brush.Color', AColorMap.MenuColor);
    end
    else
    if SameText(AComponent.ClassName, 'TIDEGradientTabSet') or SameText(AComponent.ClassName, 'TGradientTabSet') then
    begin
         SetRttiPropertyValue(AComponent,'TabColors.ActiveStart', AColorMap.Color);//AColorMap.HighlightColor);
         SetRttiPropertyValue(AComponent,'TabColors.ActiveEnd', AColorMap.Color);
         SetRttiPropertyValue(AComponent,'TabColors.InActiveStart', AColorMap.MenuColor);
         SetRttiPropertyValue(AComponent,'TabColors.InActiveEnd', AColorMap.MenuColor);
         SetRttiPropertyValue(AComponent,'Font.Color', AColorMap.FontColor);
         //SetRttiFieldValue(AComponent,'FColor', AColorMap.Color);

        if SameText(AComponent.ClassName, 'TGradientTabSet') then
        begin
            //DumpObject(GetRttiFieldValue(AComponent, 'FScroller').AsObject);
         SetRttiPropertyValue(AComponent,'FScroller.FLeftButton.BackgroundColor', AColorMap.MenuColor);
         SetRttiPropertyValue(AComponent,'FScroller.FLeftButton.Transparent', False);
         SetRttiPropertyValue(AComponent,'FScroller.FRightButton.BackgroundColor', AColorMap.MenuColor);
         SetRttiPropertyValue(AComponent,'FScroller.FRightButton.Transparent', False);
        end;
    end
    else
    if SameText(AComponent.ClassName, 'TTabSheet')  then
    with TTabSheet(AComponent) do
    begin
       //Color:=AColorMap.Color;
       Font.Color:=AColorMap.FontColor;
    end
    else
    if AComponent is TStatusBar then
    with TStatusBar(AComponent) do
    begin
       //theme is removed to allow paint TStatusBar
//        if not TColorizerLocalSettings.Settings.UseVCLStyles then
//       SetWindowTheme(TStatusBar(AComponent).Handle,'','');
       //SizeGrip is removed because can't be painted
       SizeGrip:=False;
       Color := AColorMap.Color;
       //remove the bevels
       for i := 0 to TStatusBar(AComponent).Panels.Count-1 do
        TStatusBar(AComponent).Panels[i].Bevel:=pbNone;

       Font.Color:=AColorMap.FontColor;
    end
    else
    if AComponent is TFrame then
    with TFrame(AComponent) do
    begin
      Color := AColorMap.Color;
      Font.Color:=AColorMap.FontColor;
    end
    else
    begin
      {$IFDEF DEBUG_PROFILER}
        lpignored.Add(Format('%s component %s:%s',[formatdatetime('hh:nn:ss.zzz',Now) ,AComponent.Name,AComponent.ClassName]));
      {$ENDIF}
    end;

    {$IFDEF DEBUG_PROFILER}
      lprofiler.Add(Format('%s End process component %s:%s',[formatdatetime('hh:nn:ss.zzz',Now) ,AComponent.Name,AComponent.ClassName]));
    {$ENDIF}


    //if SameText(AComponent.Name, 'TDebugLogView') then
    // DumpObject(AComponent);

    for Index := 0 to AComponent.ComponentCount - 1 do
    begin
     {$IFDEF DEBUG_MODE}
     //lcomp.Add(Format('     %s : %s',[AComponent.Components[I].Name,AComponent.Components[I].ClassName]));
     {$ENDIF}
     ProcessComponent(AColorMap, ColorXPStyle, AComponent.Components[Index], Restore);
    end;
end;

//
//type
//  TCustomActionBarClass = class(TCustomActionBar);
//  TCustomActionManagerHelper = class helper for TCustomActionManager
//  private
//    procedure SetFStyleRW(const Value: TActionBarStyle);
//    function GetFOnStyleChangedRW: TStyleChanged;
//    function GetFStyleRW: TActionBarStyle;
//  public
//   property FStyleRW : TActionBarStyle read GetFStyleRW write SetFStyleRW;
//   property FOnStyleChangedRW: TStyleChanged read GetFOnStyleChangedRW;
//  end;
//
//
//{ TCustomActionManagerHelper }
//
//function TCustomActionManagerHelper.GetFOnStyleChangedRW: TStyleChanged;
//begin
//  Result:=Self.FOnStyleChanged;
//end;
//
//function TCustomActionManagerHelper.GetFStyleRW: TActionBarStyle;
//begin
//  Result:=Self.FStyle;
//end;
//
//procedure TCustomActionManagerHelper.SetFStyleRW(const Value: TActionBarStyle);
//begin
//  Self.FStyle:=Value;
//end;


procedure RestoreActnManagerStyles;


//  procedure SetStyle(ActionManager : TCustomActionManager; const Value: TActionBarStyle);
//  var
//    I: Integer;
//  begin
//    if ActionManager.Style <> Value then
//    begin
//      ActionManager.FStyleRW := Value;
//      if Assigned(ActionManager.ActionBars) then
//      for I := 0 to ActionManager.ActionBars.Count - 1 do
//      begin
//        if Assigned(ActionManager.ActionBars[I].ActionBar) then
//        begin
//          ActionManager.ActionBars[I].ActionBar.RecreateControls;
//          if ActionManager.ActionBars[I].ActionBar.ColorMap = TCustomActionBarClass(ActionManager.ActionBars[I].ActionBar).FDefaultColorMap then
//            ActionManager.ActionBars[I].ActionBar.ColorMap := nil;
//          ActionManager.ActionBars[I].ActionBar.Invalidate;
//        end;
//      end;
//      if Assigned(ActionManager.FOnStyleChangedRW) then
//        ActionManager.FOnStyleChangedRW(ActionManager);
////      if (csDesigning in ActionManager.ComponentState) and not (csLoading in ActionManager.ComponentState) then
////        NotifyDesigner(nil);
//    end;
//  end;

{$IFNDEF DLLWIZARD}
var
  LActionManager : TActionManager;
{$ENDIF}
begin
{$IFNDEF DLLWIZARD}
 {$IFDEF DELPHI2009_UP}
  try
    if (TColorizerLocalSettings.ActnStyleList.Count>0)  and Assigned(ActionBarStyles) then
    begin
      for LActionManager in TColorizerLocalSettings.ActnStyleList{.Keys} do
      begin
         //LActionManager.Style:= ActnStyleList.Items[LActionManager];//ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)];
        if ActionBarStyles.IndexOf(DefaultActnBarStyle)>=0 then
        begin
         if Assigned(LActionManager.Style) and Assigned(ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)]) then
         begin
           //AddLog('ActionBarStyles '+ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)].GetStyleName);
           LActionManager.Style:= ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)];
           //SetStyle(LActionManager, ActionBarStyles.Style[ActionBarStyles.IndexOf(DefaultActnBarStyle)]);
         end;
        end;
      end;
    end;
  except on e: exception do //sometimes the references to the objects contained in ActionBarStyles are lost when the IDE is closed.
    AddLog(Format(' LActionManager.Style exception RestoreActnManagerStyles Message %s Trace %s ',[e.Message, e.StackTrace]));
  end;
 {$ELSE DELPHI2009_UP}
   //TODO
 {$ENDIF}

{$ENDIF}
end;

procedure RestoreIDESettings();
var
 NativeColorMap : TCustomActionBarColorMap;
begin
{$IFDEF DELPHIXE2_UP}
  if TColorizerLocalSettings.Settings.UseVCLStyles then
    if not TStyleManager.ActiveStyle.IsSystemStyle  then
     TStyleManager.SetStyle('Windows');
{$ENDIF}

{$IFDEF DELPHIXE_UP}
  NativeColorMap:=TThemedColorMap.Create(nil);
{$ELSE}
  NativeColorMap:=TStandardColorMap.Create(nil);
{$ENDIF}

  try
  {$IFDEF DELPHIXE_UP}
    RefreshIDETheme(NativeColorMap, PlatformDefaultStyle, True);
  {$ELSE}
    RefreshIDETheme(NativeColorMap, XPStyle, True);
  {$ENDIF}
  finally
    NativeColorMap.Free;
  end;
  RestoreActnManagerStyles();
end;

initialization
{$IFDEF DEBUG_PROFILER}
  DumpAllTypes;
{$ENDIF}

{$IFDEF DEBUG_PROFILER}
  ShowMessage('warning DEBUG_PROFILER mode Activated');
  lprofiler:=TStringList.Create;
  lpignored:=TStringList.Create;
  lDumped  :=TStringList.Create;
{$ENDIF}

finalization

{$IFDEF DEBUG_PROFILER}
  lprofiler.SaveToFile(ExtractFilePath(GetBplLocation())+'Profiler\profiler.txt');
  lprofiler.Free;
  lpignored.SaveToFile(ExtractFilePath(GetBplLocation())+'Profiler\ignored.txt');
  lpignored.Free;
  lDumped.Free;
{$ENDIF}
end.
