unit OfficeMoreColorsDialog;

interface

{$I mxs.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI_6_UP}Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  ComCtrls, HexaColorPicker, HSLColorPicker, StdCtrls, ExtCtrls, RGBHSLUtils,
  mbColorPreview, {$IFDEF mbXP_Lib}mbXPSpinEdit, mbXPSizeGrip,{$ELSE} Spin,{$ENDIF}
  HTMLColors;

type
  TOfficeMoreColorsWin = class(TForm)
    Pages: TPageControl;
    Standard: TTabSheet;
    Custom: TTabSheet;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ColorModel: TComboBox;
    LRed: TLabel;
    LGreen: TLabel;
    LBlue: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OKbtn: TButton;
    Cancelbtn: TButton;
    NewSwatch: TmbColorPreview;
    OldSwatch: TmbColorPreview;
    procedure ColorModelChange(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure ERedChange(Sender: TObject);
    procedure EGreenChange(Sender: TObject);
    procedure EBlueChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HexaChange(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    function GetHint(c: TColor): string;
    procedure SetAllToSel(c: TColor);
    procedure PagesChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;
  h, s, l: integer;
  {$IFDEF mbXP_Lib}
  ERed, EGreen, EBlue: TmbXPSpinEdit;
  grip: TmbXPSizeGrip;
  {$ELSE}
  ERed, EGreen, EBlue: TSpinEdit;
  {$ENDIF}

implementation

{$R *.dfm}

procedure TOfficeMoreColorsWin.CreateParams(var Params: TCreateParams);
begin 
  inherited CreateParams(Params); 
  Params.Style := WS_CAPTION or WS_SIZEBOX or WS_SYSMENU; 
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE; 
end; 

procedure TOfficeMoreColorsWin.CreateWnd;
begin 
  inherited CreateWnd; 
  SendMessage(Self.Handle, WM_SETICON, 1, 0); 
end; 

procedure TOfficeMoreColorsWin.ColorModelChange(Sender: TObject);
begin
 case ColorModel.ItemIndex of
  0:
   begin
    LRed.Caption := '&Red:';
    LGreen.Caption := '&Green:';
    LBlue.Caption := '&Blue:';
    ERed.MaxValue := 255;
    EGreen.MaxValue := 255;
    EBlue.MaxValue := 255;
    ERed.Value := GetRValue(NewSwatch.Color);
    EGreen.Value := GetGValue(NewSwatch.Color);
    EBlue.Value := GetBValue(NewSwatch.Color);
   end;
  1:
   begin
    LRed.Caption := 'H&ue:';
    LGreen.Caption := '&Sat:';
    LBlue.Caption := '&Lum:';
    ERed.MaxValue := 238;
    EGreen.MaxValue := 240;
    EBlue.MaxValue := 240;
    RGBtoHSLRange(NewSwatch.Color, h, s, l);
    ERed.Value := h;
    EGreen.Value := s;
    EBlue.Value := l;
   end;
 end;
end;

procedure TOfficeMoreColorsWin.HSLChange(Sender: TObject);
begin
 if HSL.Manual then
  case ColorModel.ItemIndex of
   0:
    begin
      ERed.Value := HSL.RValue;
      EGreen.Value := HSL.GValue;
      EBlue.Value := HSL.BValue;
      NewSwatch.Color := HSL.SelectedColor;
    end;
   1:
    begin
      ERed.Value := HSL.HValue;
      EGreen.Value := HSL.SValue;
      EBlue.Value := HSL.LValue;
      NewSwatch.Color := HSL.SelectedColor;
    end;
  end;
end;

procedure TOfficeMoreColorsWin.ERedChange(Sender: TObject);
begin
if (ERed.Text <> '') and (ERed.Focused or ERed.Button.Focused) then
 case ColorModel.ItemIndex of
  0:
   begin
    HSL.RValue := ERed.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
   end;
  1:
   begin
    HSL.HValue := ERed.Value;
    NewSwatch.Color := HSLRangeToRGB(ERed.Value, EGreen.Value, EBlue.Value);
   end;
 end;
end;

procedure TOfficeMoreColorsWin.EGreenChange(Sender: TObject);
begin
if (EGreen.Text <> '') and (EGreen.Focused or EGreen.Button.Focused) then
 case ColorModel.ItemIndex of
  0:
   begin
    HSL.GValue := EGreen.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
   end;
  1:
   begin
    HSL.SValue := EGreen.Value;
    NewSwatch.Color := HSLRangeToRGB(ERed.Value, EGreen.Value, EBlue.Value);
   end;
 end;
end;

procedure TOfficeMoreColorsWin.EBlueChange(Sender: TObject);
begin
if (EBlue.Text <> '') and (EBlue.Focused or EBlue.Button.Focused) then
 case ColorModel.ItemIndex of
  0:
   begin
    HSL.BValue := EBlue.Value;
    NewSwatch.Color := RGB(ERed.Value, EGreen.Value, EBlue.Value);
   end;
  1:
   begin
    HSL.LValue := EBlue.Value;
    NewSwatch.Color := HSLRangeToRGB(ERed.Value, EGreen.Value, EBlue.Value);
   end;
 end;
end;

procedure TOfficeMoreColorsWin.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case Key of
  VK_RETURN: ModalResult := mrOK;
  VK_ESCAPE: ModalResult := mrCancel;
 end;
end;

procedure TOfficeMoreColorsWin.HexaChange(Sender: TObject);
begin
 NewSwatch.Color := Hexa.SelectedColor;
end;

function TOfficeMoreColorsWin.GetHint(c: TColor): string;
begin
 Result := Format('RGB(%u, %u, %u)'#13'Hex: %s', [GetRValue(c), GetGValue(c), GetBValue(c), ColorToHex(c)]);
end;

procedure TOfficeMoreColorsWin.NewSwatchColorChange(Sender: TObject);
begin
 NewSwatch.Hint := GetHint(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.OldSwatchColorChange(Sender: TObject);
begin
 OldSwatch.Hint := GetHint(OldSwatch.Color);
 SetAllToSel(OldSwatch.Color);
end;

procedure TOfficeMoreColorsWin.SetAllToSel(c: TColor);
begin
 case Pages.ActivePageIndex of
  // Standard Page
  0: Hexa.SelectedColor := c;
  // Custom Page
  1:
   begin
    HSL.SelectedColor := c;
    case ColorModel.ItemIndex of
     0:
      begin
       ERed.Value := GetRValue(c);
       EGreen.Value := GetGValue(c);
       EBlue.Value := GetBValue(c);
      end;
     1:
      begin
       RGBtoHSLRange(c, h, s, l);
       ERed.Value := h;
       EGreen.Value := s;
       EBlue.Value := l;
      end;
    end;
   end;
 end;
 NewSwatch.Color := c;
end;

procedure TOfficeMoreColorsWin.PagesChange(Sender: TObject);
begin
 SetAllToSel(NewSwatch.Color);
end;

procedure TOfficeMoreColorsWin.FormResize(Sender: TObject);
begin
{$IFDEF mbXP_Lib}
grip.Left := ClientWidth - 15;
grip.Top := ClientHeight - 15;
{$ENDIF}
end;

procedure TOfficeMoreColorsWin.FormCreate(Sender: TObject);
begin
 {$IFDEF mbXP_Lib}
 ERed := TmbXPSpinEdit.CreateParented(Custom.Handle);
 EGreen := TmbXPSpinEdit.CreateParented(Custom.Handle);
 EBlue := TmbXPSpinEdit.CreateParented(Custom.Handle);
 grip := TmbXPSizeGrip.CreateParented(Self.Handle);
 {$ELSE}
 ERed := TSpinEdit.CreateParented(Custom.Handle);
 EGreen := TSpinEdit.CreateParented(Custom.Handle);
 EBlue := TSpinEdit.CreateParented(Custom.Handle);
 {$ENDIF}
 with ERed do
  begin
   Name := 'ERed';
   Width := 47;
   Height := 22;
   Left := 74;
   Top := 198;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := ERedChange;
  end;
 with EGreen do
  begin
   Name := 'EGreen';
   Width := 47;
   Height := 22;
   Left := 74;
   Top := 224;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := EGreenChange;
  end;
 with EBlue do
  begin
   Name := 'EBlue';
   Width := 47;
   Height := 22;
   Left := 74;
   Top := 251;
   Anchors := [akLeft, akBottom];
   MaxValue := 255;
   MinValue := 0;
   Value := 0;
   OnChange := EBlueChange;
  end;
 Custom.InsertControl(ERed);
 Custom.InsertControl(EGreen);
 Custom.InsertControl(EBlue);
 {$IFDEF mbXP_Lib}
 with grip do
  begin
   Name := 'grip';
   Width := 15;
   Height := 15;
   Left := 308;
   Top := 314;
   Anchors := [akRight, akBottom];
  end;
 InsertControl(grip);
 {$ENDIF}
end;

end.
