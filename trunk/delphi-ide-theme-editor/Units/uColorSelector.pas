unit uColorSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, mbDeskPickerButton, mbColorPickerControl,
  HRingPicker, HexaColorPicker, mbColorPreview, ImgList, HSVColorPicker,
  ComCtrls, mbColorTree, mbColorPalette, Tabs, mbTrackBarPicker, LColorPicker,
  HSColorPicker, HSLColorPicker, VColorPicker, pngimage;

type
  TDialogColorSelector = class(TForm)
    mbDeskPickerButton1: TmbDeskPickerButton;
    mbColorPreview1: TmbColorPreview;
    ImageList1: TImageList;
    Red: TEdit;
    Label1: TLabel;
    Green: TEdit;
    Label2: TLabel;
    Blue: TEdit;
    Label3: TLabel;
    mbColorPreview2: TmbColorPreview;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    HexaColorPicker1: THexaColorPicker;
    mbColorPalette1: TmbColorPalette;
    TabSheet4: TTabSheet;
    HSLColorPicker1: THSLColorPicker;
    TabSheet3: TTabSheet;
    HSVColorPicker1: THSVColorPicker;
    VColorPicker1: TVColorPicker;
    BtnApply: TButton;
    Button2: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Hue: TEdit;
    Lum: TEdit;
    Sat: TEdit;
    Label7: TLabel;
    Hex: TEdit;
    CheckBoxLive: TCheckBox;
    procedure mbDeskPickerButton1SelColorChange(Sender: TObject);
    procedure mbColorPalette1SelColorChange(Sender: TObject);
    procedure HSVColorPicker1Change(Sender: TObject);
    procedure VColorPicker1Change(Sender: TObject);
    procedure HexaColorPicker1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HSLColorPicker1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RedExit(Sender: TObject);
  private
    FInitializating: Boolean;
    FSelectedColor: TColor;
    FStatus       : Boolean;
    FRefreshHSVColorPicker : Boolean;
    FRefreshVColorPicker   : Boolean;
    FOnChange: TNotifyEvent;
    { Private declarations }
    procedure RefreshColors(Acolor: TColor);
    procedure SetSelectedColor(const Value: TColor);
  public
    { Public declarations }
    property SelectedColor : TColor read FSelectedColor write SetSelectedColor;
    function Execute : Boolean;
  published
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses
  GraphUtil;

{$R *.dfm}


//
//http://delphihaven.wordpress.com/2010/04/19/setting-up-a-custom-titlebar/
procedure TDialogColorSelector.BtnApplyClick(Sender: TObject);
begin
  FStatus:=True;
  Close;
end;

procedure TDialogColorSelector.Button2Click(Sender: TObject);
begin
  FStatus:=False;
  Close;
end;

function TDialogColorSelector.Execute: Boolean;
begin
  ShowModal();
  Result:=FStatus;
end;

procedure TDialogColorSelector.FormCreate(Sender: TObject);
var
  i : Integer;
begin
   FInitializating:=True;
   FStatus:=False;
   {
   mbColorPalette1.Colors.Clear;
   for i:=0 to WebNamedColorsCount-1 do
   begin
    mbColorPalette1.Colors.Add(IntToStr(WebNamedColors[i].Value));
    mbColorPalette1.ColorNames.Add(WebNamedColors[i].Name);
   end;     }
end;

procedure TDialogColorSelector.FormShow(Sender: TObject);
begin
  FInitializating:=False;
end;

procedure TDialogColorSelector.HexaColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
    RefreshColors(HexaColorPicker1.SelectedColor);
end;

procedure TDialogColorSelector.HSLColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
    RefreshColors(HSLColorPicker1.SelectedColor);
end;

procedure TDialogColorSelector.HSVColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
  if FRefreshHSVColorPicker then
  begin
   VColorPicker1.SelectedColor:=HSVColorPicker1.SelectedColor;
   RefreshColors(VColorPicker1.SelectedColor);
  end;
end;

procedure TDialogColorSelector.VColorPicker1Change(Sender: TObject);
begin
  if not FInitializating then
  begin
   FRefreshHSVColorPicker:=False;
   try
     HSVColorPicker1.SelectedColor:=VColorPicker1.SelectedColor;
     RefreshColors(HSVColorPicker1.SelectedColor);
   finally
     FRefreshHSVColorPicker:=True;
   end;
  end;
end;


procedure TDialogColorSelector.mbColorPalette1SelColorChange(Sender: TObject);
begin
  if not FInitializating then
    RefreshColors(mbcolorpalette1.SelectedColor);
end;

procedure TDialogColorSelector.mbDeskPickerButton1SelColorChange(Sender: TObject);
begin
  if not FInitializating then
    RefreshColors(mbDeskPickerButton1.SelectedColor);
end;

procedure TDialogColorSelector.RedExit(Sender: TObject);
var
 Value : Integer;
begin
   if TEdit(Sender).Text='' then
    TEdit(Sender).Text:='0';

  if TryStrToInt(TEdit(Sender).Text,Value) and (Value>255) then
   TEdit(Sender).Text:='255';
end;

procedure TDialogColorSelector.RefreshColors(Acolor: TColor);
var
  Hue, Luminance, Saturation: Word;
  s : string;
begin
  mbColorPreview1.Color:=Acolor;
  mbColorPreview2.Color:=Acolor;
  Red.Text  :=IntToStr(GetRValue(Acolor));
  Green.Text:=IntToStr(GetGValue(Acolor));
  Blue.Text :=IntToStr(GetBValue(Acolor));
  ColorRGBToHLS(ColorToRGB(Acolor),Hue,Luminance,Saturation);
  Self.Hue.Text   :=IntToStr(Hue);
  Self.Lum.Text   :=IntToStr(Luminance);
  Self.Sat.Text   :=IntToStr(Saturation);
  FmtStr(s, '%s%0.8x', [HexDisplayPrefix, Integer(Acolor)]);
  Hex.Text:=s;

  FSelectedColor:=Acolor;

  if Assigned(CheckBoxLive) and CheckBoxLive.Checked and  (@FOnChange<>nil) then
   OnChange(Self);

end;

procedure TDialogColorSelector.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  RefreshColors(Value);
end;


end.
