unit main;

interface

{$I mxs.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI_6_UP}Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, HSLColorPicker, ComCtrls, StdCtrls, mbColorPreview,
  HexaColorPicker, mbColorPalette, HSLRingPicker, HSVColorPicker, PalUtils,
  SLHColorPicker, mbDeskPickerButton, mbOfficeColorDialog, SColorPicker,
  HColorPicker, VColorPicker, mbTrackBarPicker, LColorPicker, HRingPicker,
  SLColorPicker, HSColorPicker, IniFiles, mbColorPickerControl,
  BColorPicker, GColorPicker, RColorPicker, KColorPicker, YColorPicker,
  MColorPicker, CColorPicker, CIEBColorPicker, CIEAColorPicker, Typinfo,
  CIELColorPicker, BAxisColorPicker, GAxisColorPicker, RAxisColorPicker,
  mbColorTree, mbColorList {for internet shortcuts}, {$IFDEF DELPHI_7_UP}XPMan{$ENDIF};

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    HSLColorPicker1: THSLColorPicker;
    sc: TmbColorPreview;
    uc: TmbColorPreview;
    Label1: TLabel;
    tb1: TTrackBar;
    tb2: TTrackBar;
    Label2: TLabel;
    HexaColorPicker1: THexaColorPicker;
    mbColorPalette1: TmbColorPalette;
    Button1: TButton;
    Button2: TButton;
    HSLRingPicker1: THSLRingPicker;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    HSVColorPicker1: THSVColorPicker;
    SLHColorPicker1: TSLHColorPicker;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    mbDeskPickerButton1: TmbDeskPickerButton;
    mbOfficeColorDialog1: TmbOfficeColorDialog;
    Button3: TButton;
    LColorPicker1: TLColorPicker;
    VColorPicker1: TVColorPicker;
    HColorPicker1: THColorPicker;
    SColorPicker1: TSColorPicker;
    HSColorPicker1: THSColorPicker;
    SLColorPicker1: TSLColorPicker;
    HRingPicker1: THRingPicker;
    VColorPicker2: TVColorPicker;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label4: TLabel;
    CheckBox2: TCheckBox;
    Label5: TLabel;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
    Label3: TLabel;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label6: TLabel;
    ComboBox4: TComboBox;
    Label7: TLabel;
    UpDown1: TUpDown;
    TabSheet9: TTabSheet;
    CColorPicker1: TCColorPicker;
    MColorPicker1: TMColorPicker;
    YColorPicker1: TYColorPicker;
    KColorPicker1: TKColorPicker;
    Label8: TLabel;
    RColorPicker1: TRColorPicker;
    GColorPicker1: TGColorPicker;
    BColorPicker1: TBColorPicker;
    KColorPicker2: TKColorPicker;
    MColorPicker2: TMColorPicker;
    CColorPicker2: TCColorPicker;
    YColorPicker2: TYColorPicker;
    TabSheet10: TTabSheet;
    RAxisColorPicker1: TRAxisColorPicker;
    GAxisColorPicker1: TGAxisColorPicker;
    BAxisColorPicker1: TBAxisColorPicker;
    CIELColorPicker1: TCIELColorPicker;
    CIEAColorPicker1: TCIEAColorPicker;
    CIEBColorPicker1: TCIEBColorPicker;
    CheckBox3: TCheckBox;
    TabSheet11: TTabSheet;
    mbColorList1: TmbColorList;
    mbColorTree1: TmbColorTree;
    Button5: TButton;
    Memo1: TMemo;
    Label9: TLabel;
    CheckBox4: TCheckBox;
    procedure tb1Change(Sender: TObject);
    procedure tb2Change(Sender: TObject);
    procedure HSLColorPicker1Change(Sender: TObject);
    procedure HSLColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HexaColorPicker1Change(Sender: TObject);
    procedure HexaColorPicker1MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure mbColorPalette1SelColorChange(Sender: TObject);
    procedure mbColorPalette1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HSLRingPicker1Change(Sender: TObject);
    procedure HSLRingPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HSVColorPicker1Change(Sender: TObject);
    procedure HSVColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SLHColorPicker1Change(Sender: TObject);
    procedure SLHColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure mbDeskPickerButton1SelColorChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure HSColorPicker1Change(Sender: TObject);
    procedure HSColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SLColorPicker1Change(Sender: TObject);
    procedure SLColorPicker1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HRingPicker1Change(Sender: TObject);
    procedure HRingPicker1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure VColorPicker2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure CheckBox3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{.$R mxico.res} //MXS icon resource file, for internet shortcut only

procedure TForm1.tb1Change(Sender: TObject);
begin
sc.opacity := tb1.position;
end;

procedure TForm1.tb2Change(Sender: TObject);
begin
uc.opacity := tb2.position;
end;

procedure TForm1.HSLColorPicker1Change(Sender: TObject);
begin
sc.color := HSLColorPicker1.SelectedColor;
end;

procedure TForm1.HSLColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := HSLColorPicker1.ColorUnderCursor;
end;

procedure TForm1.HexaColorPicker1Change(Sender: TObject);
begin
sc.color := hexacolorpicker1.selectedcolor;
end;

procedure TForm1.HexaColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := hexacolorpicker1.ColorUnderCursor;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
mbColorPalette1.GeneratePalette(clblue);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
mbColorpalette1.GenerateGradientPalette([clblue, clred]);
end;

procedure TForm1.mbColorPalette1SelColorChange(Sender: TObject);
begin
sc.color := mbcolorpalette1.selectedcolor;
end;

procedure TForm1.mbColorPalette1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := mbcolorpalette1.ColorUnderCursor;
end;

procedure TForm1.HSLRingPicker1Change(Sender: TObject);
begin
sc.color := HSLRingPicker1.SelectedColor;
end;

procedure TForm1.HSLRingPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := HSLRingPicker1.ColorUnderCursor;
end;

procedure TForm1.HSVColorPicker1Change(Sender: TObject);
begin
sc.color := HSVColorPicker1.SelectedColor;
VColorPicker2.Saturation := HSVColorPicker1.Saturation;
VColorPicker2.Hue := HSVColorPicker1.Hue;
end;

procedure TForm1.HSVColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.Color := HSVColorPicker1.ColorUnderCursor;
end;

procedure TForm1.SLHColorPicker1Change(Sender: TObject);
begin
sc.color := SLHColorPicker1.SelectedColor;
end;

procedure TForm1.SLHColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := SLHColorPicker1.ColorUnderCursor;
end;

procedure TForm1.mbDeskPickerButton1SelColorChange(Sender: TObject);
begin
sc.color := mbDeskPickerButton1.SelectedColor;
uc.color := mbDeskPickerButton1.SelectedColor;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 if mbOfficeColorDialog1.Execute then
  sc.color := mbOfficeColorDialog1.SelectedColor;
end;

procedure TForm1.HSColorPicker1Change(Sender: TObject);
begin
sc.color := HSColorPicker1.SelectedColor;
end;

procedure TForm1.HSColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := HSColorpicker1.ColorUnderCursor;
end;

procedure TForm1.SLColorPicker1Change(Sender: TObject);
begin
sc.color := SLColorPicker1.SelectedColor;
end;

procedure TForm1.SLColorPicker1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
uc.color := slcolorpicker1.ColorUnderCursor;
end;

procedure TForm1.HRingPicker1Change(Sender: TObject);
begin
sc.color := hringpicker1.SelectedColor;
end;

procedure TForm1.HRingPicker1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
uc.color := hringpicker1.ColorUnderCursor;
end;

procedure TForm1.VColorPicker2Change(Sender: TObject);
begin
HSVColorPicker1.Value := VColorPicker2.Value;
end;

// only for internet shortcuts
procedure TForm1.FormCreate(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(Application.ExeName) + '\MXS Website.url') do
  try
   WriteString('InternetShortcut','URL', 'http://mxs.bergsoft.net');
   WriteInteger('InternetShortcut','IconIndex', 1);
   WriteString('InternetShortcut','IconFile', '"' + Application.ExeName + '"');
  finally
   Free;
  end; 
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
HexaColorPicker1.SliderVisible := checkbox1.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
hexacolorpicker1.SliderMarker := TMArker(ComboBox1.ItemIndex);
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
hexacolorpicker1.NewArrowStyle := checkbox2.checked;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 if opendialog1.Execute then
  mbcolorpalette1.Palette := opendialog1.FileName;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
mbcolorpalette1.SortOrder := tsortorder(combobox2.itemindex);
end;

procedure TForm1.ComboBox3Change(Sender: TObject);
begin
mbcolorpalette1.Sortmode := tsortmode(combobox3.ItemIndex);
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
mbcolorpalette1.CellStyle := tcellstyle(combobox4.ItemIndex);
end;

procedure TForm1.UpDown1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
allowchange := true;
mbcolorpalette1.CellSize := abs(updown1.Position);
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
var
 i: integer;
begin
 for i := 0 to ComponentCount - 1 do
  if IsPublishedProp(components[i], 'WebSafe') = true then
   SetOrdProp(components[i], 'WebSafe', integer(checkbox3.checked));
end;

procedure TForm1.Button5Click(Sender: TObject);
var
 i: integer;
begin
 mbcolortree1.ClearColors;
 mbcolorlist1.ClearColors;
 for i := 0 to mbcolorpalette1.Colors.Count - 1 do
  begin
   mbcolortree1.AddColor('Color '+inttostr(i), StringtoColor(mbcolorpalette1.colors.Strings[i]), false);
   mbcolorlist1.AddColor('Color '+inttostr(i), StringtoColor(mbcolorpalette1.colors.Strings[i]), false);
  end;
 mbcolortree1.UpdateColors;
 mbcolorlist1.UpdateColors;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
sc.swatchstyle := checkbox4.Checked;
uc.swatchstyle := checkbox4.checked;
end;

end.
