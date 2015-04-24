type
  TLogColorItem=class(TLogColorBase)
  private
   FRegSection         :string;
  public
   constructor Create(const AForegroundColor: TColor; const ABackgroundColor: TColor; const ARegSection: string);
   function Assign(Source: TLogColorItem): Boolean;
   procedure SaveOptions(IniFile: TCustomIniFile);
   procedure LoadOptions(IniFile: TCustomIniFile);
   property RegSection: string;
  end;
