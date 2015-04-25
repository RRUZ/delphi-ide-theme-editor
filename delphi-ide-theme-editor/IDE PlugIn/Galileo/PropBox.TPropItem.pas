type
  TPropItem=class(TPersistent)
  private
   FListBox            :TCustomPropListBox;
   FParent             :TPropItem;
   FProp               :IProperty;
   FLevel              :Integer;
   FExpanded           :Boolean;
   FWasExpandable      :Boolean;
   FDirty              :Boolean;
   FColor              :TColor;
   FReference          :Boolean;
  public
   constructor Create(const AProp: IProperty; ALevel: Integer; AColor: TColor; AReference: Boolean; AListBox: TCustomPropListBox; AParent: TPropItem);
   class destructor Destroy;
   function IsOverExpandBox(X: Integer; Y: Integer): Boolean;
  published
   procedure Revert;
   property Attributes: TPropertyAttributes;
   property AutoFill: Boolean;
   property Prop: IProperty;
   property Dirty: Boolean;
   property TypeName: string;
   property GetName: string;
   property GetPath: string;
   property GetValue: string;
   property IsExpanded: Boolean;
   property IsExpandable: Boolean;
   property WasExpandable: Boolean;
   property ItemLevel: Integer;
   property Color: TColor;
   property Reference: Boolean;
   property ListBox: TCustomPropListBox;
   property Parent: TPropItem;
  end;
