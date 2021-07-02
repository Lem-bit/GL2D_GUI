unit dlGUICheckBox;

interface

 uses dlOpenGL, dlGUITypes, dlGUIFont, Graphics, dlGUIObject, dlGUIPaletteHelper;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author: Ansperi L.L., 2021                       =
  = Email : gui_proj@mail.ru                         =
  = Site  : lemgl.ru                                 =
  =                                                  =
  = Собрано на Delphi 10.3 community                 =
  ====================================================
}

 type
  TGUICheckBox = class(TGUIObject)
     private
       FChecked: Boolean;
       FText   : String;
     protected
       procedure SetAreaResize; override;
       procedure SetFontEvent; override; //Событие при создании шрифта
     public
       constructor Create(pName, pText: String; pX, pY: TInt; pTextureLink: TTextureLink = nil);
       procedure RenderText; override;
       procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
     published
       property ObjectType;
       property Name;
       property X;
       property Y;
       property Width;
       property Height;
       property Color;
       property Font;
       property Hide;
       property TextureName;
       //классы
       property Parent;
       property PopupMenuName;
       property Hint;
       property Blend;
       property Checked: Boolean read FChecked write FChecked;
       property Text: String read FText;
   end;

implementation

 const GROUP_UNCHECKED = 0;
       GROUP_CHECKED   = 1;
       CHECKBOX_SIZE   = 16;

{ TGUICheckBox }

constructor TGUICheckBox.Create(pName, pText: String; pX, pY: TInt; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcCheckBox);
  SetRect(pX, pY, 140, CHECKBOX_SIZE);

  FText    := pText;
  Area.Show:= True;

  FTextOffset.SetRect(CHECKBOX_SIZE + 4, 0, 0, 0);
  SetTextureLink(pTextureLink);

  //Основная часть
  VertexList.MakeSquare(0, 0, CHECKBOX_SIZE, CHECKBOX_SIZE, Color, GUIPalette.GetCellRect(8), GROUP_UNCHECKED);
  VertexList.MakeSquare(0, 0, CHECKBOX_SIZE, CHECKBOX_SIZE, Color, GUIPalette.GetCellRect(9), GROUP_CHECKED, True);
end;

procedure TGUICheckBox.SetAreaResize;
begin
  Area.Rect.SetRect(1, 0, CHECKBOX_SIZE - 1, CHECKBOX_SIZE - 1);
end;

procedure TGUICheckBox.SetFontEvent;
begin
  inherited;

  FTextOffset.Y:= -Trunc((Font.Height - CHECKBOX_SIZE) / 2) - 1;
end;

procedure TGUICheckBox.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;
  if Button <> gmbLeft then
    Exit;

  Checked:= not Checked;

  VertexList.SetGroupHide(GROUP_UNCHECKED, Checked);
  VertexList.SetGroupHide(GROUP_CHECKED  , not Checked);
end;

procedure TGUICheckBox.RenderText;
begin
  inherited;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, Rect.Width);
end;


end.
