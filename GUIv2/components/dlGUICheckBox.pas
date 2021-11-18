unit dlGUICheckBox;

interface

 uses dlOpenGL, dlGUITypes, dlGUIFont, Graphics, dlGUIObject, dlGUIPaletteHelper, dlGUIXMLSerial;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author  : Ansperi L.L., 2021                     =
  = Email   : gui_proj@mail.ru                       =
  = Site    : lemgl.ru                               =
  = Telegram: https://t.me/delphi_lemgl              =
  =                                                  =
  ====================================================
}

 type
  TGUICheckBox = class(TGUIObject)
     strict private
       FChecked: Boolean;
       FText   : String;
     strict private
       function GetRectCheckBox: TGUIObjectRect;
     protected
       procedure SetAreaResize; override;
       procedure SetResize; override;
       procedure SetFontEvent; override; //Событие при создании шрифта
       procedure SetText(AValue: String);
     public
       constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
       procedure RenderText; override;
       procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
     public
       [TXMLSerial] property Checked: Boolean read FChecked write FChecked;
       [TXMLSerial] property Text: String read FText write SetText;
   end;

implementation

 const GROUP_UNCHECKED = 0;
       GROUP_CHECKED   = 1;
       CHECKBOX_SIZE   = 16;

{ TGUICheckBox }

constructor TGUICheckBox.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
var ARect: TGUIObjectRect;
begin
  inherited Create(pName, gtcCheckBox);
  SetRect(0, 0, 100, CHECKBOX_SIZE);

  FText    := '';
  Area.Show:= True;
  Area.Color.SetColor(clGray);

  FTextOffset.SetRect(CHECKBOX_SIZE + 4, 0, 0, 0);
  SetTextureLink(pTextureLink);

  //Основная часть
  ARect:= GetRectCheckBox;
  VertexList.MakeSquare(ARect, Color, GUIPalette.GetCellRect(pal_CheckBox_uc), GROUP_UNCHECKED);
  VertexList.MakeSquare(ARect, Color, GUIPalette.GetCellRect(pal_CheckBox_ch), GROUP_CHECKED, True);
end;

procedure TGUICheckBox.SetAreaResize;
begin
  Area.Rect.SetRect(GetRectCheckBox);
end;

procedure TGUICheckBox.SetFontEvent;
begin
  inherited;

  FTextOffset.Y:= -Trunc((Font.Height - CHECKBOX_SIZE) / 2) - 1;
  Width:= Round(Font.GetTextWidth(FText)) + CHECKBOX_SIZE + 4;
end;

procedure TGUICheckBox.SetResize;
var ARect: TGUIObjectRect;
begin
  ARect:= GetRectCheckBox;
  VertexList.SetSizeSquare(0, ARect);
  VertexList.SetSizeSquare(4, ARect);
end;

procedure TGUICheckBox.SetText(AValue: String);
begin
  FText:= AValue;
end;

function TGUICheckBox.GetRectCheckBox: TGUIObjectRect;
begin
  Result.SetRect(Rect.X, Rect.Y, CHECKBOX_SIZE, CHECKBOX_SIZE);
end;

procedure TGUICheckBox.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;
  if not OnHit(pX, pY) then
    Exit;

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
