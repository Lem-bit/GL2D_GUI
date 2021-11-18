unit dlGUIButton;

interface

 uses dlGUITypes, dlGUIFont, Graphics, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
   TGUIButton = class(TGUIObject)
      strict private
        FCaption: String;
        FFlat   : Boolean; //Кнопка без рамки
      strict private
        procedure SetCaption(pCaption: String);
      protected
        procedure SetFontEvent; override;
        procedure RecalcTextPos;
        procedure SetResize; override; //Применить к вершинам масштабирование Width, Height
        procedure SetFlat(pFlat: Boolean);
      public
        constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);

        procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
        procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
        procedure RenderText; override;
      public
        ///Name
        [TXMLSerial] property Caption: String  read FCaption write SetCaption;
        [TXMLSerial] property Flat   : Boolean read FFlat    write SetFlat;
        [TXMLSerial] property Rect;
        [TXMLSerial] property Color;
        [TXMLSerial] property Font;
        [TXMLSerial] property Hide;
        [TXMLSerial] property TextureName;
        //классы
        [TXMLSerial] property PopupMenu;
        [TXMLSerial] property Hint;
        [TXMLSerial] property Blend;
   end;

implementation

{ TGUIButton }

const GROUP_DOWN        = 0;
      GROUP_DOWN_BORDER = 1;
      GROUP_UP          = 2;
      GROUP_UP_BORDER   = 3;

constructor TGUIButton.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcButton);

  SetRect(0, 0, 100, 25);
  FCaption := '';
  Area.Show:= True;
  FFlat    := False;

  RecalcTextPos;
  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_Frame), GROUP_UP);
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_Window), GROUP_UP_BORDER, FFlat);

  VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_2), GROUP_DOWN, True);
  VertexList.MakeSquareOffset(8, 1, Color, GUIPalette.GetCellRect(pal_3), GROUP_DOWN_BORDER, True);
end;

procedure TGUIButton.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  if not OnHit(pX, pY) then
    Exit;

  if Button <> gmbLeft then
    Exit;

  inherited;

  VertexList.SetGroupHide(GROUP_DOWN       , False);
  VertexList.SetGroupHide(GROUP_DOWN_BORDER, False or FFlat);
  VertexList.SetGroupHide(GROUP_UP         , True);
  VertexList.SetGroupHide(GROUP_UP_BORDER  , True);
end;

procedure TGUIButton.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  VertexList.SetGroupHide(GROUP_DOWN       , True);
  VertexList.SetGroupHide(GROUP_DOWN_BORDER, True);
  VertexList.SetGroupHide(GROUP_UP         , False);
  VertexList.SetGroupHide(GROUP_UP_BORDER  , False or FFlat);
end;

procedure TGUIButton.RecalcTextPos;
begin
  if Rect.Width <> 0 then
    FTextOffset.X:= Trunc((Rect.Width / 2) - (FFont.GetTextWidth(FCaption) / 2))
  else
    FTextOffset.X:= 0;

  if Rect.Height <> 0 then
    FTextOffset.Y:= Trunc((Rect.Height / 2) - (FFont.Height / 2))
  else
    FTextOffset.Y:= 0;
end;

procedure TGUIButton.RenderText;
begin
  inherited;

  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FCaption, Rect.Width);
end;

procedure TGUIButton.SetCaption(pCaption: String);
begin
  if FCaption = pCaption then
    Exit;

  FCaption:= pCaption;
  RecalcTextPos;
end;

procedure TGUIButton.SetFlat(pFlat: Boolean);
begin
  FFlat:= pFlat;

  if FFlat then
    VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(pal_Window));

  OnMouseUp(0, 0, gmbNone);
  SetAction([goaTextureNeedRecalc]);
end;

procedure TGUIButton.SetFontEvent;
begin
  inherited;

  RecalcTextPos;
end;

procedure TGUIButton.SetResize;
begin
  VertexList.SetSizeSquare(0, Rect);
  VertexList.SetSizeSquare(4, Rect, 1);

  VertexList.SetSizeSquare(8 , Rect);
  VertexList.SetSizeSquare(12, Rect, 1);

  RecalcTextPos;
end;

end.
