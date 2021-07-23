unit dlGUIButton;

interface

 uses dlGUITypes, dlGUIFont, Graphics, dlGUIObject, dlGUIPaletteHelper;

{
  ====================================================
  = Delphi OpenGL GUIv2                              =
  =                                                  =
  = Author: Ansperi L.L., 2021                       =
  = Email : gui_proj@mail.ru                         =
  = Site  : lemgl.ru                                 =
  =                                                  =
  ====================================================
}

  type
   TGUIButton = class(TGUIObject)
      private
        FCaption: String;
        FFlat   : Boolean; //Кнопка без рамки
      private
        procedure SetCaption(pCaption: String);
      protected
        procedure SetFontEvent; override;
        procedure RecalcTextPos;
        procedure SetResize; override; //Применить к вершинам масштабирование Width, Height
        procedure SetFlat(pFlat: Boolean);
      public
        constructor Create(pName: String; pCaption: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);

        procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
        procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
        procedure RenderText; override;
      published
        property Caption: String  read FCaption write SetCaption;
        property Flat   : Boolean read FFlat    write SetFlat;
      published
        property ObjectType;
        ///Name
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
   end;

implementation

{ TGUIButton }

const GROUP_DOWN        = 0;
      GROUP_DOWN_BORDER = 1;
      GROUP_UP          = 2;
      GROUP_UP_BORDER   = 3;

constructor TGUIButton.Create(pName: String; pCaption: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcButton);

  SetRect(pX, pY, 100, 25);
  FCaption:= pCaption;
  Area.Show:= True;
  FFlat    := False;

  RecalcTextPos;
  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(0, 0, Width, Height, Color, GUIPalette.GetCellRect(pal_Frame), GROUP_UP);
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_Window), GROUP_UP_BORDER, FFlat);

  VertexList.MakeSquare(0, 0, Width, Height, Color, GUIPalette.GetCellRect(pal_2), GROUP_DOWN, True);
  VertexList.MakeSquareOffset(8, 1, Color, GUIPalette.GetCellRect(pal_3), GROUP_DOWN_BORDER, True);
end;

procedure TGUIButton.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
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
    VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(4))
  else
    VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(5));

  VertexList.SetGroupHide(GROUP_DOWN_BORDER, FFlat);
  VertexList.SetGroupHide(GROUP_UP_BORDER  , FFlat);
  SetAction([goaTextureNeedRecalc]);
end;

procedure TGUIButton.SetFontEvent;
begin
  inherited;

  RecalcTextPos;
end;

procedure TGUIButton.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);

  VertexList.SetVertexPosSquare(8, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(12, 1, 1, Rect.Width - 2, Rect.Height - 2);

  RecalcTextPos;
end;

end.
