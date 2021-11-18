unit dlGUIMouse;

interface

uses dlGUITypes, dlGUIPaletteHelper, dlGUIObject, dlOpenGL, Graphics;

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
  TGUIMouse = class(TGUIObject)
    strict private
      const MOUSE_SIZE = 18;
    public
      constructor Create(pScale: TFloat; pColor: TColor; pTextureLink: TTextureLink);
      procedure OnMouseMove(pX, pY: Integer); override;
      procedure Render; override;
  end;

implementation

{ TGUIMouse }


constructor TGUIMouse.Create(pScale: TFloat; pColor: TColor; pTextureLink: TTextureLink);
begin
  inherited Create('', gtcObject);

  SetRect(0, 0, MOUSE_SIZE, MOUSE_SIZE);
  FScale   := pScale;
  FModeDraw:= GL_TRIANGLE_FAN;
  FColor.SetColor(pColor);
  Blend.Set_SrcAlpha_OneMinusSrcAlpha;
  Blend.Alpha:= 1;

  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_Mouse));
end;

procedure TGUIMouse.OnMouseMove(pX, pY: Integer);
begin
  SetPos(pX, pY);
end;

procedure TGUIMouse.Render;
begin
  inherited;
end;

end.
