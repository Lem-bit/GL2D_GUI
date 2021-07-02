unit dlGUIMouse;

interface

uses dlGUITypes, dlGUIPaletteHelper, dlGUIObject, dlOpenGL, Graphics;

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
  TGUIMouse = class(TGUIObject)
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

  FScale   := pScale;
  FModeDraw:= GL_TRIANGLE_FAN;
  FColor.SetColor(pColor);
  Blend.Set_SrcAlpha_OneMinusSrcAlpha;
  Blend.Alpha:= 1;

  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(0, 0, 16, 16, Color, GUIPalette.GetCellRect(18));
end;

procedure TGUIMouse.OnMouseMove(pX, pY: Integer);
begin
  X:= pX;
  Y:= pY;
end;

procedure TGUIMouse.Render;
begin
  //glBlendEquation(GL_FUNC_ADD);

  inherited;

end;

end.
