unit dlGUIImage;

interface

uses Graphics, dlGUITypes, dlGUIObject;

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
   TGUIImage = class(TGUIObject)
     protected
       procedure SetResize; override; //Применить к вершинам масштабирование Width, Height
     public
       constructor Create(pName: String; pX, pY, pW, pH: Integer; pTextureLink: TTextureLink = nil);
       procedure SetImageLink(const ATextureLink: TTextureLink);
     published
       property ObjectType;
       property Name;
       property X;
       property Y;
       property Width;
       property Height;
       property TextureName;
       //классы
       property Parent;
       property PopupMenuName;
       property Hint;
       property Blend;
   end;

implementation

{ TGUIImage }

constructor TGUIImage.Create(pName: String; pX, pY, pW, pH: Integer; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcImage);
  SetRect(pX, pY, pW, pH);

  Area.Show:= True;

  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(0, 0, pW, pH, FColor.GetColor, pTextureLink);
end;

procedure TGUIImage.SetImageLink(const ATextureLink: TTextureLink);
var T: TTextureLinkSquadArr;
    Link: TTextureLink;
begin
  SetTextureLink(ATextureLink);

  //Получаем новую текстуру
  Link:= GetTextureLink;

  //Если обнулили текстуру то
  if not Assigned(Link) then
  begin
    VertexList.SetVertexTextureOne(0, 0);
    Exit;
  end;

  //Если есть текстура то применим новые размеры
  T.SetCoord(0, 0, Width, 0, Width, Height, 0, Height);
  VertexList.SetVertexTextureMap(0, T);
end;

procedure TGUIImage.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
end;


end.
