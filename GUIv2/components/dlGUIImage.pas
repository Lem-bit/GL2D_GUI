unit dlGUIImage;

interface

uses Graphics, dlGUITypes, dlGUIObject, dlGUIXmlSerial;

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
   TGUIImage = class(TGUIObject)
     strict private
       FProportion: Boolean;
     strict private
       procedure SetProportion(value: Boolean);
     protected
       procedure SetResize; override; //Применить к вершинам масштабирование Width, Height
     public
       constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
       procedure SetImageLink(const ATextureLink: TTextureLink);
     published
       [TXMLSerial] property Proportion: Boolean read FProportion write SetProportion;
   end;

implementation

{ TGUIImage }

constructor TGUIImage.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcImage);
  SetRect(0, 0, 200, 200);

  FProportion:= False;
  Area.Show  := True;
  VertexList.MakeSquare(Rect, Color, pTextureLink);

  SetImageLink(pTextureLink);
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
  if Proportion then
    T.SetCoord(0, 0, Width, 0, Width, Height, 0, Height)
  else
    T.SetCoord(0, 0, ATextureLink.Width, 0, ATextureLink.Width, ATextureLink.Height, 0, ATextureLink.Height);

  VertexList.SetVertexTextureMap(0, T);
end;

procedure TGUIImage.SetProportion(value: Boolean);
begin
  if FProportion = value then
    Exit;

  FProportion:= value;
  SetImageLink(Self.GetTextureLink);
end;

procedure TGUIImage.SetResize;
begin
  VertexList.SetSizeSquare(0, Rect);
end;


end.
