unit dlGUIBevel;

interface

uses
  dlOpenGL, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUIBevelShape = (
                      bsBox,   //Квадрат
                      bsTop,   //Только верхняя часть
                      bsLeft,  //Только левая часть
                      bsRight, //Только правая часть
                      bsBottom //Только нижняя часть
                    );

  TGUIBevel = class(TGUIObject)
    strict private
      FShape: TGUIBevelShape;
    strict private
      procedure SetShape(pValue: TGUIBevelShape);
    protected
      procedure SetResize; override;
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
    public
     [TXMLSerial] property Rect;
     [TXMLSerial] property Shape: TGUIBevelShape read FShape write SetShape;
  end;

implementation

{ TGUIBevel }

constructor TGUIBevel.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcBevel);

  FShape   := bsBox;
  FModeDraw:= GL_LINE_LOOP;

  SetRect(0, 0, 200, 200);

  SetTextureLink(pTextureLink);

  //Список вершин
  VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_2));
  //Установим единичную текстуру
  VertexList.SetVertexTextureOne;

  //Список вершин внутренней части
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_7));
  //Установим единичную текстуру как образец возьмем 4-ю вершину, преобразовать все остальные с 4-й
  VertexList.SetVertexTextureOne(4, 4);
end;

procedure TGUIBevel.SetResize;
begin
  VertexList.SetSizeSquare(0, Rect);
  VertexList.SetSizeSquare(4, Rect, 1);
end;

procedure TGUIBevel.SetShape(pValue: TGUIBevelShape);
begin
  FShape:= pValue;

  case FShape of
    //
    bsBox   : VertexList.SetVertexShowInList(true , [-1]);

    //
    bsTop   : VertexList.SetVertexShowInList(false, [0, 1, 4, 5]);

    //
    bsLeft  : VertexList.SetVertexShowInList(false, [0, 3, 4, 7]);

    //
    bsRight : VertexList.SetVertexShowInList(false, [1, 2, 5, 6]);

    //
    bsBottom: VertexList.SetVertexShowInList(false, [2, 3, 6, 7]);
  end;
end;

end.
