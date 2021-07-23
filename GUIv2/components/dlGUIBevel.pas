unit dlGUIBevel;

interface

uses
  dlOpenGL, dlGUITypes, dlGUIObject, dlGUIPaletteHelper;

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
  TGUIBevelShape = (
                      bsBox,   //Квадрат
                      bsTop,   //Только верхняя часть
                      bsLeft,  //Только левая часть
                      bsRight, //Только правая часть
                      bsBottom //Только нижняя часть
                    );

  TGUIBevel = class(TGUIObject)
    private
      FShape: TGUIBevelShape;
    private
      procedure SetShape(pValue: TGUIBevelShape);
    protected
      procedure SetResize; override;
    public
      constructor Create(pName: String; pX, pY, pW, pH: Integer; pTextureLink: TTextureLink = nil);
    public
      property Shape: TGUIBevelShape read FShape write SetShape;
  end;

implementation

{ TGUIBevel }

constructor TGUIBevel.Create(pName: String; pX, pY, pW, pH: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, gtcBevel);

  FShape   := bsBox;
  FModeDraw:= GL_LINE_LOOP;

  SetRect(pX, pY, pW, pH);

  SetTextureLink(pTextureLink);

  //Список вершин
  VertexList.MakeSquare(0, 0, Width, Height, Color, GUIPalette.GetCellRect(pal_2));
  //Замыкающая квадрат вершина
  VertexList.Vertex[VertexList.Count - 1].GapOccur:= True;
  //Установим единичную текстуру
  VertexList.SetVertexTextureOne;

  //Список вершин внутренней части
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_7));
  //Установим единичную текстуру как образец возьмем 4-ю вершину, преобразовать все остальные с 4-й
  VertexList.SetVertexTextureOne(4, 4);
end;

procedure TGUIBevel.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);
end;

procedure TGUIBevel.SetShape(pValue: TGUIBevelShape);
begin
  FShape:= pValue;

  case FShape of
    //true так как значение инвертируется если элемент не найден в списке
    bsBox   : VertexList.SetVertexHideList(true , [-1]);

    //
    bsTop   : VertexList.SetVertexHideList(false, [0, 1, 4, 5]);

    //
    bsLeft  : VertexList.SetVertexHideList(false, [0, 3, 4, 7]);

    //
    bsRight : VertexList.SetVertexHideList(false, [1, 2, 5, 6]);

    //
    bsBottom: VertexList.SetVertexHideList(false, [2, 3, 6, 7]);
  end;
end;

end.
