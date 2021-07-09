unit dlGUIEditBox;

interface

uses Windows, Graphics, Classes, dlOpenGL, dlGUITypes, dlGUIObject, dlGUIPaletteHelper;

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
  TGUIBorderStyle = (
                       bsSingle, //Показывать рамку
                       bsBottom, //Нижняя полоса
                       bsNone    //Не показывать рамку
                    );

  TGUITypeInput = (
                    tiAll,     //Любые символы 32..255
                    tiNumbers, //Только цифры
                    tiFloat,   //Цифры с точкой
                    tiCustom   //Свои значения (заполняется поле) Mask
                   );

  TGUIEditBox = class(TGUIObject)
    private
      FOffsetX    : Integer; //Сдвиг текста

      FDrawText   : String; //Текст который рисуем
      FText       : String; //Текст
      FCursor     : TGUICursor; //Мигающий курсор
      FCursorWidth: Integer; //Ширина курсора

      FUpdate     : Boolean; //Текст изменился
      FMaxLength  : Integer; //Макс длинна текста

      FTypeInput  : TGUITypeInput; //Тип ввода символов (любые, только цифры)
      FMask       : String; //Маска ввода (какие символы разрешено вводить)
      FReadOnly   : Boolean; //Нельзя менять данные в поле ввода

      FBorderStyle: TGUIBorderStyle;
    private
      function SetCursorPos(pValue: Integer): Boolean;
      procedure SetMaxLength(pMaxLength: Integer);
      procedure SetText(pText: String);
      procedure UpdateCursorRect;
      procedure SetBorderStyle(pBorderStyle: TGUIBorderStyle);
      procedure ResetCursor;
    protected
      procedure SetFontEvent; override;
      procedure SetResize; override;
    public
      constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);

      procedure OnKeyDown(var Key: Word; Shift: TShiftState); override;
      procedure OnKeyPress(Key: Char); override;

      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;

      procedure RenderText; override;
      procedure Render; override;
    published
      property ObjectType;
      property Name;
      property X;
      property Y;
      property Width;
      property Height;
      property Font;
      property Hide;
      property TextureName;
      //классы
      property Parent;
      property PopupMenuName;
      property Hint;
      property Blend;

      property CursorWidth: Integer         read FCursorWidth write FCursorWidth;
      property Text       : String          read FText        write SetText;
      property MaxLength  : Integer         read FMaxLength   write SetMaxLength;
      property TypeInput  : TGUITypeInput   read FTypeInput   write FTypeInput;
      property Mask       : String          read FMask        write FMask;
      property ReadOnly   : Boolean         read FReadOnly    write FReadOnly;
      property BorderStyle: TGUIBorderStyle read FBorderStyle write SetBorderStyle;
  end;

implementation

const GROUP_CURSOR = 1;

{ TGUIEditBox }

constructor TGUIEditBox.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcEditBox);

  FCursor:= TGUICursor.Create(clWhite);
  SetRect(pX, pY, 80, 20);

  FCursorWidth     := 1;
  FCursor.CharPos  := 1;
  FCursor.RenderPos:= 0.0;
  FOffsetX         := 0;
  FTypeInput       := tiAll;
  FMask            := '';
  Area.Show        := True;

  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(0, 0, Width, Height, Color, GUIPalette.GetCellRect(pal_Frame));
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_Window));
  UpdateCursorRect;
end;

procedure TGUIEditBox.OnKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Enable then
    Exit;

  if not Focused then
    Exit;

  case Key of
    VK_LEFT   : SetCursorPos(FCursor.CharPos - 1);
    VK_RIGHT  : SetCursorPos(FCursor.CharPos + 1);
    VK_END    : SetCursorPos((Length(FText) - FOffsetX) + 1);
    VK_HOME   : ResetCursor;
    VK_BACK   : if SetCursorPos(FCursor.CharPos - 1) then
                   Delete(FText, FCursor.CharPos + FOffsetX, 1);
    VK_DELETE : if SetCursorPos(FCursor.CharPos) then
                   Delete(FText, FCursor.CharPos + FOffsetX, 1);
  end;
end;

procedure TGUIEditBox.OnKeyPress(Key: Char);
var AnsiKey: AnsiChar;
begin
  if not Enable then
    Exit;

  if not Focused then
    Exit;

  if FReadOnly then
    Exit;

  if (FMaxLength <> 0) and (Length(FText) >= FMaxLength) then
    Exit;

  AnsiKey:= UTF8EncodeToShortString(Key)[1];
  if AnsiKey < AnsiChar(31) then
    Exit;

  case FTypeInput of

    //Любые символы
    tiAll    : ;

    //Только цифры
    tiNumbers:
      if not (AnsiKey in ['0'..'9']) then
        Exit;

    //Цифры и точка
    tiFloat  :
      if not (AnsiKey in ['0'..'9', '.']) then
        Exit;

    //По своей "маске"
    tiCustom :
      if (Pos(Key, Mask) = 0) then
        Exit;

  end;

  Insert(Key, FText, FCursor.CharPos + FOffsetX);
  SetCursorPos(FCursor.CharPos + 1);
end;

procedure TGUIEditBox.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i   : Integer;
    BPos: TFloat;
begin
  inherited;

  //Показываем курсор
  FCursor.ResetCursor;

  BPos:= 0.0;

  for i := 1 to Length(FDrawText) do
  begin
    BPos:= BPos + Font.GetTextWidth(FDrawText[i]);
    if BPos > (pX - X) then Break;
  end;

  SetCursorPos(i);
end;

procedure TGUIEditBox.Render;
begin
  if FBorderStyle <> bsNone then
    inherited
  else
  begin
    AfterObjRender;
    RenderText;
  end;

  if not Enable then
    Exit;

  if not Focused then
    Exit;

  if ReadOnly then
    Exit;

  FCursor.Render;
end;

procedure TGUIEditBox.RenderText;
begin
  inherited;

  if FUpdate then
  begin
    FDrawText:= Copy(FText, FOffsetX + 1, Length(FText));
    FUpdate  := False;
  end;

  if Font.Height > Height then
    Exit;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FDrawText, Width);
end;

procedure TGUIEditBox.ResetCursor;
begin
  FOffsetX:= 0;
  SetCursorPos(1);
end;

procedure TGUIEditBox.SetBorderStyle(pBorderStyle: TGUIBorderStyle);
begin
  FBorderStyle:= pBorderStyle;

  case FBorderStyle of

    bsBottom: begin
                //Переводим метод прорисовки в GL_LINE_LOOP
                FModeDraw:= GL_LINE_LOOP;
                //Показываем только вершины 2, 3, 6, 7
                VertexList.SetVertexHideList(false, [2, 3]);
                //Сообщаем что на 3-й вершине объект заканчивается и начинается другой
                VertexList.Vertex[3].GapOccur:= True;
                //Устанавливаем текстурные координаты
                VertexList.SetVertexTextureOne(0, 0);
              end;

    bsNone, bsSingle:
              begin
                //Переводим метод прорисовки в GL_QUADS (по умолчанию у TGUIObject)
                FModeDraw:= GL_QUADS;
                //
                VertexList.SetVertexHideList(true, [-1]);
                //
                VertexList.Vertex[3].GapOccur:= False;
                //
                VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(5));
                VertexList.SetVertexTextureMap(4, GUIPalette.GetCellRect(4));
              end;

  end;

end;

function TGUIEditBox.SetCursorPos(pValue: Integer): Boolean;
var OldCharPos: Integer;
begin
  FUpdate        := True;
  Result         := True;
  OldCharPos     := FCursor.CharPos;
  FCursor.CharPos:= pValue;

  if (FCursor.CharPos + FOffsetX < 1) or (Length(FText) < 1) then
  begin
    FCursor.CharPos  := 1;
    FCursor.RenderPos:= 0.0;
    Result           := False;
  end;

  if (FCursor.CharPos + FOffsetX) > Length(FText) + 1 then
  begin
    FCursor.CharPos:= OldCharPos;
    SetCursorPos(FCursor.CharPos);
    Exit;
  end;

  FCursor.RenderPos:= Font.GetTextWidth(Copy(FText, FOffsetX + 1, FCursor.CharPos - 1));
  FCursor.ResetCursor;

  //Зашли за границу
  if FCursor.RenderPos > Width - 4 then
  begin
    FOffsetX := FOffsetX + 1;
    SetCursorPos(FCursor.CharPos - 1);
    Exit;
  end;

  //Сдвиг курсора влево
  if (FCursor.CharPos < 1) and (FOffsetX > 0) then
  begin
    FOffsetX := FOffsetX - 1;
    SetCursorPos(FCursor.CharPos + 1);
    Exit;
  end;

end;

procedure TGUIEditBox.SetFontEvent;
begin
  inherited;

  if Rect.Height <> 0 then
    FTextOffset.Y:= Trunc(((Rect.Height - 2 - FFont.GetTextHeight) / 2))
  else
    FTextOffset.Y:= 0;
end;

procedure TGUIEditBox.SetMaxLength(pMaxLength: Integer);
begin
  FMaxLength:= pMaxLength;
  if FMaxLength < 0 then
    FMaxLength:= 0;
end;

procedure TGUIEditBox.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Width - 2, Rect.Height - 2);
  UpdateCursorRect;
end;

procedure TGUIEditBox.SetText(pText: String);
begin
  FText  := pText;
  FUpdate:= True;
  ResetCursor;
end;

procedure TGUIEditBox.UpdateCursorRect;
begin
  FCursor.Rect.SetRect(Rect.X + 1, Rect.Y + 2, FCursorWidth, Rect.Height - 4);
end;

end.
