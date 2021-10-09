unit dlGUIEditBox;

interface

uses SysUtils, Clipbrd, Windows, Graphics, Classes, dlOpenGL, dlGUITypes, dlGUIObject, dlGUIPaletteHelper,
  dlGUIFont;

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

  TGUIEditBoxSelection = class
    strict private
      const START_POS = 1;
            END_POS   = 2;
    strict private
      FUpdate    : Boolean; //Обновилась область выбора

      FOnSelect  : Boolean; //На данный момент идет выбор
      FSelStart  : Integer;
      FSelEnd    : Integer;

      FStartOffset: Integer;

      FText      : String;  //Выбранный текст

      FBlend     : TBlendParam;
      FRect      : TGUIObjectRect;
      FCurrSEPos : array[1..2] of Integer; //Текущие (расчитанные) стартовая и конечная позиции
      FParent    : TGUIObject;
    private
      procedure SetText(const AText: String);

      procedure SetSelStart(value: integer);
      procedure SetSelEnd(value: integer);

      function GetCurrentStartPos: Integer;
      function GetCurrentEndPos: Integer;

      procedure ResetUpdate; //Сбросить флаг обновления области выбора
    private
      property SelStart   : Integer read FSelStart    write SetSelStart;
      property SelEnd     : Integer read FSelEnd      write SetSelEnd;
      property StartOffset: Integer read FStartOffset write FStartOffset;
      property OnSelect   : Boolean read FOnSelect    write FOnSelect;
    public
      constructor Create(const AParent: TGUIObject);
      destructor Destroy; override;

      procedure Calc;
      procedure Render;
      procedure Cancel;
    public
      property CurrStart: Integer read GetCurrentStartPos;
      property CurrEnd  : Integer read GetCurrentEndPos;
      property Text     : String  read FText;
  end;

  TGUIEditBox = class(TGUIObject)
    strict private const
      VK_COPY_KEY  = 'C'; //CTRL + C
      VK_PASTE_KEY = 'V'; //CTRL + V
      VK_CUT_KEY   = 'X'; //CTRL + X
    strict private
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

      FSelection  : TGUIEditBoxSelection; //Выбор текста
    private
      //Позиция символа по координате
      function CharPosByCoord(pCoord: Integer): Integer;

      function SetCursorPos(pValue: Integer): Boolean;
      procedure SetMaxLength(pMaxLength: Integer);
      procedure SetText(pText: String);
      procedure UpdateCursorRect;
      procedure SetBorderStyle(pBorderStyle: TGUIBorderStyle);
      procedure ResetCursor;
      function GetOffsetX: Integer;

      procedure DeleteSelection;

      procedure DoKeyDown(var Key: Word; Shift: TShiftState);
      procedure DoKeyDownSelected(var Key: Word; Shift: TShiftState);

    protected
      procedure SetFontEvent; override;
      procedure SetResize; override;
    public
      OnCopy : TGUIProc;
      OnPaste: TGUIProc;
    public
      constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;

      procedure OnKeyDown(var Key: Word; Shift: TShiftState); override;
      procedure OnKeyPress(Key: Char); override;

      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;

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

function TGUIEditBox.CharPosByCoord(pCoord: Integer): Integer;
var i   : integer;
    BPos: TFloat;
begin
  BPos:= 0.0;

  for i := 1 to Length(FDrawText) do
  begin
    BPos:= BPos + Font.GetTextWidth(FDrawText[i]);
    if BPos > (pCoord - X) then
      Break;
  end;

  Result:= i;
end;

constructor TGUIEditBox.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcEditBox);

  FCursor   := TGUICursor.Create(clWhite);
  FSelection:= TGUIEditBoxSelection.Create(Self);
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

procedure TGUIEditBox.DeleteSelection;
begin
  if not FSelection.OnSelect then
    Exit;

  FSelection.Calc;
  Delete(FText, FSelection.GetCurrentStartPos, FSelection.GetCurrentEndPos);

  if (FOffsetX > FSelection.StartOffset)  then
      FOffsetX:= FSelection.StartOffset;

  SetCursorPos(FSelection.GetCurrentStartPos - FOffsetX);
  FSelection.Cancel;
  FUpdate:= True;
end;

destructor TGUIEditBox.Destroy;
begin
  if Assigned(FCursor) then
    FreeAndNil(FCursor);

  if Assigned(FSelection) then
    FreeAndNil(FSelection);

  inherited;
end;

procedure TGUIEditBox.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
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

procedure TGUIEditBox.DoKeyDownSelected(var Key: Word; Shift: TShiftState);

procedure ActiveSelection;
begin
  if FSelection.OnSelect then
    Exit;

  FSelection.OnSelect   := True;
  FSelection.StartOffset:= FOffsetX;
  FSelection.SelStart   := FCursor.CharPos;
  FSelection.SelEnd     := FCursor.CharPos + FOffsetX;
  FSelection.Calc;
end;

begin
  //Если шифт не нажат значит обрабатываем как простое нажатие
  if not (ssShift in Shift) then
    case Key of
      VK_LEFT, VK_RIGHT, VK_END, VK_HOME:
        begin
          FSelection.Cancel;
          DoKeyDown(Key, Shift);
          Exit;
        end;
    end;

  //Если шифт нажат
  ActiveSelection;

  case Key of
    VK_LEFT  : begin
                 SetCursorPos(FCursor.CharPos - 1);
                 FSelection.SelEnd:= FCursor.CharPos + FOffsetX;
                 FSelection.Calc;
               end;
    VK_RIGHT : begin
                 SetCursorPos(FCursor.CharPos + 1);
                 FSelection.SelEnd:= FCursor.CharPos + FOffsetX;
                 FSelection.Calc;
               end;
    VK_END   : begin
                 SetCursorPos((Length(FText) - FOffsetX) + 1);
                 FSelection.SelEnd:= Length(FText) + 1;
                 FSelection.Calc;
               end;
    VK_HOME  : begin
                 ResetCursor;
                 FSelection.SelEnd:= 1;
                 FSelection.Calc;
               end;

    VK_BACK  ,
    VK_DELETE: DeleteSelection;
  end;

end;

function TGUIEditBox.GetOffsetX: Integer;
begin
  Result:= FOffsetX;
end;

procedure TGUIEditBox.OnKeyDown(var Key: Word; Shift: TShiftState);
begin
  if not Enable then
    Exit;

  if not Focused then
    Exit;

  //Копировать при ReadOnly можно
  if ssCtrl in Shift then
    if SameText(AnsiUpperCase(String(AnsiChar(Key))), VK_COPY_KEY) then
    begin
      if Assigned(OnCopy) then
        OnCopy(Self, nil);

      Clipboard.AsText:= FSelection.Text;
    end;

  //Вставлять и печатать при ReadOnly нельзя
  if ReadOnly then
    Exit;

  //Вырезать
  if ssCtrl in Shift then
    if SameText(AnsiUpperCase(String(AnsiChar(Key))), VK_CUT_KEY)  then
    begin
      Clipboard.AsText:= FSelection.Text;
      DeleteSelection;
    end;

  //Вставить
  if ssCtrl in Shift then
    if SameText(AnsiUpperCase(String(AnsiChar(Key))), VK_PASTE_KEY)  then
    begin
      if ReadOnly then
        Exit;

      if Assigned(OnPaste) then
        OnPaste(Self, nil);

      DeleteSelection;
      Insert(Clipboard.AsText, FText, FCursor.CharPos + FOffsetX);
      SetCursorPos(FCursor.CharPos + Length(Clipboard.AsText));
      Exit;
    end;

  if (FSelection.OnSelect) or (ssShift in Shift) then
    DoKeyDownSelected(Key, Shift)
  else
    DoKeyDown(Key, Shift);
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

  if FSelection.OnSelect then
    DeleteSelection;

  Insert(Key, FText, FCursor.CharPos + FOffsetX);
  SetCursorPos(FCursor.CharPos + 1);
end;

procedure TGUIEditBox.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var Index: integer;
begin
  inherited;

  //Показываем курсор
  FCursor.ResetCursor;

  if Button <> gmbLeft then
  begin
    Index:= CharPosByCoord(pX);
    if (Index > FSelection.SelStart) and
       (Index < FSelection.SelEnd) then
    Exit;
  end;

  FSelection.Cancel;
  SetCursorPos(CharPosByCoord(pX));

  if not (goaFocused in GetAction) then
    Exit;

  //Предполагаем что может начаться выделение текста
  FSelection.SelStart   := FCursor.CharPos; //индекс
  FSelection.StartOffset:= FOffsetX;
end;

procedure TGUIEditBox.OnMouseMove(pX, pY: Integer);
begin
  inherited;

  if Assigned(PopupMenu) then
    if goaDown in PopupMenu.GetAction then
      Exit;

  if not (goaFocused in GetAction) then
    Exit;

  //Выделяем текст
  if not (goaDown in GetAction) then
    Exit;

  FSelection.OnSelect:= True;

  if (pX < Rect.X) then
    SetCursorPos(FCursor.CharPos - 1);

  SetCursorPos(CharPosByCoord(pX));
  FSelection.SelEnd := FCursor.CharPos + FOffsetX;
end;

procedure TGUIEditBox.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  if Button <> gmbLeft then
    Exit;

  if not (goaFocused in GetAction) then
    Exit;

  if not FSelection.OnSelect then
    Exit;

  if Assigned(PopupMenu) then
    if goaDown in PopupMenu.GetAction then
      Exit;

  SetCursorPos(CharPosByCoord(pX));
  FSelection.SelEnd := FCursor.CharPos + FOffsetX;
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

  if not (goaFocused in GetAction) then
    if FSelection.OnSelect then
      FSelection.Cancel;

  FSelection.Calc;
  FSelection.Render;

  Hint.Enable:= True;
  Hint.Text:= FSelection.Text;

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
                VertexList.SetVertexShowInList(false, [2, 3]);
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
                VertexList.SetVertexShowInList(true, [-1]);
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

  if FOffsetX < 0 then
    FOffsetX:= 0;

  if (FCursor.CharPos + FOffsetX < 1) or (Length(FText) < 1) then
  begin
    FCursor.CharPos  := 1;
    FCursor.RenderPos:= 0.0;
    Result           := False;
  end;

  if (FCursor.CharPos + FOffsetX) > Length(FText) + 1 then
  begin
    FCursor.CharPos:= OldCharPos;

    if FCursor.CharPos <> pValue then
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
    FTextOffset.Y:= Trunc(((Rect.Height - 2 - FFont.Height) / 2))
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

{ TGUIEditBoxSelection }

procedure TGUIEditBoxSelection.Calc;
var OX, SX, EX: Integer;
    Parent: TGUIEditBox;
begin
  if not OnSelect then
    Exit;

  if not FUpdate then
    Exit;

  if not Assigned(FParent) then
    Exit;

  Parent:= TGUIEditBox(FParent);
  FRect.SetRect(Parent.Rect);

  FCurrSEPos[START_POS]:= SelStart + StartOffset;
  FCurrSEPos[END_POS  ]:= SelEnd - FCurrSEPos[START_POS];

  //Обратное направление выбора
  if FCurrSEPos[END_POS] < 0 then
  begin
    FCurrSEPos[END_POS]  := FCurrSEPos[START_POS] - SelEnd;
    FCurrSEPos[START_POS]:= SelEnd;
  end;

  //Копируем выбранный текст
  SetText(Copy(Parent.Text, FCurrSEPos[START_POS], FCurrSEPos[END_POS]));

  //
  SX:= Round(Parent.Font.GetTextWidth(
      Copy(Parent.Text, 1 + Parent.GetOffsetX, FCurrSEPos[START_POS] - Parent.GetOffsetX - 1))
    );
  EX:= Round(Parent.Font.GetTextWidth(
      Copy(Parent.Text, FCurrSEPos[START_POS], FCurrSEPos[END_POS]))
    );
  OX:= Round(Parent.Font.GetTextWidth(
       Copy(Parent.Text, 1 + Parent.GetOffsetX, FCurrSEPos[END_POS] - (Parent.GetOffsetX + 1) + FCurrSEPos[START_POS] ))
    );

  if EX > OX then
    EX:= EX - (EX - OX);

  if Parent.GetOffsetX > FCurrSEPos[START_POS] then
    FRect.X:= Parent.Rect.X
  else
    FRect.X:= Parent.Rect.X + SX;

  if SX + EX > Parent.Rect.Width then
    FRect.Width:= Parent.Rect.Width - SX
  else
    FRect.Width:= EX;

  ResetUpdate;
end;

procedure TGUIEditBoxSelection.Cancel;
begin
  OnSelect  := False;
  SelStart  := 0;
  SelEnd    := 0;
  SetText('');
  ResetUpdate;
end;

constructor TGUIEditBoxSelection.Create(const AParent: TGUIObject);
begin
  FBlend:= TBlendParam.Create;
  FBlend.Set_One_One;
  FParent:= AParent;
end;

destructor TGUIEditBoxSelection.Destroy;
begin
  if Assigned(FBlend) then
    FreeAndNil(FBlend);

  inherited;
end;

function TGUIEditBoxSelection.GetCurrentEndPos: integer;
begin
  Result:= FCurrSEPos[END_POS];
end;

function TGUIEditBoxSelection.GetCurrentStartPos: Integer;
begin
  Result:= FCurrSEPos[START_POS];
end;

procedure TGUIEditBoxSelection.Render;
begin
  if not OnSelect then
    Exit;

  FBlend.Bind;
  TGLColor.glColor3fx($00333333);
  FRect.SetRect(FRect);
  FRect.Render(0, GL_QUADS);
end;

procedure TGUIEditBoxSelection.ResetUpdate;
begin
  FUpdate:= False;
end;

procedure TGUIEditBoxSelection.SetSelEnd(value: integer);
begin
  FSelEnd:= Value;
  FUpdate:= True;
end;

procedure TGUIEditBoxSelection.SetSelStart(value: integer);
begin
  FSelStart:= Value;
  FUpdate  := True;
end;

procedure TGUIEditBoxSelection.SetText(const AText: String);
begin
  FText:= AText;
end;

end.
