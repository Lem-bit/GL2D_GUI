unit dlGUIFont;

interface

 uses dlGUITypes, dlGUICodePage, Windows, Graphics, System.Classes, SysUtils, dlOpenGL;

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
   //Символ
   TGUIFontChar = record
     public
       Sym   : char;     //Символ
       Width : TFloat;   //Ширина символа
       s, t  : TFloat;   //Начальные координаты текстуры
       sw, tw: TFloat;   //Конечные координаты
       color : array[0..3] of TGLColor4Rec; //Цвет символа
     public
       //Установить цвет символа
       procedure SetColor(pColor: Integer);
       procedure ClearTextureCoord;
   end;

   //Для FontMaker
   TGUIFontStream = record
     public
       Name         : String[255]; //Название шрифта
       Size         : Integer; //Размер шрифта
       Height       : Integer; //Высота шрифта
       SquadSize    : Integer; //Размер квадрата для символа
       OffsetX      : Integer; //Сдвиг символов по X
       OffsetY      : Integer; //Сдвиг символов по Y
       ShowGrid     : Boolean; //Показывать сетку
       CharInfo     : array[0..High(Byte)] of TGUIFontChar; //Параметры символов
       Grades       : array[0..1] of Integer; //Максимальная длинна по ширине и высоте
       OffsetTexture: TTextureOffset; //Сдвиг области текстуры (для не стандартных шрифтов)
       CharSpace    : TFloat; //Расстояние между символами
   end;

   //Состояние нужно знать когда шрифт увеличился или произошли какие то изменения
   TGUIFontStatus  = (gfsNone, gfsUpdate);
   TGUIFontSetter  = (fsetColor, fsetScale, fsetTexture);
   TGUIFontSetterA = Set of TGUIFontSetter;

   //Шрифт
   TGUIFontCharInfo = array[0..High(Byte)] of TGUIFontChar;
   TGUIFont = class(TPersistent)
     strict private
       FSelfTexture  : Boolean;
     private
       FName         : String[255]; //Название шрифта
       FSize         : Integer;   //Размер шрифта
       FHeight       : Integer;   //Высота шрифта
       FSquadSize    : Integer;   //Размер квадратной области для символа
       FGrades       : array[0..1] of Integer; //Размер сетки 16x16 например
       FCharInfo     : TGUIFontCharInfo; //Информация о символах
       FTextureLink  : TTextureLink; //Текстура
       FFileName     : String[255];  //Имя файла
       FColor        : TGLColor; //Основной цвет
       FOffsetX      : Integer;  //Сдвиг символов по X
       FOffsetY      : Integer;  //Сдвиг символов по Y
       FShowArea     : Boolean;  //Показывать рамку вывода
       FScale        : TFloat;   //Размер шрифта
       //Опции
       FTextureOffset: TTextureOffset;
       FCharSpace    : TFloat; //Корректор расстояния между символами

       FStatus       : TGUIFontStatus;
       FSetter       : TGUIFontSetterA;
     private
       procedure FreeSelfTexture;
       procedure SetColor(pColor: TColor);
       function GetColor: TColor;
       procedure UpdateStatus(pStatus: TGUIFontSetter);
       //
       function GetHeight: TFloat;
     public
       constructor Create(pTextureLink: TTextureLink);
       destructor Destroy; override;
       //Загрузить информацию о текстуре
       function LoadInfoFromFile(const AFileName: String): Boolean;
       //Пересчитать текстурные координаты
       function CalcTextureCoord: Boolean;
       //Отобразить текст
       procedure RenderText(const pX, pY: TFloat; const pText: String; pMaxWidth: Integer; pUseLineBreak: Boolean = false);
       procedure Text(const pX, pY: TFloat; const pText: String; pMaxWidth: Integer = 0);
       procedure CopyFrom(pFont: TGUIFont);
       procedure CopyMemoryFrom(pFont: TGUIFont);
       //Скопировать объект текстуры
       procedure SetTextureLink(pTextureLink: TTextureLink);
       function GetTextureName: String;
       function GetTextureLink: TTextureLink;
       //
       //Получить макс кол-во символов в заданной ширине
       function GetTextWidthMax(pText: String; pMax: TFloat): Integer;
       function GetTextWidth(pText: String): TFloat;
       //
       procedure GetTextRect(const pText: String; out pWidth, pHeight: Integer);
       //
       procedure SetScale(pScale: TFloat);
     public
       property _State  : TGUIFontStatus   read FStatus     write FStatus;
       property _Setter : TGUIFontSetterA  read FSetter;

       property Color   : TColor           read GetColor    write SetColor;
       property ColorObj: TGLColor         read FColor;
       property CharInfo: TGUIFontCharInfo read FCharInfo;
       property ShowArea: Boolean          read FShowArea   write FShowArea;

       property Height  : TFloat           read GetHeight;
     published
       property TextureName: String        read GetTextureName;
       property Scale      : TFloat        read FScale      write SetScale;
   end;
   PGUIFont = ^TGUIFont;

implementation

{ TGUIFont }

function TGUIFont.GetTextureLink: TTextureLink;
begin
  Result:= FTextureLink;
end;

function TGUIFont.GetTextureName: String;
begin
  Result:= '';

  if not Assigned(FTextureLink) then
    Exit;

  Result:= FTextureLink.Name;
end;

procedure TGUIFont.CopyFrom(pFont: TGUIFont);
begin
  if not Assigned(FTextureLink) then
  begin
    FTextureLink:= TTextureLink.Create;
    FSelfTexture:= True;
  end;

  FTextureLink.CopyFrom(pFont.FTextureLink);
  LoadInfoFromFile(Copy(FTextureLink.FileName, 0, FTextureLink.FileName.Length - 4));

  Scale  := pFont.Scale;
  FStatus:= gfsUpdate;
  FSetter:= FSetter + [fsetTexture];
end;

procedure TGUIFont.SetTextureLink(pTextureLink: TTextureLink);
begin
  if not Assigned(pTextureLink) then
    Exit;

  //Класс сам создал эту текстуру это не ссылка нужно очистить (mem leak)
  FreeSelfTexture;

  FTextureLink:= pTextureLink;
  LoadInfoFromFile(Copy(FTextureLink.FileName, 0, FTextureLink.FileName.Length - 4));

  UpdateStatus(fsetTexture);
end;

procedure TGUIFont.CopyMemoryFrom(pFont: TGUIFont);
begin
  CopyMemory(PGUIFont(Self), Pointer(pFont), TGUIFont.InstanceSize);
end;

procedure TGUIFont.SetColor(pColor: TColor);
var FID: Integer;
begin
  if not Assigned(FColor) then
    Exit;

  if pColor = FColor.GetColor then
    Exit;

  FColor.SetColor(pColor);
  FSetter:= FSetter + [fsetColor];

  for FID := Low(FCharInfo) to High(FCharInfo) do
  begin
    FCharInfo[FID].color[0].SetColor(FColor);
    FCharInfo[FID].color[1].SetColor(FColor);
    FCharInfo[FID].color[2].SetColor(FColor);
    FCharInfo[FID].color[3].SetColor(FColor);
  end;

end;

procedure TGUIFont.SetScale(pScale: TFloat);
begin
  FScale := pScale;
  UpdateStatus(fsetScale);
end;

constructor TGUIFont.Create(pTextureLink: TTextureLink);
begin
  FScale      := 1.0;
  FShowArea   := False;
  FColor      := TGLColor.Create;
  FStatus     := gfsNone;
  FSetter     := [];
  FTextureLink:= nil;//TTextureLink.Create;
  FColor.SetColor(clWhite);
  SetTextureLink(pTextureLink);
end;

destructor TGUIFont.Destroy;
begin
  FreeSelfTexture;
  FreeAndNil(FColor);
  inherited;
end;

procedure TGUIFont.FreeSelfTexture;
begin
  if not Assigned(FTextureLink) then
    Exit;

  if FSelfTexture then
  begin
    FreeAndNil(FTextureLink);
    FSelfTexture:= False;
  end;
end;

function TGUIFont.GetColor: TColor;
begin
  Result:= FColor.GetColor;
end;

function TGUIFont.GetHeight: TFloat;
begin
  Result:= FScale * FHeight;
end;

procedure TGUIFont.GetTextRect(const pText: String; out pWidth, pHeight: Integer);
var Buf: TStringList;
    i: integer;
    Str: String;
begin
  Buf:= TStringList.Create;
  Buf.Text:= pText;
  pWidth:= 0;

  for i := 0 to Buf.Count - 1 do
    if pWidth < Length(Buf[i]) then
    begin
      pWidth:= Length(Buf[i]);
      str   := Buf[i];
    end;

  pWidth := Trunc(GetTextWidth(str));
  pHeight:= Buf.Count * FHeight;

  Buf.Free;
end;

function TGUIFont.GetTextWidth(pText: String): TFloat;
var FID: Integer;
    Buf: AnsiString;
begin
  Result:= 0.0;
  Buf:= AnsiString(pText);

  for FID := 1 to Length(Buf) do
    Result:= Result + FCharInfo[ord(Buf[FID])].Width + FCharSpace;

  Result:= Result * FScale;
end;

function TGUIFont.GetTextWidthMax(pText: String; pMax: TFloat): Integer;
var i: integer;
begin
  Result:= Length(pText);

  for i := 1 to Length(pText) do
    if GetTextWidth(Copy(pText, 1, i)) >= pMax then
    begin
      Result:= i + 4;
      Break;
    end;
end;

function TGUIFont.LoadInfoFromFile(const AFileName: String): Boolean;
var Stream  : TFileStream;
    //Bitmap  : TBitmap;
    FontInfo: TGUIFontStream;
begin
  Result:= False;

  if not FileExists(AFileName) then
    Exit;

  //Bitmap:= TBitmap.Create;
  Stream:= TFileStream.Create(AFileName, fmOpenRead);
  try
    Stream.Position:= 0;
    Stream.Read(FontInfo, SizeOf(FontInfo));
  //  Bitmap.LoadFromStream(Stream);

{ //Вычисление высоты шрифта
   FSize = 10, FHeight = -13, -1 - для коррекции (последний пиксель)

   Формула X = ABS(FHeight) - FSize = 3, FHeight = FHeight - X + 1

   Должно получиться
   FHeight -15
}
    Self.FFileName    := ShortString(AFileName);
    Self.FName        := ShortString(String(FontInfo.Name));
    Self.FSize        := FontInfo.Size;
    Self.FHeight      := ABS(FontInfo.Height - (ABS(FontInfo.Height) - FSize));
    Self.FSquadSize   := FontInfo.SquadSize;
    Self.FOffsetX     := FontInfo.OffsetX;
    Self.FOffsetY     := FontInfo.OffsetY;
    Self.FCharSpace   := FontInfo.CharSpace;

    CopyMemory(@Self.FGrades       , @FontInfo.Grades       , SizeOf(FontInfo.Grades));
    CopyMemory(@Self.FCharInfo     , @FontInfo.CharInfo     , SizeOf(FontInfo.CharInfo));
    CopyMemory(@Self.FTextureOffset, @FontInfo.OffsetTexture, SizeOf(FontInfo.OffsetTexture));

    CalcTextureCoord;

    UpdateStatus(fsetTexture);
  finally
    Stream.Free;
  end;

end;

procedure TGUIFont.RenderText(const pX, pY: TFloat; const pText: String; pMaxWidth: Integer; pUseLineBreak: Boolean = false);
var FID       : Integer;
    abs_pos   : TFloat;
    char_id   : Byte;
    Line      : Integer;
begin
  if Trim(pText) = '' then
    Exit;

  if not Assigned(FTextureLink) then
    Exit;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glBlendEquation(GL_FUNC_ADD);

  glEnable(GL_TEXTURE_2D);
  FTextureLink.Bind;

  glPushMatrix;
    abs_pos:= 0;
    Line   := 0;
    glTranslatef(pX, pY, 0);
    glColor4f(FColor.R, FColor.G, FColor.B, FColor.A);
    glScalef(FScale, FScale, FScale);

      for FID:= 1 to Length(pText) do
      begin
        //Перенос на другую строку
        if (pText[FID] = #13) then
          if not pUseLineBreak then
            Break
          else
          begin
            Line:= Line + FHeight + 2;
            abs_pos:= 0;
            Continue;
          end;

        char_id:= GUILocale.GetIndexChar(pText[FID]);
        if (pMaxWidth > 0) and (((abs_pos + FCharInfo[char_id].Width) * FScale) >= pMaxWidth) then
          Break;

        //Отобразить символ
        glBegin(GL_QUADS);
          FCharInfo[char_id].color[0].BindColor;
          glTexCoord2f(FCharInfo[char_id].s , FCharInfo[char_id].t);
            glVertex2f(abs_pos, Line);

          FCharInfo[char_id].color[1].BindColor;
          glTexCoord2f(FCharInfo[char_id].sw, FCharInfo[char_id].t);
            glVertex2f(abs_pos + FCharInfo[char_id].Width, Line);

          FCharInfo[char_id].color[2].BindColor;
          glTexCoord2f(FCharInfo[char_id].sw, FCharInfo[char_id].tw);
            glVertex2f(abs_pos + FCharInfo[char_id].Width, Line + FHeight);

          FCharInfo[char_id].color[3].BindColor;
          glTexCoord2f(FCharInfo[char_id].s, FCharInfo[char_id].tw);
            glVertex2f(abs_pos, Line + FHeight);
        glEnd;

        //Показать рамку вокруг символа
        if ShowArea then begin
          glDisable(GL_TEXTURE_2D);
          glColor3f(0.5, 0.5, 0.5);
          glBegin(GL_LINE_LOOP);
            glVertex2f(abs_pos, Line);
            glVertex2f(abs_pos + FCharInfo[char_id].Width, Line);
            glVertex2f(abs_pos + FCharInfo[char_id].Width, Line + FHeight);
            glVertex2f(abs_pos, Line + FHeight);
          glEnd;
          glEnable(GL_TEXTURE_2D);
        end;

        abs_pos:= abs_pos + FCharInfo[char_id].Width + FCharSpace;
      end;

  glPopMatrix;

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

function TGUIFont.CalcTextureCoord: Boolean;
var FID : Integer;
    Row : Integer;
    Col : Integer;
    X, Y: Integer;
begin
  Result:= False;

  if not Assigned(FTextureLink) then
    Exit;

  for FID := Low(FCharInfo) to High(FCharInfo) do
  begin
    if FGrades[0] = 0 then
      Exit;
    if FGrades[1] = 0 then
      Exit;

    Row:= FID mod FGrades[0];
    Col:= FID div FGrades[1];

    X:= (Row * FSquadSize) + FOffsetX + FTextureOffset.X - 1;
    Y:= (Col * FSquadSize) + FOffsetY + FTextureOffset.Y - 1;

    //Координаты для текстуры
    //X, Width
    FCharInfo[FID].s := X / FTextureLink.Width;
    FCharInfo[FID].sw:= FCharInfo[FID].s + ((FCharInfo[FID].Width + FTextureOffset.W + 1) / FTextureLink.Width);
    //Y, Height
    FCharInfo[FID].t := -(Y / FTextureLink.Height);
    FCharInfo[FID].tw:= -((Y + FHeight + FTextureOffset.H) / FTextureLink.Height);
  end;

  Result:= True;
end;

procedure TGUIFont.Text(const pX, pY: TFloat; const pText: String; pMaxWidth: Integer);
begin
  RenderText(pX, pY, pText, pMaxWidth, False);
end;

procedure TGUIFont.UpdateStatus(pStatus: TGUIFontSetter);
begin
  FSetter:= FSetter + [pStatus];
  FStatus:= gfsUpdate;
end;

{ TGUIFontChar }

procedure TGUIFontChar.ClearTextureCoord;
begin
  s := 0.0;
  t := 0.0;
  sw:= 0.0;
  tw:= 0.0;
end;

procedure TGUIFontChar.SetColor(pColor: Integer);
var FID: Integer;
begin
  for FID := Low(Color) to High(Color) do
    color[FID]:= TGLColor.ColorToGLColor4Rec(pColor);
end;

end.
