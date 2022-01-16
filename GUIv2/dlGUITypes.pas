unit dlGUITypes;

interface

uses Graphics, Classes, Windows, SysUtils, dlOpenGL, dlGUIXMLSerial;

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

//
const TEX_LINK_EMPTY = 0; //Ссылка на пустую текстуру

//Процедуры для GUI
type
  //Процедура для объекта Sender - объект, ParamObj - указатель на параметры
  //Передать параметр @Value, Получить параметр PInteger(ParamObj)^
  TGUIProc = procedure(Sender: TObject; ParamObj: Pointer = nil) of Object;
  TGUIMouseButton = (gmbNone, gmbLeft, gmbRight, gmbMiddle);
  TGUIOrientation = (goHorizontal, goVertical);

//Переназначение стандартных типов
type
  TMousePoint = record
    X, Y: integer;
  end;
//===========================================
  TCoord2Di = record
    public
      X, Y: Integer;
    public
      procedure SetValue(pX, pY: Integer);
      procedure SetDefault;
  end;

//Двумерные координаты
  TCoord2Df = record
  public
    X, Y: Single;
  public
    procedure SetValue(pX, pY: Single);
    procedure SetDefault;
    function ToString: String;
  end;

//===========================================
  TTexFormat     = (tfRGB, tfRGBA, tfBGR, tfBGRA);
  TTexEnvMode    = (teReplace, teDecal, teModulate, teAdd, teAddSigned, teInterpolate, teSubtract, teDot3RGB, teDot3RGBA);
  TTexFilterType = (ftNearest, ftLinear, ftNearestMipmapNearest, ftLinearMipmapNearest, ftNearestMipmapLinear, ftLinearMipmapLinear);

//Текстурные координаты
  TTextureCoord = record
  private
    //Расчитанные координаты
    FUCalc, FVCalc: Single;
  public
    //Координаты текстуры
    U, V: Single;
  public
    procedure SetValue(pU, pV: Single);
    function ToString: String;
  public
    procedure SetCalculatedUV(pUCalc, pVCalc: Single);
  public
    property UCalc: Single read FUCalc;
    property VCalc: Single read FVCalc;
  end;

  //Список координат для 4 вершин
  TTextureLinkSquadArr = record
    public
      Index: array[0..3] of TTextureCoord;
    public
      procedure SetCoord(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y: Single);
      procedure SetSize(pW, pH: Single);
  end;

  TTextureOffset = record
  public
    X, Y, W, H: Byte;
  public
    procedure SetValue(pX, pY, pW, pH: Byte);
  end;

//Ошибка при загрузке текстуры из файла
  TTextureError = record
    public
      Exists : Boolean; //Есть ошибка
      Message: String;  //Сообщение ошибки
    public
      procedure SetError(const AExists: Boolean; const AMessage: String);
  end;

//Ссылка на текстуру
  TTextureLink = class(TPersistent)
    private
      FWidth       : Integer;  //Ширина текстуры
      FHeight      : Integer;  //Высота текстуры
      FLink        : Cardinal; //Ссылка на текстуру

      FName        : String;   //Название текстуры
      FFileName    : String;   //Файл текстуры
      FEnable      : Boolean;  //Разрешить Bind или нет

      FTFormat     : Cardinal; //Формат текстуры
      FTEnvMode    : Cardinal; //Режим отображения текстуры
      FTFilter     : Cardinal; //Фильтр текстуры

      FRGBConvert  : Boolean;  //Текстура конвертированная RGB->RGBA
      FRGBMaskColor: TColor;   //Цвет маски
    public
      constructor Create;
      destructor Destroy; override;

      function Bind: Boolean;
      procedure CopyFrom(pTextureLink: TTextureLink);

      function IsEmpty: Boolean;
    public
      property Enable      : Boolean  read FEnable       write FEnable;

      property Width       : Integer  read FWidth        write FWidth;
      property Height      : Integer  read FHeight       write FHeight;
      property Link        : Cardinal read FLink         write FLink;

      property FileName    : String   read FFileName     write FFileName;
      property p_Format    : Cardinal read FTFormat      write FTFormat;
      property p_EnvMode   : Cardinal read FTEnvMode     write FTEnvMode;
      property p_Filter    : Cardinal read FTFilter      write FTFilter;
      property RGBConv     : Boolean  read FRGBConvert   write FRGBConvert;
      property RGBMaskColor: TColor   read FRGBMaskColor write FRGBMaskColor;
    published
      property Name        : String  read FName          write FName;
    published
      [TXMLSerial] property LinkName: String read FName write FName;
  end;

//===========================================
//Цвет
  TGLColor4Arr = array[0..3] of Single;
  TGLColor3Arr = array[0..2] of Single;
  TGLColor     = class;

  TGLColor3Rec  = record
    public
      R, G, B: Single;
    public
      procedure BindColor;
      procedure SetColor(pR, pG, pB: Single); overload;
      procedure SetColor(pColor: TColor); overload;
      procedure SetColor(pColor: TGLColor); overload;
      procedure ClearColor;
  end;

  TGLColor4Rec = record
    public
      R, G, B, A: Single;
    public
      procedure BindColor;
      procedure SetColor(pR, pG, pB, pA: Single); overload;
      procedure SetColor(pColor: TColor); overload;
      procedure SetColor(pColor: TGLColor); overload;
      procedure CopyFrom(AColor4Rec: TGLColor4Rec);
      procedure ClearColor;
  end;

  TGLColor = class
  public
    R: Single;
    G: Single;
    B: Single;
    A: Single;
  public
    constructor Create(pColor: TColor); overload;
    constructor Create(pR, pG, pB: Single); overload;
    constructor Create(pR, pG, pB, pA: Single); overload;
    destructor Destroy; override;

    class procedure glColor3fx(pColor: Integer); overload;
    class function ColorToGLColor3Rec(pColor: Integer): TGLColor3Rec;
    class function ColorToGLColor4Rec(pColor: Integer): TGLColor4Rec;

    //OpenGL команды
    procedure glColor3fx; overload; //Установить glColor3f(R, G, B);
    procedure glColor4fx; //Установить glColor3f(R, G, B, A);
    //
    //Получить среднее значение цвета
    function GetMeanColor: Single;

    //Это черный цвет
    function IsBlack: Boolean;

    //Сеттер и геттер текста
    procedure SetColor(pColor: TColor); overload;
    procedure SetColor(pR, pG, pB: Single); overload;
    procedure SetColor(pR, pG, pB, pA: Single); overload;

    function GetColor: TColor; overload;
    function GetColor3: TGLColor3Arr; overload;
    function GetColor4: TGLColor4Arr; overload;

    //Очистить параметры цвета в 0.0
    procedure ClearColor;
  end;

//==============================================
//Точка (POINT) для построения всех объектов GUI
  TVertexClass = class
  public
    Vertex    : TCoord2Df;      //Координаты точки
    TexCoord  : TTextureCoord;  //Текстурные координаты
    Color     : TGLColor;       //Цвет точки
    Hide      : Boolean;        //Скрывать вершину или нет
    Group     : Byte;           //Группа, для группового изменения состояния
    GapOccur  : Boolean;        //Разрыв на этой вершине (glBegin, glEnd) при прорисовке в режиме GL_LINE_LOOP
  public
    constructor Create(pX, pY: Single; pColor: TColor = clWhite; pTU: Single = 0.0; pTV: Single = 0.0; pGroup: Byte = 0; pHide: Boolean = false);
    procedure SetColor(pColor: TColor); //Установить цвет
    procedure SetCoord(pX, pY: Single); //Установить текущие координаты
    procedure SetTexCoord(pX, pY: Single); //Установить текстурные координаты
  public
    destructor Destroy; override;
  end;

//===========================================
//Оффсет на анимированные изображения
  TOffsetImage = class(TPersistent)
  private
    FX: Single;
    FY: Single;
    FW: Single;
    FH: Single;
  public
    constructor Create; overload;
    constructor Create(pX, pY, pW, pH: Single); overload;
  published
    [TXMLSerial] property X: Single read FX write FX;
    [TXMLSerial] property Y: Single read FY write FY;
    [TXMLSerial] property W: Single read FW write FW;
    [TXMLSerial] property H: Single read FH write FH;
  end;

//===========================================
//Параметры прозрачности
  TBlendParam = class
  strict private
    FEnable  : Boolean;  //Включен или нет
    FSrc     : Cardinal; //Значение BlendFunc
    FDst     : Cardinal;
    FAlpha   : Single;  //Цвет альфа канала
    FEquation: Cardinal;   //
  public
    [TXMLSerial] property Enable  : Boolean read FEnable   write FEnable;
    [TXMLSerial] property Src     : Cardinal   read FSrc      write FSrc;
    [TXMLSerial] property Dst     : Cardinal   read FDst      write FDst;
    [TXMLSerial] property Alpha   : Single  read FAlpha    write FAlpha;
    [TXMLSerial] property Equation: Cardinal   read FEquation write FEquation;
  public
    constructor Create;
    procedure SetParam(const AEnable: Boolean; const ASrc, ADst: Cardinal);
    procedure CopyFrom(ABlend: TBlendParam);

    procedure Bind;
    procedure Set_One_One;
    procedure Set_SrcAlpha_OneMinusSrcAlpha;
  end;

//===========================================
//Таймер
  TTimerObj = class
  public
    Enable  : Boolean;  //Включен таймер или нет
    Value   : Cardinal; //Значение
    MaxValue: Cardinal; //Максимальное значение после которого сбрасывается Value
    RepeatT : Boolean;  //Повторять
  public
    procedure Update;
    procedure ClearParam;
  end;

//===========================================
//Область объекта
   TGUIObjectRect = record
     public
       [TXMLSerial] X     : Integer; //Позиция по X
       [TXMLSerial] Y     : Integer; //Позиция по Y
       [TXMLSerial] Width : Integer; //Ширина
       [TXMLSerial] Height: Integer; //Высота
     public
       procedure SetPos(pX, pY: Integer);
       procedure SetRect(pRect: TGUIObjectRect; pOffset: Integer = 0); overload;
       procedure SetRect(pX, pY, pW, pH: Integer); overload;
       procedure SetSize(pWidth, pHeight: Integer);

       procedure SetAnchWidth (pWidth: Integer);
       procedure SetAnchHeight(pHeight: Integer);
     public
       //Нарисовать квадрат по размерам
       procedure Render(pOffset: Single = 0.0; pMode: Cardinal = GL_LINE_LOOP; pColor: TColor = clWhite);
       function ToString: String; overload;
   end;

//===========================================
//Курсор
  TGUICursor = class
    private
      FRect           : TGUIObjectRect; //Позиция курсора и размер
      FCursorCharPos  : Integer;     //Позиция текущего символа
      FCursorRenderPos: Single;   //Позиция прорисовки курсора
      FCursorTime     : Cardinal; //Время курсора
      FCursorShow     : Boolean;  //Показывать курсор или нет
      FColor          : TGLColor; //Цвет курсора
      FWaitTime       : Cardinal; //Время ожидания перед морганием
    public
      property Rect     : TGUIObjectRect read FRect;
      property CharPos  : Integer        read FCursorCharPos   write FCursorCharPos;
      property RenderPos: Single         read FCursorRenderPos write FCursorRenderPos;
    public
      constructor Create(pColor: TColor);
      destructor Destroy; override;
      procedure ResetCursor;
      procedure Render;
  end;

//Рамка
  TGUITypeArea = class
  private
    //Показывать рамку или нет
    FShow      : Boolean;
    //Рамка скрыта
    FVisible   : Boolean;
    //Установленный цвет
    FColor     : TGLColor;
    //Размер пикселя
    FPointSize : Single;
    //Ширина линии
    FLineWidth : Single;
    //Позиция X, Y
    FRect      : TGUIObjectRect;
    //
    FOffset    : Integer;
    //Режим прорисовки
    FDrawMode  : Cardinal;
    //Смешивание цветов
    FBlend     : TBlendParam;
    //Скорость анимации
    FSpeed     : Single;
    //Включить плавное появление
    FAnimEnable: Boolean;
  private
    procedure SetVisible(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;

    procedure SetDefaultColor;
  public
    property Rect      : TGUIObjectRect read FRect       write FRect;
    property Show      : Boolean        read FShow       write FShow;
    property Visible   : Boolean        read FVisible    write SetVisible;
    property Color     : TGLColor       read FColor;
    property PointSize : Single         read FPointSize  write FPointSize;
    property LineWidth : Single         read FLineWidth  write FLineWidth;
    property Speed     : Single         read FSpeed      write FSpeed;
    property Offset    : Integer        read FOffset     write FOffset;
    property DrawMode  : Cardinal          read FDrawMode   write FDrawMode;
    property Blend     : TBlendParam    read FBlend      write FBlend;
    property AnimEnable: Boolean        read FAnimEnable write FAnimEnable;
  public
  end;

implementation

{ TCoord2Df }

procedure TCoord2Df.SetDefault;
begin
  SetValue(0.0, 0.0);
end;

procedure TCoord2Df.SetValue(pX, pY: Single);
begin
  X:= pX;
  Y:= pY;
end;

function TCoord2Df.ToString: String;
begin
  Result:= Format('X=%f, Y=%f', [X, Y]);
end;

{ TCoordTetxture }

procedure TTextureCoord.SetCalculatedUV(pUCalc, pVCalc: Single);
begin
  FUCalc:= pUCalc;
  FVCalc:= pVCalc;
end;

procedure TTextureCoord.SetValue(pU, pV: Single);
begin
  U:= pU;
  V:= pV;
end;

function TTextureCoord.ToString: String;
begin
  Result:= Format('U=%f, V=%f', [U, V]);
end;

{ TGLColor }

procedure TGLColor.ClearColor;
begin
  R:= 0;
  G:= 0;
  B:= 0;
  A:= 0;
end;

constructor TGLColor.Create(pColor: TColor);
begin
  SetColor(pColor);
end;

constructor TGLColor.Create(pR, pG, pB: Single);
begin
  SetColor(pR, pG, pB);
end;

class function TGLColor.ColorToGLColor3Rec(pColor: Integer): TGLColor3Rec;
begin
  Result.R := Byte(pColor) / 255;
  Result.G := Byte(pColor shr 8)  / 255;
  Result.B := Byte(pColor shr 16) / 255;
end;

class function TGLColor.ColorToGLColor4Rec(pColor: Integer): TGLColor4Rec;
begin
  Result.R := Byte(pColor) / 255;
  Result.G := Byte(pColor shr 8)  / 255;
  Result.B := Byte(pColor shr 16) / 255;
  Result.A := 1;
  //Альфа канал не трогаем, меняем только цвет
  // Result.A := Byte(pColor shr 24) / 255;
end;

constructor TGLColor.Create(pR, pG, pB, pA: Single);
begin
  SetColor(pR, pG, pB, pA);
end;

destructor TGLColor.Destroy;
begin
  inherited;
end;

function TGLColor.GetColor3: TGLColor3Arr;
begin
  Result[0]:= R;
  Result[1]:= G;
  Result[2]:= B;
end;

function TGLColor.GetColor: TColor;
begin
  Result:= RGB(Round(Self.R * 255),
               Round(Self.G * 255),
               Round(Self.B * 255));
end;

function TGLColor.GetColor4: TGLColor4Arr;
begin
  Result[0]:= R;
  Result[1]:= G;
  Result[2]:= B;
  Result[3]:= A;
end;

function TGLColor.GetMeanColor: Single;
begin
  Result:= (R + G + B) / 3;
end;

class procedure TGLColor.glColor3fx(pColor: Integer);
begin
  glColor3f(
       Byte(pColor) / 255,
       Byte(pColor shr 8)  / 255,
       Byte(pColor shr 16) / 255
       );
end;

procedure TGLColor.glColor3fx;
begin
  glColor3f(R, G, B);
end;

procedure TGLColor.glColor4fx;
begin
  glColor4f(R, G, B, A);
end;

function TGLColor.IsBlack: Boolean;
begin
  Result:= R + G + B = 0.0;
end;

procedure TGLColor.SetColor(pColor: TColor);
begin
  R := Byte(pColor) / 255;
  G := Byte(pColor shr 8)  / 255;
  B := Byte(pColor shr 16) / 255;
  A := Byte(pColor shr 24) / 255;
end;

procedure TGLColor.SetColor(pR, pG, pB: Single);
begin
  R:= pR;
  G:= pG;
  B:= pB;
  A:= 0.0;
end;

procedure TGLColor.SetColor(pR, pG, pB, pA: Single);
begin
  R:= pR;
  G:= pG;
  B:= pB;
  A:= pA;
end;

{ TGUITypeArea }

constructor TGUITypeArea.Create;
begin
  FColor    := TGLColor.Create;
  FDrawMode := GL_LINE_LOOP;
  FPointSize:= 1.0;
  FLineWidth:= 1;
  FSpeed    := 0.01;
  FBlend    := TBlendParam.Create;
  FRect.SetRect(0, 0, 0, 0);

  SetDefaultColor;
end;

destructor TGUITypeArea.Destroy;
begin
  FreeAndNil(FBlend);
  FreeAndNil(FColor);

  inherited;
end;

procedure TGUITypeArea.Render;
begin
  if not FShow then
    Exit;

  if not FVisible then
    Exit;

  glDisable(GL_TEXTURE_2D);

  Blend.Bind;

  glPointSize(FPointSize);
  glLineWidth(FLineWidth);

  if FAnimEnable then
  begin
    if FColor.A < 1.0 then
      FColor.A:= FColor.A + FSpeed;

    if FColor.A > 1.0 then
      FColor.A := 1.0;
  end;

  Rect.Render(FOffset, FDrawMode, FColor.GetColor);

  glPointSize(1);
  glLineWidth(1);

end;

procedure TGUITypeArea.SetDefaultColor;
begin
  FColor.SetColor(0.5, 0.5, 0.5, 0.0);
end;

procedure TGUITypeArea.SetVisible(Value: Boolean);
begin
  if FVisible = Value then
    Exit;

  FVisible:= Value;

  //Сбрасываем цвет
  if not FVisible then
    FColor.A:= 0.0;
end;

{ TVertexPointClass }

constructor TVertexClass.Create(pX, pY: Single; pColor: TColor; pTU, pTV: Single; pGroup: Byte; pHide: Boolean);
begin
  Color    := TGLColor.Create;
  SetCoord(pX, pY);
  SetColor(pColor);
  SetTexCoord(pTU, pTV);
  Group    := pGroup;
  Hide     := pHide;
  GapOccur := False;
end;

destructor TVertexClass.Destroy;
begin
  if Assigned(Color) then
    FreeAndNil(Color);

  inherited;
end;

procedure TVertexClass.SetColor(pColor: TColor);
begin
  Color.SetColor(pColor);
end;

procedure TVertexClass.SetCoord(pX, pY: Single);
begin
  Vertex.X:= pX;
  Vertex.Y:= pY;
end;

procedure TVertexClass.SetTexCoord(pX, pY: Single);
begin
  TexCoord.U:= pX;
  TexCoord.V:= pY;
end;

{ TOffsetImage }

constructor TOffsetImage.Create(pX, pY, pW, pH: Single);
begin
  FX:= pX;
  FY:= pY;
  FW:= pW;
  FH:= pH;
end;

constructor TOffsetImage.Create;
begin
  FX:= 0;
  FY:= 0;
  FW:= 0;
  FH:= 0;
end;

{ TTextureOffset }

procedure TTextureOffset.SetValue(pX, pY, pW, pH: Byte);
begin
  X:= pX;
  Y:= pY;
  W:= pW;
  H:= pH;
end;

{ TBlendParam }

procedure TBlendParam.Bind;
begin
  if not FEnable then
  begin
    glDisable(GL_BLEND);
    Exit;
  end;

  glEnable(GL_BLEND);
  glBlendFunc(FSrc, FDst);
  glBlendEquation(FEquation);
end;

procedure TBlendParam.CopyFrom(ABlend: TBlendParam);
begin
  if not Assigned(ABlend) then
    Exit;

  Enable  := ABlend.Enable;
  Src     := ABlend.Src;
  Dst     := ABlend.Dst;
  Alpha   := ABlend.Alpha;
  Equation:= ABlend.Equation;
end;

constructor TBlendParam.Create;
begin
  FEnable  := False;
  FSrc     := GL_ZERO;
  FDst     := GL_ZERO;
  FEquation:= GL_FUNC_ADD;
  FAlpha   := 1.0;
end;

procedure TBlendParam.SetParam(const AEnable: Boolean; const ASrc, ADst: Cardinal);
begin
  FEnable:= AEnable;
  FSrc   := ASrc;
  FDst   := ADst;
end;

procedure TBlendParam.Set_One_One;
begin
  SetParam(True, GL_ONE, GL_ONE);
end;

procedure TBlendParam.Set_SrcAlpha_OneMinusSrcAlpha;
begin
  SetParam(True, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

{ TTimerObj }

procedure TTimerObj.ClearParam;
begin
  Enable  := False;
  Value   := 0;
  MaxValue:= 100;
  RepeatT := False;
end;

procedure TTimerObj.Update;
begin
  if not Enable then
    Exit;

  inc(Value);
  if Value > MaxValue then
  begin

    if not RepeatT then
    begin
       Enable:= False;
       Value := MaxValue;
       Exit;
    end;

    Value:= 0;
  end;
end;

{ TTextureLinkSquadArr }

procedure TTextureLinkSquadArr.SetCoord(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y: Single);
begin
  Index[0].U:= p1x;
  Index[0].V:= p1y;

  Index[1].U:= p2x;
  Index[1].V:= p2y;

  Index[2].U:= p3x;
  Index[2].V:= p3y;

  Index[3].U:= p4x;
  Index[3].V:= p4y;
end;

procedure TTextureLinkSquadArr.SetSize(pW, pH: Single);
begin
  SetCoord(1 ,  1,
           pW,  1,
           pW, pH,
           1 , pH);
end;

{ TGUICursor }

constructor TGUICursor.Create(pColor: TColor);
begin
  FRect.SetRect(0, 0, 0 ,0);

  FCursorRenderPos:= 0;
  FCursorCharPos  := 0;
  FCursorTime     := GetCurrentTime;
  FCursorShow     := False;
  FWaitTime       := 500;
  FColor          := TGLColor.Create(pColor);
end;

destructor TGUICursor.Destroy;
begin
  FreeAndNil(FColor);
  inherited;
end;

procedure TGUICursor.Render;
var Corr: Single;
begin
  if GetCurrentTime - FCursorTime > FWaitTime then
  begin
    FCursorShow:= not FCursorShow;
    FCursorTime:= GetCurrentTime;
  end;

  if not FCursorShow then
    Exit;

  glPushMatrix;
    glBegin(GL_QUADS);
      Corr:= -0.5;
      FColor.glColor4fx;
      glVertex2f(FRect.X + FCursorRenderPos + Corr              , FRect.Y + Corr);
      glVertex2f(FRect.X + FCursorRenderPos + Corr + FRect.Width, FRect.Y + Corr);
      glVertex2f(FRect.X + FCursorRenderPos + Corr + FRect.Width, FRect.Y + Corr + FRect.Height);
      glVertex2f(FRect.X + FCursorRenderPos + Corr              , FRect.Y + Corr + FRect.Height);
  glEnd;

  glPopMatrix;
end;

procedure TGUICursor.ResetCursor;
begin
  FCursorShow:= True;
  FCursorTime:= GetCurrentTime;
end;

{ TGUIObjectRect }

procedure TGUIObjectRect.Render(pOffset: Single = 0.0; pMode: Cardinal = GL_LINE_LOOP; pColor: TColor = clWhite);
var ACorr: Single;
    Buf  : TGLColor4Rec;
begin
  Buf:= TGLColor.ColorToGLColor4Rec(pColor);
  glColor4f(Buf.R, Buf.G, Buf.B, Buf.A);

  {MakeSquare}
  glBegin(pMode);
    //Корректировка - для OpenGL чтобы он считал ровно один пиксель а не 0.5
    ACorr:= pOffset + 0.3;
    glVertex2f(X + ACorr, Y + ACorr);
    glVertex2f(X + Width - ACorr, Y + ACorr);
    glVertex2f(X + Width - ACorr, Y + Height - ACorr);
    glVertex2f(X + ACorr, Y + Height - ACorr);
  glEnd;
end;

procedure TGUIObjectRect.SetAnchHeight(pHeight: Integer);
begin
  Height:= pHeight;
end;

procedure TGUIObjectRect.SetAnchWidth(pWidth: Integer);
begin
  Width:= pWidth;
end;

procedure TGUIObjectRect.SetPos(pX, pY: Integer);
begin
  X:= pX;
  Y:= pY;
end;

procedure TGUIObjectRect.SetRect(pRect: TGUIObjectRect; pOffset: Integer = 0);
begin
  SetPos (pRect.X + pOffset          , pRect.Y + pOffset     );
  SetSize(pRect.Width - (pOffset * 2), pRect.Height - (pOffset * 2));
end;

procedure TGUIObjectRect.SetRect(pX, pY, pW, pH: Integer);
begin
  SetPos(pX, pY);
  SetSize(pW, pH);
end;

procedure TGUIObjectRect.SetSize(pWidth, pHeight: Integer);
begin
  Width := pWidth;
  Height:= pHeight;
end;

function TGUIObjectRect.ToString: String;
begin
  Result:= Format('X=%d, Y=%d, Width=%d, Height=%d', [X, Y, Width, Height]);
end;

{ TTextureError }

procedure TTextureError.SetError(const AExists: Boolean; const AMessage: String);
begin
  Exists := AExists;
  Message:= AMessage;
end;

{ TTextureLink }

function TTextureLink.Bind: Boolean;
begin
  Result:= False;

  if IsEmpty then
    Exit;

  glBindTexture(GL_TEXTURE_2D, FLink);
  Result:= True;
end;

procedure TTextureLink.CopyFrom(pTextureLink: TTextureLink);
begin
  if not Assigned(pTextureLink) then
    Exit;

  Self.Width       := pTextureLink.Width;
  Self.Height      := pTextureLink.Height;
  Self.Link        := pTextureLink.Link;
  Self.Name        := pTextureLink.Name;
  Self.FileName    := pTextureLink.FileName;
  Self.FEnable     := pTextureLink.FEnable;
  Self.p_Format    := pTextureLink.p_Format;
  Self.p_EnvMode   := pTextureLink.p_EnvMode;
  Self.p_Filter    := pTextureLink.p_Filter;
  Self.RGBConv     := pTextureLink.RGBConv;
  Self.RGBMaskColor:= pTextureLink.RGBMaskColor;
end;

constructor TTextureLink.Create;
begin
  FWidth       := 0;
  FHeight      := 0;
  FLink        := TEX_LINK_EMPTY;
  FName        := '';
  FFileName    := '';
  FEnable      := False;
  FTFormat     := GL_RGB;
  FTEnvMode    := GL_MODULATE;
  FTFilter     := GL_LINEAR;
  FRGBConvert  := False;
  FRGBMaskColor:= clBlack;
end;

destructor TTextureLink.Destroy;
begin
//Не нужно тут текстуру удалять, она не создается этим классом
//Удаление перенесено в TextureList
//  if (not IsEmpty) then
//    glDeleteTextures(1, @Link);

  inherited;
end;

function TTextureLink.IsEmpty: Boolean;
begin
  Result:= (Link = TEX_LINK_EMPTY) and (Trim(FName) = '');
end;

{ TGLColor3Rec }

procedure TGLColor3Rec.BindColor;
begin
  glColor3f(R, G, B);
end;

procedure TGLColor3Rec.ClearColor;
begin
  R:= 0.0;
  G:= 0.0;
  B:= 0.0;
end;

procedure TGLColor3Rec.SetColor(pColor: TColor);
begin
  R := Byte(pColor) / 255;
  G := Byte(pColor shr 8)  / 255;
  B := Byte(pColor shr 16) / 255;
end;

procedure TGLColor3Rec.SetColor(pColor: TGLColor);
begin
 if not Assigned(pColor) then
   Exit;

  R:= pColor.R;
  G:= pColor.G;
  B:= pColor.B;

end;

procedure TGLColor3Rec.SetColor(pR, pG, pB: Single);
begin
  R:= pR;
  G:= pG;
  B:= pB;
end;

{ TGLColor4Rec }

procedure TGLColor4Rec.BindColor;
begin
  glColor4f(R, G, B, A);
end;

procedure TGLColor4Rec.ClearColor;
begin
  R:= 0.0;
  G:= 0.0;
  B:= 0.0;
  A:= 0.0;
end;

procedure TGLColor4Rec.CopyFrom(AColor4Rec: TGLColor4Rec);
begin
  R:= AColor4Rec.R;
  G:= AColor4Rec.G;
  B:= AColor4Rec.B;
  A:= AColor4Rec.A;
end;

procedure TGLColor4Rec.SetColor(pColor: TColor);
begin
  R := Byte(pColor) / 255;
  G := Byte(pColor shr 8)  / 255;
  B := Byte(pColor shr 16) / 255;
  A := Byte(pColor shr 24) / 255;
end;

procedure TGLColor4Rec.SetColor(pColor: TGLColor);
begin
  if not Assigned(pColor) then
    Exit;

  R:= pColor.R;
  G:= pColor.G;
  B:= pColor.B;
  A:= 1;
// A:= pColor.A;
end;

procedure TGLColor4Rec.SetColor(pR, pG, pB, pA: Single);
begin
  R:= pR;
  G:= pG;
  B:= pB;
  A:= pA;
end;

{ TCoord2DI }

procedure TCoord2DI.SetDefault;
begin
  X:= 0;
  Y:= 0;
end;

procedure TCoord2DI.SetValue(pX, pY: Integer);
begin
  X:= pX;
  Y:= pY;
end;

end.
