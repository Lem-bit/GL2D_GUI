unit dlGUITypes;

interface

uses Graphics, Classes, Windows, SysUtils, dlOpenGL;

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

//
const TEX_LINK_EMPTY = 0; //Ссылка на пустую текстуру

//Процедуры для GUI
type
  //Процедура для объекта Sender - объект, ParamObj - указатель на параметры
  //Передать параметр @Value, Получить параметр PInteger(ParamObj)^
  TGUIProc = procedure(Sender: TObject; ParamObj: Pointer = nil) of Object;
  TGUIMouseButton = (gmbLeft, gmbRight, gmbMiddle);

//Переназначение стандартных типов
type
  TInt   = integer;
  TFloat = single;
  TUInt  = Cardinal;


  TMousePoint = record
    X, Y: TInt;
  end;
//===========================================
//Двумерные координаты
  TCoord2D = record
  public
    X, Y: TFloat;
  public
    procedure SetValue(pX, pY: TFloat);
    function ToString: String;
  end;

  TCoord2DLine     = array[0..1] of TCoord2D;
  TCoord2DTriangle = array[0..2] of TCoord2D;
  TCoord2DSquare   = array[0..3] of TCoord2D;

//Трехмерные координаты
  TCoord3D = record
  public
    X, Y, Z: TFloat;
  public
    procedure CopyFrom(pObject: TCoord3D);
    procedure glVertex3fx;
    procedure SetValue(pX, pY, pZ: TFloat);
    function ToString: String;
  end;

  TCoord3DLine     = array[0..1] of TCoord3D;
  TCoord3DTriangle = array[0..2] of TCoord3D;
  TCoord3DSquare   = array[0..3] of TCoord3D;

//===========================================
  TTexFormat = (tfRGB, tfRGBA, tfBGR, tfBGRA);
  TTexEnvMode    = (teReplace, teDecal, teModulate, teAdd, teAddSigned, teInterpolate, teSubtract, teDot3RGB, teDot3RGBA);
  TTexFilterType = (ftNearest, ftLinear, ftNearestMipmapNearest, ftLinearMipmapNearest, ftNearestMipmapLinear, ftLinearMipmapLinear);

//Текстурные координаты
  TTextureCoord = record
  private
    //Расчитанные координаты
    FUCalc, FVCalc: TFloat;
  public
    //Координаты текстуры
    U, V: TFloat;
  public
    procedure SetValue(pU, pV: TFloat);
    function ToString: String;
  public
    procedure SetCalculatedUV(pUCalc, pVCalc: TFloat);
  public
    property UCalc: TFloat read FUCalc;
    property VCalc: TFloat read FVCalc;
  end;

  //Список координат для 4 вершин
  TTextureLinkSquadArr = record
    public
      Index: array[0..3] of TTextureCoord;
    public
      procedure SetCoord(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y: TFloat);
      procedure SetSize(pW, pH: TFloat);
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
      FWidth       : TInt;     //Ширина текстуры
      FHeight      : TInt;     //Высота текстуры
      FLink        : TUInt;    //Ссылка на текстуру

      FName        : String;   //Название текстуры
      FFileName    : String;   //Файл текстуры
      FEnable      : Boolean;  //Разрешить Bind или нет

      FTFormat     : TUInt;    //Формат текстуры
      FTEnvMode    : TUInt;    //Режим отображения текстуры
      FTFilter     : TUInt;    //Фильтр текстуры

      FRGBConvert  : Boolean;  //Текстура конвертированная RGB->RGBA
      FRGBMaskColor: TColor;   //Цвет маски
    public
      constructor Create;
      destructor Destroy; override;

      function Bind: Boolean;
      procedure CopyFrom(pTextureLink: TTextureLink);

      function IsEmpty: Boolean;
    public
      property Enable      : Boolean read FEnable       write FEnable;

      property Width       : TInt    read FWidth        write FWidth;
      property Height      : TInt    read FHeight       write FHeight;
      property Link        : TUInt   read FLink         write FLink;

      property FileName    : String  read FFileName     write FFileName;
      property p_Format    : TUInt   read FTFormat      write FTFormat;
      property p_EnvMode   : TUInt   read FTEnvMode     write FTEnvMode;
      property p_Filter    : TUInt   read FTFilter      write FTFilter;
      property RGBConv     : Boolean read FRGBConvert   write FRGBConvert;
      property RGBMaskColor: TColor  read FRGBMaskColor write FRGBMaskColor;
    published
      property Name: String read FName write FName;
  end;

//===========================================
//Цвет
  TGLColor4Arr = array[0..3] of TFloat;
  TGLColor3Arr = array[0..2] of TFloat;

  TGLColor = class;
  TGLColor3Rec  = record
  public
    R, G, B: TFloat;
  public
    procedure BindColor;
    procedure SetColor(pR, pG, pB: TFloat); overload;
    procedure SetColor(pColor: TGLColor); overload;
    procedure ClearColor;
  end;

  TGLColor = class
  public
    R: TFloat;
    G: TFloat;
    B: TFloat;
    A: TFloat;
  public
    constructor Create(pColor: TColor); overload;
    constructor Create(pR, pG, pB: TFloat); overload;
    constructor Create(pR, pG, pB, pA: TFloat); overload;

    //OpenGL команды
    class procedure glColor3fx(pColor: Integer); overload;
    class function ColorToGLColor3Rec(pColor: Integer): TGLColor3Rec;

    procedure glColor3fx; overload; //Установить glColor3f(R, G, B);
    procedure glColor4fx; //Установить glColor3f(R, G, B, A);
    //
    //Получить среднее значение цвета
    function GetMeanColor: TFloat;

    //Это черный цвет
    function IsBlack: Boolean;

    //Сеттер и геттер текста
    procedure SetColor(pColor: TColor); overload;
    procedure SetColor(pR, pG, pB: TFloat); overload;
    procedure SetColor(pR, pG, pB, pA: TFloat); overload;

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
    Vertex  : TCoord2D;       //Координаты точки
    TexCoord: TTextureCoord;  //Текстурные координаты
    Color   : TGLColor;       //Цвет точки
    Hide    : Boolean;        //Скрывать вершину или нет
    Group   : Byte;           //Группа, для группового изменения состояния
    GapOccur: Boolean;        //Разрыв на этой вершине (glBegin, glEnd) при прорисовке в режиме GL_LINE_LOOP
  public
    constructor Create(pX, pY: TFloat; pColor: TColor = clWhite; pTU: TFloat = 0.0; pTV: TFloat = 0.0; pGroup: Byte = 0; pHide: Boolean = false);
    procedure SetColor(pColor: TColor); //Установить цвет
    procedure SetCoord(pX, pY: TFloat); //Установить текущие координаты
    procedure SetTexCoord(pX, pY: TFloat); //Установить текстурные координаты
  public
    destructor Destroy; override;
  end;

//===========================================
//Оффсет на анимированные изображения
  TOffsetImage = class(TPersistent)
  private
    FX: TFloat;
    FY: TFloat;
    FW: TFloat;
    FH: TFloat;
  public
    constructor Create; overload;
    constructor Create(pX, pY, pW, pH: TFloat); overload;
  published
    property X: TFloat read FX write FX;
    property Y: TFloat read FY write FY;
    property W: TFloat read FW write FW;
    property H: TFloat read FH write FH;
  end;

//===========================================
//Параметры прозрачности
  TBlendParam = class
  private
    FEnable  : Boolean; //Включен или нет
    FSrc     : TUInt;   //Значение BlendFunc
    FDst     : TUInt;
    FAlpha   : TFloat;  //Цвет альфа канала
    FEquation: TUInt;   //
  public
    property Enable  : Boolean read FEnable   write FEnable;
    property Src     : TUInt   read FSrc      write FSrc;
    property Dst     : TUInt   read FDst      write FDst;
    property Alpha   : TFloat  read FAlpha    write FAlpha;
    property Equation: TUInt   read FEquation write FEquation;
  public
    constructor Create;
    procedure SetParam(const AEnable: Boolean; const ASrc, ADst: TUInt);

    procedure Bind;
    procedure Set_One_One;
    procedure Set_SrcAlpha_OneMinusSrcAlpha;
  end;

//===========================================
//Таймер
  TTimerObj = class
  public
    Enable  : Boolean; //Включен таймер или нет
    Value   : TUInt;   //Значение
    MaxValue: TUInt;   //Максимальное значение после которого сбрасывается Value
    RepeatT : Boolean; //Повторять
  public
    procedure Update;
    procedure ClearParam;
  end;

//===========================================
//Область объекта
   TGUIObjectRect = record
     public
       X     : TInt; //Позиция по X
       Y     : TInt; //Позиция по Y
       Width : TInt; //Ширина
       Height: TInt; //Высота
     public
       procedure SetPos(pX, pY: TInt);
       procedure SetRect(pRect: TGUIObjectRect); overload;
       procedure SetRect(pX, pY, pW, pH: TInt); overload;
       procedure SetSize(pWidth, pHeight: TInt);
     public
       //Нарисовать квадрат по размерам
       procedure Render(pOffset: Integer = 0; pMode: TUInt = GL_LINE_LOOP);
   end;

//===========================================
//Курсор
  TGUICursor = class
    private
      FRect           : TGUIObjectRect; //Позиция курсора и размер
      FCursorCharPos  : TInt;     //Позиция текущего символа
      FCursorRenderPos: TFloat;   //Позиция прорисовки курсора
      FCursorTime     : Cardinal; //Время курсора
      FCursorShow     : Boolean;  //Показывать курсор или нет
      FColor          : TGLColor; //Цвет курсора
      FWaitTime       : Cardinal; //Время ожидания перед морганием
    public
      property Rect     : TGUIObjectRect read FRect;
      property CharPos  : TInt           read FCursorCharPos   write FCursorCharPos;
      property RenderPos: TFloat         read FCursorRenderPos write FCursorRenderPos;
    public
      constructor Create(pColor: TColor);
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
    FPointSize : TFloat;
    //Ширина линии
    FLineWidth : TFloat;
    //Позиция X, Y
    FRect      : TGUIObjectRect;
    //
    FOffset    : TInt;
    //Режим прорисовки
    FDrawMode  : TUInt;
    //Смешивание цветов
    FBlend     : TBlendParam;
    //Скорость анимации
    FSpeed     : TFloat;
    //Включить плавное появление
    FAnimEnable: Boolean;
  private
    procedure SetVisible(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
  public
    property Rect      : TGUIObjectRect read FRect       write FRect;
    property Show      : Boolean        read FShow       write FShow;
    property Visible   : Boolean        read FVisible    write SetVisible;
    property Color     : TGLColor       read FColor;
    property PointSize : TFloat         read FPointSize  write FPointSize;
    property LineWidth : TFloat         read FLineWidth  write FLineWidth;
    property Speed     : TFloat         read FSpeed      write FSpeed;
    property Offset    : TInt           read FOffset     write FOffset;
    property DrawMode  : TUInt          read FDrawMode   write FDrawMode;
    property Blend     : TBlendParam    read FBlend      write FBlend;
    property AnimEnable: Boolean        read FAnimEnable write FAnimEnable;
  public
  end;

implementation

{ TCoord2D }

procedure TCoord2D.SetValue(pX, pY: TFloat);
begin
  X:= pX;
  Y:= pY;
end;

function TCoord2D.ToString: String;
begin
  Result:= Format('X=%f, Y=%f', [X, Y]);
end;

{ TCoord3D }

procedure TCoord3D.CopyFrom(pObject: TCoord3D);
begin
  X:= pObject.X;
  Y:= pObject.Y;
  Z:= pObject.Z;
end;

procedure TCoord3D.glVertex3fx;
begin
  glVertex3f(X, Y, Z);
end;

procedure TCoord3D.SetValue(pX, pY, pZ: TFloat);
begin
  X:= pX;
  Y:= pY;
  Z:= pZ;
end;

function TCoord3D.ToString: String;
begin
  Result:= Format('X=%f, Y=%f, Z=%f', [X, Y, Z]);
end;

{ TCoordTetxture }

procedure TTextureCoord.SetCalculatedUV(pUCalc, pVCalc: TFloat);
begin
  FUCalc:= pUCalc;
  FVCalc:= pVCalc;
end;

procedure TTextureCoord.SetValue(pU, pV: TFloat);
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
  inherited Create;
  SetColor(pColor);
end;

constructor TGLColor.Create(pR, pG, pB: TFloat);
begin
  inherited Create;
  SetColor(pR, pG, pB);
end;

class function TGLColor.ColorToGLColor3Rec(pColor: Integer): TGLColor3Rec;
begin
  Result.R:= (pColor mod $100) / 255;
  Result.G:= ((pColor div $100) mod $100) / 255;
  Result.B := (pColor div $10000) / 255;
end;

constructor TGLColor.Create(pR, pG, pB, pA: TFloat);
begin
  inherited Create;
  SetColor(pR, pG, pB, pA);
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

function TGLColor.GetMeanColor: TFloat;
begin
  Result:= (R + G + B) / 3;
end;

class procedure TGLColor.glColor3fx(pColor: Integer);
begin
  glColor3f((pColor mod $100) / 255,
            ((pColor div $100) mod $100) / 255,
            (pColor div $10000) / 255);
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
  R := (pColor mod $100) / 255;
  G := ((pColor div $100) mod $100) / 255;
  B := (pColor div $10000) / 255;
  A := 0.0;
end;

procedure TGLColor.SetColor(pR, pG, pB: TFloat);
begin
  R:= pR;
  G:= pG;
  B:= pB;
  A:= 0.0;
end;

procedure TGLColor.SetColor(pR, pG, pB, pA: TFloat);
begin
  R:= pR;
  G:= pG;
  B:= pB;
  A:= pA;
end;

{ TGUITypeArea }

constructor TGUITypeArea.Create;
begin
  FColor    := TGLColor.Create(0.5, 0.5, 0.5, 0.0);
  FDrawMode := GL_LINE_LOOP;
  FPointSize:= 1.0;
  FLineWidth:= 1;
  FSpeed    := 0.01;
  FBlend    := TBlendParam.Create;
  FRect.SetRect(0, 0, 0, 0);
end;

destructor TGUITypeArea.Destroy;
begin
  FreeAndNil(FBlend);
  FreeAndNil(FColor);

  inherited;
end;

procedure TGUITypeArea.Render;
begin
  if not FShow then Exit;
  if not FVisible then Exit;

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

  FColor.glColor4fx;
  Rect.Render(FOffset, FDrawMode);

  glPointSize(1);
  glLineWidth(1);

end;

procedure TGUITypeArea.SetVisible(Value: Boolean);
begin
  FVisible:= Value;

  //Сбрасываем цвет
  if not FVisible then
    FColor.A:= 0.0;
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

procedure TGLColor3Rec.SetColor(pColor: TGLColor);
begin
 if not Assigned(pColor) then
   Exit;

  R:= pColor.R;
  G:= pColor.G;
  B:= pColor.B;

end;

procedure TGLColor3Rec.SetColor(pR, pG, pB: TFloat);
begin
  R:= pR;
  G:= pG;
  B:= pB;
end;

{ TVertexPointClass }

constructor TVertexClass.Create(pX, pY: TFloat; pColor: TColor; pTU, pTV: TFloat; pGroup: Byte; pHide: Boolean);
begin
  Color   := TGLColor.Create;
  SetCoord(pX, pY);
  SetColor(pColor);
  SetTexCoord(pTU, pTV);
  Group   := pGroup;
  Hide    := pHide;
  GapOccur:= False;
end;

destructor TVertexClass.Destroy;
begin
  if (Color <> nil) and Assigned(Color) then
    Color.Free;

  inherited;
end;

procedure TVertexClass.SetColor(pColor: TColor);
begin
  Color.SetColor(pColor);
end;

procedure TVertexClass.SetCoord(pX, pY: TFloat);
begin
  Vertex.X:= pX;
  Vertex.Y:= pY;
end;

procedure TVertexClass.SetTexCoord(pX, pY: TFloat);
begin
  TexCoord.U:= pX;
  TexCoord.V:= pY;
end;

{ TOffsetImage }

constructor TOffsetImage.Create(pX, pY, pW, pH: TFloat);
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

constructor TBlendParam.Create;
begin
  FEnable  := False;
  FSrc     := GL_ZERO;
  FDst     := GL_ZERO;
  FEquation:= GL_FUNC_ADD;
  FAlpha   := 1.0;
end;

procedure TBlendParam.SetParam(const AEnable: Boolean; const ASrc, ADst: TUInt);
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

procedure TTextureLinkSquadArr.SetCoord(p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y: TFloat);
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

procedure TTextureLinkSquadArr.SetSize(pW, pH: TFloat);
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

procedure TGUICursor.Render;
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
      FColor.glColor4fx;
      glVertex2f(FRect.X + FCursorRenderPos              , FRect.Y);
      glVertex2f(FRect.X + FCursorRenderPos + FRect.Width, FRect.Y);
      glVertex2f(FRect.X + FCursorRenderPos + FRect.Width, FRect.Y + FRect.Height);
      glVertex2f(FRect.X + FCursorRenderPos              , FRect.Y + FRect.Height);
  glEnd;

  glPopMatrix;
end;

procedure TGUICursor.ResetCursor;
begin
  FCursorShow:= True;
  FCursorTime:= GetCurrentTime;
end;

{ TGUIObjectRect }

procedure TGUIObjectRect.Render(pOffset: Integer = 0; pMode: TUInt = GL_LINE_LOOP);
begin
  glBegin(pMode);
    glVertex2f(X - pOffset, Y - pOffset);
    glVertex2f(X + Width + pOffset, Y - pOffset);
    glVertex2f(X + Width + pOffset, Y + Height + pOffset);
    glVertex2f(X - pOffset, Y + Height + pOffset);
  glEnd;
end;

procedure TGUIObjectRect.SetPos(pX, pY: TInt);
begin
  X:= pX;
  Y:= pY;
end;

procedure TGUIObjectRect.SetRect(pRect: TGUIObjectRect);
begin
  SetPos(pRect.X, pRect.Y);
  SetSize(pRect.Width, pRect.Height);
end;

procedure TGUIObjectRect.SetRect(pX, pY, pW, pH: TInt);
begin
  SetPos(pX, pY);
  SetSize(pW, pH);
end;

procedure TGUIObjectRect.SetSize(pWidth, pHeight: TInt);
begin
  Width := pWidth;
  Height:= pHeight;
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
   if not IsEmpty then
    glDeleteTextures(1, @Link);

  inherited;
end;

function TTextureLink.IsEmpty: Boolean;
begin
  Result:= Link = TEX_LINK_EMPTY;
end;

end.
