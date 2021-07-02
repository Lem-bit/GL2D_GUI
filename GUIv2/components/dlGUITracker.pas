﻿unit dlGUITracker;

interface

uses SysUtils, Classes, Graphics, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIImage, dlGUIButton;

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
  TGUITrackerStyle = (tsVertical, tsHorizontal);
  TGUITracker = class(TGUIObject)
  private
    const BTN_UPL = 1; //Кнопка 1
          BTN_DNR = 2; //Кнопка 2
          TRK_OBJ = 3; //Трекер
  private
    FSize        : TInt; //Размер квадрата (кнопки)
    FButton      : array[BTN_UPL..BTN_DNR] of TGUIButton; //1 - up, 2 - down
    FTrack       : TGUIImage;
    FTrackOffsetX: TInt; //
    FTrackOffsetY: TInt; //
    FStyle       : TGUITrackerStyle; //Стиль прорисовки
    FDownColor   : TColor;  //Цвет при нажатии на элемент управления
    FShowTrack   : Boolean; //Показывать трекер

    FMaxValue    : TInt;    //Макс кол-во элементов
    FCurrValue   : TInt;    //Текущее положение на каком элементе
  private
    //Позиция кнопки по X, pBtnIndex - индекс кнопки BTN_UP, BTN_DN...
    function GetObjPosX(pObjIndex: integer): Integer;
    function GetObjPosY(pObjIndex: integer): Integer;

    //Макс высота
    function GetMaxHeight: TInt;
    //Макс ширина
    function GetMaxWidth: TInt;
  private
    procedure SetShowTrack(pValue: Boolean);
    procedure SetMaxValue(pValue: TInt);

    function GetCurrPoint: TInt; //Текущее положение по Х или Y в зависимости от значение FCurrValue
    procedure SetTrackPos(pValue: TInt); //Установить позицию трекера
    procedure SetValue(pValue: TInt);

    procedure SetTrackerPosByValue(pValue: TInt); //Установить позицию трекера по номеру элемента FCurrValue например

    procedure ButtonUpLeftClick(Sender: TObject; ParamObj: Pointer = nil);
    procedure ButtonDnRightClick(Sender: TObject; ParamObj: Pointer = nil);
  protected
    procedure SetResize; override;
  public
    OnMove    : TGUIProc; //Перемещение трекера
    OnMinValue: TGUIProc; //Минимальное значение
    OnMaxValue: TGUIProc; //Максимальное значение
  public
    constructor Create(pLeft, pTop, pWidth, pHeight: TInt; pTrackerStyle: TGUITrackerStyle; pTextureLink: TTextureLink = nil);
    procedure SetTextureLink(pTextureLink: TTextureLink); override;

    function GetTrackerValue: TInt; //Текущий элемент
    function GetTrackerPos: TInt; //Позиция трекера 0..100 в процентах

    procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseMove(pX, pY: Integer); override;
    procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: TInt); override;
    procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: TInt); override;

    //Изменить размер трекера по Parent.Rect с учетом BorderSize вернуть максимальное значение для Rect.Width или Height
    //В зависимости от типа трекера
    function Resize(pRect: TGUIObjectRect; pBorderSize: TInt = 0; pOffset: TInt = 0): TInt;

    procedure Render; override;
  public
    property DownColor: TColor  read FDownColor write FDownColor;
    property ShowTrack: Boolean read FShowTrack write SetShowTrack;
    property MaxValue : TInt    read FMaxValue  write SetMaxValue;
    property Size     : TInt    read FSize;
  end;

  //Класс - Интерфейс с трекерами для объектов (ListBox, Memo, Table...)
  TGUITrackerIntf = class(TGUIObject)
    //Трекер
    const V_TR = 1;
          H_TR = 2;
          W_TR = 16;
    protected
      FTracker: array[1..2] of TGUITracker;
    private
      //Создать трекеры
      procedure CreateTrackers;

    protected
      FBorder   : TInt;
      FMaxWidth : TInt;
      FMaxHeight: TInt;
    protected
      function GetHorizTracker: TGUITracker;
      function GetVertTracker: TGUITracker;
    protected
      property HTracker: TGUITracker read GetHorizTracker;
      property VTracker: TGUITracker read GetVertTracker;
    protected
      procedure SetResize; override;
    public
      procedure SetTextureLink(pTextureLink: TTextureLink); override;
    public
      constructor Create(pName: String; pType: TGUITypeComponent; pX, pY, pW, pH: Integer; pTextureLink: TTextureLink);
      destructor Destroy; override;

      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: TInt); override;
      procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: TInt); override;

      procedure Render; override;
  end;

implementation

{ TGUITracker }

procedure TGUITracker.ButtonUpLeftClick(Sender: TObject; ParamObj: Pointer);
begin
  SetValue(FCurrValue - 1);
  SetTrackPos(GetCurrPoint);
end;

procedure TGUITracker.ButtonDnRightClick(Sender: TObject; ParamObj: Pointer);
begin
  SetValue(FCurrValue + 1);
  SetTrackPos(GetCurrPoint);
end;

function TGUITracker.GetMaxHeight: TInt;
begin
  Result:= Height - (FSize * 3);
end;

function TGUITracker.GetMaxWidth: TInt;
begin
  Result:= Width - (FSize * 2);
end;

function TGUITracker.GetCurrPoint: TInt;
begin
  //Получить позицию трекера по текущему элементу
  Result:= 0;

  case FStyle of
    tsVertical  : Result:= Round(
                        {Стартовая позиция} GetObjPosY(TRK_OBJ) +
                        {Позиция трекера} ((GetMaxHeight / MaxValue) * FCurrValue)
                       );
    tsHorizontal: Result:= Round(
                        {Стартовая позиция} GetObjPosX(TRK_OBJ) +
                        {Позиция трекера} ((GetMaxWidth / MaxValue) * FCurrValue)
                       );
  end;
end;

function TGUITracker.GetTrackerPos: TInt;
var Persent: TFloat;
begin
  //Получить текущий элемент по позиции трекера
  Result:= 0;

  case FStyle of
    tsVertical  : begin
                    //Процентное соотношение трекера ко всей высоте (для трекера)
                    Persent:= ((FTrack.Y - Y - FSize) / GetMaxHeight) * 100;
                    //Процентное соотношение * Макс кол-во элементов
                    Result := Round((MaxValue / 100) * Persent);
                  end;
    tsHorizontal: begin
                    Persent:= ((FTrack.X - X - FSize) / GetMaxWidth) * 100;
                    Result := Round((MaxValue / 100) * Persent);
                  end;
  end;

end;

procedure TGUITracker.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
    ControlDown: Boolean;
begin
  inherited;

  ControlDown:= False;

  if Assigned(FTrack) then
  begin
    FTrack.OnMouseDown(pX, pY, Button);  //Для GetAction goaDown

    if (goaDown in FTrack.GetAction) then
    begin
      FTrackOffsetX:= pX - FTrack.X;
      FTrackOffsetY:= pY - FTrack.Y;
      FTrack.Color := FDownColor;
     // ControlDown  := True;
    end;
  end;

  for i := Low(FButton) to High(FButton) do
    if Assigned(FButton[i]) then
    begin
      FButton[i].OnMouseDown(pX, pY, Button);

      if goaDown in FButton[i].GetAction then
      begin
        FButton[i].Color:= FDownColor;
        ControlDown     := True;
      end;
    end;

  if not Assigned(FTrack) then
    Exit;

  if ControlDown then
    Exit;

  if not OnHit(pX, pY) then
    Exit;

  if Button <> gmbLeft then
    Exit;

  case FStyle of
    tsVertical: begin
      SetTrackPos(Y + Trunc((pY - Y) - (FSize / 2)) );
      SetValue(GetTrackerPos); {*}
      FTrackOffsetY:= pY - FTrack.Y;
    end;

    tsHorizontal: begin
      SetTrackPos(X + Trunc((pX - X) - (FSize / 2)) );
      SetValue(GetTrackerPos); {*}
      FTrackOffsetX:= pX - FTrack.X;
    end;
  end;

  FTrack.SetAction([goaDown]);
end;

procedure TGUITracker.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  inherited;

  if Assigned(FTrack) then
  begin
    FTrack.OnMouseUp(pX, pY, Button);

    if not (goaDown in FTrack.GetAction) then
      FTrack.Color:= clWhite;

  end;

  for i := Low(FButton) to High(FButton) do
    if Assigned(FButton[i]) then
    begin
      FButton[i].OnMouseUp(pX, pY, Button);

      if not (goaDown in FButton[i].GetAction) then
        FButton[i].Color:= clWhite;
    end;
end;

procedure TGUITracker.OnMouseMove(pX, pY: Integer);
var i: integer;
begin
  inherited;

  if Assigned(FTrack) then
  begin
    FTrack.OnMouseMove(pX, pY);

    if (goaDown in FTrack.GetAction) then
      case FStyle of
        tsVertical  : begin
                        SetTrackPos(pY - FTrackOffsetY);
                        SetValue(Round(GetTrackerPos));
                      end;
        tsHorizontal: begin
                        SetTrackPos(pX - FTrackOffsetX);
                        SetValue(Round(GetTrackerPos));
                      end;
      end;

  end;

  for i := Low(FButton) to High(FButton) do
    if Assigned(FButton[i]) then
      FButton[i].OnMouseMove(pX, pY);
end;

constructor TGUITracker.Create(pLeft, pTop, pWidth, pHeight: TInt; pTrackerStyle: TGUITrackerStyle; pTextureLink: TTextureLink);
var i: integer;
    imgOff: integer;
begin
  inherited Create;

  SetRect(pLeft, pTop, pWidth, pHeight);

  FStyle    := pTrackerStyle;
  FSize     := 16; //  FSize:= pWidth; Размер по умолчанию
  FDownColor:= $00ADADAD;//clSilver;
  FShowTrack:= True;
  FMaxValue := 1;
  FCurrValue:= 0;
  imgOff    := 0;

  SetTextureLink(pTextureLink);

  case FStyle of
    tsVertical  : begin
      FSize := pWidth;
      imgOff:= 0;
    end;

    tsHorizontal: begin
      FSize := pHeight;
      imgOff:= High(FButton);
    end;
  end;

  //Создаем кнопки
  for i := Low(FButton) to High(FButton) do
  begin
    FButton[i]:= TGUIButton.Create('', '', 0, 0, pTextureLink);
    FButton[i].Flat:= True;
    FButton[i].SetRect(GetObjPosX(i), GetObjPosY(i), FSize, FSize);
    FButton[i].VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(11 + i + imgOff));
    FButton[i].VertexList.SetVertexTextureMap(8, GUIPalette.GetCellRect(11 + i + imgOff));
  end;

  //Нажатия на кнопки
  FButton[BTN_UPL].OnClick:= ButtonUpLeftClick;
  FButton[BTN_DNR].OnClick:= ButtonDnRightClick;

  //Трекер
  if FStyle <> tsHorizontal then
    imgOff:= 0
  else
    imgOff:= 1;

  FTrack:= TGUIImage.Create('', GetObjPosX(TRK_OBJ), GetObjPosY(TRK_OBJ), FSize, FSize, pTextureLink);
  FTrack.VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(25 + imgOff));
  FTrack.Area.Show:= False;

  //Область трекера + кнопок
  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(4));
end;

function TGUITracker.GetObjPosX(pObjIndex: integer): Integer;
begin
  Result:= 0;

  case FStyle of
    tsVertical:
      case pObjIndex of
        BTN_UPL: Result:= X;
        BTN_DNR: Result:= X;
        TRK_OBJ: Result:= X;
      end;

    //Горизонтальный
    tsHorizontal:
      case pObjIndex of
        BTN_UPL: Result:= X;
        BTN_DNR: Result:= X + Width - FSize;
        TRK_OBJ: Result:= X + FSize;
      end;
  end;
end;

function TGUITracker.GetObjPosY(pObjIndex: integer): Integer;
begin
  Result:= 0;

  case FStyle of
    tsVertical:
      case pObjIndex of
        BTN_UPL: Result:= Y;
        BTN_DNR: Result:= Y + Height - FSize;
        TRK_OBJ: Result:= Y + FSize;
      end;

    //Горизонтальный
    tsHorizontal:
      case pObjIndex of
        BTN_UPL: Result:= Y;
        BTN_DNR: Result:= Y;
        TRK_OBJ: Result:= Y;
      end;
  end;
end;

function TGUITracker.GetTrackerValue: TInt;
begin
  Result:= FCurrValue;
end;

procedure TGUITracker.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: TInt);
begin
  ButtonDnRightClick(Self, nil);
end;

procedure TGUITracker.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: TInt);
begin
  ButtonUpLeftClick(Self, nil);
end;

procedure TGUITracker.Render;
var i: integer;
begin
  inherited;

  if Hide then
    Exit;

  if Assigned(FTrack) then
    FTrack.Render;

  for i := Low(FButton) to High(FButton) do
    if Assigned(FButton[i]) then
      FButton[i].Render;
end;

function TGUITracker.Resize(pRect: TGUIObjectRect; pBorderSize: TInt = 0; pOffset: TInt = 0): TInt;
begin
  Result:= 0;

  //Изменить положение и размер трекера по новым размерам
  //pRect от "родителя" с учетом размера рамки BorderSize
  //и с учетом показа горизонтального трекера pOffset
  //Возвращаем результат максимальной ширины/высоты для вывода данных с учетом размера трекера

  case FStyle of
    tsVertical  : begin
        SetRect(
                 pRect.X + pRect.Width - FSize - pBorderSize,
                 pRect.Y + pBorderSize,
                 FSize,
                 pRect.Height - (pBorderSize * 2) - pOffset
                );

        Result:= pRect.Width - (pBorderSize * 2);
        if not Hide then
          Result:= Result - Width;

        //Установить положение трекера на дорожке в зависимости от выбранного элемента
        SetTrackerPosByValue(FCurrValue);
    end;

    tsHorizontal: begin
        SetRect(pRect.X + pBorderSize,
                pRect.Y + pRect.Height - pBorderSize - FSize,
                pRect.Width - (pBorderSize * 2),
                FSize);

        Result:= pRect.Height - (pBorderSize * 2);
        if not Hide then
          Result:= Result - Height;

        SetTrackerPosByValue(FCurrValue);
    end;
  end;
end;

procedure TGUITracker.SetMaxValue(pValue: TInt);
begin
  //Установим максимальное значение
  FMaxValue:= pValue;

  if FMaxValue < 1 then
    FMaxValue:= 1;

  if FCurrValue > FMaxValue then
    FCurrValue:= FMaxValue;

  //Сместим трекер относительно значения
  SetTrackerPosByValue(FCurrValue);
end;

procedure TGUITracker.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);

  if Assigned(FButton[BTN_UPL]) then
    FButton[BTN_UPL].SetRect(GetObjPosX(BTN_UPL), GetObjPosY(BTN_UPL), FSize, FSize);
  if Assigned(FButton[BTN_DNR]) then
    FButton[BTN_DNR].SetRect(GetObjPosX(BTN_DNR), GetObjPosY(BTN_DNR), FSize, FSize);

  if Assigned(FTrack) then
    FTrack.SetRect(GetObjPosX(TRK_OBJ), GetObjPosY(TRK_OBJ), FSize, FSize);
end;

procedure TGUITracker.SetShowTrack(pValue: Boolean);
var i: integer;
begin
  FShowTrack := pValue;
  FTrack.Hide:= not FShowTrack;

  for i := Low(FButton) to High(FButton) do
    FButton[i].Enable:= FShowTrack;
end;

procedure TGUITracker.SetTrackerPosByValue(pValue: TInt);
begin

  case FStyle of
    tsVertical  : SetTrackPos(GetCurrPoint);
    tsHorizontal: SetTrackPos(GetCurrPoint);
  end;

end;

procedure TGUITracker.SetTrackPos(pValue: TInt);
begin
  if Hide then
  begin
    case FStyle of
      tsVertical  : FTrack.Y:= 0;
      tsHorizontal: FTrack.X:= 0;
    end;
    Exit;
  end;

  case FStyle of
    tsVertical:
    begin
      //Верхняя граница
      if pValue < Y + FSize then
      begin
        pValue:= Y + FSize;

        if Assigned(OnMinValue) then
          OnMinValue(Self, nil);
      end;

      //Нижняя граница
      if pValue > Y + Height - (FSize * 2) then
      begin
        pValue:= Y + Height - (FSize * 2);

        if Assigned(OnMinValue) then
          OnMaxValue(Self, nil);
      end;

      FTrack.Y:= pValue;
    end;

    tsHorizontal:
    begin
      if pValue < X + FSize then
      begin
        pValue:= X + FSize;

        if Assigned(OnMinValue) then
          OnMinValue(Self, nil);
      end;

      if pValue > X + Width - (FSize * 2) then
      begin
        pValue:= X + Width - (FSize * 2);

        if Assigned(OnMaxValue) then
          OnMinValue(Self, nil);
      end;

      FTrack.X:= pValue;
    end;
  end;

  if Assigned(OnMove) then
    OnMove(Self, @pValue);

end;

procedure TGUITracker.SetValue(pValue: TInt);
begin
  //Установим значение
  FCurrValue:= pValue;

  if FCurrValue < 0 then
    FCurrValue:= 0;

  if FCurrValue > FMaxValue then
    FCurrValue:= FMaxValue;

end;

procedure TGUITracker.SetTextureLink(pTextureLink: TTextureLink);
var i: integer;
begin
  inherited;

  if Assigned(FTrack) then
    FTrack.SetTextureLink(pTextureLink);

  for i := Low(FButton) to High(FButton) do
    if Assigned(FButton[i]) then
      FButton[i].SetTextureLink(pTextureLink);
end;


{ TGUITrackerIntf }

constructor TGUITrackerIntf.Create(pName: String; pType: TGUITypeComponent; pX, pY, pW, pH: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, pType);
  CreateTrackers;

  SetRect(pX, pY, pW, pH);

  HTracker.Hide:= True;
  VTracker.Hide:= True;

  HTracker.MaxValue:= 100;
  VTracker.MaxValue:= 100;
end;

procedure TGUITrackerIntf.CreateTrackers;
begin
  FTracker[V_TR]:= TGUITracker.Create(Width - W_TR, 1, W_TR, Height - 2, tsVertical);
  FTracker[H_TR]:= TGUITracker.Create(1, Height - W_TR, Width - 2, W_TR, tsHorizontal);
end;

destructor TGUITrackerIntf.Destroy;
var i: integer;
begin
  for i := Low(FTracker) to High(FTracker) do
    FreeAndNil(FTracker[i]);

  inherited;
end;

function TGUITrackerIntf.GetHorizTracker: TGUITracker;
begin
  Result:= FTracker[H_TR];
end;

function TGUITrackerIntf.GetVertTracker: TGUITracker;
begin
  Result:= FTracker[V_TR];
end;

procedure TGUITrackerIntf.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  inherited;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].OnMouseDown(pX, pY, Button);
end;

procedure TGUITrackerIntf.OnMouseMove(pX, pY: Integer);
var i: integer;
begin
  inherited;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].OnMouseMove(pX, pY);
end;

procedure TGUITrackerIntf.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  inherited;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].OnMouseUp(pX, pY, Button);
end;

procedure TGUITrackerIntf.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: TInt);
begin
  if goaFocused in GetAction then
    VTracker.OnMouseWheelDown(Shift, MPosX, MPosY);
end;

procedure TGUITrackerIntf.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: TInt);
begin
  if goaFocused in GetAction then
    VTracker.OnMouseWheelUp(Shift, MPosX, MPosY);
end;

procedure TGUITrackerIntf.Render;
var i: integer;
begin
  inherited;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].Render;

end;

procedure TGUITrackerIntf.SetResize;
var AOffset: TInt;
begin
  AOffset:= 0;
  if not HTracker.Hide then
    AOffset:= HTracker.Size;

  //Изменение позиции и размера трекеров
  FMaxWidth := VTracker.Resize(Self.Rect, FBorder, AOffset);
  FMaxHeight:= HTracker.Resize(Self.Rect, FBorder);

  inherited;
end;

procedure TGUITrackerIntf.SetTextureLink(pTextureLink: TTextureLink);
var i: integer;
begin
  inherited;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].SetTextureLink(pTextureLink);
end;

end.