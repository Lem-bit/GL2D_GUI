unit dlGUITrackBar;

interface

uses SysUtils, dlGUITypes, dlGUIFont, Graphics, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUITrackBar = class(TGUIObject)
  strict private
    const GROUP_TRACK = 1;
    const GROUP_FILL  = 2;
  strict private
    FValue      : Integer; //Текущее значение
    FMaxValue   : Integer; //Максимальное значение
    FBorderWidth: Integer; //Ширина рамки

    FTrackDown  : Boolean; //Нажали на трекер
    FTrackWidth : Integer; //Ширина трекера
    FTrackPos   : Integer; //Позиция трекера
    FTrackOffset: Integer; //Оффсет при нажатии на трекер

    FTrackColor : TColor;  //Цвет трекера
    FFillColor  : TColor;  //Цвет области за трекером

    FOrient     : TGUIOrientation; //Ориентация
  strict private
    function GetRectTracker: TGUIObjectRect; //Рамка (область трекера)
    function GetRectTrack  : TGUIObjectRect; //Трекер
    function GetRectOffset : TGUIObjectRect; //Область за трекером
  strict private
    function GetColor: TColor;

    procedure RecalcTextPos;
    procedure SetValue(pValue: Integer);
    procedure SetMaxValue(pValue: Integer);
    procedure SetTrackPos;
    procedure SetTrackWidth(pValue: Integer);
    procedure SetTrackColor(pValue: TColor);
    procedure SetFillColor(pValue: TColor);
    procedure SetOrient(pValue: TGUIOrientation);

    function GetRealWidth: Integer; //Ширина поля трекера
    function GetCellValue: TFloat;  //Размер (соотношение) 1 пикселя
  protected
    procedure SetFontEvent; override;
    procedure SetResize; override;
    procedure SetColor(pColor: TColor); override;
  public
    [TXMLSerial] OnTrackMin : TGUIProc;
    [TXMLSerial] OnTrackMax : TGUIProc;
    [TXMLSerial] OnTrackMove: TGUIProc;

    constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
    procedure RenderText; override;

    procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseMove(pX, pY: Integer); override;

  public
    [TXMLSerial] property FillColor : TColor          read FFillColor  write SetFillColor;
    [TXMLSerial] property TrackColor: TColor          read FTrackColor write SetTrackColor;
    [TXMLSerial] property TrackWidth: Integer         read FTrackWidth write SetTrackWidth;
    [TXMLSerial] property Value     : Integer         read FValue      write SetValue;
    [TXMLSerial] property MaxValue  : Integer         read FMaxValue   write SetMaxValue;
    [TXMLSerial] property Color     : TColor          read GetColor    write SetColor;
    [TXMLSerial] property Orient    : TGUIOrientation read FOrient     write SetOrient;
  end;

implementation

{ TGUIButton }

constructor TGUITrackBar.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcTrackBar);

  case FOrient of
    goHorizontal: Rect.SetRect(0, 0, 150, 15);
    goVertical  : Rect.SetRect(0, 0, 15, 150);
  end;

  Value       := 0; //Специально не FValue
  FMaxValue   := 100;
  FBorderWidth:= 1;

  Color       := $171717;
  FFillColor  := Color;
  FTrackWidth := 10;
  FTrackPos   := 0;
  FTrackOffset:= 0;
  FTrackDown  := False;
  FTrackColor := clGray;
  Area.Show   := True;

  SetTextureLink(pTextureLink);
  SetOrient(goHorizontal);
end;

function TGUITrackBar.GetCellValue: TFloat;
begin
  Result:= 0;
  case FOrient of
    goHorizontal:
      Result:= (Width - FTrackWidth) / FMaxValue;
    goVertical:
      Result:= (Height - FTrackWidth) / FMaxValue;
  end;
end;

function TGUITrackBar.GetColor: TColor;
begin
  Result:= FColor.GetColor;
end;

function TGUITrackBar.GetRealWidth: Integer;
begin
  Result:= 0;
  case FOrient of
    goHorizontal:
      Result:= Width - FTrackWidth;
    goVertical:
      Result:= Height - FTrackWidth;
  end;
end;

function TGUITrackBar.GetRectOffset: TGUIObjectRect;
begin
  Result.SetRect(0, 0, 0, 0);
  case FOrient of
    goHorizontal: Result.SetRect(0, 0, FTrackPos + 1, Rect.Height);
    goVertical  : Result.SetRect(0, FTrackPos, Rect.Width, Rect.Height - FTrackPos);
  end;
end;

function TGUITrackBar.GetRectTrack: TGUIObjectRect;
begin
  Result.SetRect(0, 0, 0, 0);
  case FOrient of
    goHorizontal: Result.SetRect(FTrackPos, 0, FTrackWidth, Rect.Height);
    goVertical  : Result.SetRect(0, FTrackPos, Rect.Width, FTrackWidth);
  end;
end;

function TGUITrackBar.GetRectTracker: TGUIObjectRect;
begin
  Result.SetRect(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

procedure TGUITrackBar.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  if Button <> gmbLeft then
    Exit;

  if OnHit(pX, pY) then
  case FOrient of
    goHorizontal:
    begin
      Value:= Trunc((pX - Rect.X - (FTrackWidth / 2)) / GetCellValue);
      FTrackOffset:= (Rect.X + FTrackPos) - pX;
    end;

    goVertical:
    begin
      Value:= FMaxValue - (Trunc((pY - Rect.Y - (FTrackWidth / 2)) / GetCellValue));
      FTrackOffset:= ((Rect.Y + FTrackPos) - pY);
    end;
  end;

  FTrackDown:= True;
end;

procedure TGUITrackBar.OnMouseMove(pX, pY: Integer);
begin
  inherited;

  if not (goaDown in FAction) then
    Exit;

  if not FTrackDown then
    Exit;

  case FOrient of
    goHorizontal:
      FTrackPos:= FTrackPos + pX - (Rect.X + FTrackPos) + FTrackOffset;
    goVertical:
      FTrackPos:= FTrackPos + pY - (Rect.Y + FTrackPos) + FTrackOffset;
  end;

  if FTrackPos < 0 then
  begin
    FTrackPos:= 0;

    if Assigned(OnTrackMin) then
      OnTrackMin(Self, @Value);
  end;

  if FTrackPos > GetRealWidth then
  begin
    FTrackPos:= GetRealWidth;

    if Assigned(OnTrackMax) then
      OnTrackMax(Self, @Value);
  end;

  case FOrient of
    goHorizontal:
      FValue:= Round(FTrackPos / GetCellValue);
    goVertical:
      FValue:= FMaxValue - Round(FTrackPos / GetCellValue);
  end;

  SetTrackPos;

  if Assigned(OnTrackMove) then
    OnTrackMove(Self, @Value);

  {!}RecalcTextPos;
end;

procedure TGUITrackBar.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;
  FTrackDown:= False;
end;

procedure TGUITrackBar.RecalcTextPos;
begin
  if Rect.Width <> 0 then
    FTextOffset.X:= Trunc(((Rect.Width - FBorderWidth * 3) / 2) - (FFont.GetTextWidth(FValue.ToString) / 2))
  else
    FTextOffset.X:= 0;

  if Rect.Height <> 0 then
    FTextOffset.Y:= Trunc(((Rect.Height - (FBorderWidth * 3)) / 2) - (FFont.Height / 2))
  else
    FTextOffset.Y:= 0;
end;

procedure TGUITrackBar.RenderText;
begin
  inherited;

  Font.Text(Rect.X + TextRect.X, Rect.Y + TextRect.Y, FValue.ToString, 0);
end;

procedure TGUITrackBar.SetColor(pColor: TColor);
begin
  FColor.SetColor(pColor);
  VertexList.SetGroupColor(0, pColor);
end;

procedure TGUITrackBar.SetFillColor(pValue: TColor);
begin
  FFillColor:= pValue;
  VertexList.SetGroupColor(GROUP_FILL, pValue);
end;

procedure TGUITrackBar.SetFontEvent;
begin
  inherited;
  RecalcTextPos;
end;

procedure TGUITrackBar.SetMaxValue(pValue: Integer);
begin
  FMaxValue:= pValue;

  if FMaxValue < 0 then
    FMaxValue:= 0;

  if FValue > FMaxValue then
    Value:= FMaxValue
  else
    Value:= FValue; //Для обновления позиции
end;

procedure TGUITrackBar.SetOrient(pValue: TGUIOrientation);
begin
  FOrient:= pValue;
  VertexList.Clear;

  //Рамка
  VertexList.MakeSquare(GetRectTracker, Color, GUIPalette.GetCellRect(pal_2));
  //Область трекера
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_0));
  //Область заполнения за трекером
  VertexList.MakeSquareOffset(0, 2, FFillColor, GUIPalette.GetCellRect(pal_0), GROUP_FILL);
  //Трекер
  VertexList.MakeSquare(GetRectTrack, FTrackColor, GUIPalette.GetCellRect(pal_Track), GROUP_TRACK);
end;

procedure TGUITrackBar.SetResize;
begin
  VertexList.SetSizeSquare(0, GetRectTracker);
  VertexList.SetSizeSquare(4, GetRectTracker, 1);
  SetTrackPos;
end;

procedure TGUITrackBar.SetTrackColor(pValue: TColor);
begin
  FTrackColor:= pValue;
  VertexList.SetGroupColor(GROUP_TRACK, FTrackColor);
end;

procedure TGUITrackBar.SetTrackPos;
begin
  VertexList.SetRectSquare(8 , GetRectOffset, 1);
  VertexList.SetRectSquare(12, GetRectTrack);
end;

procedure TGUITrackBar.SetTrackWidth(pValue: Integer);
begin
  FTrackWidth:= pValue;

  if FTrackWidth < 4 then
    FTrackWidth:= 4;

  SetTrackPos;
end;

procedure TGUITrackBar.SetValue(pValue: Integer);
begin
  FValue:= pValue;

  if FValue < 0         then FValue:= 0;
  if FValue > FMaxValue then FValue:= FMaxValue;

  case FOrient of
    goHorizontal:
      FTrackPos:= Trunc(GetCellValue * FValue);
    goVertical:
      FTrackPos:= Trunc(GetCellValue * (FMaxValue - FValue));
  end;

  SetTrackPos;
  RecalcTextPos;
end;

end.
