unit dlGUITrackBar;

interface

uses SysUtils, dlGUITypes, dlGUIFont, Graphics, dlGUIObject, dlGUIPaletteHelper;

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
  TGUITrackBar = class(TGUIObject)
  private
    FValue      : Integer; //Текущее значение
    FMaxValue   : Integer; //Максимальное значение
    FBorderWidth: Integer; //Ширина рамки

    FTrackDown  : Boolean; //Нажали на трекер
    FTrackWidth : Integer; //Ширина трекера
    FTrackPos   : Integer; //Позиция трекера
    FTrackOffset: Integer; //Оффсет при нажатии на трекер

    FTrackColor : TColor;
  private
    function GetColor: TColor;

    procedure RecalcTextPos;
    procedure SetValue(pValue: Integer);
    procedure SetMaxValue(pValue: Integer);
    procedure SetTrackPos;
    procedure SetTrackWidth(pValue: Integer);
    procedure SetTrackColor(pValue: TColor);

    function GetRealWidth: Integer; //Ширина поля трекера
    function GetCellValue: TFloat;  //Размер (соотношение) 1 пикселя
  protected
    procedure SetFontEvent; override;
    procedure SetResize; override;
    procedure SetColor(pColor: TColor); override;
    procedure SetAreaResize; override;
  public
    OnTrackMin : TGUIProc;
    OnTrackMax : TGUIProc;
    OnTrackMove: TGUIProc;

    constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
    procedure RenderText; override;

    procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseMove(pX, pY: Integer); override;

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

    property TrackColor: TColor  read FTrackColor write SetTrackColor;
    property TrackWidth: Integer read FTrackWidth write SetTrackWidth;
    property Value     : Integer read FValue      write SetValue;
    property MaxValue  : Integer read FMaxValue   write SetMaxValue;
    property Color     : TColor  read GetColor    write SetColor;
  end;

implementation

const GROUP_TRACK = 1;

{ TGUIButton }

constructor TGUITrackBar.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, gtcTrackBar);

  Value       := 0; //Специально не FValue
  FMaxValue   := 100;
  FBorderWidth:= 1;

  Color       := $171717;
  FTrackWidth := 10;
  FTrackPos   := 0;
  FTrackOffset:= 0;
  FTrackDown  := False;
  FTrackColor := clGray;
  Area.Show   := True;

  Rect.SetRect(pX, pY, 150, 15);

  SetTextureLink(pTextureLink);

  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_2));
  VertexList.MakeSquareOffset(0, 1, Color, GUIPalette.GetCellRect(pal_0));
  VertexList.MakeSquare(0, 0, FTrackWidth, Rect.Height, FTrackColor, GUIPalette.GetCellRect(pal_Track), GROUP_TRACK);

end;

function TGUITrackBar.GetCellValue: TFloat;
begin
  Result:= (Width - FTrackWidth) / FMaxValue;
end;

function TGUITrackBar.GetColor: TColor;
begin
  Result:= FColor.GetColor;
end;

function TGUITrackBar.GetRealWidth: Integer;
begin
  Result:= Width - FTrackWidth;
end;

procedure TGUITrackBar.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
begin
  inherited;

  if OnHit(pX, pY) then
    Value:= Trunc((pX - Rect.X - (FTrackWidth / 2)) / GetCellValue);

  if (pX >= Rect.X + FTrackPos) and (pX <= Rect.X + FTrackPos + FTrackWidth) and
     (pY >= Rect.Y   ) and (pY <= Rect.Y + Rect.Height) then
     begin
       FTrackOffset:= (Rect.X + FTrackPos) - pX;
       FTrackDown  := True;
       Exit;
     end;

end;

procedure TGUITrackBar.OnMouseMove(pX, pY: Integer);
begin
  inherited;

  if not (goaDown in FAction) then
    Exit;

  if not FTrackDown then
    Exit;

  FTrackPos:= FTrackPos +  pX - (Rect.X + FTrackPos) + FTrackOffset;

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

  FValue:= Round(FTrackPos / GetCellValue);
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

procedure TGUITrackBar.SetAreaResize;
begin
  Area.Rect.SetRect(0, -1, Rect.Width + 1, Rect.Height + 1);
end;

procedure TGUITrackBar.SetColor(pColor: TColor);
begin
  FColor.SetColor(pColor);
  VertexList.SetGroupColor(0, pColor);
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

procedure TGUITrackBar.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);
  VertexList.SetVertexPosSquare(8, FTrackPos, 0, FTrackWidth, Rect.Height);
end;

procedure TGUITrackBar.SetTrackColor(pValue: TColor);
begin
  FTrackColor:= pValue;
  VertexList.SetGroupColor(GROUP_TRACK, FTrackColor);
end;

procedure TGUITrackBar.SetTrackPos;
begin
  VertexList.SetVertexPosSquare(8, FTrackPos, 0, FTrackWidth, Rect.Height);
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

  FTrackPos:= Trunc(GetCellValue * FValue);
  SetTrackPos;
  RecalcTextPos;
end;

end.
