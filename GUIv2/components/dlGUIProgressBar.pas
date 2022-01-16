unit dlGUIProgressBar;

interface

uses SysUtils, Graphics, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUIProgressBar = class(TGUIObject)
    strict private
     type
       TGUIShowTextProp = (
                          stNone, //Никогда не отображать
                          stShow, //Отображать значение
                          stOnlyMouse //Отображать только когда наведена мышь
                        );

    strict private
      FMax         : Integer;
      FValue       : Integer;
      FFormat      : String; //Формат строки
      FStrValue    : String;

      FBorderWidth : Integer;
      FShowText    : TGUIShowTextProp;
    strict private
      procedure RecalcTextPos;

      procedure SetValue(pValue: Integer);
      procedure SetBorderWidth(pValue: Integer);

      function GetRectWidthValue: TGUIObjectRect;
      function GetColor: TColor;

      procedure SetBorderColor(pColor: TColor);
      function GetBorderColor: TColor;

      procedure SetFormat(pValue: String);
    protected
      procedure SetColor(pColor: TColor); override;
      procedure SetFontEvent; override;
      procedure SetResize; override; //Применить к вершинам масштабирование Width, Height
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
      procedure SetColorGradient(pAColor, pBColor, pCColor, pDColor: TColor);

      procedure SetDefaultColor;
      procedure RenderText; override;
    public
      [TXMLSerial] property Rect;
      [TXMLSerial] property StrFormat  : String           read FFormat        write SetFormat;
      [TXMLSerial] property ShowText   : TGUIShowTextProp read FShowText      write FShowText       default stNone;
      [TXMLSerial] property BorderColor: TColor           read GetBorderColor write SetBorderColor;
      [TXMLSerial] property BorderWidth: Integer          read FBorderWidth   write SetBorderWidth;
      [TXMLSerial] property Max        : Integer          read FMax           write FMax;
      [TXMLSerial] property Value      : Integer          read FValue         write SetValue;
      [TXMLSerial] property Color      : TColor           read GetColor       write SetColor;
  end;

implementation

const GROUP_BORDER = 0;
      GROUP_VALUE  = 1;

{ TGUIProgressBar }

constructor TGUIProgressBar.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcProgressBar);
  SetRect(0, 0, 200, 25);

  FBorderWidth:= 1;
  FMax        := 100;
  FValue      := 0;
  Area.Show   := True;
{  Color       := clBlack;
  BorderColor := clBlack;}
  FFormat     := '%s %%';

  SetTextureLink(pTextureLink);
  RecalcTextPos;

  VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_6), GROUP_BORDER);
  VertexList.MakeSquare(GetRectWidthValue, Color, GUIPalette.GetCellRect(pal_3), GROUP_VALUE);
end;

function TGUIProgressBar.GetBorderColor: TColor;
begin
  Result:= VertexList.GetGroupColor(GROUP_BORDER, 1);
end;

function TGUIProgressBar.GetColor: TColor;
begin
  Result:= FColor.GetColor;
end;

function TGUIProgressBar.GetRectWidthValue: TGUIObjectRect;
begin
  Result.SetRect(0, 0, 0, 0);

  if FValue = 0 then
    Exit;

  Result.SetRect(Rect.X,
                 Rect.Y,
                 Round( ((Rect.Width - (FBorderWidth * 2) ) / FMax) * FValue ) + FBorderWidth * 2,
                 Rect.Height);
end;

procedure TGUIProgressBar.SetFontEvent;
begin
  inherited;
  RecalcTextPos;
end;

procedure TGUIProgressBar.SetFormat(pValue: String);
begin
  if Pos('%s', pValue) = 0 then
    Exit;

  FFormat:= pValue;
end;

procedure TGUIProgressBar.RecalcTextPos;
var TxtWidth: Single;
begin
  FStrValue    := Format(FFormat, [ FValue.ToString ]);
  TxtWidth     := FFont.GetTextWidth(FStrValue);
  FTextOffset.X:= Round((Rect.Width / 2) - (TxtWidth / 2));
  FTextOffset.Y:= Round(((Rect.Height - (FBorderWidth * 2)) / 2) - (FFont.Height / 2));

  if Rect.X + FTextOffset.X < Rect.X then
    FTextOffset.X:= 0;

  if TxtWidth > Rect.Width then
    FStrValue:= '';
end;

procedure TGUIProgressBar.RenderText;
begin
  if FShowText = stNone then
    Exit;

  if (FShowText = stOnlyMouse) and (not Area.Visible) then
    Exit;

  inherited;
  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FStrValue, Rect.Width);
end;

procedure TGUIProgressBar.SetBorderColor(pColor: TColor);
begin
  VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(pal_0));
  VertexList.SetGroupColor(GROUP_BORDER, pColor);
end;

procedure TGUIProgressBar.SetBorderWidth(pValue: Integer);
begin
  FBorderWidth:= pValue;

  if FBorderWidth < 1 then
    FBorderWidth:= 1;

end;

procedure TGUIProgressBar.SetColor(pColor: TColor);
begin
  FColor.SetColor(pColor);
  VertexList.SetVertexTextureMap(4, GUIPalette.GetCellRect(pal_0));
  VertexList.SetGroupColor(GROUP_VALUE, FColor.GetColor);
end;

procedure TGUIProgressBar.SetColorGradient(pAColor, pBColor, pCColor, pDColor: TColor);
begin
  VertexList.SetGroupColorSquare(GROUP_VALUE, [pAColor, pBColor, pCColor, pDColor]);
end;

procedure TGUIProgressBar.SetDefaultColor;
begin
  VertexList.SetVertexTextureMap(0, GUIPalette.GetCellRect(pal_6));
  VertexList.SetGroupColor(GROUP_BORDER, clWhite);
  VertexList.SetVertexTextureMap(4, GUIPalette.GetCellRect(pal_3));
  VertexList.SetGroupColor(GROUP_VALUE , clWhite);
end;

procedure TGUIProgressBar.SetResize;
begin
  VertexList.SetSizeSquare(0, Rect);
  VertexList.SetSizeSquare(4, GetRectWidthValue, FBorderWidth);
  RecalcTextPos;
end;

procedure TGUIProgressBar.SetValue(pValue: Integer);
begin
  FValue:= pValue;

  if FValue > FMax then
    FValue:= FMax;

  if FValue < 0 then
    FValue:= 0;

  RecalcTextPos;
  VertexList.SetSizeSquare(4, GetRectWidthValue, FBorderWidth);
end;

end.
