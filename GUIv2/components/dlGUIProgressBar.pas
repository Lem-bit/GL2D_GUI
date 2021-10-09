unit dlGUIProgressBar;

interface

uses SysUtils, Graphics, dlGUITypes, dlGUIObject, dlGUIPaletteHelper;

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
    private
     type
       TGUIShowTextProp = (
                          stNone, //Никогда не отображать
                          stShow, //Отображать значение
                          stOnlyMouse //Отображать только когда наведена мышь
                        );

    private
      FMax         : Integer;
      FValue       : Integer;
      FStrValue    : String;

      FBorderWidth : Integer;
      FShowText    : TGUIShowTextProp;
    private
      procedure RecalcTextPos;

      procedure SetValue(pValue: Integer);
      procedure SetBorderWidth(pValue: Integer);

      function GetRectWidthValue: Integer;
      function GetColor: TColor;

      procedure SetBorderColor(pColor: TColor);
      function GetBorderColor: TColor;
    protected
      procedure SetColor(pColor: TColor); override;
      procedure SetFontEvent; override;
      procedure SetResize; override; //Применить к вершинам масштабирование Width, Height
      procedure SetAreaResize; override;
    public
      constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
      procedure SetColorGradient(pAColor, pBColor, pCColor, pDColor: TColor);

      procedure RenderText; override;
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

      property ShowText   : TGUIShowTextProp read FShowText      write FShowText       default stNone;
      property BorderColor: TColor           read GetBorderColor write SetBorderColor;
      property BorderWidth: Integer          read FBorderWidth   write SetBorderWidth;
      property Max        : Integer          read FMax           write FMax;
      property Value      : Integer          read FValue         write SetValue;
      property Color      : TColor           read GetColor       write SetColor;
  end;

implementation

const GROUP_BORDER = 0;
      GROUP_VALUE  = 1;

{ TGUIProgressBar }

constructor TGUIProgressBar.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, gtcProgressBar);
  SetRect(pX, pY, 200, 25);

  FBorderWidth:= 1;
  FMax        := 100;
  FValue      := 0;
  Area.Show   := True;
  Color       := clWhite;

  SetTextureLink(pTextureLink);
  RecalcTextPos;

  VertexList.MakeSquare(0, 0, Width, Height, Color, GUIPalette.GetCellRect(pal_6), GROUP_BORDER);
  VertexList.MakeSquare(FBorderWidth, FBorderWidth, GetRectWidthValue, Height - (FBorderWidth * 2), Color, GUIPalette.GetCellRect(pal_3), GROUP_VALUE);
end;

function TGUIProgressBar.GetBorderColor: TColor;
begin
  Result:= VertexList.GetGroupColor(GROUP_BORDER, 1);
end;

function TGUIProgressBar.GetColor: TColor;
begin
  Result:= FColor.GetColor;
end;

function TGUIProgressBar.GetRectWidthValue: Integer;
begin
  Result:= 0;

  if FValue = 0 then
    Exit;

  Result:= Trunc( ((Rect.Width - (FBorderWidth * 2)) / FMax) * FValue );
end;

procedure TGUIProgressBar.SetFontEvent;
begin
  inherited;
  RecalcTextPos;
end;

procedure TGUIProgressBar.RecalcTextPos;
begin
  FStrValue:= FValue.ToString + '%';

  if Rect.Width <> 0 then
    FTextOffset.X:= Trunc(((Rect.Width - FBorderWidth * 3) / 2) - (FFont.GetTextWidth(FStrValue) / 2)) else
    FTextOffset.X:= 0;

  if Rect.Height <> 0 then
    FTextOffset.Y:= Trunc(((Rect.Height - (FBorderWidth * 3)) / 2) - (FFont.Height / 2)) else
    FTextOffset.Y:= 0;
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

procedure TGUIProgressBar.SetAreaResize;
begin
  Area.Rect.SetRect(1, 0, Width - 1, Height -1);
end;

procedure TGUIProgressBar.SetBorderColor(pColor: TColor);
begin
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
  VertexList.SetGroupColor(GROUP_VALUE, pColor);
end;

procedure TGUIProgressBar.SetColorGradient(pAColor, pBColor, pCColor, pDColor: TColor);
begin
  VertexList.SetGroupColorSquare(GROUP_VALUE, [pAColor, pBColor, pCColor, pDColor]);
end;

procedure TGUIProgressBar.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, FBorderWidth, FBorderWidth, GetRectWidthValue, Rect.Height - (FBorderWidth * 2));
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
  VertexList.SetVertexPosSquare(4, FBorderWidth, FBorderWidth, GetRectWidthValue, Rect.Height - (FBorderWidth * 2));
end;

end.
