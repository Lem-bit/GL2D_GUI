unit dlGUILabel;

interface

uses SysUtils, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUILabel = class(TGUIObject)
    strict private
      FText    : String;  //Текст
      FWordWarp: Boolean; //Переносить текст на след строку или нет
    strict private
      procedure SetText(pText: String);
    protected
      procedure SetFontEvent; override;
      procedure SetResize; override;
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
      procedure RenderText; override;
      procedure Render; override;
      procedure ClearTexture;
    public
      [TXMLSerial] property Text    : String  read FText     write SetText;
      [TXMLSerial] property WordWarp: Boolean read FWordWarp write FWordWarp;
  end;

implementation

{ TGUILabel }

procedure TGUILabel.ClearTexture;
begin
  SetTextureLink(nil);
end;

constructor TGUILabel.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcLabel);
  Rect.SetRect(0, 0, 0, 0);

  FWordWarp:= False;
  FText    := '';

  SetTextureLink(pTextureLink);
  VertexList.MakeSquare(Rect.X, Rect.Y, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_Window));
end;

procedure TGUILabel.Render;
begin
  if GetTextureLink = nil then
  begin
    RenderText;
    Exit;
  end;

  inherited;
end;

procedure TGUILabel.RenderText;
begin
  inherited;
  FFont.RenderText(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FText, Rect.Width, FWordWarp);
end;

procedure TGUILabel.SetFontEvent;
var AWidth, AHeight: Integer;
begin
  inherited;

  Font.GetTextRect(FText, AWidth, AHeight);
  Width := AWidth;
  Height:= AHeight;
  SetResize;
end;

procedure TGUILabel.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
end;

procedure TGUILabel.SetText(pText: String);
begin
  if SameText(FText, pText) then
    Exit;

  FText:= pText;
  SetFontEvent;
end;

end.
