unit dlGUIPanel;

interface

uses dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUIPanel = class(TGUIObject)
    strict private
      //Рамка
      FBorder: Integer;
    strict private
      procedure SetBorder(value: Integer);
    protected
      procedure SetResize; override;
    public
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
    public
      [TXMLSerial] property Border: Integer read FBorder write SetBorder;
  end;

implementation

{ TGUIPanel }

constructor TGUIPanel.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcObject);
  SetRect(0, 0, 200, 200);

  SetTextureLink(pTextureLink);

  FBorder:= 1;

  VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_Frame));
  VertexList.MakeSquareOffset(0, FBorder, Color, GUIPalette.GetCellRect(pal_Window));
end;

procedure TGUIPanel.SetBorder(value: Integer);
begin
  FBorder:= Value;
  if FBorder < 0 then
    FBorder:= 0;

  SetResize;
end;

procedure TGUIPanel.SetResize;
begin
  VertexList.SetSizeSquare(0, Rect);
  VertexList.SetSizeSquare(4, Rect, FBorder);
end;

end.
