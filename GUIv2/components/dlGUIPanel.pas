unit dlGUIPanel;

interface

uses dlGUITypes, dlGUIObject, dlGUIPaletteHelper;

type
  TGUIPanel = class(TGUIObject)
    private
      //Рамка
      FBorder: Integer;
    private
      procedure SetBorder(value: Integer);
      procedure ResizeBorder;
    protected
      procedure SetResize; override;
    public
      constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
    public
      property Border: Integer read FBorder write SetBorder;
  end;

implementation

{ TGUIPanel }

constructor TGUIPanel.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, gtcObject);

  SetRect(pX, pY, 200, 200);

  SetTextureLink(pTextureLink);

  FBorder:= 1;

  VertexList.MakeSquare(0, 0, Width, Height, Color, GUIPalette.GetCellRect(pal_6));
  VertexList.MakeSquare(FBorder, FBorder, Width - (FBorder * 2), Height - (FBorder * 2), Color, GUIPalette.GetCellRect(pal_Window));
end;

procedure TGUIPanel.ResizeBorder;
begin
  VertexList.SetVertexPosSquare(4, FBorder, FBorder, Rect.Width - (FBorder * 2), Rect.Height - (FBorder * 2));
end;

procedure TGUIPanel.SetBorder(value: Integer);
begin
  FBorder:= Value;
  if FBorder < 0 then
    FBorder:= 0;

  ResizeBorder;
end;

procedure TGUIPanel.SetResize;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Width, Height);
  ResizeBorder;
end;

end.
