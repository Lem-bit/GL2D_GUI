unit dlGUIListBox;

interface

uses SysUtils, Classes, Graphics, dlOpenGL, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIButton, dlGUIImage,
  dlGUITracker, dlGUIPopupMenu;

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
  TGUIListBoxItem = class(TGUIObject)
  strict private
    FText      : String;
    FSelected  : Boolean;
    FBrushColor: TColor;
    FOffsetX   : Integer;
    FOutText   : String;

    FSelectedColor: TColor; //Цвет если элемент выбран
  private
    procedure SetText(pText: String);
    procedure SetBrushColor(pValue: TColor);
  protected
    procedure SetFontEvent; override;
    procedure SetAreaResize; override;
   // procedure SetResize; override;
  public
    procedure SetTextureLink(pTextureLink: TTextureLink); override;
  public
    constructor Create(pText: String; pTextureLink: TTextureLink);
    procedure Render; override;
    procedure RenderText; override;
    procedure SetOffsetX(pValue: Integer);
  public
    property BrushColor   : TColor  read FBrushColor    write SetBrushColor;
    property SelectedColor: TColor  read FSelectedColor write FSelectedColor;
    property Text         : String  read FText          write SetText;
    property Selected     : Boolean read FSelected      write FSelected;
  end;

  TGUIListBox = class(TGUITrackerIntf)
  private
    const GR_MAIN = 1;  //Группа вершин
  private
    FClickOnTrack : Boolean; //Нажали на трекер

    FLineSpacing  : Integer; //Межстрочный интервал
    FBufMouse     : TMousePoint;
    FItemHeight   : Integer;

    FSelected     : Integer; //Выбранный элемент
    FYOffset      : Integer; //Сдвиг по Y
    FXOffset      : Integer; //Сдвиг по Х
    FItem         : TList;   //Список элементов

    FBrushColor   : TColor;
  private
    procedure SetMaxVertTracker;
    procedure SetMaxHorizTracker;

    function GetItem(index: integer): TGUIListBoxItem;
  private
    function ItemAt(pIndex: Integer): Boolean;
    function SelectItem(pCurrIndex, pIndex: Integer): Integer;

    procedure OnMoveHorizTracker(Sender: TObject; ParamObj: Pointer = nil);
    procedure OnMoveVertTracker(Sender: TObject; ParamObj: Pointer = nil);

    procedure SetLineSpacing(value: integer);
  protected
    procedure SetFontEvent; override;
    procedure SetResize; override;
    procedure SetHide(pHide: Boolean); override;
  public
    constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
    destructor Destroy; override;

    procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
    procedure OnMouseMove(pX, pY: Integer); override;
    procedure OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer); override;
    procedure OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer); override;

    procedure SetTextureLink(pTextureLink: TTextureLink); override;

    procedure Render; override;
  public
    OnElementSelect: TGUIProc;
    //Получить какой то другой активный popup например у ListBox
    function GetChildItemPopup: TGUIObject; override;
    procedure VisibleVTracker(pVisible: Boolean);
    procedure VisibleHTracker(pVisible: Boolean);
  public
    procedure Add(const pText: String);
    function IndexOf(const pText: String): Integer;
    procedure Delete(pIndex: Integer);
    function Count: integer;
    procedure Clear;

    procedure LoadFromFile(const AFileName: String);
    procedure SaveToFile(const AFileName: String);
  public
    property Selected: Integer     read FSelected;
    property BrushColor: TColor    read FBrushColor    write FBrushColor;
    property LineSpacing: Integer  read FLineSpacing   write SetLineSpacing;
    property Items[index: integer]: TGUIListBoxItem read GetItem;
  end;

implementation

{ TGUIListBox }

procedure TGUIListBox.Add(const pText: String);
var NewItem: TGUIListBoxItem;
begin
  NewItem:= TGUIListBoxItem.Create(pText, Self.GetTextureLink);
  NewItem.Font.CopyFrom(Self.Font);
  NewItem.BrushColor:= FBrushColor;
  FItem.Add(NewItem);
  SetMaxVertTracker;
  SetMaxHorizTracker;
end;

procedure TGUIListBox.Delete(pIndex: Integer);
var Item: TGUIListBoxItem;
    CID : Integer;
begin
  Item:= Items[pIndex];
  if not Assigned(Item) then
    Exit;

  SelectItem(FSelected, -1);
  FSelected:= -1;

  try
    Item.Free;
    FItem.Items[pIndex]:= nil;
  finally
    FItem.Pack;
  end;

  CID:= pIndex;
  if not Assigned(Items[CID]) then
    CID:= pIndex - 1;
  if not Assigned(Items[CID]) then
    CID:= -1;

  if Assigned(Items[CID]) then
  begin
    SelectItem(-1, CID);
    FSelected:= CID;
  end;

  SetMaxVertTracker;
  SetMaxHorizTracker;
end;

procedure TGUIListBox.Clear;
var i: integer;
begin
  try
    for i := 0 to FItem.Count - 1 do
      TGUIListBoxItem(FItem[i]).Free;

  finally
    FItem.Clear;
  end;
end;

function TGUIListBox.Count: integer;
begin
  Result:= 0;

  if Assigned(FItem) then
    Result:= FItem.Count;
end;

procedure TGUIListBox.SetFontEvent;
var i: integer;
    Item: TGUIListBoxItem;
begin
  inherited;
  FItemHeight:= Round(Font.Height) + 10;

  SetMaxVertTracker;

  if not Assigned(FItem) then
    Exit;

  HTracker.MaxValue:= 0;

  for i := 0 to FItem.Count - 1 do
  begin
    Item:= TGUIListBoxItem(FItem.Items[i]);

    Item.Font.CopyFrom(Self.Font);
    Item.FTextOffset.SetPos(1, Round(FItemHeight - Self.Font.Height) div 2);

    //Сразу пересчитываем макс ширину
    if HTracker.ShowTrack then
      if Item.Text.Length > HTracker.MaxValue then
        HTracker.MaxValue:= Item.Text.Length;

  end;
end;

procedure TGUIListBox.SetHide(pHide: Boolean);
begin
  inherited;
end;

procedure TGUIListBox.SetLineSpacing(value: integer);
begin
  FLineSpacing:= value;

  if FLineSpacing < 0 then
    FLineSpacing:= 0;
end;

procedure TGUIListBox.SetMaxHorizTracker;
begin
  HTracker.MaxValue:= 0;

  if Count < 1 then
    Exit;

  if Font.Scale < 0.1 then
    Exit;

  if HTracker.MaxValue < Items[Count - 1].Text.Length then
    HTracker.MaxValue:= Items[Count - 1].Text.Length;
end;

procedure TGUIListBox.SetMaxVertTracker;
begin
  if FItemHeight <> 0 then
    VTracker.MaxValue := FItem.Count - (FMaxHeight div (FItemHeight + FLineSpacing)) else
    VTracker.MaxValue := FItem.Count;
end;

constructor TGUIListBox.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink);
begin
  inherited Create(pName, gtcListBox, pX, pY, 200, 200, pTextureLink);

  FBorder       := 2;
  FYOffset      := 0;
  FXOffset      := 0;
  FLineSpacing  := 0;
  FItem         := TList.Create;
  FItem.Capacity:= 10000;
  FBrushColor   := $00202020;

  SetRect(pX, pY, 200, 200);

  //Область компонента
  VertexList.MakeSquare(0, 0, Rect.Width, Rect.Height, Color, GUIPalette.GetCellRect(pal_Frame));
  //Рамка
  VertexList.MakeSquare(FBorder, FBorder, Rect.Width - (FBorder * 2), Rect.Height - (FBorder * 2), Color, GUIPalette.GetCellRect(pal_2));
  VertexList.MakeSquare(FBorder, FBorder, FMaxWidth, FMaxHeight, Color, GUIPalette.GetCellRect(pal_0), GR_MAIN);
  VertexList.SetGroupColor(GR_MAIN, FBrushColor);

  VTracker.OnMove:= OnMoveVertTracker;
  HTracker.OnMove:= OnMoveHorizTracker;

  SetTextureLink(pTextureLink);
  SetResize; //Для трекеров
end;

destructor TGUIListBox.Destroy;
var i: integer;
begin
  if Assigned(FItem) then
  begin
    for i := 0 to FItem.Count - 1 do
      TGUIListBoxItem(FItem.Items[i]).Free;

    FreeAndNil(FItem);
  end;

  inherited;
end;

function TGUIListBox.GetChildItemPopup: TGUIObject;
begin
  Result:= nil;

  if not Assigned(Items[FSelected]) then Exit;
  if not Assigned(Items[FSelected].PopupMenu) then Exit;
  if FClickOnTrack then Exit;

  Result:= Items[FSelected].PopupMenu;
end;

function TGUIListBox.GetItem(index: integer): TGUIListBoxItem;
begin
  Result:= nil;

  if not ItemAt(index) then
    Exit;

  Result:= TGUIListBoxItem(FItem.Items[index]);

end;

procedure TGUIListBox.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  inherited;

  if not OnHit(pX, pY) then
    Exit;

  FClickOnTrack:= False;
  for i := Low(FTracker) to High(FTracker) do
  begin
    FTracker[i].OnMouseDown(pX, pY, Button);
    FClickOnTrack:= FTracker[i].OnHit(pX, pY) or FClickOnTrack;
  end;

  if (FClickOnTrack) then
    Exit;

  if not ItemAt(FYOffset) then
    Exit;

  for i := FYOffset to FItem.Count - 1 do
    if TGUIListBoxItem(FItem.Items[i]).OnHit(pX, pY) then
    begin
      FSelected:= SelectItem(FSelected, i);
      SetAction([goaItemSelect]);
      Break;
    end;

end;

procedure TGUIListBox.OnMouseMove(pX, pY: Integer);
var i: integer;
begin
  inherited;

  if not Assigned(FItem) then
    Exit;

  if not (goaFocused in GetAction) then
    Exit;

  FBufMouse.X:= pX;
  FBufMouse.Y:= pY;

  for i := FYOffset to FItem.Count - 1 do
    TGUIListBoxItem(FItem.Items[i]).OnMouseMove(pX, pY);
end;

procedure TGUIListBox.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
var i: integer;
begin
  inherited;

  if goaItemSelect in GetAction then
  begin
    if Assigned(OnElementSelect) then
       OnElementSelect(Self, @FSelected);

    RemoveAction([goaItemSelect]);
  end;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].OnMouseUp(pX, pY, Button);
end;

procedure TGUIListBox.OnMouseWheelDown(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  if not (goaFocused in GetAction) then
    Exit;

  VTracker.OnMouseWheelDown(Shift, MPosX, MPosY);
  SetAction([goaWhell]);
end;

procedure TGUIListBox.OnMouseWheelUp(Shift: TShiftState; MPosX, MPosY: Integer);
begin
  if not (goaFocused in GetAction) then
    Exit;

  VTracker.OnMouseWheelUp(Shift, MPosX, MPosY);
  SetAction([goaWhell]);
end;

procedure TGUIListBox.OnMoveHorizTracker(Sender: TObject; ParamObj: Pointer);
begin
  FXOffset:= Round(HTracker.GetTrackerPos);
end;

procedure TGUIListBox.OnMoveVertTracker(Sender: TObject; ParamObj: Pointer);
begin
  FYOffset:= VTracker.GetTrackerValue;
end;

procedure TGUIListBox.SaveToFile(const AFileName: String);
var Buf: TStringList;
    i: integer;
begin
  if Trim(AFileName) = '' then
    Exit;

  Buf:= TStringList.Create;
  try
    for i:= 0 to FItem.Count - 1 do
      Buf.Add(TGUIListBoxItem(FItem[i]).Text);

    Buf.SaveToFile(AFileName);
  finally
    FreeAndNil(Buf);
  end;

end;

function TGUIListBox.SelectItem(pCurrIndex, pIndex: Integer): Integer;
begin
  Result:= -1;

  if not ItemAt(pIndex) then
    Exit;

  //Отключаем старый выбор
  if ItemAt(pCurrIndex) and TGUIListBoxItem(FItem.Items[pCurrIndex]).Selected then
    TGUIListBoxItem(FItem.Items[pCurrIndex]).Selected:= False;

  TGUIListBoxItem(FItem.Items[pIndex]).Selected:= True;
  Result:= pIndex;
end;

procedure TGUIListBox.SetResize;
var AOffset: Integer;
begin
  AOffset:= 0;
  if not HTracker.Hide then
    AOffset:= HTracker.Size;

  //Изменение позиции и размера трекеров
  FMaxWidth := VTracker.Resize(Self.Rect, FBorder, AOffset);
  FMaxHeight:= HTracker.Resize(Self.Rect, FBorder);

  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 1, 1, Rect.Width - 2, Rect.Height - 2);
  VertexList.SetVertexPosSquare(8, FBorder, FBorder, FMaxWidth, FMaxHeight);
end;

procedure TGUIListBox.SetTextureLink(pTextureLink: TTextureLink);
var i: integer;
begin
  inherited;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].SetTextureLink(pTextureLink);
end;

function TGUIListBox.IndexOf(const pText: String): Integer;
var i: integer;
begin
  Result:= -1;

  for i := 0 to FItem.Count - 1 do
    if SameText(TGUIListBoxItem(FItem[i]).Text, pText) then
    begin
      Result:= i;
      Break;
    end;
end;

function TGUIListBox.ItemAt(pIndex: Integer): Boolean;
begin
  Result:= not ((FItem = nil) or (pIndex < 0) or (pIndex > FItem.Count - 1));
end;

procedure TGUIListBox.LoadFromFile(const AFileName: String);
var Buf: TStringList;
    i: integer;
begin
  if not FileExists(AFileName) then
    Exit;

  Buf:= TStringList.Create;
  try
    Clear;
    Buf.LoadFromFile(AFileName);

    for i := 0 to Buf.Count - 1 do
      Add(Buf.Text);

  finally
    FreeAndNil(Buf);
  end;

end;

procedure TGUIListBox.VisibleHTracker(pVisible: Boolean);
begin
  HTracker.Hide:= not pVisible;
  SetResize;
end;

procedure TGUIListBox.VisibleVTracker(pVisible: Boolean);
begin
  VTracker.Hide:= not pVisible;
  SetResize;
end;

procedure TGUIListBox.Render;
var i        : integer;
    TopPos   : Integer; //
    Index    : Integer;
    Item     : TGUIListBoxItem;
    ItemRect : TGUIObjectRect;
    Space    : Integer; //Расстояние между элементами
begin
  if FHide then
    Exit;

  inherited;

  //Номер элемента для прорисовки
  Index:= 0;

  ItemRect.X     := X + FBorder;
  ItemRect.Y     := 0;
  ItemRect.Width := FMaxWidth;
  ItemRect.Height:= FItemHeight;
  TopPos         := Y + FBorder;

  Space:= 0;
  //Прорисовка элементов
  for i := FYOffset to FItem.Count - 1 do
  begin
    Inc(Space, FLineSpacing);
    ItemRect.Y:= TopPos + (ItemRect.Height * Index) + Space;

    if ItemRect.Y + ItemRect.Height > Y + FMaxHeight then
      Break;

    Item:= TGUIListBoxItem(FItem.Items[i]);
    Item.SetOffsetX(FXOffset);
    Item.Rect.SetRect(ItemRect);
    Item.Render;

    if goaWhell in GetAction then
      Item.OnMouseMove(FBufMouse.X, FBufMouse.Y);

    Inc(Index, 1);
  end;

  for i := Low(FTracker) to High(FTracker) do
    FTracker[i].Render;

  if goaWhell in GetAction then
    RemoveAction([goaWhell]);
end;

{ TGUIListBoxItem }

constructor TGUIListBoxItem.Create(pText: String; pTextureLink: TTextureLink);
begin
  inherited Create;

  FText         := pText;
  FOutText      := pText;
  FSelected     := False;
  Area.Show     := True;
  FSelectedColor:= $004F4F4F;

{  Area.Color.SetColor($00002E5B);
  Area.Blend.Set_SrcAlpha_OneMinusSrcAlpha;
  Area.DrawMode     := GL_QUADS;
  Area.Speed        := 0.04;
  Area.AnimEnable   := True;}

  VertexList.MakeSquare(0, 0, 0, 0, Color, nil);
end;

procedure TGUIListBoxItem.Render;
begin
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);

  if FSelected then
    Color:= FSelectedColor
  else
    Color:= FBrushColor;

  inherited;

 // FRect.Render();
end;

procedure TGUIListBoxItem.RenderText;
begin
  inherited;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FOutText, Rect.Width);
end;

procedure TGUIListBoxItem.SetAreaResize;
begin
  inherited;

  if Assigned(PopupMenu) then
    Area.Show:= PopupMenu.Hide
  else
    Area.Show:= True;
end;

procedure TGUIListBoxItem.SetBrushColor(pValue: TColor);
begin
  Color      := pValue;
  FBrushColor:= pValue;
end;

procedure TGUIListBoxItem.SetFontEvent;
begin
  inherited;
  FTextOffset.SetPos(1, Round(Height - Font.Height) div 2);
end;

procedure TGUIListBoxItem.SetOffsetX(pValue: Integer);
begin
  if (FOffsetX = pValue) then
    Exit;

  FOffsetX:= pValue;
  FOutText:= Copy(FText, FOffsetX, Length(FText) - FOffsetX + 1);
end;

procedure TGUIListBoxItem.SetText(pText: String);
begin
  FText   := pText;
  FOutText:= Copy(FText, FOffsetX, Length(FText) - FOffsetX + 1);
end;

procedure TGUIListBoxItem.SetTextureLink(pTextureLink: TTextureLink);
begin
 //
end;

end.
