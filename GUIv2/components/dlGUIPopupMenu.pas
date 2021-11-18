unit dlGUIPopupMenu;

interface

uses Classes, dlOpenGL, SysUtils, Graphics, dlGUIFont, dlGUITypes, dlGUIObject, dlGUIPaletteHelper, dlGUIXmlSerial;

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
  TGUIMenuItem = class(TGUIObject)
    strict private
      const DEFAULT_ITEM_HEIGHT = 21;
      const DEFAULT_LINE_HEIGHT = 10;
      const DIVIDE_LINE = '--'; //Разделяющая линия
    strict private
      FMenuName       : String;  //Название меню
      FMenuLine       : Boolean; //Для элемента --
      FFontCustomColor: TColor;  //Кастомный цвет шрифта
    private
      function GetMenuName: String;
      //Проверить можно ли установить фокус на меню
      function FocusDisable: Boolean;
    private
      property FontCustomColor: TColor read FFontCustomColor write FFontCustomColor;
    published
      procedure SetResize; override;
    public
      constructor Create(pName: String; pMenuName: String; pX, pY: Integer);
      destructor Destroy; override;

      function OnHit(pX, pY: Integer): Boolean; override;
      procedure RenderText; override;
    public
      [TXMLSerial] property MenuName: String  read GetMenuName write FMenuName;
  end;

  TGUIPopupMenu = class(TGUIObject)
    strict private
      FSelected: Integer; //Выбранный элемент
    strict private
      [TXMLSerial] FMenu: TList;
    strict private
      //Подготовить список меню
      function IsExists(pIndex: integer): Boolean;
      procedure PrepareMenuItems;
      function GetItem(pIndex: integer): TGUIMenuItem;
    protected
      procedure SetHide(pHide: Boolean); override;
      procedure SetFontEvent; override;
    public
      //Создать главное меню Popup
      constructor Create(pName: String = ''; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;

      //Добавить элемент меню
      procedure Add(pMenuName: String; pEnable: Boolean = True; pProc: TGUIProc = nil; pColor: TColor = clWhite);
      //Удалить пункт меню
      procedure DeleteItem(pIndex: Integer); overload;
      procedure DeleteItem(pMenuName: String); overload;
      //Кол-во элементов меню
      function Count: integer;
    public
      [TXMLSerial] OnPopup: TGUIProc;
    public
      //Установить ссылку на текстуру
      procedure SetTextureLink(pTextureLink: TTextureLink); override;

      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure Render; override;
    public
      property Item[index: integer]: TGUIMenuItem read GetItem;
  end;

implementation

{ TGUIPopupMenu }

function TGUIPopupMenu.Count: integer;
begin
  Result:= FMenu.Count;
end;

constructor TGUIPopupMenu.Create(pName: String = ''; pTextureLink: TTextureLink = nil);
begin
  inherited Create(pName, gtcPopupMenu);
  FMenu    := TList.Create;
  FSelected:= -1;
  FHide    := True;
  SetRect(0, 0, 0, 0);
end;

procedure TGUIPopupMenu.DeleteItem(pMenuName: String);
var FID: Integer;
    ID : Integer;
begin
  ID:= -1;

  for FID := 0 to FMenu.Count - 1 do
    if SameText(pMenuName, Item[FID].MenuName) then
    begin
      ID:= FID;
      Break;
    end;

  DeleteItem(ID);
end;

destructor TGUIPopupMenu.Destroy;
var i: integer;
begin
  if Assigned(FMenu) then
    for i := 0 to FMenu.Count - 1 do
      TGUIMenuItem(FMenu.Items[i]).Free;

  FreeAndNil(FMenu);
  inherited;
end;

procedure TGUIPopupMenu.DeleteItem(pIndex: Integer);
begin
  if not IsExists(pIndex) then
    Exit;

  Item[pIndex].Destroy;
  FMenu.Delete(pIndex);
  FMenu.Pack;
end;

function TGUIPopupMenu.GetItem(pIndex: integer): TGUIMenuItem;
begin
  Result:= nil;

  if not IsExists(pIndex) then
    Exit;

  Result:= TGUIMenuItem(FMenu.Items[pIndex]);
end;

function TGUIPopupMenu.IsExists(pIndex: integer): Boolean;
begin
  Result:= (pIndex > -1) and (pIndex < FMenu.Count);
end;

procedure TGUIPopupMenu.Add(pMenuName: String; pEnable: Boolean = True; pProc: TGUIProc = nil; pColor: TColor = clWhite);
var Item: TGUIMenuItem;
begin
  Item:= TGUIMenuItem.Create(Self.Name + '.Menu', pMenuName, 0, 0);
  Item.SetTextureLink(GetTextureLink);
  Item.Font.SetTextureLink(Font.GetTextureLink);
  Item.Enable         := pEnable;
  Item.FontCustomColor:= pColor;
  Item.Color          := Color;
  Item.Parent         := Self;
  Item.OnClick        := pProc;
  FMenu.Add(Item);
end;

procedure TGUIPopupMenu.OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton);
var MenuElement: TGUIMenuItem;
begin
  //Если меню где то раскрыто а мы нажимаем в другое место
  //Старое меню должно закрыться
  if not OnHit(pX, pY) then
    FHide:= True;

  if (Button = gmbRight) and (FHide) then
  begin

    if IsExists(FSelected) then
      Exit;

    if Assigned(OnPopup) then
      Self.OnPopup(Self, nil);

    RemoveAction([goaDown]);
    FSelected:= -1;
    Rect.SetPos(pX, pY);
    PrepareMenuItems;
    FHide:= False;
    Exit;
  end;

  if Hide then
    Exit;

  MenuElement:= Item[FSelected];

  if (MenuElement <> nil) and (MenuElement.FocusDisable) then
    Exit;

  //Реакция только на левую кнопку мыши
  //if Button = gmbLeft then
  SetAction([goaDown]);

end;

procedure TGUIPopupMenu.OnMouseMove(pX, pY: Integer);
var i: integer;
    Item: TGUIMenuItem;
begin
  if not Assigned(FMenu) then
    Exit;

  FSelected:= -1;
  for i := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu.Items[i]);
    Item.OnMouseMove(pX, pY);

    if Item.OnHit(pX, pY) then
      FSelected:= i;
  end;
end;

procedure TGUIPopupMenu.OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton);
begin
  if goaDown in FAction then
  begin
    if IsExists(FSelected) then
    begin
      if Item[FSelected].FocusDisable then
        Exit;

      if Assigned(Item[FSelected].OnClick) then
        Item[FSelected].OnClick(Item[FSelected], @FSelected);
    end;

    FSelected:= -1;
    FHide:= True;
  end;
end;

procedure TGUIPopupMenu.PrepareMenuItems;
var FID      : Integer;
    MaxWidth : Integer;
    MaxHeight: Integer;
    Item     : TGUIMenuItem;

    ItemW    : Integer;
begin
  MaxWidth:= 0;
  MaxHeight:= 0;

  if not Assigned(Parent) then
    Exit;

  //Ищем макс ширину текста
  for FID := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu.Items[FID]);
    if Item.Font.GetTextureLink = nil then
      Item.font.SetTextureLink(Self.Font.GetTextureLink);

    ItemW:= Trunc(Item.Font.GetTextWidth(Item.MenuName));
    if ItemW > MaxWidth then
      MaxWidth:= ItemW;
  end;

  MaxWidth:= MaxWidth + 4;

  //Пересчитываем все элементы меню
  for FID := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu.Items[FID]);

    //Размеры элемента
    Item.SetRect(
      Self.Rect.X,
      Self.Rect.Y + MaxHeight,
      MaxWidth,
      Item.Height
    );

    Inc(MaxHeight, Item.Height);
  end;

  Rect.SetSize(MaxWidth, MaxHeight);
  VertexList.SetRectSquare(0, Rect);

end;

procedure TGUIPopupMenu.SetFontEvent;
var FID: Integer;
begin
  inherited;

  for FID := 0 to FMenu.Count - 1 do
    TGUIMenuItem(FMenu.Items[FID]).FFont.SetTextureLink(Self.Font.GetTextureLink);
end;

procedure TGUIPopupMenu.SetHide(pHide: Boolean);
begin
  inherited;
  FSelected:= -1;
end;

procedure TGUIPopupMenu.SetTextureLink(pTextureLink: TTextureLink);
var i: integer;
    Item: TGUIMenuItem;
begin
  inherited;

  if not Assigned(FMenu) then
    Exit;

  for i := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu[i]);
    Item.SetTextureLink(pTextureLink);
  end;

end;

procedure TGUIPopupMenu.Render;
var FID: Integer;
    Item: TGUIMenuItem;
begin
  if FHide then
    Exit;

  if FFont._State <> gfsNone then
    SetFontEvent;

  for FID := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu.Items[FID]);
    Item.Render;
  end;

end;

{ TGUIMenuItem }

constructor TGUIMenuItem.Create(pName, pMenuName: String; pX, pY: Integer);
begin
  inherited Create(pName);
  FMenuLine := SameText(pMenuName, DIVIDE_LINE);
  FArea.Show:= not FMenuLine;

  FMenuName:= pMenuName;
  FArea.DrawMode:= GL_QUADS;
  FArea.Color.SetColor($00303030);
  FTextOffset.SetPos(2, 2);

  if FMenuLine then
  begin
    SetRect(0, 0, 0, DEFAULT_LINE_HEIGHT);
    VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_PopupDiv));
  end
  else
  begin
    SetRect(0, 0, 0, DEFAULT_ITEM_HEIGHT);
    VertexList.MakeSquare(Rect, Color, GUIPalette.GetCellRect(pal_3));
  end;

end;

function TGUIMenuItem.FocusDisable: Boolean;
begin
  Result:= (not FEnable) or FMenuLine;
end;

function TGUIMenuItem.GetMenuName: String;
begin
  if FMenuLine then
    Result:= DIVIDE_LINE
  else
    Result:= FMenuName;
end;

function TGUIMenuItem.OnHit(pX, pY: Integer): Boolean;
begin
  Result:=
     (pX >= FRect.X) and
     (pY >= FRect.Y) and
     (pX <= FRect.X + FRect.Width) and
     (pY <= FRect.Y + FRect.Height - 1);
end;

procedure TGUIMenuItem.RenderText;
begin
  inherited;

  if not FEnable then
    Font.Color:= clGray
  else
    Font.Color:= FFontCustomColor;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FMenuName, 0, True);
end;

procedure TGUIMenuItem.SetResize;
begin
  VertexList.SetSizeSquare(0, Rect);
end;

destructor TGUIMenuItem.Destroy;
begin

  inherited;
end;


end.
