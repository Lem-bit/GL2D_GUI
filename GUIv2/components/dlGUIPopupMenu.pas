unit dlGUIPopupMenu;

interface

uses Classes, dlOpenGL, SysUtils, Graphics, dlGUIFont, dlGUITypes, dlGUITypesRTTI, dlGUIObject, dlGUIPaletteHelper;

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
  TGUIMenuItem = class(TGUIObject)
    private
      FMenuName       : String;  //Название меню
      FDisable        : Boolean; //Меню отключенное
      FMenuLine       : Boolean; //Для элемента --
      FMenuCaption    : Boolean; //Это заголовок
      FFontCustomColor: TColor;  //Кастомный цвет шрифта
      FBackgroundColor: TColor;  //Цвет бэкграунда

      //Для оптимизации прорисовки
      FDrawSelected   : Boolean; //Как прорисован элемент как выбранный или нет
    private
      function GetMenuName: String;
      //Переключить отображение вершин с обычного на выбранный и обратно
      procedure DrawMenuSelected(pSelected: Boolean);
      //Проверить можно ли установить фокус на меню
      function FocusDisable: Boolean;
      //Установить фоновый текст у меню
      procedure SetBackgroundColor(pColor: TColor);
    published
      procedure SetResize; override;
    public
      constructor Create(pName: String; pMenuName: String; pX, pY: Integer);
      destructor Destroy; override;
      procedure RenderText; override;
      procedure SetColorGradient(pVertexA, pVertexB, pVertexC, pVertexD: TColor);
    published
      property IsCaption      : Boolean          read FMenuCaption     write FMenuCaption;
      property Disable        : Boolean          read FDisable         write FDisable;
      property MenuName       : String           read GetMenuName      write FMenuName;
      property BackgroundColor: TColor           read FBackgroundColor write SetBackgroundColor;
  end;

  TGUIPopupMenu = class(TGUIObject)
    private
      FMenu      : TList;
      FSelected  : Integer; //Выбранный элемент
    private
      //Подготовить список меню
      function IsExists(pIndex: integer): Boolean;
      procedure PrepareMenuItems;
      function GetItem(pIndex: integer): TGUIMenuItem;
    protected
      procedure SetHide(pHide: Boolean); override;
      procedure SetFontEvent; override;
    public
      //Создать главное меню Popup
      constructor Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
      destructor Destroy; override;

      //Добавить элемент меню
      procedure Add(pMenuName: String; pDisable: Boolean = False; pProc: TGUIProc = nil; pColor: TColor = clWhite);
      //Удалить пункт меню
      procedure DeleteItem(pIndex: Integer); overload;
      procedure DeleteItem(pMenuName: String); overload;
      //Кол-во элементов меню
      function Count: integer;

      procedure SetTextureLink(pTextureLink: TTextureLink); override;
    public
      OnPopup: TGUIProc;

     // function RTTIGetPublishedListItem: String;
      function RTTIGetPublishedList(pIgnoreList: TStringList = nil): String; override;

      procedure OnMouseMove(pX, pY: Integer); override;
      procedure OnMouseDown(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure OnMouseUp(pX, pY: Integer; Button: TGUIMouseButton); override;
      procedure AfterObjRender; override;
      procedure RenderText; override;
      procedure Render; override;
    public
      property Item[index: integer]: TGUIMenuItem read GetItem;
    published
       property ObjectType;
       property Name;
       property Color;
       property Font;
       property TextureName;
       //классы
       property Blend;

  end;

implementation

const GROUP_MENU     = 0;
      GROUP_SELECTOR = 1;
      DIVIDE_LINE    = '--'; //Разделяющая линия

{ TGUIPopupMenu }

function TGUIPopupMenu.Count: integer;
begin
  Result:= FMenu.Count;
end;

constructor TGUIPopupMenu.Create(pName: String; pX, pY: Integer; pTextureLink: TTextureLink = nil);
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
    if SameText(pMenuName, Item[FID].FMenuName) then
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

procedure TGUIPopupMenu.Add(pMenuName: String; pDisable: Boolean = False; pProc: TGUIProc = nil; pColor: TColor = clWhite);
var Item: TGUIMenuItem;
begin
  Item:= TGUIMenuItem.Create(Self.Name + '.Menu', pMenuName, 0, 0);
  Item.FDisable        := pDisable;
  Item.FFontCustomColor:= pColor;
  Item.Color           := Color;
  Item.Parent          := Self;
  Item.OnClick         := pProc;
  Item.SetTextureLink(Self.GetTextureLink);
  Item.Font.SetTextureLink(Self.Font.GetTextureLink);
  FMenu.Add(Item);

  PrepareMenuItems;
end;

procedure TGUIPopupMenu.AfterObjRender;
begin
  inherited;

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
var FID : Integer;
    Item: TGUIMenuItem;
begin
  inherited;

  //Если этот же элемент то нет смысла искать какой выбран
  if IsExists(FSelected) then
    if TGUIMenuItem(FMenu.Items[FSelected]).OnHit(pX, pY) then Exit;

  FSelected:= -1;

  for FID := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu.Items[FID]);
    Item.DrawMenuSelected(False);

    if not Item.OnHit(pX, pY) then Continue;
    if FSelected > -1         then Continue;

    FSelected  := FID;

    if Item.FMenuLine    then Continue;
    if Item.FMenuCaption then Continue;

    Item.DrawMenuSelected(True);
  end;

  if FSelected < 0 then
    VertexList.SetGroupHide(GROUP_SELECTOR, True);
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

  //Ищем макс ширину текста
  for FID := 0 to FMenu.Count - 1 do
    with TGUIMenuItem(FMenu.Items[FID]) do
    begin
      ItemW:= Trunc(Font.GetTextWidth(FMenuName));
       if ItemW > MaxWidth then MaxWidth:= ItemW;
    end;

  //Пересчитываем все элементы меню
  for FID := 0 to FMenu.Count - 1 do
  begin
    Item:= TGUIMenuItem(FMenu.Items[FID]);

    //Назначение каких либо общих свойств
    if Self.GetTextureLink <> nil then
      Item.SetTextureLink(Self.GetTextureLink);

    //Размеры элемента
    Item.SetRect(
      Self.Rect.X,
      MaxHeight + Self.Rect.Y,
      MaxWidth + (Item.FTextOffset.X * 4),
      Round(Item.Font.Height) + (Item.FTextOffset.Y * 4)
    );

    if (Item.Blend.Alpha <> 0.0) then
      Item.Blend.Alpha:= Self.Blend.Alpha;

    if Item.FMenuLine then
      Item.Height:= Item.Height div 2;

    Item.VertexList.SetGroupHide(GROUP_MENU    , False);
    Item.VertexList.SetGroupHide(GROUP_SELECTOR, True);

    Inc(MaxHeight, Item.Height);

    Item.SetResize;
  end;

  Rect.SetSize(MaxWidth, MaxHeight);

end;

procedure TGUIPopupMenu.SetFontEvent;
var FID: Integer;
begin
  inherited;

  for FID := 0 to FMenu.Count - 1 do
    TGUIMenuItem(FMenu.Items[FID]).FFont.SetTextureLink(Self.Font.GetTextureLink);

  PrepareMenuItems;
end;

procedure TGUIPopupMenu.SetHide(pHide: Boolean);
begin
  inherited;
  FSelected:= -1;
end;

procedure TGUIPopupMenu.SetTextureLink(pTextureLink: TTextureLink);
begin
  inherited;
  PrepareMenuItems;
end;

procedure TGUIPopupMenu.Render;
var FID: Integer;
begin
  if FFont._State <> gfsNone then
    SetFontEvent;

  if FHide then
    Exit;

  for FID := 0 to FMenu.Count - 1 do
    TGUIMenuItem(FMenu.Items[FID]).Render;

end;

procedure TGUIPopupMenu.RenderText;
var FID: Integer;
begin
  inherited;

  for FID := 0 to FMenu.Count - 1 do
    with TGUIMenuItem(FMenu.Items[FID]) do
      RenderText;
end;

function TGUIPopupMenu.RTTIGetPublishedList(pIgnoreList: TStringList): String;
var FID: Integer;
    List: TStringList;
    Item: TGUIMenuItem;
begin
  Result:= '';
  List:= TStringList.Create;

  try
    //Узнаем все свойства родителя
    RTTIGetObjectPropList(Self, List);
    List.Add(objRTTI.RawFormat(rawItemList, Self));

    //Узнаем все свойства элементов
    for FID := 0 to FMenu.Count - 1 do
    begin
      Item:= TGUIMenuItem(FMenu.Items[FID]);

      List.Add(objRTTI.RawFormat(rawItem, Item));
      List.Add(Item.RTTIGetPublishedList());
      List.Add(Item.RTTIGetProcList);
      List.Add(objRTTI.RawFormat(rawEndItem, Item));
    end;

    List.Add(objRTTI.RawFormat(rawEndItemList, Self));
    Result:= List.Text;
    List.SaveToFile('.\published\' + Self.ClassName + '_' + TGUITypeDefNames[Self.ObjectType].Name + '.txt');
  finally
    List.Free;
  end;
end;

{ TGUIMenuItem }

constructor TGUIMenuItem.Create(pName, pMenuName: String; pX, pY: Integer);
begin
  inherited Create(pName);
  FMenuName:= pMenuName;
  FMenuLine:= SameText(FMenuName, DIVIDE_LINE);
  FTextOffset.SetPos(2, 2);

  if FMenuLine then
  begin
    FMenuName:= '';
    VertexList.MakeSquare(0, 0, 100, 25, Color, GUIPalette.GetCellRect(pal_PopupDiv), GROUP_MENU);
  end
  else
    VertexList.MakeSquare(0, 0, 100, 25, Color, GUIPalette.GetCellRect(pal_3), GROUP_MENU);

  VertexList.MakeSquare(0, 0, 100, 25, Color, GUIPalette.GetCellRect(pal_Frame), GROUP_SELECTOR, True);
end;

function TGUIMenuItem.FocusDisable: Boolean;
begin
  Result:= FDisable or FMenuLine or FMenuCaption;
end;

function TGUIMenuItem.GetMenuName: String;
begin
  if FMenuLine then
    Result:= DIVIDE_LINE
  else
    Result:= FMenuName;
end;

procedure TGUIMenuItem.RenderText;
begin
  inherited;

  if FDisable then
    Font.Color:= clGray
  else
    Font.Color:= FFontCustomColor;

  Font.Text(Rect.X + FTextOffset.X, Rect.Y + FTextOffset.Y, FMenuName);
end;

procedure TGUIMenuItem.SetBackgroundColor(pColor: TColor);
begin
  FBackgroundColor:= pColor;
  VertexList.SetGroupColor(GROUP_MENU, pColor);
end;

procedure TGUIMenuItem.SetColorGradient(pVertexA, pVertexB, pVertexC, pVertexD: TColor);
begin
  VertexList.SetColor(0, pVertexA);
  VertexList.SetColor(1, pVertexB);
  VertexList.SetColor(2, pVertexC);
  VertexList.SetColor(3, pVertexD);
end;

procedure TGUIMenuItem.SetResize;
begin
  inherited;
  VertexList.SetVertexPosSquare(0, 0, 0, Rect.Width, Rect.Height);
  VertexList.SetVertexPosSquare(4, 0, 0, Rect.Width, Rect.Height);
end;

destructor TGUIMenuItem.Destroy;
begin

  inherited;
end;

procedure TGUIMenuItem.DrawMenuSelected(pSelected: Boolean);
begin
  //Не переключаем лишний раз если элемент не поменял вид прорисовки
  if FDrawSelected = pSelected then
    Exit;

  FDrawSelected:= pSelected;
  VertexList.SetGroupHide(GROUP_MENU, pSelected);
  VertexList.SetGroupHide(GROUP_SELECTOR, not pSelected);
end;

end.
